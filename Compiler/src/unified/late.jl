# The experimental late pipeline (§5.7 follow-ups): typed IR that CARRIES
# closure regions. The consumer side of the UnifiedBackend's
# PRE-materialization region IR (`closure` ops + `cell_shared` cells, after
# the capture-decision fixpoint, before `materialize_closures!`): unified
# inference extended to descend into deferred regions and to type shared-cell
# contents as a structural fixpoint over every store site, home and deferred
# alike. The extension itself lives in `infer_ir!`/`transfers.jl`
# (`infer_closure!`, `closure_callee_transfer`, the poison discipline); this
# file is the query surface.
#
# What the pipeline delivers is PRECISION, not representation:
#
#   - visible closure call sites get the body's inferred return type
#     (zoo5b's `inc()` types as `Int64`, so the enclosing return does too);
#   - shared-cell reads refine to the content join — every store to the
#     variable is syntactically present in the enclosing IR, so
#     self-referential stores (`x = x + 1`) iterate to a fixpoint;
#   - deferred bodies are typed "as if", under param joins from visible call
#     sites.
#
# NO typed containers are produced, deliberately. It is not possible to type
# a generic closure's field: any later assignment in the closure body could
# read an updated world table inference knows nothing about, so the stored
# type is unknowable at the nominal type's (world-persistent) definition
# time — a typed field would bake an inference-world fact into the type and
# turn a semantically legal later-world store into a throw. Typed layouts
# are reserved for LATE-generated closures (the await mechanism's
# per-specialization continuations, §5.6), which do not outlive their
# inference world. The refinement discipline that keeps the in-world answer
# sound is enforced in the engine:
#
#   - ESCAPE: a closure value that flows anywhere but the callee position of
#     a visible call escapes; its params degrade to declared/`Any`, and the
#     cells it captures are poisoned — after materialization the closure's
#     mutable capture fields are untyped and settable by ANY holder, so
#     their reads are `Any` even when every visible store is monomorphic.
#   - WORLD: a `latestworld` barrier that may execute after a closure's
#     creation makes it unrefinable (`closure_shifted`): its body may run
#     against a newer method/binding table (§5.8 world-split discipline).
#     Redefinition within a world stays covered by the ordinary
#     backedge/invalidation contract.
#
# The content joins are still computed for poisoned cells (diagnostics and
# the future await consumer) — they are just never used for refinement.

"""
    typed_region_ir!(m, argtypes; state=UInferState()) -> NamedTuple

Late-pipeline inference query (§5.7 follow-ups): run descent-extended
`infer_ir!` over a PRE-materialization region-path method body — a
`UnifiedIR.IR` or anything with an `.ir` property (a UnifiedBackend
`LoweredMethod`). `argtypes` are lattice elements for region 1's args
(position 1 = the function itself, as everywhere in the port).

Mutates the IR (type/flag columns) like `infer_ir!` and returns

  - `ir`       — the typed region IR (closure ops and shared cells intact);
  - `rettype`  — the inferred return lattice element;
  - `cells`    — shared-cell content joins: `Dict` of cell stmt id =>
    (content = join, poisoned = Bool). Poisoned cells (a capturing closure
    escapes or is world-shifted, or the cell escapes as a value) read as
    `Any` in the typed IR; their join is diagnostic only.
  - `closures` — per-closure summaries: `Dict` of closure stmt id =>
    (rettype = body join, escaped, shifted).

No materialization happens here: containers are whatever lowering
produces (untyped — see the block comment for why), and execution
differentials run through the lowering path's materialized output.
"""
function typed_region_ir!(m, argtypes::Vector{Any}; state::UInferState = UInferState())
    ir = m isa UnifiedIR.IR ? m : m.ir
    rt = infer_ir!(ir, argtypes; state)
    content = get(ir.meta, :cell_content, Dict{Int32,Any}())::Dict{Int32,Any}
    crets = get(ir.meta, :closure_rets, Dict{Int32,Any}())::Dict{Int32,Any}
    escaped = get(ir.meta, :closure_escaped, Set{Int32}())::Set{Int32}
    shifted = get(ir.meta, :closure_shifted, Set{Int32}())::Set{Int32}
    poisoned = get(ir.meta, :poisoned_cells, Set{Int32}())::Set{Int32}
    cells = Dict{Int32,NamedTuple{(:content, :poisoned),Tuple{Any,Bool}}}()
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"cell_shared" || continue
        cells[s.id] = (content = widenucond(get(content, s.id, Union{})),
                       poisoned = s.id in poisoned)
    end
    closures = Dict{Int32,NamedTuple{(:rettype, :escaped, :shifted),Tuple{Any,Bool,Bool}}}()
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"closure" || continue
        closures[s.id] = (rettype = widenucond(get(crets, s.id, Union{})),
                          escaped = s.id in escaped,
                          shifted = s.id in shifted)
    end
    return (; ir, rettype = rt, cells, closures)
end
