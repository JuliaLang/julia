"""
    JuliaLowering.UnifiedBackend

The UnifiedIR direct-lowering backend: port of JuliaLowering's backend targeting UnifiedIR (§10.1 of the UnifiedIR
design): Julia lowering emitting structured region IR directly, with no
goto-based linear IR detour.

The pipeline reuses JuliaLowering's own front half unchanged — macro expansion
(`expand_forms_1`), desugaring (`expand_forms_2`), scope analysis
(`resolve_scopes`) — and replaces the final linearization pass
(`linearize_ir` / `compile_lambda` in `linear_ir.jl`) with a tree→region
emitter (`src/emit.jl`) producing `UnifiedIR.IR`:

  - structured source → `if` / `loop` / `try` region ops (no gotos)
  - variables → region args (arguments), plain SSA values (dominating
    single-assignment locals), or frame-class cells (§6) with explicit
    undef vocabulary (`cell_isdefined` / `throw_undef_if_not`)
  - `while` → `loop` with body-initial condition test; `break`/`continue` →
    sealed core exit terminators targeting the loop body region
  - `symbolicblock` (JuliaLowering's break-block) → single-iteration loop
    (the WebAssembly-block pattern of §5.9)
  - `try`/`catch` → `try` op with a `REGION_HANDLER` region whose leading
    region arg is the exception
  - supported local functions → REAL `closure` region ops (§5.7): the
    deferred body holds the lambda, captured locals become `cell_shared`
    cells, the shared promotion fixpoint (`promote_capture_cells!`) decides
    value-vs-shared capture structurally, and `materialize.jl` turns
    residual closure regions back into runtime closure types (value fields
    type-parameterized; shared cells `Core.Box`) plus extracted method IRs

# Covered forms (post-desugar kinds, mirroring linear_ir.jl's `compile`)

literals / `Value` / `Symbol` / `SourceLocation` / `inert` (constant pool);
`top` / `core` (GLOBAL operands); global variables (`globalref` statements —
reads stay ordered because they can throw); `call` / `new` / `splatnew`;
splat via the pre-desugared `Core._apply_iterate` call; `=` / `constdecl`
(globals via `setglobal!` / `declare_const` calls); `block` / `scope_block`;
`if` / `elseif`; condition-position `&&` / `||` (nested value-`if`s);
`_while` / `_do_while` / `symbolicblock` / `break` (loops, `break`,
`continue`, valued block breaks); `return`; `trycatchelse` (try/catch);
`tryfinally` without crossing exits; `isdefined` / `newvar` /
`throw_undef_if_not` (cell undef vocabulary); `boundscheck`,
`gc_preserve_begin/end`, `latestworld`, `copyast`; `meta` / `inbounds` /
`inline` / `noinline` / `purity` / `loopinfo` are dropped (flag/column
material); `removable`, `unused_only`, `TOMBSTONE`, `Placeholder`;
static parameters (SPARAM operands); method extraction from top-level thunks.

# Unsupported in v1 (throws `UnsupportedForm`, never mis-lowers)

  - `@label` / `@goto` (`symboliclabel` / `symbolicgoto`): would need the
    `K"cfg"` island form
  - `tryfinally` with a `return`/`break` crossing it (linear_ir's tag-based
    finally lowering is not ported; simple finally works), `tryfinally` with
    a dynamic-scope operand (`@with`), and `try`/`catch`/`else`
  - `foreigncall` / `cfunction` / `static_eval` (ccall and friends)
  - `opaque_closure_method` / `new_opaque_closure` (opaque closures stay
    inline in the method body), and `captured_local` (locals captured into
    global methods)

Closure-region v1 bails (the statement falls back to the EAGER path through
`convert_closures`, which lowers these exactly as before): multi-method
local functions and kwargs closures, local methods with `where` static
parameters, varargs local functions, declared return types on local
functions, closure signatures referencing locals, and closures capturing an
enclosing frame's static parameter.

The emitter additionally has a capture-analysis mode over
PRE-closure-conversion trees (emit.jl `AnalysisState`, driven by
../capture_analysis.jl): throwaway IR with closure-region capture
FOOTPRINTS, from which `analyze_captures_precise!` reads the
mem2reg-precise boxing verdicts that the eager path's `convert_closures`
then applies.

Public API: [`lower_to_ir`](@ref), [`LoweredMethod`](@ref),
[`UnsupportedForm`](@ref).
"""
module UnifiedBackend

# Bindings resolve through the enclosing JuliaLowering (dual-mode safe: the
# same JuliaSyntax instance JuliaLowering runs on, and hence the same
# UnifiedIR instance JuliaSyntax registered its kinds with).
const JuliaLowering = Base.parentmodule(@__MODULE__)
const JuliaSyntax = JuliaLowering.JuliaSyntax
const UnifiedIR = JuliaSyntax.UnifiedIR

using .UnifiedIR: Builder, append_stmt!, open_region!, close_region!, finish!,
    verify_ir, StmtId, RegionId, Operand, op_stmt, op_region,
    op_inline, vop, REGION_ARM, REGION_HANDLER, REGION_LOOP_BODY

using .JuliaSyntax: SyntaxTree, kind, children, numchildren, is_leaf, @K_str

using .JuliaLowering: expand_forms_1, expand_forms_2, resolve_scopes,
    convert_closures, Bindings, BindingInfo,
    eval_closure_type, reserve_module_binding_i

export lower_to_ir, LoweredMethod, UnsupportedForm

"""
    UnsupportedForm(form, detail)

Thrown when the emitter meets a (desugared) syntax form it does not support.
Per the fidelity rule, unsupported constructs error instead of mis-lowering.
"""
struct UnsupportedForm <: Exception
    form::String
    detail::String
end
UnsupportedForm(form) = UnsupportedForm(form, "")
function Base.showerror(io::IO, e::UnsupportedForm)
    print(io, "UnsupportedForm: ", e.form)
    isempty(e.detail) || print(io, " — ", e.detail)
end

"""
    LoweredMethod(name, nargs, slotnames, ir)

One lowered method body. `nargs` counts all region-1 arguments including the
implicit `#self#` (so `interpret(m.ir, self, args...)` takes `nargs` values);
`slotnames` are the argument names; `ir` is the sealed, verified
`UnifiedIR.IR`.
"""
struct LoweredMethod
    name::Symbol
    nargs::Int
    slotnames::Vector{Symbol}
    ir::UnifiedIR.IR
end

Base.show(io::IO, m::LoweredMethod) =
    print(io, "LoweredMethod(", m.name, ", nargs=", m.nargs, ", ",
          UnifiedIR.nstmts(m.ir), " stmts)")

include("emit.jl")
include("materialize.jl")

# Diagnostic path counters: top-level statements lowered through the
# closure-region path vs the eager `convert_closures` fallback. Tests read
# them to assert coverage; `reset_path_counts!()` zeroes both.
const REGION_STMTS = Ref(0)
const EAGER_STMTS = Ref(0)
reset_path_counts!() = (REGION_STMTS[] = 0; EAGER_STMTS[] = 0; nothing)

"""
    lower_to_ir(mod::Module, src::String; filename="none") -> Vector{LoweredMethod}

Parse `src`, drive JuliaLowering's front half (macro expansion → desugaring →
scope analysis) unchanged, then lower each method lambda directly to
`UnifiedIR.IR` (structured region form, no goto detour).

The DEFAULT path consumes the PRE-closure-conversion tree: supported local
functions become real `closure` region ops, captured locals become
`cell_shared` plans, the shared promotion fixpoint decides captures
structurally, and residual closure regions are materialized back to runtime
closure types + extracted method IRs (materialize.jl). A top-level statement
containing any v1-bailed construct (see emit.jl's region-mode block comment)
falls back — loudly, per the fidelity rule — to the eager path through
`convert_closures`, which lowers exactly as before. Every produced IR is
checked with `UnifiedIR.verify_ir(ir; level=1)` before being returned.
"""
function lower_to_ir(mod::Module, src::String; filename::AbstractString = "none")
    out = LoweredMethod[]
    st0 = JuliaSyntax.parseall(SyntaxTree, src; filename = String(filename))
    stmts = kind(st0) == K"toplevel" ? collect(children(st0)) : [st0]
    world = Base.get_world_counter()
    for st in stmts
        ctx1, ex1 = expand_forms_1(mod, st, false, world)
        ctx2, ex2 = expand_forms_2(ctx1, ex1)
        ctx3, ex3 = resolve_scopes(ctx2, ex2)
        ms = LoweredMethod[]
        try
            collect_methods_region!(ms, ctx3, ex3)
            REGION_STMTS[] += 1
        catch err
            err isa UnsupportedForm || rethrow()
            # per-statement fallback to the eager (convert_closures) path
            empty!(ms)
            ctx4, ex4 = convert_closures(ctx3, ex3)
            collect_methods!(ms, ctx4, ex4)
            EAGER_STMTS[] += 1
        end
        append!(out, ms)
    end
    for m in out
        verify_ir(m.ir; level = 1)
    end
    return out
end

# Walk a top-level thunk's body extracting `K"method"` definition forms
# (`[K"method" fname metadata lambda]`) and lower each lambda. This is the
# "handle enough of the toplevel thunk to extract per-method bodies" mode:
# the thunk's other side effects (method table mutation, const decls) are not
# executed here.
function collect_methods!(out::Vector{LoweredMethod}, jlctx, ex)
    k = kind(ex)
    if k == K"method" && numchildren(ex) == 3 && kind(ex[3]) == K"lambda"
        push!(out, emit_method(jlctx, ex))
        return nothing
    end
    if k == K"lambda"
        # descend only into top-level thunks; method lambdas are lowered via
        # their enclosing K"method" form
        if ex.is_toplevel_thunk
            collect_methods!(out, jlctx, ex[3])
        end
        return nothing
    end
    is_leaf(ex) && return nothing
    (k == K"inert" || k == K"inert_syntaxtree" || k == K"quote") && return nothing
    for c in children(ex)
        collect_methods!(out, jlctx, c)
    end
    return nothing
end

# The region-path collector walks the PRE-conversion thunk: top-frame method
# forms lower through the closure-region path (nested closures are handled
# inside emit_method_region!, not lifted to toplevel yet).
function collect_methods_region!(out::Vector{LoweredMethod}, jlctx, ex)
    k = kind(ex)
    if k == K"method" && numchildren(ex) == 3 && kind(ex[3]) == K"lambda"
        emit_method_region!(out, jlctx, ex)
        return nothing
    end
    if k == K"lambda"
        if ex.is_toplevel_thunk
            collect_methods_region!(out, jlctx, ex[3])
        end
        return nothing
    end
    is_leaf(ex) && return nothing
    (k == K"inert" || k == K"inert_syntaxtree" || k == K"quote") && return nothing
    for c in children(ex)
        collect_methods_region!(out, jlctx, c)
    end
    return nothing
end

end # module UnifiedBackend
