"""
    UnifiedIR

One IR data structure for the Julia compiler and external compilers: a flat
statement table with hybrid regions, layout states (dense / editable /
floating), a namespaced kind registry, extension-column universes, and
exactly two renaming points (`compact!`, `schedule!`).

Zero dependencies; testable without Julia semantics via the `test` dialect
and the textual format. See `unifiedir-design.md` for the specification.
"""
module UnifiedIR

export Kind, StmtId, RegionId, Value, Operand, IR, Builder, RemapSet,
    # kinds
    @K_str, register_dialect!, register_kind!, kindname, kindinfo,
    is_terminator, owns_regions, result_arity,
    OC_VALUE, OC_STMT, OC_REGION, OC_BLOCK, OC_CONST, OC_IMM, OC_ANY,
    FLAG_CONSISTENT, FLAG_EFFECT_FREE, FLAG_NOTHROW, FLAG_TERMINATES,
    FLAG_REMOVABLE, FLAG_PURE, FLAG_INLINE, FLAG_NOINLINE,
    # operands
    op_stmt, op_block, op_region, op_inline, vop, optag, op_value,
    # regions
    Region, RegionKind, Activation,
    REGION_BODY, REGION_ARM, REGION_GUARD, REGION_LOOP_BODY, REGION_HANDLER, REGION_BLOCK,
    ACT_IMMEDIATE, ACT_DEFERRED, ACT_RESUME,
    # builder
    append_stmt!, open_region!, open_guard_region!, close_region!, finish!,
    build_if!, build_loop!,
    # core API
    layout, generation, nstmts, nregions, getregion, root_region,
    stmt_kind, stmt_type, stmt_flag, stmt_region, set_type!, set_flag!, add_flag!,
    nops, getop, setop!, operands, comes_before, visible,
    each_stmt, region_stmts, region_terminator,
    # dense mutation
    replace_stmt!, delete_stmt!, replace_uses!, flush_renames!,
    # editable
    editable, insert_before!, insert_after!, push_stmt!,
    wrap_in_if!, wrap_in_loop!, inline_region!, splice_body!, compact!,
    # floating
    float!, schedule!, CausalityError,
    # columns
    DenseCol, SparseCol, DictColumns, ProvenanceCol, Semantic, Annotation, Derived,
    hasrefs, remap_refs!, semclass, convert_universe,
    # AttrGraph substrate + generic tree porcelain (§3.7 Level 1)
    AttrGraph, compact_graph!, collect_syntax!, Tree, NodeList,
    # verification / analyses
    verify_ir, VerifyError, use_counts, AnalysisCache,
    # passes
    dce!, promote_cells!, fold_constant_branches!,
    # cell promotion (the mem2reg suite, promote.jl). The individual join
    # passes stay unexported (qualified access) so providers may bind
    # same-named lattice-aware wrappers; the driver is the public entry.
    promote_fixpoint!,
    # text
    print_ir, parse_ir, struct_eq, display_maxlines!,
    # test dialect interpreter
    interpret

include("kinds.jl")
include("operands.jl")
include("columns.jl")
include("attrgraph.jl")
include("core.jl")
include("tree.jl")
include("refs.jl")
include("builder.jl")
include("stmts.jl")
include("verify.jl")
include("dense.jl")
include("editable.jl")
include("surgery.jl")
include("compact.jl")
include("floating.jl")
include("analysis.jl")
include("passes.jl")
include("promote.jl")
include("testdialect.jl")
include("print.jl")
include("parse.jl")
include("interp.jl")

function __init__()
    # Session-local kind numbering: re-register the test dialect on load.
    # The Base-baked instance (bootstrap substrate for JuliaSyntax) keeps its
    # registry clean of the test dialect; the loadable package registers it
    # (its own test suite and the textual-format tests use it).
    if parentmodule(@__MODULE__) !== Base
        register_test_dialect!()
    end
end

end # module UnifiedIR
