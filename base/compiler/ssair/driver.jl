# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: LineInfoNode

if false
    import Base: Base, @show
else
    macro show(s)
        return :(println(stdout, $(QuoteNode(s)), " = ", $(esc(s))))
    end
end

include("compiler/ssair/ir.jl")
include("compiler/ssair/domtree.jl")
include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/queries.jl")
include("compiler/ssair/passes.jl")
include("compiler/ssair/inlining.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")
#@isdefined(Base) && include("compiler/ssair/show.jl")

function normalize_expr(stmt::Expr)
    if stmt.head === :gotoifnot
        return GotoIfNot(stmt.args[1], stmt.args[2]::Int)
    elseif stmt.head === :return
        return (length(stmt.args) == 0) ? ReturnNode(nothing) : ReturnNode(stmt.args[1])
    elseif stmt.head === :unreachable
        return ReturnNode()
    else
        return stmt
    end
end

function normalize(@nospecialize(stmt), meta::Vector{Any})
    if isa(stmt, Expr)
        if stmt.head == :meta
            args = stmt.args
            if length(args) > 0
                push!(meta, stmt)
            end
            return nothing
        elseif stmt.head === :line
            return nothing # deprecated - we shouldn't encounter this
        else
            return normalize_expr(stmt)
        end
    end
    return stmt
end

function just_construct_ssa(ci::CodeInfo, code::Vector{Any}, nargs::Int, sv::OptimizationState)
    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    idx = 1
    oldidx = 1
    changemap = fill(0, length(code))
    while idx <= length(code)
        if code[idx] isa Expr && ci.ssavaluetypes[idx] === Union{}
            if !(idx < length(code) && isexpr(code[idx+1], :unreachable))
                insert!(code, idx + 1, ReturnNode())
                insert!(ci.codelocs, idx + 1, ci.codelocs[idx])
                insert!(ci.ssavaluetypes, idx + 1, Union{})
                if oldidx < length(changemap)
                    changemap[oldidx + 1] = 1
                end
                idx += 1
            end
        end
        idx += 1
        oldidx += 1
    end
    renumber_ir_elements!(code, changemap)

    inbounds_depth = 0 # Number of stacked inbounds
    meta = Any[]
    flags = fill(0x00, length(code))
    for i = 1:length(code)
        stmt = code[i]
        if isexpr(stmt, :inbounds)
            arg1 = stmt.args[1]
            if arg1 === true # push
                inbounds_depth += 1
            elseif arg1 === false # clear
                inbounds_depth = 0
            elseif inbounds_depth > 0 # pop
                inbounds_depth -= 1
            end
            stmt = nothing
        else
            stmt = normalize(stmt, meta)
        end
        code[i] = stmt
        if !(stmt === nothing)
            if inbounds_depth > 0
                flags[i] |= IR_FLAG_INBOUNDS
            end
        end
    end
    strip_trailing_junk!(ci, code, flags)
    cfg = compute_basic_blocks(code)
    defuse_insts = scan_slot_def_use(nargs, ci, code)
    @timeit "domtree 1" domtree = construct_domtree(cfg)
    ir = let code = Any[nothing for _ = 1:length(code)]
            IRCode(code, Any[], ci.codelocs, flags, cfg, collect(LineInfoNode, ci.linetable), sv.slottypes, meta, sv.sptypes)
        end
    @timeit "construct_ssa" ir = construct_ssa!(ci, code, ir, domtree, defuse_insts, nargs, sv.sptypes, sv.slottypes)
    return ir
end

function run_passes(ci::CodeInfo, nargs::Int, sv::OptimizationState)
    ir = just_construct_ssa(ci, copy_exprargs(ci.code), nargs, sv)
    #@Base.show ("after_construct", ir)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @timeit "compact 1" ir = compact!(ir)
    @timeit "Inlining" ir = ssa_inlining_pass!(ir, ir.linetable, sv)
    #@timeit "verify 2" verify_ir(ir)
    ir = compact!(ir)
    #@Base.show ("before_sroa", ir)
    @timeit "domtree 2" domtree = construct_domtree(ir.cfg)
    @timeit "SROA" ir = getfield_elim_pass!(ir, domtree)
    #@Base.show ir.new_nodes
    #@Base.show ("after_sroa", ir)
    ir = adce_pass!(ir)
    #@Base.show ("after_adce", ir)
    @timeit "type lift" ir = type_lift_pass!(ir)
    @timeit "compact 3" ir = compact!(ir)
    #@Base.show ir
    if JLOptions().debug_level == 2
        @timeit "verify 3" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    return ir
end
