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

function normalize(@nospecialize(stmt), meta::Vector{Any})
    if isa(stmt, Expr)
        if stmt.head === :meta
            args = stmt.args
            if length(args) > 0
                push!(meta, stmt)
            end
            return nothing
        end
    end
    return stmt
end

function convert_to_ircode(ci::CodeInfo, code::Vector{Any}, coverage::Bool, nargs::Int, sv::OptimizationState)
    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    idx = 1
    oldidx = 1
    changemap = fill(0, length(code))
    labelmap = coverage ? fill(0, length(code)) : changemap
    prevloc = zero(eltype(ci.codelocs))
    stmtinfo = sv.stmt_info
    while idx <= length(code)
        codeloc = ci.codelocs[idx]
        if coverage && codeloc != prevloc && codeloc != 0
            # insert a side-effect instruction before the current instruction in the same basic block
            insert!(code, idx, Expr(:code_coverage_effect))
            insert!(ci.codelocs, idx, codeloc)
            insert!(ci.ssavaluetypes, idx, Nothing)
            insert!(stmtinfo, idx, nothing)
            changemap[oldidx] += 1
            if oldidx < length(labelmap)
                labelmap[oldidx + 1] += 1
            end
            idx += 1
            prevloc = codeloc
        end
        if code[idx] isa Expr && ci.ssavaluetypes[idx] === Union{}
            if !(idx < length(code) && isa(code[idx + 1], ReturnNode) && !isdefined((code[idx + 1]::ReturnNode), :val))
                # insert unreachable in the same basic block after the current instruction (splitting it)
                insert!(code, idx + 1, ReturnNode())
                insert!(ci.codelocs, idx + 1, ci.codelocs[idx])
                insert!(ci.ssavaluetypes, idx + 1, Union{})
                insert!(stmtinfo, idx + 1, nothing)
                if oldidx < length(changemap)
                    changemap[oldidx + 1] += 1
                    coverage && (labelmap[oldidx + 1] += 1)
                end
                idx += 1
            end
        end
        idx += 1
        oldidx += 1
    end
    renumber_ir_elements!(code, changemap, labelmap)

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
    strip_trailing_junk!(ci, code, stmtinfo, flags)
    cfg = compute_basic_blocks(code)
    types = Any[]
    stmts = InstructionStream(code, types, stmtinfo, ci.codelocs, flags)
    ir = IRCode(stmts, cfg, collect(LineInfoNode, ci.linetable), sv.slottypes, meta, sv.sptypes)
    return ir
end

function slot2reg(ir::IRCode, ci::CodeInfo, nargs::Int, sv::OptimizationState)
    # need `ci` for the slot metadata, IR for the code
    defuse_insts = scan_slot_def_use(nargs, ci, ir.stmts.inst)
    @timeit "construct_ssa" ir = construct_ssa!(ci, ir, defuse_insts, nargs, sv.sptypes, sv.slottypes) # consumes `ir`
    return ir
end

function run_passes(ci::CodeInfo, nargs::Int, sv::OptimizationState)
    preserve_coverage = coverage_enabled(sv.mod)
    ir = convert_to_ircode(ci, copy_exprargs(ci.code), preserve_coverage, nargs, sv)
    ir = slot2reg(ir, ci, nargs, sv)
    #@Base.show ("after_construct", ir)
    @timeit "compact 1" ir = compact!(ir)
    @timeit "Inlining" ir = ssa_inlining_pass!(ir, ir.linetable, sv)
    #@timeit "verify 2" verify_ir(ir)
    ir = compact!(ir)
    #@Base.show ("before_sroa", ir)
    @timeit "SROA" ir = getfield_elim_pass!(ir)
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
