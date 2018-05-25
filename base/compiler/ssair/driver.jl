using Core: LineInfoNode
const NullLineInfo = LineInfoNode(@__MODULE__, Symbol(""), Symbol(""), 0, 0)

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
include("compiler/ssair/inlining2.jl")
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

function normalize(@nospecialize(stmt), meta::Vector{Any}, table::Vector{LineInfoNode}, loc::RefValue{Int})
    if isa(stmt, Expr)
        if stmt.head == :meta
            args = stmt.args
            if length(args) > 0
                a1 = args[1]
                if a1 === :push_loc
                    let
                        current = loc[]
                        filename = args[2]::Symbol
                        methodname = NullLineInfo.method
                        mod = table[current].mod
                        line = 0
                        for i = 3:length(args)
                            ai = args[i]
                            if ai isa Symbol
                                methodname = ai
                            elseif ai isa Int32
                                line = Int(ai)
                            elseif ai isa Int64
                                line = Int(ai)
                            elseif ai isa Module
                                mod = ai
                            end
                        end
                        push!(table, LineInfoNode(mod, methodname, filename, line, current))
                        loc[] = length(table)
                    end
                elseif a1 === :pop_loc
                    n = (length(args) > 1) ? args[2]::Int : 1
                    for i in 1:n
                        current = loc[]
                        current = table[current].inlined_at
                        current == 0 && break
                        loc[] = current
                    end
                else
                    push!(meta, stmt)
                end
            end
            return nothing
        elseif stmt.head === :line
            return nothing # deprecated - we shouldn't encounter this
        else
            return normalize_expr(stmt)
        end
    elseif isa(stmt, LabelNode)
        return nothing
    elseif isa(stmt, LineNumberNode)
        let # need to expand this node so that it is source-location independent
            current = loc[]
            info = table[current]
            methodname = info.method
            mod = info.mod
            file = stmt.file
            file isa Symbol || (file = info.file)
            line = stmt.line
            push!(table, LineInfoNode(mod, methodname, file, line, info.inlined_at))
            loc[] = length(table)
        end
        return nothing
    end
    return stmt
end

function just_construct_ssa(ci::CodeInfo, code::Vector{Any}, nargs::Int, linetable::Vector{LineInfoNode})
    mod = linetable[1].mod
    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    idx = 1
    while idx <= length(code)
        stmt = code[idx]
        if isexpr(stmt, :(=))
            stmt = stmt.args[2]
        end
        if isa(stmt, Expr) && stmt.typ === Union{}
            if !(idx < length(code) && isexpr(code[idx+1], :unreachable))
                insert!(code, idx + 1, ReturnNode())
                idx += 1
            end
        end
        idx += 1
    end
    reindex_labels!(code) # update labels changed above

    inbounds_depth = 0 # Number of stacked inbounds
    meta = Any[]
    lines = fill(0, length(code))
    flags = fill(0x00, length(code))
    let loc = RefValue(1)
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
                stmt = normalize(stmt, meta, linetable, loc)
            end
            code[i] = stmt
            if !(stmt === nothing)
                lines[i] = loc[]
                if inbounds_depth > 0
                     flags[i] |= IR_FLAG_INBOUNDS
                end
            end
        end
    end
    code = strip_trailing_junk!(code, lines, flags)
    cfg = compute_basic_blocks(code)
    defuse_insts = scan_slot_def_use(nargs, ci, code)
    @timeit "domtree 1" domtree = construct_domtree(cfg)
    ir = let code = Any[nothing for _ = 1:length(code)]
             argtypes = ci.slottypes[1:(nargs+1)]
            IRCode(code, Any[], lines, flags, cfg, linetable, argtypes, mod, meta)
        end
    @timeit "construct_ssa" ir = construct_ssa!(ci, code, ir, domtree, defuse_insts, nargs)
    return ir
end

function run_passes(ci::CodeInfo, nargs::Int, linetable::Vector{LineInfoNode}, sv::OptimizationState)
    ir = just_construct_ssa(ci, copy(ci.code), nargs, linetable)
    #@Base.show ("after_construct", ir)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @timeit "compact 1" ir = compact!(ir)
    #@timeit "verify 1" verify_ir(ir)
    @timeit "Inlining" ir = ssa_inlining_pass!(ir, linetable, sv)
    #@timeit "verify 2" verify_ir(ir)
    @timeit "domtree 2" domtree = construct_domtree(ir.cfg)
    ir = compact!(ir)
    #@Base.show ("before_sroa", ir)
    @timeit "SROA" ir = getfield_elim_pass!(ir, domtree)
    #@Base.show ir.new_nodes
    #@Base.show ("after_sroa", ir)
    ir = adce_pass!(ir)
    #@Base.show ("after_adce", ir)
    @timeit "type lift" ir = type_lift_pass!(ir)
    @timeit "compact 3" ir = compact!(ir)
    #@Base.show ir
    @timeit "verify 3" (verify_ir(ir); verify_linetable(linetable))
    return ir
end
