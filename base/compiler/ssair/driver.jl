struct LineInfoNode
    mod::Module
    method::Symbol
    file::Symbol
    line::Int
    inlined_at::Int
end
const NullLineInfo = LineInfoNode(@__MODULE__, Symbol(""), Symbol(""), 0, 0)

include("compiler/ssair/ir.jl")
include("compiler/ssair/domtree.jl")
include("compiler/ssair/slot2ssa.jl")
include("compiler/ssair/queries.jl")
include("compiler/ssair/passes.jl")
include("compiler/ssair/verify.jl")
include("compiler/ssair/legacy.jl")

macro show(s)
    # return :(println($(QuoteNode(s)), " = ", $(esc(s))))
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
        elseif stmt.head === :gotoifnot
            return GotoIfNot(stmt.args...)
        elseif stmt.head === :return
            return ReturnNode((length(stmt.args) == 0 ? (nothing,) : stmt.args)...)
        elseif stmt.head === :unreachable
            return ReturnNode()
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

function run_passes(ci::CodeInfo, nargs::Int, linetable::Vector{LineInfoNode})
    mod = linetable[1].mod
    ci.code = copy(ci.code)
    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    idx = 1
    while idx <= length(ci.code)
        stmt = ci.code[idx]
        if isexpr(stmt, :(=))
            stmt = stmt.args[2]
        end
        if isa(stmt, Expr) && stmt.typ === Union{}
            if !(idx < length(ci.code) && isexpr(ci.code[idx+1], :unreachable))
                insert!(ci.code, idx + 1, ReturnNode())
                idx += 1
            end
        end
        idx += 1
    end
    reindex_labels!(ci.code)
    meta = Any[]
    lines = fill(0, length(ci.code))
    let loc = RefValue(1)
        for i = 1:length(ci.code)
            stmt = ci.code[i]
            stmt = normalize(stmt, meta, linetable, loc)
            ci.code[i] = stmt
            if !(stmt === nothing)
                lines[i] = loc[]
            end
        end
    end
    ci.code = strip_trailing_junk!(ci.code, lines)
    cfg = compute_basic_blocks(ci.code)
    defuse_insts = scan_slot_def_use(nargs, ci)
    domtree = construct_domtree(cfg)
    ir = let code = Any[nothing for _ = 1:length(ci.code)]
             argtypes = ci.slottypes[1:(nargs+1)]
            IRCode(code, lines, cfg, argtypes, mod, meta)
        end
    ir = construct_ssa!(ci, ir, domtree, defuse_insts, nargs)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    domtree = construct_domtree(ir.cfg)
    ir = compact!(ir)
    verify_ir(ir)
    ir = getfield_elim_pass!(ir, domtree)
    ir = compact!(ir)
    ir = type_lift_pass!(ir)
    ir = compact!(ir)
    verify_ir(ir)
    return ir
end
