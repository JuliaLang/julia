# This file is a part of Julia. License is MIT: https://julialang.org/license

function inflate_ir(ci::CodeInfo, linfo::MethodInstance)
    sptypes = sptypes_from_meth_instance(linfo)
    if ci.inferred
        argtypes, _ = matching_cache_argtypes(linfo, nothing)
    else
        argtypes = Any[ Any for i = 1:length(ci.slotflags) ]
    end
    return inflate_ir(ci, sptypes, argtypes)
end

function inflate_ir(ci::CodeInfo, sptypes::Vector{Any}, argtypes::Vector{Any})
    code = copy_exprargs(ci.code) # TODO: this is a huge hot-spot
    for i = 1:length(code)
        if isa(code[i], Expr)
            code[i] = normalize_expr(code[i])
        end
    end
    cfg = compute_basic_blocks(code)
    for i = 1:length(code)
        stmt = code[i]
        urs = userefs(stmt)
        for op in urs
            val = op[]
            if isa(val, SlotNumber)
                op[] = Argument(val.id)
            end
        end
        stmt = urs[]
        # Translate statement edges to bb_edges
        if isa(stmt, GotoNode)
            code[i] = GotoNode(block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            code[i] = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
        elseif isa(stmt, PhiNode)
            code[i] = PhiNode(Any[block_for_inst(cfg, edge) for edge in stmt.edges], stmt.values)
        elseif isa(stmt, Expr) && stmt.head === :enter
            stmt.args[1] = block_for_inst(cfg, stmt.args[1])
            code[i] = stmt
        else
            code[i] = stmt
        end
    end
    ssavaluetypes = ci.ssavaluetypes isa Vector{Any} ? copy(ci.ssavaluetypes) : Any[ Any for i = 1:(ci.ssavaluetypes::Int) ]
    ir = IRCode(code, ssavaluetypes, copy(ci.codelocs), copy(ci.ssaflags), cfg, collect(LineInfoNode, ci.linetable),
                argtypes, Any[], sptypes)
    return ir
end

function replace_code_newstyle!(ci::CodeInfo, ir::IRCode, nargs::Int)
    @assert isempty(ir.new_nodes)
    # All but the first `nargs` slots will now be unused
    resize!(ci.slotflags, nargs+1)
    ci.code = ir.stmts
    ci.codelocs = ir.lines
    ci.linetable = ir.linetable
    ci.ssavaluetypes = ir.types
    ci.ssaflags = ir.flags
    for metanode in ir.meta
        push!(ci.code, metanode)
        push!(ci.codelocs, 1)
        push!(ci.ssavaluetypes, Any)
        push!(ci.ssaflags, 0x00)
    end
    # Translate BB Edges to statement edges
    # (and undo normalization for now)
    for i = 1:length(ci.code)
        stmt = ci.code[i]
        urs = userefs(stmt)
        for op in urs
            val = op[]
            if isa(val, Argument)
                op[] = SlotNumber(val.n)
            end
        end
        stmt = urs[]
        if isa(stmt, GotoNode)
            ci.code[i] = GotoNode(first(ir.cfg.blocks[stmt.label].stmts))
        elseif isa(stmt, GotoIfNot)
            ci.code[i] = Expr(:gotoifnot, stmt.cond, first(ir.cfg.blocks[stmt.dest].stmts))
        elseif isa(stmt, PhiNode)
            ci.code[i] = PhiNode(Any[last(ir.cfg.blocks[edge].stmts) for edge in stmt.edges], stmt.values)
        elseif isa(stmt, ReturnNode)
            if isdefined(stmt, :val)
                ci.code[i] = Expr(:return, stmt.val)
            else
                ci.code[i] = Expr(:unreachable)
            end
        elseif isa(stmt, Expr) && stmt.head === :enter
            stmt.args[1] = first(ir.cfg.blocks[stmt.args[1]].stmts)
            ci.code[i] = stmt
        else
            ci.code[i] = stmt
        end
    end
end

# used by some tests
inflate_ir(ci::CodeInfo) = inflate_ir(ci, Any[], Any[ Any for i = 1:length(ci.slotflags) ])
