# This file is a part of Julia. License is MIT: https://julialang.org/license

function inflate_ir(ci::CodeInfo, linfo::MethodInstance)
    sptypes = sptypes_from_meth_instance(linfo)
    if ci.inferred
        argtypes, _ = matching_cache_argtypes(linfo, nothing, false)
    else
        argtypes = LatticeElement[ ⊤ for i = 1:length(ci.slotflags) ]
    end
    return inflate_ir(ci, sptypes, argtypes)
end

function inflate_ir(ci::CodeInfo, sptypes::Argtypes, argtypes::Argtypes)
    code = copy_exprargs(ci.code) # TODO: this is a huge hot-spot
    cfg = compute_basic_blocks(code)
    for i = 1:length(code)
        stmt = code[i]
        # Translate statement edges to bb_edges
        if isa(stmt, GotoNode)
            code[i] = GotoNode(block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            code[i] = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
        elseif isa(stmt, PhiNode)
            code[i] = PhiNode(Int32[block_for_inst(cfg, Int(edge)) for edge in stmt.edges], stmt.values)
        elseif isa(stmt, Expr) && stmt.head === :enter
            stmt.args[1] = block_for_inst(cfg, stmt.args[1]::Int)
            code[i] = stmt
        end
    end
    nstmts = length(code)
    ssavaluetypes = let ssavaluetypes = ci.ssavaluetypes
        if isa(ssavaluetypes, SSAValueTypes)
            # NOTE thes `ssavaluetypes` have been widened
            LatticeElement[ NativeType(ssavaluetypes[i]) for i in 1:length(ssavaluetypes) ]
        else
            LatticeElement[ ⊤ for _ in 1:(ssavaluetypes::Int) ]
        end
    end
    stmts = InstructionStream(code, ssavaluetypes, Any[nothing for i = 1:nstmts], copy(ci.codelocs), copy(ci.ssaflags))
    ir = IRCode(stmts, cfg, collect(LineInfoNode, ci.linetable), argtypes, Any[], sptypes)
    return ir
end

function replace_code_newstyle!(ci::CodeInfo, ir::IRCode, nargs::Int)
    @assert isempty(ir.new_nodes)
    # All but the first `nargs` slots will now be unused
    resize!(ci.slotflags, nargs)
    stmts = ir.stmts
    ci.code, ci.codelocs, ci.ssaflags, ci.linetable =
        stmts.inst, stmts.line, stmts.flag, ir.linetable
    resize!(ci.ssavaluetypes::SSAValueTypes, length(stmts.type))
    copy!(ci.ssavaluetypes::SSAValueTypes, stmts.type)
    for metanode in ir.meta
        push!(ci.code, metanode)
        push!(ci.codelocs, 1)
        push!(ci.ssavaluetypes::SSAValueTypes, ⊤)
        push!(ci.ssaflags, IR_FLAG_NULL)
    end
    # Translate BB Edges to statement edges
    # (and undo normalization for now)
    for i = 1:length(ci.code)
        stmt = ci.code[i]
        if isa(stmt, GotoNode)
            stmt = GotoNode(first(ir.cfg.blocks[stmt.label].stmts))
        elseif isa(stmt, GotoIfNot)
            stmt = GotoIfNot(stmt.cond, first(ir.cfg.blocks[stmt.dest].stmts))
        elseif isa(stmt, PhiNode)
            stmt = PhiNode(Int32[last(ir.cfg.blocks[edge].stmts) for edge in stmt.edges], stmt.values)
        elseif isa(stmt, Expr) && stmt.head === :enter
            stmt.args[1] = first(ir.cfg.blocks[stmt.args[1]::Int].stmts)
        end
        ci.code[i] = stmt
    end
end

# used by some tests
inflate_ir(ci::CodeInfo) = inflate_ir(ci, Argtypes(), LatticeElement[ ⊤ for i = 1:length(ci.slotflags) ])
