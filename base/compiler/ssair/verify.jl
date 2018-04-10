if !isdefined(@__MODULE__, Symbol("@verify_error"))
    macro verify_error(arg)
        arg isa String && return esc(:(println($arg)))
        arg isa Expr && arg.head === :string || error()
        pushfirst!(arg.args, :println)
        arg.head = :call
        return esc(arg)
    end
end

function check_op(ir::IRCode, domtree::DomTree, @nospecialize(op), use_bb::Int, use_idx::Int)
    if isa(op, SSAValue)
        if op.id > length(ir.stmts)
            def_bb = block_for_inst(ir.cfg, ir.new_nodes[op.id - length(ir.stmts)][1])
        else
            def_bb = block_for_inst(ir.cfg, op.id)
        end
        if (def_bb == use_bb)
            if op.id > length(ir.stmts)
                @assert ir.new_nodes[op.id - length(ir.stmts)][1] <= use_idx
            else
                @assert op.id < use_idx
            end
        else
            if !dominates(domtree, def_bb, use_bb)
                enable_new_optimizer[] = false
                @show ir
                @verify_error "Basic Block $def_bb does not dominate block $use_bb (tried to use value $(op.id))"
                error()
            end
        end
    elseif isa(op, Union{SlotNumber, TypedSlot})
        enable_new_optimizer[] = false
        #@error "Left over slot detected in converted IR"
        error()
    end
end

function verify_ir(ir::IRCode)
    # For now require compact IR
    # @assert isempty(ir.new_nodes)
    # Verify CFG
    last_end = 0
    for (idx, block) in pairs(ir.cfg.blocks)
        if first(block.stmts) != last_end + 1
            enable_new_optimizer[] = false
            #ranges = [(idx,first(bb.stmts),last(bb.stmts)) for (idx, bb) in pairs(ir.cfg.blocks)]
            @show ranges
            @show (first(block.stmts), last_end)
            @verify_error "First statement of BB $idx ($(first(block.stmts))) does not match end of previous ($last_end)"
            error()
        end
        last_end = last(block.stmts)
        for p in block.preds
            if !(idx in ir.cfg.blocks[p].succs)
                @verify_error "Predeccsor $p of block $idx not in successor list"
                error()
            end
        end
        for s in block.succs
            if !(idx in ir.cfg.blocks[s].preds)
                @show ir.cfg
                @show ir
                @show ir.argtypes
                @verify_error "Successor $s of block $idx not in predecessor list"
                error()
            end
        end
    end
    # Verify statements
    domtree = construct_domtree(ir.cfg)
    for (bb, idx, stmt) in bbidxstmt(ir)
        if isa(stmt, PhiNode)
            @assert length(stmt.edges) == length(stmt.values)
            for i = 1:length(stmt.edges)
                edge = stmt.edges[i]
                if !(edge == 0 && bb == 1) && !(edge in ir.cfg.blocks[bb].preds)
                    enable_new_optimizer[] = false
                    @show ir.argtypes
                    @show ir
                    @verify_error "Edge $edge of φ node $idx not in predecessor list"
                    error()
                end
                edge == 0 && continue
                isassigned(stmt.values, i) || continue
                val = stmt.values[i]
                phiT = ir.types[idx]
                if isa(val, SSAValue)
                    if !(types(ir)[val] ⊑ phiT)
                        #@verify_error """
                        #    PhiNode $idx, has operand $(val.id), whose type is not a sub lattice element.
                        #    PhiNode type was $phiT
                        #    Value type was $(ir.types[val.id])
                        #"""
                        #error()
                    end
                end
                check_op(ir, domtree, val, edge, last(ir.cfg.blocks[stmt.edges[i]].stmts)+1)
            end
        elseif isa(stmt, PhiCNode)
            for i = 1:length(stmt.values)
                val = stmt.values[i]
                if !isa(val, SSAValue)
                    @verify_error "Operand $i of PhiC node $idx must be an SSA Value."
                    error()
                end
                if !isa(ir[val], UpsilonNode)
                    @verify_error "Operand $i of PhiC node $idx must reference an Upsilon node."
                    error()
                end
            end
        else
            for op in userefs(stmt)
                op = op[]
                check_op(ir, domtree, op, bb, idx)
            end
        end
    end
end
