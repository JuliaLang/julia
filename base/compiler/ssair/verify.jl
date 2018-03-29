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
                #@Base.show ir
                #@Base.show ir.cfg
                #@Base.error "Basic Block $def_bb does not dominate block $use_bb (tried to use value $(op.id))"
                error()
            end
        end
    elseif isa(op, Union{SlotNumber, TypedSlot})
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
            #ranges = [(idx,first(bb.stmts),last(bb.stmts)) for (idx, bb) in pairs(ir.cfg.blocks)]
            #@Base.show ranges
            #@Base.show (first(block.stmts), last_end)
            error()
        end
        last_end = last(block.stmts)
        for p in block.preds
            idx in ir.cfg.blocks[p].succs || error()
        end
        for s in block.succs
            idx in ir.cfg.blocks[s].preds || error()
        end
    end
    # Verify statements
    domtree = construct_domtree(ir.cfg)
    for (bb, idx, stmt) in bbidxstmt(ir)
        if isa(stmt, PhiNode)
            @assert length(stmt.edges) == length(stmt.values)
            for i = 1:length(stmt.edges)
                edge = stmt.edges[i]
                if !(edge in ir.cfg.blocks[bb].preds)
                    #@Base.show ir
                    #@Base.show (idx, edge, bb, ir.cfg.blocks[bb].preds)
                    error()
                end
                isassigned(stmt.values, i) || continue
                val = stmt.values[i]
                phiT = ir.types[idx]
                if isa(val, SSAValue)
                    if !(types(ir)[val] ⊑ phiT)
                        #@error """
                        #    PhiNode $idx, has operand $(val.id), whose type is not a sub lattice element.
                        #    PhiNode type was $phiT
                        #    Value type was $(ir.types[val.id])
                        #"""
                        #error()
                    end
                end
                check_op(ir, domtree, val, edge, last(ir.cfg.blocks[stmt.edges[i]].stmts)+1)
            end
        else
            for op in userefs(stmt)
                op = op[]
                check_op(ir, domtree, op, bb, idx)
            end
        end
    end
end
