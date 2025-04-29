# This file is a part of Julia. License is MIT: https://julialang.org/license

irshow_was_loaded() = invokelatest(isdefined, Compiler.IRShow, :debuginfo_firstline)
function maybe_show_ir(ir::IRCode)
    if irshow_was_loaded()
        # ensure we use I/O that does not yield, as this gets called during compilation
        invokelatest(Core.Main.Base.show, Core.stdout, "text/plain", ir)
    else
        Core.show(ir)
    end
    Core.println(Core.stdout)
end

if !isdefined(@__MODULE__, Symbol("@verify_error"))
    macro verify_error(arg)
        arg isa String && return esc(:(print && println($(GlobalRef(Core, :stderr)), $arg)))
        isexpr(arg, :string) || error("verify_error macro expected a string expression")
        pushfirst!(arg.args, GlobalRef(Core, :stderr))
        pushfirst!(arg.args, :println)
        arg.head = :call
        return esc(quote
            $arg
            maybe_show_ir(ir)
        end)
    end
end

is_toplevel_expr_head(head::Symbol) = head === :global || head === :method || head === :thunk
is_value_pos_expr_head(head::Symbol) = head === :static_parameter
function check_op(ir::IRCode, domtree::DomTree, @nospecialize(op), use_bb::Int, use_idx::Int, printed_use_idx::Int, print::Bool, isforeigncall::Bool, arg_idx::Int,
    allow_frontend_forms::Bool, @nospecialize(raise_error))
    if isa(op, SSAValue)
        op.id > 0 || @verify_error "Def ($(op.id)) is invalid in final IR"
        if op.id > length(ir.stmts)
            def_bb = block_for_inst(ir.cfg, ir.new_nodes.info[op.id - length(ir.stmts)].pos)
        else
            def_bb = block_for_inst(ir.cfg, op.id)
        end
        if (def_bb == use_bb)
            if op.id > length(ir.stmts)
                @assert ir.new_nodes.info[op.id - length(ir.stmts)].pos <= use_idx
            else
                if op.id >= use_idx
                    @verify_error "Def ($(op.id)) does not dominate use ($(use_idx)) in same BB"
                    raise_error()
                end
            end
        else
            if !dominates(domtree, def_bb, use_bb) && !(bb_unreachable(domtree, def_bb) && bb_unreachable(domtree, use_bb))
                # At the moment, we allow GC preserve tokens outside the standard domination notion
                @verify_error "Basic Block $def_bb does not dominate block $use_bb (tried to use value %$(op.id) at %$(printed_use_idx))"
                raise_error()
            end
        end

        use_inst = ir[op]
        if isa(use_inst[:stmt], Union{GotoIfNot, GotoNode, ReturnNode}) && !(isa(use_inst[:stmt], ReturnNode) && !isdefined(use_inst[:stmt], :val))
            # Allow uses of `unreachable`, which may have been inserted when
            # an earlier block got deleted, but for some reason we didn't figure
            # out yet that this entire block is dead also.
            @verify_error "At statement %$use_idx: Invalid use of value statement or terminator %$(op.id)"
            raise_error()
        end
    elseif isa(op, GlobalRef)
        if op.mod !== Core && op.mod !== Base
            (valid_worlds, alldef) = scan_leaf_partitions(nothing, op, WorldWithRange(min_world(ir.valid_worlds), ir.valid_worlds)) do _, _, bpart
                is_defined_const_binding(binding_kind(bpart))
            end
            if !alldef || max_world(valid_worlds) < max_world(ir.valid_worlds) || min_world(valid_worlds) > min_world(ir.valid_worlds)
                @verify_error "Unbound or partitioned GlobalRef not allowed in value position"
                raise_error()
            end
        end
    elseif isa(op, Expr)
        # Only Expr(:boundscheck) is allowed in value position
        if isforeigncall && arg_idx == 1 && op.head === :call
            # Allow a tuple in symbol position for foreigncall - this isn't actually
            # a real call - it's interpreted in global scope by codegen. However,
            # we do need to keep this a real use, because it could also be a pointer.
        elseif !is_value_pos_expr_head(op.head)
            if !allow_frontend_forms || op.head !== :opaque_closure_method
                @verify_error "Expr not allowed in value position"
                raise_error()
            end
        end
    elseif isa(op, Union{OldSSAValue, NewSSAValue})
        @verify_error "At statement %$use_idx: Left over SSA marker ($op)"
        raise_error()
    elseif isa(op, SlotNumber)
        @verify_error "Left over slot detected in converted IR"
        raise_error()
    end
end

function count_int(val::Int, arr::Vector{Int})
    n = 0
    for x in arr
        if x === val
            n += 1
        end
    end
    n
end

_debuginfo_firstline(debuginfo::Union{DebugInfo,DebugInfoStream}) = IRShow.debuginfo_firstline(debuginfo)
function verify_ir(ir::IRCode, print::Bool=true,
                   allow_frontend_forms::Bool=false,
                   𝕃ₒ::AbstractLattice = SimpleInferenceLattice.instance,
                   mi::Union{Nothing,MethodInstance}=nothing)
    function raise_error()
        error_args = Any["IR verification failed."]
        if irshow_was_loaded()
            # ensure we use I/O that does not yield, as this gets called during compilation
            firstline = invokelatest(_debuginfo_firstline, ir.debuginfo)
        else
            firstline = nothing
        end
        if firstline !== nothing
            file, line = firstline
            push!(error_args, "\n", "    Code location: ", file, ":", line)
        end
        if mi !== nothing
            push!(error_args, "\n", "  Method instance: ", mi)
        end
        invokelatest(error, error_args...)
    end
    # Verify CFG graph. Must be well formed to construct domtree
    if !(length(ir.cfg.blocks) - 1 <= length(ir.cfg.index) <= length(ir.cfg.blocks))
        @verify_error "CFG index length ($(length(ir.cfg.index))) does not correspond to # of blocks $(length(ir.cfg.blocks))"
        raise_error()
    end
    if length(ir.stmts.stmt) != length(ir.stmts)
        @verify_error "IR stmt length is invalid $(length(ir.stmts.stmt)) / $(length(ir.stmts))"
        raise_error()
    end
    if length(ir.stmts.type) != length(ir.stmts)
        @verify_error "IR type length is invalid $(length(ir.stmts.type)) / $(length(ir.stmts))"
        raise_error()
    end
    if length(ir.stmts.info) != length(ir.stmts)
        @verify_error "IR info length is invalid $(length(ir.stmts.info)) / $(length(ir.stmts))"
        raise_error()
    end
    if length(ir.stmts.line) != length(ir.stmts) * 3
        @verify_error "IR line length is invalid $(length(ir.stmts.line)) / $(length(ir.stmts) * 3)"
        raise_error()
    end
    if length(ir.stmts.flag) != length(ir.stmts)
        @verify_error "IR flag length is invalid $(length(ir.stmts.flag)) / $(length(ir.stmts))"
        raise_error()
    end
    # For now require compact IR
    # @assert isempty(ir.new_nodes)
    # Verify CFG
    last_end = 0
    # Verify CFG graph. Must be well formed to construct domtree
    for (idx, block) in pairs(ir.cfg.blocks)
        for p in block.preds
            p == 0 && continue
            if !(1 <= p <= length(ir.cfg.blocks))
                @verify_error "Predecessor $p of block $idx out of bounds for IR"
                raise_error()
            end
            c = count_int(idx, ir.cfg.blocks[p].succs)
            if c == 0
                @verify_error "Predecessor $p of block $idx not in successor list"
                raise_error()
            elseif c == 2
                if count_int(p, block.preds) != 2
                    @verify_error "Double edge from $p to $idx not correctly accounted"
                    raise_error()
                end
            end
        end
        for s in block.succs
            if !(1 <= s <= length(ir.cfg.blocks))
                @verify_error "Successor $s of block $idx out of bounds for IR"
                raise_error()
            end
            if !(idx in ir.cfg.blocks[s].preds)
                #Base.@show ir.cfg
                #Base.@show ir
                #Base.@show ir.argtypes
                @verify_error "Successor $s of block $idx not in predecessor list"
                raise_error()
            end
        end
        if !(1 <= first(block.stmts) <= length(ir.stmts))
            @verify_error "First statement of BB $idx ($(first(block.stmts))) out of bounds for IR (length=$(length(ir.stmts)))"
            raise_error()
        end
        if !(1 <= last(block.stmts) <= length(ir.stmts))
            @verify_error "Last statement of BB $idx ($(last(block.stmts))) out of bounds for IR (length=$(length(ir.stmts)))"
            raise_error()
        end
        if idx <= length(ir.cfg.index) && last(block.stmts) + 1 != ir.cfg.index[idx]
            @verify_error "End of BB $idx ($(last(block.stmts))) is not one less than CFG index ($(ir.cfg.index[idx]))"
            raise_error()
        end
    end
    # Verify statements
    domtree = construct_domtree(ir.cfg.blocks)
    for (idx, block) in pairs(ir.cfg.blocks)
        if first(block.stmts) != last_end + 1
            #ranges = [(idx,first(bb.stmts),last(bb.stmts)) for (idx, bb) in pairs(ir.cfg.blocks)]
            @verify_error "First statement of BB $idx ($(first(block.stmts))) does not match end of previous ($last_end)"
            raise_error()
        end
        last_end = last(block.stmts)
        terminator = ir[SSAValue(last_end)][:stmt]

        bb_unreachable(domtree, idx) && continue
        if isa(terminator, ReturnNode)
            if !isempty(block.succs)
                @verify_error "Block $idx ends in return or unreachable, but has successors"
                raise_error()
            end
        elseif isa(terminator, GotoNode)
            if length(block.succs) != 1 || block.succs[1] != terminator.label
                @verify_error "Block $idx successors ($(block.succs)), does not match GotoNode terminator ($(terminator.label))"
                raise_error()
            end
        elseif isa(terminator, GotoIfNot)
            if terminator.dest == idx + 1
                @verify_error "Block $idx terminator forms a double edge to block $(idx+1)"
                raise_error()
            end
            if length(block.succs) != 2 || (block.succs != [terminator.dest, idx+1] && block.succs != [idx+1, terminator.dest])
                @verify_error "Block $idx successors ($(block.succs)), does not match GotoIfNot terminator"
                raise_error()
            end
        elseif isa(terminator, EnterNode)
            @label enter_check
            if length(block.succs) == 1
                if terminator.catch_dest != 0
                    @verify_error "Block $idx successors ($(block.succs)), does not match :enter terminator"
                    raise_error()
                end
            elseif (block.succs != Int[terminator.catch_dest, idx+1] && block.succs != Int[idx+1, terminator.catch_dest])
                @verify_error "Block $idx successors ($(block.succs)), does not match :enter terminator"
                raise_error()
            end
        else
            if length(block.succs) != 1 || block.succs[1] != idx + 1
                # As a special case, we allow extra statements in the BB of an :enter
                # statement, until we can do proper CFG manipulations during compaction.
                for stmt_idx in first(block.stmts):last(block.stmts)
                    stmt = ir[SSAValue(stmt_idx)][:stmt]
                    if isa(stmt, EnterNode)
                        terminator = stmt
                        @goto enter_check
                    end
                    isa(stmt, PhiNode) || break
                end
                termidx = last(block.stmts)
                stmttyp = ir.stmts[termidx][:type]
                if isempty(block.succs) && stmttyp == Union{}
                    # Allow fallthrough terminators that are known to error to
                    # be removed from the CFG. Ideally we'd add an unreachable
                    # here, but that isn't always possible.
                else
                    @verify_error "Block $idx successors ($(block.succs)), does not match fall-through terminator %$termidx ($terminator)::$stmttyp"
                    raise_error()
                end
            end
        end
    end
    if length(ir.stmts) != last(ir.cfg.blocks[end].stmts)
        @verify_error "End of last BB $(last(ir.cfg.blocks[end].stmts)) does not match last IR statement $(length(ir.stmts))"
        raise_error()
    end
    lastbb = 0
    is_phinode_block = false
    firstidx = 1
    lastphi = 1
    for (bb, idx) in bbidxiter(ir)
        if bb != lastbb
            is_phinode_block = true
            lastphi = firstidx = idx
            lastbb = bb
        end
        # We allow invalid IR in dead code to avoid passes having to detect when
        # they're generating dead code.
        bb_unreachable(domtree, bb) && continue
        stmt = ir[SSAValue(idx)][:stmt]
        stmt === nothing && continue
        if isa(stmt, PhiNode)
            if !is_phinode_block
                @verify_error "φ node $idx is not at the beginning of the basic block $bb"
                raise_error()
            end
            lastphi = idx
            @assert length(stmt.edges) == length(stmt.values)
            for i = 1:length(stmt.edges)
                edge = stmt.edges[i]
                for j = (i+1):length(stmt.edges)
                    edge′ = stmt.edges[j]
                    if edge == edge′
                        # TODO: Move `unique` to Core.Compiler. For now we assume the predecessor list is always unique.
                        @verify_error "Edge list φ node $idx in bb $bb not unique (double edge?)"
                        raise_error()
                    end
                end
                if !(edge == 0 && bb == 1) && !(edge in ir.cfg.blocks[bb].preds)
                    #Base.@show ir.argtypes
                    #Base.@show ir
                    @verify_error "Edge $edge of φ node $idx not in predecessor list"
                    raise_error()
                end
                edge == 0 && continue
                if bb_unreachable(domtree, Int(edge))
                    # TODO: Disallow?
                    #@verify_error "Unreachable edge from #$edge should have been cleaned up at idx $idx"
                    #raise_error()
                    continue
                end
                isassigned(stmt.values, i) || continue
                val = stmt.values[i]
                phiT = ir.stmts[idx][:type]
                if isa(val, SSAValue)
                    if !⊑(𝕃ₒ, types(ir)[val], phiT)
                        #@verify_error """
                        #    PhiNode $idx, has operand $(val.id), whose type is not a sub lattice element.
                        #    PhiNode type was $phiT
                        #    Value type was $(ir.stmts[val.id][:type])
                        #"""
                        #raise_error()
                    end
                end
                check_op(ir, domtree, val, Int(edge), last(ir.cfg.blocks[stmt.edges[i]].stmts)+1, idx, print, false, i,
                    allow_frontend_forms, raise_error)
            end
            continue
        end

        if is_phinode_block && !is_valid_phiblock_stmt(stmt)
            if !isa(stmt, Expr) || !is_value_pos_expr_head(stmt.head)
                # Go back and check that all non-PhiNodes are valid value-position
                for validate_idx in firstidx:(lastphi-1)
                    validate_stmt = ir[SSAValue(validate_idx)][:stmt]
                    isa(validate_stmt, PhiNode) && continue
                    check_op(ir, domtree, validate_stmt, bb, idx, idx, print, false, 0,
                        allow_frontend_forms, raise_error)
                end
                is_phinode_block = false
            end
        end
        if isa(stmt, PhiCNode)
            for i = 1:length(stmt.values)
                val = stmt.values[i]
                if !isa(val, SSAValue)
                    @verify_error "Operand $i of PhiC node $idx must be an SSA Value."
                    raise_error()
                end
                if !isa(ir[val][:stmt], UpsilonNode)
                    @verify_error "Operand $i of PhiC node $idx must reference an Upsilon node."
                    raise_error()
                end
            end
        elseif isterminator(stmt)
            if idx != last(ir.cfg.blocks[bb].stmts)
                @verify_error "Terminator $idx in bb $bb is not the last statement in the block"
                raise_error()
            end
            if !isa(stmt, ReturnNode) && ir[SSAValue(idx)][:type] !== Any
                @verify_error "Explicit terminators (other than ReturnNode) must have `Any` type"
                raise_error()
            end
        else
            isforeigncall = false
            if isa(stmt, Expr)
                if stmt.head === :(=)
                    @verify_error "Assignment should have been removed during SSA conversion"
                    raise_error()
                elseif stmt.head === :isdefined
                    if length(stmt.args) > 2
                        @verify_error "malformed isdefined"
                        raise_error()
                    end
                    if stmt.args[1] isa GlobalRef
                        # undefined GlobalRef is OK in isdefined
                        continue
                    end
                elseif stmt.head === :throw_undef_if_not
                    if length(stmt.args) > 3
                        @verify_error "malformed throw_undef_if_not"
                        raise_error()
                    end
                    if stmt.args[1] isa GlobalRef
                        # undefined GlobalRef is OK in throw_undef_if_not
                        continue
                    end
                elseif stmt.head === :gc_preserve_end
                    # We allow gc_preserve_end tokens to span across try/catch
                    # blocks, which isn't allowed for regular SSA values, so
                    # we skip the validation below.
                    continue
                elseif stmt.head === :foreigncall
                    isforeigncall = true
                elseif stmt.head === :isdefined && length(stmt.args) == 1 &&
                        isexpr(stmt.args[1], :static_parameter)
                    # a GlobalRef or static_parameter isdefined check does not evaluate its argument
                    continue
                elseif stmt.head === :call
                    f = stmt.args[1]
                    if f isa GlobalRef && f.name === :cglobal
                        # TODO: these are not yet linearized
                        continue
                    end
                elseif stmt.head === :leave
                    for i in 1:length(stmt.args)
                        arg = stmt.args[i]
                        if !isa(arg, Union{Nothing, SSAValue})
                            @verify_error "Malformed :leave - Expected `Nothing` or SSAValue"
                            raise_error()
                        elseif isa(arg, SSAValue)
                            enter_stmt = ir[arg::SSAValue][:stmt]
                            if !isa(enter_stmt, Nothing) && !isa(enter_stmt, EnterNode)
                                @verify_error "Malformed :leave - argument ssavalue should point to `nothing` or :enter"
                                raise_error()
                            end
                        end
                    end
                end
            end
            n = 1
            for op in userefs(stmt)
                op = op[]
                check_op(ir, domtree, op, bb, idx, idx, print, isforeigncall, n,
                    allow_frontend_forms, raise_error)
                n += 1
            end
        end
    end
end

function verify_linetable(di::DebugInfoStream, nstmts::Int, print::Bool=true)
    @assert 3nstmts == length(di.codelocs)
    for i in 1:nstmts
        edge = di.codelocs[3i-1]
        if !(edge == 0 || get(di.edges, edge, nothing) isa DebugInfo)
            @verify_error "Malformed debuginfo index into edges"
        end
    end
end
