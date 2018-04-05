function ssa_inlining_pass!(ir::IRCode, domtree, linetable, sv::OptimizationState)
    # Go through the function, perfoming simple ininlingin (e.g. replacing call by constants
    # and analyzing legality of inlining).
    @timeit "analysis" todo = assemble_inline_todo!(ir, domtree, linetable, sv)
    isempty(todo) && return ir
    # Do the actual inlining for every call we identified
    @timeit "execution" ir = batch_inline!(todo, ir, domtree, linetable, sv)
    return ir
end

function batch_inline!(todo, ir, domtree, linetable, sv)
    # Compute the new CFG first (modulo statement ranges, which will be computed below)
    new_cfg_blocks = BasicBlock[]
    inserted_block_ranges = UnitRange{Int}[]
    todo_bbs = Tuple{Int, Int}[]
    first_bb = 0
    bb_rename = zeros(Int, length(ir.cfg.blocks))
    split_targets = IdSet{Int}()
    merged_orig_blocks = IdSet{Int}()
    for (idx, _a, _b, _c, _d, ir2, lie) in todo
        # A linear inline does not modify the CFG
        lie && continue
        # Figure out if we need to split the BB
        need_split_before = false
        need_split = true
        block = block_for_inst(ir.cfg, idx)
        last_block_idx = last(ir.cfg.blocks[block].stmts)

        if !isempty(ir2.cfg.blocks[1].preds)
            need_split_before = true
        end

        if first_bb != block
            new_range = first_bb+1:block
            bb_rename[new_range] = (1:length(new_range)) .+ length(new_cfg_blocks)
            append!(new_cfg_blocks, map(copy, ir.cfg.blocks[new_range]))
            push!(merged_orig_blocks, last(new_range))
        end
        first_bb = block
        if false # TODO: ((idx+1) == last_block_idx && isa(ir[SSAValue(last_block_idx)], GotoNode))
            need_split = false
            post_bb_id = -ir[SSAValue(last_block_idx)].label
        else
            post_bb_id = length(new_cfg_blocks) + length(ir2.cfg.blocks) + (need_split_before ? 1 : 0)
            need_split = true #!(idx == last_block_idx)
        end

        if !need_split
            delete!(merged_orig_blocks, last(new_range))
        end

        push!(todo_bbs, (length(new_cfg_blocks) - 1 + (need_split_before ? 1 : 0), post_bb_id))

        delete!(split_targets, length(new_cfg_blocks))
        orig_succs = copy(new_cfg_blocks[end].succs)
        empty!(new_cfg_blocks[end].succs)
        if need_split_before
            bb_rename_range = (1:length(ir2.cfg.blocks)) .+ length(new_cfg_blocks)
            push!(new_cfg_blocks[end].succs, length(new_cfg_blocks)+1)
            append!(new_cfg_blocks, ir2.cfg.blocks)
        else
            # Merge the last block that was already there with the first block we're adding
            bb_rename_range = (1:length(ir2.cfg.blocks)) .+ (length(new_cfg_blocks) - 1)
            append!(new_cfg_blocks[end].succs, ir2.cfg.blocks[1].succs)
            append!(new_cfg_blocks, ir2.cfg.blocks[2:end])
        end
        if need_split
            push!(new_cfg_blocks, BasicBlock(ir.cfg.blocks[block].stmts,
                Int[], orig_succs))
            push!(split_targets, length(new_cfg_blocks))
        end
        new_block_range = (length(new_cfg_blocks)-length(ir2.cfg.blocks)+1):length(new_cfg_blocks)
        push!(inserted_block_ranges, new_block_range)

        # Fixup the edges of the newely added blocks
        for (old_block, new_block) in enumerate(bb_rename_range)
            if old_block != 1 || need_split_before
                p = new_cfg_blocks[new_block].preds
                map!(p, p) do old_pred_block
                    bb_rename_range[old_pred_block]
                end
            end
            if new_block != last(new_block_range)
                s = new_cfg_blocks[new_block].succs
                map!(s, s) do old_succ_block
                    bb_rename_range[old_succ_block]
                end
            end
        end

        if need_split_before
            push!(new_cfg_blocks[first(bb_rename_range)].preds, first(bb_rename_range)-1)
        end

        for (old_block, new_block) in enumerate(bb_rename_range)
            if (length(new_cfg_blocks[new_block].succs) == 0)
                terminator_idx = last(ir2.cfg.blocks[old_block].stmts)
                terminator = ir2[SSAValue(terminator_idx)]
                if isa(terminator, ReturnNode) && isdefined(terminator, :val)
                    push!(new_cfg_blocks[new_block].succs, post_bb_id)
                    if need_split
                        push!(new_cfg_blocks[post_bb_id].preds, new_block)
                    end
                end
            end
        end
    end
    new_range = first_bb+1:length(ir.cfg.blocks)
    bb_rename[new_range] = (1:length(new_range)) .+ length(new_cfg_blocks)
    append!(new_cfg_blocks, ir.cfg.blocks[new_range])

    # Rename edges original bbs
    for (orig_bb, bb) in pairs(bb_rename)
        p, s = new_cfg_blocks[bb].preds, new_cfg_blocks[bb].succs
        map!(p, p) do pred_bb
            pred_bb == length(bb_rename) && return length(new_cfg_blocks)
            bb_rename[pred_bb+1]-1
        end
        if !(orig_bb in merged_orig_blocks)
            map!(s, s) do succ_bb
                bb_rename[succ_bb]
            end
        end
    end

    for bb in collect(split_targets)
        s = new_cfg_blocks[bb].succs
        map!(s, s) do succ_bb
            bb_rename[succ_bb]
        end
    end

    # Rename any annotated original bb references
    for bb in 1:length(new_cfg_blocks)
        s = new_cfg_blocks[bb].succs
        map!(s, s) do succ_bb
            succ_bb < 0 ? bb_rename[-succ_bb] : succ_bb
        end
    end

    compact = IncrementalCompact(ir)
    compact.result_bbs = new_cfg_blocks
    nnewnodes = length(compact.result) + sum(todo) do (_a, _b, _c, _d, _e, ir2, _f)
        length(ir2.stmts) + length(ir2.new_nodes)
    end
    resize!(compact, nnewnodes)
    (inline_idx, (isva, isinvoke, isapply, na), method, spvals, inline_linetable, inline_ir, lie) = popfirst!(todo)
    for (idx, stmt) in compact
        if compact.idx-1 == inline_idx
            # Ok, do the inlining here
            inline_cfg = inline_ir.cfg
            linetable_offset = length(linetable)
            # Append the linetable of the inlined function to our line table
            for entry in inline_linetable
                push!(linetable, LineInfoNode(entry.mod, entry.method, entry.file, entry.line, compact.result_lines[idx]))
            end
            # If the iterator already moved on to the next basic block,
            # temorarily re-open in again.
            refinish = false
            if compact.result_idx == first(compact.result_bbs[compact.active_result_bb].stmts)
                compact.active_result_bb -= 1
                refinish = true
            end
            argexprs = copy(stmt.args)
            # At the moment we will allow globalrefs in argument position, turn those into ssa values
            for (aidx, aexpr) in pairs(argexprs)
                if isa(aexpr, GlobalRef)
                    argexprs[aidx] = insert_node_here!(compact, aexpr, compact_exprtype(compact, aexpr), compact.result_lines[idx])
                end
            end
            argexprs = rewrite_exprargs((node, typ)->insert_node_here!(compact, node, typ, compact.result_lines[idx]),
                                        arg->compact_exprtype(compact, arg), isinvoke, isapply, argexprs)
            if isva
                vararg = mk_tuplecall!(compact, argexprs[na:end], compact.result_lines[idx])
                argexprs = Any[argexprs[1:(na - 1)]..., vararg]
            end
            # Special case inlining that maintains the current basic block if there's only one BB in the target
            if lie
                terminator = inline_ir[SSAValue(last(inline_cfg.blocks[1].stmts))]
                compact[idx] = nothing
                inline_compact = IncrementalCompact(compact, inline_ir, compact.result_idx)
                for (idx′, stmt′) in inline_compact
                    # This dance is done to maintain accurate usage counts in the
                    # face of rename_arguments! mutating in place - should figure out
                    # something better eventually.
                    inline_compact[idx′] = nothing
                    stmt′ = ssa_substitute!(stmt′, argexprs, method.sig, spvals)
                    compact.result_lines[idx′] += linetable_offset
                    if isa(stmt′, ReturnNode)
                        isa(stmt′.val, SSAValue) && (compact.used_ssas[stmt′.val.id] += 1)
                        compact.ssa_rename[compact.idx-1] = stmt′.val
                        stmt′ = nothing
                    end
                    inline_compact[idx′] = stmt′
                end
                just_fixup!(inline_compact)
                compact.result_idx = inline_compact.result_idx
                refinish && finish_current_bb!(compact)
            else
                bb_offset, post_bb_id = popfirst!(todo_bbs)
                # This implements the need_split_before flag above
                need_split_before = !isempty(inline_ir.cfg.blocks[1].preds)
                if need_split_before
                    finish_current_bb!(compact)
                end
                pn = PhiNode()
                compact[idx] = nothing
                inline_compact = IncrementalCompact(compact, inline_ir, compact.result_idx)
                for (idx′, stmt′) in inline_compact
                    inline_compact[idx′] = nothing
                    stmt′ = ssa_substitute!(stmt′, argexprs, method.sig, spvals)
                    compact.result_lines[idx′] += linetable_offset
                    if isa(stmt′, ReturnNode)
                        if isdefined(stmt′, :val)
                            push!(pn.edges, inline_compact.active_result_bb-1)
                            push!(pn.values, stmt′.val)
                            stmt′ = GotoNode(post_bb_id)
                        end
                    elseif isa(stmt′, GotoNode)
                        stmt′ = GotoNode(stmt′.label + bb_offset)
                    elseif isa(stmt′, Expr) && stmt′.head == :enter
                        stmt′ = Expr(:enter, stmt′.args[1] + bb_offset)
                    elseif isa(stmt′, GotoIfNot)
                        stmt′ = GotoIfNot(stmt′.cond, stmt′.dest + bb_offset)
                    elseif isa(stmt′, PhiNode)
                        stmt′ = PhiNode(Any[edge+bb_offset for edge in stmt′.edges], stmt′.values)
                    end
                    inline_compact[idx′] = stmt′
                end
                just_fixup!(inline_compact)
                compact.result_idx = inline_compact.result_idx
                compact.active_result_bb = inline_compact.active_result_bb
                for i = 1:length(pn.values)
                    isassigned(pn.values, i) || continue
                    if isa(pn.values[i], SSAValue)
                        compact.used_ssas[pn.values[i].id] += 1
                    end
                end
                if length(pn.edges) == 1
                    compact.ssa_rename[compact.idx-1] = pn.values[1]
                else
                    pn_ssa = insert_node_here!(compact, pn, stmt.typ, compact.result_lines[idx])
                    compact.ssa_rename[compact.idx-1] = pn_ssa
                end
                refinish && finish_current_bb!(compact)
            end
            if !isempty(todo)
                (inline_idx, (isva, isinvoke, isapply, na), method, spvals, inline_linetable, inline_ir, lie) = popfirst!(todo)
            else
                inline_idx = -1
            end
        elseif isa(stmt, GotoNode)
            compact[idx] = GotoNode(bb_rename[stmt.label])
        elseif isa(stmt, Expr) && stmt.head == :enter
            compact[idx] = Expr(:enter, bb_rename[stmt.args[1]])
        elseif isa(stmt, GotoIfNot)
            compact[idx] = GotoIfNot(stmt.cond, bb_rename[stmt.dest])
        elseif isa(stmt, PhiNode)
            compact[idx] = PhiNode(Any[edge == length(bb_rename) ? length(new_cfg_blocks) : bb_rename[edge+1]-1 for edge in stmt.edges], stmt.values)
        end
    end

    ir = finish(compact)
end

function spec_lambda(@nospecialize(atype), sv::OptimizationState, @nospecialize(invoke_data))
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atype, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atype <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atype, sv.params.world)
    end
end

function rewrite_exprargs(inserter, exprtype, isinvoke, isapply, argexprs)
    if isapply
        new_argexprs = Any[argexprs[2]]
        # Flatten all tuples
        for arg in argexprs[3:end]
            tupT = exprtype(arg)
            t = widenconst(tupT)
            for i = 1:length(t.parameters)
                # Insert a getfield call here
                new_call = Expr(:call, Core.getfield, arg, i)
                new_call.typ = getfield_tfunc(tupT, Const(i))
                push!(new_argexprs, inserter(new_call, new_call.typ))
            end
        end
        argexprs = new_argexprs
    end
    if isinvoke
        argexpr0 = argexprs[2]
        argexprs = argexprs[4:end]
        pushfirst!(argexprs, argexpr0)
    end
    argexprs
end

function maybe_make_invoke!(ir, idx, @nospecialize(etype), atypes::Vector{Any}, sv::OptimizationState,
                    @nospecialize(atype_unlimited), isinvoke, isapply, @nospecialize(invoke_data))
    nu = countunionsplit(atypes)
    nu > 1 && return # TODO: The old optimizer did union splitting here. Is this the right place?
    ex = Expr(:invoke)
    linfo = spec_lambda(atype_unlimited, sv, invoke_data)
    linfo === nothing && return
    add_backedge!(linfo, sv)
    argexprs = ir[SSAValue(idx)].args
    argexprs = rewrite_exprargs((node, typ)->insert_node!(ir, idx, typ, node), arg->exprtype(arg, ir, ir.mod),
        isinvoke, isapply, argexprs)
    pushfirst!(argexprs, linfo)
    ex.typ = etype
    ex.args = argexprs
    ir[SSAValue(idx)] = ex
end

function exprtype_func(@nospecialize(arg1), ir)
    ft = exprtype(arg1, ir, ir.mod)
    if isa(ft, Const)
        f = ft.val
    elseif isa(ft, Conditional)
        f = nothing
        ft = Bool
    else
        f = nothing
        if !(isconcretetype(ft) || (widenconst(ft) <: Type)) || has_free_typevars(ft)
            # TODO: this is really aggressive at preventing inlining of closures. maybe drop `isconcretetype` requirement?
            return nothing
        end
    end
    return (f, ft)
end

function assemble_inline_todo!(ir, domtree, linetable, sv)
    todo = Tuple{Int, Tuple{Bool, Bool, Bool, Int}, Method, Vector{Any}, Vector{LineInfoNode}, IRCode, Bool}[]
    for (idx, stmt) in pairs(ir.stmts)
        isexpr(stmt, :call) || continue
        eargs = stmt.args
        isempty(eargs) && continue
        arg1 = eargs[1]

        res = exprtype_func(arg1, ir)
        res !== nothing || continue
        (f, ft) = res

        atypes = Vector{Any}(undef, length(stmt.args))
        atypes[1] = ft
        ok = true
        for i = 2:length(stmt.args)
            a = exprtype(stmt.args[i], ir, ir.mod)
            (a === Bottom || isvarargtype(a)) && (ok = false; break)
            atypes[i] = a
        end
        ok || continue

        # Check if we match any of the early inliners
        res = early_inline_special_case(ir, f, ft, stmt, atypes)
        if res !== nothing
            ir.stmts[idx] = res[1]
            continue
        end

        if f !== Core.invoke && f !== Core._apply && (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            # No inlining for builtins (other than what's handled in the early inliner)
            continue
        end

        # Special handling for Core.invoke and Core._apply, which can follow the normal inliner
        # logic with modified inlining target
        isapply = isinvoke = false

        # Handle _apply
        isapply = false
        if f === Core._apply
            new_atypes = Any[]
            res = exprtype_func(stmt.args[2], ir)
            res !== nothing || continue
            (f, ft) = res
            # Push function type
            push!(new_atypes, ft)
            # Try to figure out the signature of the function being called
            ok = true
            for (typ, def) in zip(atypes[3:end], stmt.args[3:end])
                typ = widenconst(typ)
                # We don't know what'll be in a SimpleVector, bail out
                isa(typ, SimpleVector) && (ok = false; break)
                # TODO: We could basically run the iteration protocol here
                if !isa(typ, DataType) || typ.name !== Tuple.name || isvatuple(typ)
                    ok = false
                    break
                end
                length(typ.parameters) <= sv.params.MAX_TUPLE_SPLAT || (ok = false; break)
                # As a special case, if we can see the tuple() call, look at it's arguments to find
                # our types. They can be more precise (e.g. f(Bool, A...) would be lowered as
                # _apply(f, tuple(Bool)::Tuple{DataType}, A), which might not be precise enough to
                # get a good method match. This pattern is used in the array code a bunch.
                if isa(def, SSAValue) && is_tuple_call(ir, ir[def])
                    for tuparg in ir[def].args[2:end]
                        push!(new_atypes, exprtype(tuparg , ir, ir.mod))
                    end
                else
                    append!(new_atypes, typ.parameters)
                end
            end
            ok || continue
            atypes = new_atypes
            isapply = true
        end

        if isapply && f !== Core.invoke && (isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction || isa(f, Builtin) || ft ⊑ Builtin)
            # Even though we don't do inlining or :invoke, for intrinsic functions, we do want to eliminate apply if possible.
            stmt.args = rewrite_exprargs((node, typ)->insert_node!(ir, idx, typ, node), arg->exprtype(arg, ir, ir.mod),
                false, isapply, stmt.args)
            continue
        end

        # Handle invoke
        invoke_data = nothing
        if f === Core.invoke && length(atypes) >= 3
            res = compute_invoke_data(atypes, stmt.args, sv)
            res === nothing && continue
            (f, ft, atypes, argexprs, invoke_data) = res
        end
        isinvoke = invoke_data !== nothing

        atype_unlimited = argtypes_to_type(atypes)

        # In :invoke, make sure that the arguments we're passing are a subtype of the
        # signature we're invoking.
        (invoke_data === nothing || atype_unlimited <: invoke_data.types0) || continue

        # TODO: Bail out here if inlining is disabled

        # Special case inliners for regular functions
        if late_inline_special_case!(ir, idx, stmt, atypes, f, ft, _topmod(ir.mod))
            continue
        end

        # Compute the limited type if necessary
        if length(atype_unlimited.parameters) - 1 > sv.params.MAX_TUPLETYPE_LEN
            atype = limit_tuple_type(atype_unlimited, sv.params)
        else
            atype = atype_unlimited
        end

        # Ok, now figure out what method to call
        @timeit "method matching" if invoke_data === nothing
            min_valid = UInt[typemin(UInt)]
            max_valid = UInt[typemax(UInt)]
            meth = _methods_by_ftype(atype, 1, sv.params.world, min_valid, max_valid)
            if meth === false || length(meth) != 1
                maybe_make_invoke!(ir, idx, stmt.typ, atypes, sv, atype_unlimited, false, isapply, nothing)
                continue
            end
            meth = meth[1]::SimpleVector
            metharg = meth[1]::Type
            methsp = meth[2]::SimpleVector
            method = meth[3]::Method
        else
            method = invoke_data.entry.func
            (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                    atype_unlimited, method.sig)::SimpleVector
            methsp = methsp::SimpleVector
        end


        methsig = method.sig
        if !(atype <: metharg)
            maybe_make_invoke!(ir, idx, stmt.typ, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            continue
        end

        # Check whether this call just evaluates to a constant
        if isa(f, widenconst(ft)) && !isdefined(method, :generator) && method.pure &&
                isa(stmt.typ, Const) && stmt.typ.actual && is_inlineable_constant(stmt.typ.val)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            continue
        end

        # Check that we habe the correct number of arguments
        na = Int(method.nargs)
        npassedargs = length(atypes)
        if na != npassedargs && !(na > 0 && method.isva)
            # we have a method match only because an earlier
            # inference step shortened our call args list, even
            # though we have too many arguments to actually
            # call this function
            continue
        end

        # Bail out if any static parameters are left as TypeVar
        ok = true
        for i = 1:length(methsp)
            isa(methsp[i], TypeVar) && (ok = false; break)
        end
        ok || continue

        # Find the linfo for this methods (Generated functions are expanded here if necessary)
        linfo = code_for_method(method, metharg, methsp, sv.params.world, true) # Union{Nothing, MethodInstance}
        if !isa(linfo, MethodInstance)
            maybe_make_invoke!(ir, idx, stmt.typ, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            continue
        end

        if invoke_api(linfo) == 2
            # in this case function can be inlined to a constant
            add_backedge!(linfo, sv)
            ir[SSAValue(idx)] = linfo.inferred_const
            continue
        end

        # Handle vararg functions
        isva = na > 0 && method.isva
        if isva
            @assert length(atypes) >= na - 1
            va_type = tuple_tfunc(Tuple{Any[widenconst(typ) for typ in atypes]...})
            atypes = Any[atypes[1:(na - 1)]..., va_type]
        end

        # Go see if we already have a pre-inferred result
        res = find_inferred!(ir, idx, linfo, atypes, sv)
        res === nothing && continue

        (rettype, inferred) = res

        if inferred === nothing
            maybe_make_invoke!(ir, idx, stmt.typ, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            continue
        end

        src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), inferred)
        src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), inferred)

        if !(src_inferred && src_inlineable)
            maybe_make_invoke!(ir, idx, stmt.typ, atypes, sv, atype_unlimited, isinvoke, isapply, invoke_data)
            continue
        end

        # At this point we're committedd to performing the inlining, add the backedge
        add_backedge!(linfo, sv)

        if isa(inferred, CodeInfo)
            src = inferred
            ast = copy_exprargs(inferred.code)
        else
            src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred::Vector{UInt8})::CodeInfo
            # TODO: It seems like PhiNodes are shared between compressed codeinfos, making this copy necessary
            ast = copy_exprargs(src.code)
        end

        @timeit "inline IR inflation" if src.codelocs === nothing
            topline = LineInfoNode(method.module, method.name, method.file, Int(method.line), 0)
            inline_linetable = [topline]
            push!(ast, LabelNode(length(ast) + 1))
            ir2 = just_construct_ssa(src, ast, na-1, inline_linetable)
        else
            ir2, inline_linetable = inflate_ir(src), src.linetable
        end
        #verify_ir(ir2)

        push!(todo, (idx, (isva, isinvoke, isapply, na), method, Any[methsp...], inline_linetable, ir2, linear_inline_eligible(ir2)))
    end
    todo
end

function mk_tuplecall!(compact, args, line_idx)
    e = Expr(:call, TOP_TUPLE, args...)
    e.typ = tuple_tfunc(Tuple{Any[widenconst(compact_exprtype(compact, x)) for x in args]...})
    return insert_node_here!(compact, e, e.typ, line_idx)
end

function linear_inline_eligible(ir)
    length(ir.cfg.blocks) == 1 || return false
    terminator = ir[SSAValue(last(ir.cfg.blocks[1].stmts))]
    isa(terminator, ReturnNode) || return false
    isdefined(terminator, :val) || return false
    return true
end

function compute_invoke_data(atypes, argexprs, sv)
    ft = widenconst(atypes[2])
    invoke_tt = widenconst(atypes[3])
    if !(isconcretetype(ft) || ft <: Type) || !isType(invoke_tt) ||
            has_free_typevars(invoke_tt) || has_free_typevars(ft) || (ft <: Builtin)
        # TODO: this is really aggressive at preventing inlining of closures. maybe drop `isconcretetype` requirement?
        return nothing
    end
    if !(isa(invoke_tt.parameters[1], Type) &&
            invoke_tt.parameters[1] <: Tuple)
        return nothing
    end
    invoke_tt = invoke_tt.parameters[1]
    invoke_types = rewrap_unionall(Tuple{ft, unwrap_unionall(invoke_tt).parameters...}, invoke_tt)
    invoke_entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt),
                            invoke_types, sv.params.world)
    invoke_entry === nothing && return nothing
    invoke_data = InvokeData(ft.name.mt, invoke_entry,
                             invoke_types, nothing, nothing)
    atype0 = atypes[2]
    argexpr0 = argexprs[2]
    atypes = atypes[4:end]
    argexprs = argexprs[4:end]
    pushfirst!(atypes, atype0)
    pushfirst!(argexprs, argexpr0)
    f = isdefined(ft, :instance) ? ft.instance : nothing
    (f, ft, atypes, argexprs, invoke_data)
end

function early_inline_special_case(ir::IRCode, @nospecialize(f), @nospecialize(ft), e::Expr, atypes::Vector{Any})
    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes) == 3
        # typeassert(x::S, T) => x, when S<:T
        a3 = atypes[3]
        if (isType(a3) && !has_free_typevars(a3) && atypes[2] ⊑ a3.parameters[1]) ||
            (isa(a3, Const) && isa(a3.val, Type) && atypes[2] ⊑ a3.val)
            return (e.args[2],)
        end
    end
    topmod = _topmod(ir.mod)
    # special-case inliners for known pure functions that compute types
    if true #sv.params.inlining
        if isa(e.typ, Const) # || isconstType(e.typ)
            val = e.typ.val
            if (f === apply_type || f === fieldtype || f === typeof || f === (===) ||
                f === Core.sizeof || f === isdefined ||
                istopfunction(topmod, f, :typejoin) ||
                istopfunction(topmod, f, :isbits) ||
                istopfunction(topmod, f, :promote_type) ||
                (f === Core.kwfunc && length(atypes) == 2) ||
                (is_inlineable_constant(val) &&
                 (contains_is(_PURE_BUILTINS, f) ||
                  (f === getfield && effect_free(e, ir, ir.mod, false)) ||
                  (isa(f, IntrinsicFunction) && is_pure_intrinsic_optim(f)))))
                return (quoted(val),)
            end
        end
    end

    return nothing
end

function late_inline_special_case!(ir::IRCode, idx::Int, stmt::Expr, atypes::Vector{Any}, @nospecialize(f), @nospecialize(ft), topmod::Module)
    if length(atypes) == 3 && istopfunction(topmod, f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
        cmp_call = Expr(:call, GlobalRef(Core, :(===)), stmt.args[2], stmt.args[3])
        cmp_call.typ = Bool
        cmp_call_ssa = insert_node!(ir, idx, Bool, cmp_call)
        not_call = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), cmp_call_ssa)
        not_call.typ = Bool
        ir[SSAValue(idx)] = not_call
        return true
    elseif length(atypes) == 3 && istopfunction(topmod, f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
        subtype_call = Expr(:call, GlobalRef(Core, :(<:)), arg_T2, arg_T1)
        subtype_call.typ = Bool
        ir[SSAValue(idx)] = subtype_call
        return true
    elseif f === return_type
        if isconstType(stmt.typ)
            ir[SSAValue(idx)] = quoted(stmt.typ.parameters[1])
            return true
        elseif isa(stmt.typ, Const)
            ir[SSAValue(idx)] = quoted(stmt.typ.val)
            return true
        end
    end
    return false
end

function ssa_substitute!(val, arg_replacements, spsig, spvals)
    if isa(val, Argument)
        return arg_replacements[val.n]
    end
    if isa(val, Expr)
        e = val::Expr
        head = e.head
        if head === :static_parameter
            return quoted(spvals[e.args[1]])
        elseif head === :foreigncall
            @assert !isa(spsig, UnionAll) || !isempty(spvals)
            for i = 1:length(e.args)
                if i == 2
                    e.args[2] = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), e.args[2], spsig, spvals)
                elseif i == 3
                    argtuple = Any[
                        ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, spvals)
                        for argt
                        in e.args[3] ]
                    e.args[3] = svec(argtuple...)
                end
            end
    #=
        elseif head === :boundscheck
            if boundscheck === :propagate
                return e
            elseif boundscheck === :off
                return false
            else
                return true
            end
    =#
        end
    end
    urs = userefs(val)
    urs === () && return val
    for op in urs
        op[] = ssa_substitute!(op[], arg_replacements, spsig, spvals)
    end
    urs[]
end

function find_inferred!(ir, idx, linfo, atypes, sv)
    # see if the method has a InferenceResult in the current cache
    # or an existing inferred code info store in `.inferred`
    haveconst = false
    for i in 1:length(atypes)
        a = atypes[i]
        if isa(a, Const) && !isdefined(typeof(a.val), :instance) && !(isa(a.val, Type) && issingletontype(a.val))
            # have new information from argtypes that wasn't available from the signature
            haveconst = true
            break
        end
    end
    if haveconst
        inf_result = cache_lookup(linfo, atypes, sv.params.cache) # Union{Nothing, InferenceResult}
    else
        inf_result = nothing
    end
    if isa(inf_result, InferenceResult) && isa(inf_result.src, CodeInfo)
        linfo = inf_result.linfo
        result = inf_result.result
        if (inf_result.src::CodeInfo).pure
            if isa(result, Const)
                inferred_const = result.val
            elseif isconstType(result)
                inferred_const = result.parameters[1]
            end
            if @isdefined(inferred_const) && is_inlineable_constant(inferred_const)
                add_backedge!(linfo, sv)
                ir[SSAValue(idx)] = quoted(inferred_const)
                return nothing
            end
        end
        inferred = inf_result.src
        rettype = widenconst(result)
    elseif isdefined(linfo, :inferred)
        inferred = linfo.inferred
        rettype = linfo.rettype
    else
        rettype = Any
        inferred = nothing
    end
    return (rettype, inferred)
end
