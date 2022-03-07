
function codeinst_to_ir(interp::AbstractInterpreter, code::CodeInstance)
    src = code.inferred
    mi = code.def

    src === nothing && return src

    if !isa(src, CodeInfo)
        src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src::Vector{UInt8})::CodeInfo
    end

    return inflate_ir(src, mi)
end

function tristate_merge!(ir::IRCode, e::Effects)
    nothing
end

function collect_const_args2(argtypes::Vector{Any})
    return Any[ let a = widenconditional(argtypes[i])
                    isa(a, Const) ? a.val :
                    isconstType(a) ? (a::DataType).parameters[1] :
                    (a::DataType).instance
                end for i in 1:length(argtypes) ]
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, @nospecialize(atype),
                                  sv::IRCode, max_methods::Int)
    return CallMeta(Any, false, Effects())
end

mutable struct TwoPhaseVectorView <: AbstractVector{Int}
    const data::Vector{Int}
    count::Int
    const range::UnitRange{Int}
end
size(tpvv::TwoPhaseVectorView) = (tpvv.count,)
function getindex(tpvv::TwoPhaseVectorView, i::Int)
    checkbounds(tpvv, i)
    @inbounds tpvv.data[first(tpvv.range) + i - 1]
end
function push!(tpvv::TwoPhaseVectorView, v::Int)
    tpvv.count += 1
    tpvv.data[first(tpvv.range) + tpvv.count - 1] = v
    return nothing
end

"""
    mutable struct TwoPhaseDefUseMap

This struct is intended as a memory- and GC-pressure-efficient mechanism
for incrementally computing def-use maps. The idea is that the def-use map
is constructed into two passes over the IR. In the first, we simply count the
the number of uses, computing the number of uses for each def as well as the
total number of uses. In the second pass, we actually fill in the def-use
information.

The idea is that either of these two phases can be combined with other useful
work that needs to scan the instruction stream anyway, while avoiding the
significant allocation pressure of e.g. allocating an array for every SSA value
or attempting to dynamically move things around as new uses are discovered.

The def-use map is presented as a vector of vectors. For every def, indexing
into the map will return a vector of uses.
"""
mutable struct TwoPhaseDefUseMap <: AbstractVector{TwoPhaseVectorView}
    ssa_uses::Vector{Int}
    data::Vector{Int}
    complete::Bool
end

function complete!(tpdum::TwoPhaseDefUseMap)
    cumsum = 0
    for i = 1:length(tpdum.ssa_uses)
        this_val = cumsum + 1
        cumsum += tpdum.ssa_uses[i]
        tpdum.ssa_uses[i] = this_val
    end
    resize!(tpdum.data, cumsum)
    fill!(tpdum.data, 0)
    tpdum.complete = true
end

function TwoPhaseDefUseMap(nssas::Int)
    TwoPhaseDefUseMap(zeros(Int, nssas),
        Int[], false)
end

function count!(tpdum::TwoPhaseDefUseMap, arg::SSAValue)
    @assert !tpdum.complete
    tpdum.ssa_uses[arg.id] += 1
end

function kill_def_use!(tpdum::TwoPhaseDefUseMap, def::Int, use::Int)
    if !tpdum.complete
        tpdum.ssa_uses[def] -= 1
    else
        @assert false && "TODO"
    end
end
kill_def_use!(tpdum::TwoPhaseDefUseMap, def::SSAValue, use::Int) =
    kill_def_use!(tpdum::TwoPhaseDefUseMap, def.id, use)

function getindex(tpdum::TwoPhaseDefUseMap, idx::Int)
    @assert tpdum.complete
    range = tpdum.ssa_uses[idx]:(idx == length(tpdum.ssa_uses) ? length(tpdum.data) : (tpdum.ssa_uses[idx + 1] - 1))
    # TODO: Make logarithmic
    nelems = 0
    for i in range
        tpdum.data[i] == 0 && break
        nelems += 1
    end
    return TwoPhaseVectorView(tpdum.data, nelems, range)
end

function reprocess_instruction!(interp::AbstractInterpreter, ir::IRCode, mi_cache,
                                frame::InferenceState,
                                tpdum::TwoPhaseDefUseMap, idx::Int, bb::Union{Int, Nothing},
                                @nospecialize(inst), @nospecialize(typ),
                                phi_revisit)
    isnothrow = false

    function update_phi!(from, to)
        if length(ir.cfg.blocks[to].preds) == 0
            return
        end
        for idx in ir.cfg.blocks[to].stmts
            stmt = ir.stmts[idx][:inst]
            isa(stmt, Nothing) && continue
            isa(stmt, PhiNode) || break
            for (i, edge) in enumerate(stmt.edges)
                if edge == from
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                    push!(phi_revisit, idx)
                    break
                end
            end
        end
    end

    if isa(inst, GotoIfNot)
        cond = argextype(inst.cond, ir)
        if isa(cond, Const)
            if isa(inst.cond, SSAValue)
                kill_def_use!(tpdum, inst.cond, idx)
            end
            if bb === nothing
                bb = block_for_inst(ir, idx)
            end
            if (cond.val)::Bool
                ir.stmts[idx][:inst] = nothing
                kill_edge!(ir, bb, inst.dest, update_phi!)
            else
                ir.stmts[idx][:inst] = GotoNode(inst.dest)
                kill_edge!(ir, bb, bb+1, update_phi!)
            end
            return true
        end
        return false
    else
        if isa(inst, Expr) || isa(inst, PhiNode)
            if isa(inst, PhiNode) || inst.head === :call || inst.head === :new
                if isa(inst, PhiNode)
                    rt = abstract_eval_phi(interp, inst, nothing, ir)
                else
                    (;rt, effects) = abstract_eval_statement_expr(interp, inst, nothing, ir)
                    # All other effects already guaranteed effect free by construction
                    if effects.nothrow === ALWAYS_TRUE
                        ir.stmts[idx][:flag] |= IR_FLAG_EFFECT_FREE
                    end
                end
                if !(typ ⊑ rt)
                    ir.stmts[idx][:type] = rt
                    return true
                end
            elseif inst.head === :invoke
                mi′ = inst.args[1]
                argtypes = collect_argtypes(interp, inst.args[2:end], nothing, ir)
                code = get(mi_cache, mi′, nothing)
                if code !== nothing
                    effects = decode_effects(code.ipo_purity_bits)
                    if is_total_or_error(effects) && is_all_const_arg(argtypes)
                        args = collect_const_args2(argtypes)
                        try
                            value = Core._call_in_world_total(get_world_counter(interp), args...)
                            if is_inlineable_constant(value) || call_result_unused(sv)
                                # If the constant is not inlineable, still do the const-prop, since the
                                # code that led to the creation of the Const may be inlineable in the same
                                # circumstance and may be optimizable.
                                rr = Const(value)
                            end
                            isnothrow = true
                        catch
                            rr = Union{}
                        end
                    else
                        ir′ = codeinst_to_ir(interp, code)
                        if ir′ !== nothing
                            rr = ir_abstract_constant_propagation(interp, mi_cache, frame, mi′, ir′, argtypes)
                        else
                            rr = typ
                        end
                    end
                    if !(typ ⊑ rr)
                        ir.stmts[idx][:type] = rr
                        return true
                    end
                end
            else
                ccall(:jl_, Cvoid, (Any,), inst)
                error()
            end
        elseif isa(inst, ReturnNode)
            # Handled at the very end
            return false
        elseif isa(inst, PiNode)
            rr = tmeet(argextype(inst.val, ir), inst.typ)
            if !(typ ⊑ rr)
                ir.stmts[idx][:type] = rr
                return true
            end
        else
            ccall(:jl_, Cvoid, (Any,), inst)
            error()
        end
    end
    return false
end

function _ir_abstract_constant_propagation(interp::AbstractInterpreter, mi_cache, frame::InferenceState, mi::MethodInstance, ir, argtypes)
    argtypes = va_process_argtypes(argtypes, mi)
    argtypes_refined = Bool[!(ir.argtypes[i] ⊑ argtypes[i]) for i = 1:length(argtypes)]
    empty!(ir.argtypes)
    append!(ir.argtypes, argtypes)
    ssa_refined = BitSet()

    ultimate_rt = Union{}
    bbs = ir.cfg.blocks
    ip = BitSetBoundedMinPrioritySet(length(bbs))
    push!(ip, 1)
    all_rets = Int[]

    tpdum = TwoPhaseDefUseMap(length(ir.stmts))

    """
        process_terminator!

    Process the terminator and add the successor to `ip`. Returns whether a
    backedge was seen.
    """
    function process_terminator!(ip, bb, idx)
        inst = ir.stmts[idx][:inst]
        if isa(inst, ReturnNode)
            if isdefined(inst, :val)
                push!(all_rets, idx)
            end
            return false
        elseif isa(inst, GotoNode)
            backedge = inst.label < bb
            !backedge && push!(ip, inst.label)
            return backedge
        elseif isa(inst, GotoIfNot)
            backedge = inst.dest < bb
            !backedge && push!(ip, inst.dest)
            push!(ip, bb + 1)
            return backedge
        elseif isexpr(inst, :enter)
            dest = inst.args[1]::Int
            @assert dest > bb
            push!(ip, dest)
            push!(ip, bb + 1)
            return false
        else
            push!(ip, bb + 1)
            return false
        end
    end

    # Fast path: Scan both use counts and refinement in one single pass of
    #            of the instructions. In the absence of backedges, this will
    #            converge.
    while !isempty(ip)
        bb = popfirst!(ip)
        stmts = bbs[bb].stmts
        lstmt = last(stmts)
        for idx = stmts
            inst = ir.stmts[idx][:inst]
            typ = ir.stmts[idx][:type]
            any_refined = false
            for ur in userefs(inst)
                val = ur[]
                if isa(val, Argument)
                    any_refined |= argtypes_refined[val.n]
                elseif isa(val, SSAValue)
                    any_refined |= val.id in ssa_refined
                    count!(tpdum, val)
                end
            end
            if isa(inst, PhiNode) && idx in ssa_refined
                any_refined = true
                delete!(ssa_refined, idx)
            end
            if any_refined && reprocess_instruction!(interp, ir, mi_cache,
                    frame, tpdum, idx, bb, inst, typ, ssa_refined)
                push!(ssa_refined, idx)
            end
            if idx == lstmt && process_terminator!(ip, bb, idx)
                @goto residual_scan
            end
            if ir.stmts[idx][:type] === Bottom
                break
            end
        end
    end
    @goto compute_rt

@label residual_scan
    stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))
    # Slow Path Phase 1.A: Complete use scanning
    while !isempty(ip)
        bb = popfirst!(ip)
        stmts = bbs[bb].stmts
        lstmt = last(stmts)
        for idx = stmts
            inst = ir.stmts[idx][:inst]
            typ = ir.stmts[idx][:type]
            for ur in userefs(inst)
                val = ur[]
                if isa(val, Argument)
                    if argtypes_refined[val.n]
                        push!(stmt_ip, idx)
                    end
                elseif isa(val, SSAValue)
                    count!(tpdum, val)
                end
            end
            idx == lstmt && process_terminator!(ip, bb, idx)
        end
    end

    # Slow Path Phase 1.B: Assemble def-use map
    complete!(tpdum)
    push!(ip, 1)
    while !isempty(ip)
        bb = popfirst!(ip)
        stmts = bbs[bb].stmts
        lstmt = last(stmts)
        for idx = stmts
            inst = ir.stmts[idx][:inst]
            typ = ir.stmts[idx][:type]
            for ur in userefs(inst)
                val = ur[]
                if isa(val, SSAValue)
                    push!(tpdum[val.id], idx)
                end
            end
            idx == lstmt && process_terminator!(ip, bb, idx)
        end
    end

    # Slow Path Phase 2: Use def-use map to converge cycles.
    # TODO: It would be possible to return to the fast path after converging
    #       each cycle, but that's somewhat complicated.
    for val in ssa_refined
        append!(stmt_ip, tpdum[val])
    end

    while !isempty(stmt_ip)
        idx = popfirst!(stmt_ip)
        inst = ir.stmts[idx][:inst]
        typ = ir.stmts[idx][:type]
        if reprocess_instruction!(interp, ir, mi_cache, frame,
                tpdum, idx, nothing, inst, typ, ssa_refined)
            append!(stmt_ip, tpdum[idx])
        end
    end

@label compute_rt
    ultimate_rt = Union{}
    for idx in all_rets
        bb = block_for_inst(ir.cfg, idx)
        if bb != 1 && length(ir.cfg.blocks[bb].preds) == 0
            # Could have discovered this block is dead after the initial scan
            continue
        end
        inst = ir.stmts[idx][:inst]
        ultimate_rt = tmerge(ultimate_rt, argextype(inst.val, ir))
    end
    return ultimate_rt
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, mi_cache, frame::InferenceState, mi::MethodInstance, ir, argtypes)
    if __measure_typeinf__[]
        inf_frame = Timings.InferenceFrameInfo(mi, frame.world, Any[], Any[], length(ir.argtypes))
        Timings.enter_new_timer(inf_frame)
        v = _ir_abstract_constant_propagation(interp, mi_cache, frame, mi, ir, argtypes)
        append!(inf_frame.slottypes, ir.argtypes)
        Timings.exit_current_timer(inf_frame)
        return v
    else
        return _ir_abstract_constant_propagation(interp, mi_cache, frame, mi, ir, argtypes)
    end
end
