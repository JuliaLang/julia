
function codeinst_to_ir(interp::AbstractInterpreter, code::CodeInstance)
    src = code.inferred
    mi = code.def

    if isa(src, Vector{UInt8})
        src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src)::CodeInfo
    end

    isa(src, CodeInfo) || return src

    return inflate_ir(src, mi)
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, @nospecialize(atype),
                                  sv::IRCode, max_methods::Int)
    return CallMeta(Any, Effects(), false)
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
    ssa_uses = zeros(Int, nssas)
    data = Int[]
    complete = false
    return TwoPhaseDefUseMap(ssa_uses, data, complete)
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
    kill_def_use!(tpdum, def.id, use)

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

function concrete_eval_invoke(interp::AbstractInterpreter, ir::IRCode, mi_cache,
    inst::Expr, mi::MethodInstance)
    code = get(mi_cache, mi, nothing)
    code === nothing && return nothing
    argtypes = collect_argtypes(interp, inst.args[2:end], nothing, ir)
    effects = decode_effects(code.ipo_purity_bits)
    if is_foldable(effects) && is_all_const_arg(argtypes)
        args = collect_semi_const_args(argtypes, 1)
        world = get_world_counter(interp)
        value = try
            Core._call_in_world_total(world, args...)
        catch
            return Union{}
        end
        if is_inlineable_constant(value)
            return Const(value)
        end
    else
        ir′ = codeinst_to_ir(interp, code)
        if ir′ !== nothing
            return _ir_abstract_constant_propagation(interp, mi_cache, mi, ir′, argtypes)
        end
    end
    return nothing
end

function reprocess_instruction!(interp::AbstractInterpreter, ir::IRCode, mi::MethodInstance,
                                mi_cache,
                                tpdum::TwoPhaseDefUseMap, idx::Int, bb::Union{Int, Nothing},
                                @nospecialize(inst), @nospecialize(typ),
                                phi_revisit::BitSet)
    function update_phi!(from::Int, to::Int)
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
            if isa(inst, PhiNode) || inst.head === :call || inst.head === :foreigncall || inst.head === :new
                if isa(inst, PhiNode)
                    rt = abstract_eval_phi(interp, inst, nothing, ir)
                else
                    (;rt, effects) = abstract_eval_statement_expr(interp, inst, nothing, ir, mi)
                    # All other effects already guaranteed effect free by construction
                    if is_nothrow(effects)
                        ir.stmts[idx][:flag] |= IR_FLAG_EFFECT_FREE
                    end
                end
                if !⊑(typeinf_lattice(interp), typ, rt)
                    ir.stmts[idx][:type] = rt
                    return true
                end
            elseif inst.head === :invoke
                mi′ = inst.args[1]::MethodInstance
                if mi′ !== mi # prevent infinite loop
                    rr = concrete_eval_invoke(interp, ir, mi_cache, inst, mi′)
                    if rr !== nothing
                        if !⊑(typeinf_lattice(interp), typ, rr)
                            ir.stmts[idx][:type] = rr
                            return true
                        end
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

function _ir_abstract_constant_propagation(interp::AbstractInterpreter, mi_cache,
        mi::MethodInstance, ir::IRCode, argtypes::Vector{Any}; extra_reprocess = nothing)
    argtypes = va_process_argtypes(argtypes, mi)
    argtypes_refined = Bool[!⊑(typeinf_lattice(interp), ir.argtypes[i], argtypes[i]) for i = 1:length(argtypes)]
    empty!(ir.argtypes)
    append!(ir.argtypes, argtypes)
    ssa_refined = BitSet()

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
    function process_terminator!(ip::BitSetBoundedMinPrioritySet, bb::Int, idx::Int)
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
            any_refined = extra_reprocess === nothing ? false : (idx in extra_reprocess)
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
            if any_refined && reprocess_instruction!(interp, ir, mi, mi_cache,
                    tpdum, idx, bb, inst, typ, ssa_refined)
                push!(ssa_refined, idx)
            end
            if idx == lstmt && process_terminator!(ip, bb, idx)
                @goto residual_scan
            end
            if typ === Bottom && !isa(inst, PhiNode)
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
        if reprocess_instruction!(interp, ir, mi, mi_cache,
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
        inst = ir.stmts[idx][:inst]::ReturnNode
        rt = argextype(inst.val, ir)
        ultimate_rt = tmerge(ultimate_rt, rt)
    end

    return ultimate_rt
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, mi_cache,
    frame::InferenceState, mi::MethodInstance, ir::IRCode, argtypes::Vector{Any})
    if __measure_typeinf__[]
        inf_frame = Timings.InferenceFrameInfo(mi, frame.world, Any[], Any[], length(ir.argtypes))
        Timings.enter_new_timer(inf_frame)
        v = _ir_abstract_constant_propagation(interp, mi_cache, mi, ir, argtypes)
        append!(inf_frame.slottypes, ir.argtypes)
        Timings.exit_current_timer(inf_frame)
        return v
    else
        T = _ir_abstract_constant_propagation(interp, mi_cache, mi, ir, argtypes)
        return T
    end
end
