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
        range = tpdum.ssa_uses[def]:(def == length(tpdum.ssa_uses) ? length(tpdum.data) : (tpdum.ssa_uses[def + 1] - 1))
        # TODO: Sorted
        useidx = findfirst(idx->tpdum.data[idx] == use, range)
        @assert useidx !== nothing
        idx = range[useidx]
        while idx < lastindex(range)
            ndata = tpdum.data[idx+1]
            ndata == 0 && break
            tpdum.data[idx] = ndata
        end
        tpdum.data[idx + 1] = 0
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

struct IRInterpretationState
    ir::IRCode
    mi::MethodInstance
    world::UInt
    argtypes_refined::Vector{Bool}
    tpdum::TwoPhaseDefUseMap
    ssa_refined::BitSet
    lazydomtree::LazyDomtree
    function IRInterpretationState(interp::AbstractInterpreter,
        ir::IRCode, mi::MethodInstance, world::UInt, argtypes::Vector{Any})
        argtypes = va_process_argtypes(optimizer_lattice(interp), argtypes, mi)
        for i = 1:length(argtypes)
            argtypes[i] = widenslotwrapper(argtypes[i])
        end
        argtypes_refined = Bool[!⊑(optimizer_lattice(interp), ir.argtypes[i], argtypes[i]) for i = 1:length(argtypes)]
        empty!(ir.argtypes)
        append!(ir.argtypes, argtypes)
        tpdum = TwoPhaseDefUseMap(length(ir.stmts))
        ssa_refined = BitSet()
        lazydomtree = LazyDomtree(ir)
        return new(ir, mi, world, argtypes_refined, tpdum, ssa_refined, lazydomtree)
    end
end

function codeinst_to_ir(interp::AbstractInterpreter, code::CodeInstance)
    src = @atomic :monotonic code.inferred
    mi = code.def
    if isa(src, Vector{UInt8})
        src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src)::CodeInfo
    else
        isa(src, CodeInfo) || return nothing
    end
    return inflate_ir(src, mi)
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype),
                                  sv::IRCode, max_methods::Int)
    return CallMeta(Any, Effects(), NoCallInfo())
end

function collect_limitations!(@nospecialize(typ), ::IRCode)
    @assert !isa(typ, LimitedAccuracy) "semi-concrete eval on recursive call graph"
    return typ
end

function concrete_eval_invoke(interp::AbstractInterpreter,
    inst::Expr, mi::MethodInstance, irsv::IRInterpretationState)
    mi_cache = WorldView(code_cache(interp), irsv.world)
    code = get(mi_cache, mi, nothing)
    if code === nothing
        return Pair{Any, Bool}(nothing, false)
    end
    argtypes = collect_argtypes(interp, inst.args[2:end], nothing, irsv.ir)
    argtypes === nothing && return Pair{Any, Bool}(Union{}, false)
    effects = decode_effects(code.ipo_purity_bits)
    if is_foldable(effects) && is_all_const_arg(argtypes, #=start=#1)
        args = collect_const_args(argtypes, #=start=#1)
        world = get_world_counter(interp)
        value = try
            Core._call_in_world_total(world, args...)
        catch
            return Pair{Any, Bool}(Union{}, false)
        end
        return Pair{Any, Bool}(Const(value), true)
    else
        ir′ = codeinst_to_ir(interp, code)
        if ir′ !== nothing
            irsv′ = IRInterpretationState(interp, ir′, mi, irsv.world, argtypes)
            return _ir_abstract_constant_propagation(interp, irsv′)
        end
    end
    return Pair{Any, Bool}(nothing, is_nothrow(effects))
end

function abstract_eval_phi_stmt(interp::AbstractInterpreter, phi::PhiNode, ::Int, irsv::IRInterpretationState)
    return abstract_eval_phi(interp, phi, nothing, irsv.ir)
end

function propagate_control_effects!(interp::AbstractInterpreter, idx::Int, stmt::GotoIfNot,
        irsv::IRInterpretationState, reprocess::Union{Nothing, BitSet, BitSetBoundedMinPrioritySet})
    # Nothing to do for most abstract interpreters, but if the abstract
    # interpreter has control-dependent lattice effects, it can override
    # this method.
    return false
end

function reprocess_instruction!(interp::AbstractInterpreter,
    idx::Int, bb::Union{Int, Nothing}, @nospecialize(inst), @nospecialize(typ),
    irsv::IRInterpretationState, reprocess::Union{Nothing, BitSet, BitSetBoundedMinPrioritySet})
    ir = irsv.ir
    if isa(inst, GotoIfNot)
        cond = inst.cond
        condval = maybe_extract_const_bool(argextype(cond, ir))
        if condval isa Bool
            function update_phi!(from::Int, to::Int)
                if length(ir.cfg.blocks[to].preds) == 0
                    # Kill the entire block
                    for idx in ir.cfg.blocks[to].stmts
                        ir.stmts[idx][:inst] = nothing
                        ir.stmts[idx][:type] = Union{}
                        ir.stmts[idx][:flag] = IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
                    end
                    return
                end
                for idx in ir.cfg.blocks[to].stmts
                    stmt = ir.stmts[idx][:inst]
                    isa(stmt, Nothing) && continue # allowed between `PhiNode`s
                    isa(stmt, PhiNode) || break
                    for (i, edge) in enumerate(stmt.edges)
                        if edge == from
                            deleteat!(stmt.edges, i)
                            deleteat!(stmt.values, i)
                            push!(irsv.ssa_refined, idx)
                            break
                        end
                    end
                end
            end
            if isa(cond, SSAValue)
                kill_def_use!(irsv.tpdum, cond, idx)
            end
            if bb === nothing
                bb = block_for_inst(ir, idx)
            end
            ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
            if condval
                ir.stmts[idx][:inst] = nothing
                ir.stmts[idx][:type] = Any
                kill_edge!(ir, bb, inst.dest, update_phi!)
            else
                ir.stmts[idx][:inst] = GotoNode(inst.dest)
                kill_edge!(ir, bb, bb+1, update_phi!)
            end
            return true
        end
        return propagate_control_effects!(interp, idx, inst, irsv, reprocess)
    end

    rt = nothing
    if isa(inst, Expr)
        head = inst.head
        if head === :call || head === :foreigncall || head === :new || head === :splatnew
            (; rt, effects) = abstract_eval_statement_expr(interp, inst, nothing, ir, irsv.mi)
            ir.stmts[idx][:flag] |= flags_for_effects(effects)
            if is_foldable(effects) && isa(rt, Const) && is_inlineable_constant(rt.val)
                ir.stmts[idx][:inst] = quoted(rt.val)
            end
        elseif head === :invoke
            mi′ = inst.args[1]::MethodInstance
            if mi′ !== irsv.mi # prevent infinite loop
                rt, nothrow = concrete_eval_invoke(interp, inst, mi′, irsv)
                if nothrow
                    ir.stmts[idx][:flag] |= IR_FLAG_NOTHROW
                    if isa(rt, Const) && is_inlineable_constant(rt.val)
                        ir.stmts[idx][:inst] = quoted(rt.val)
                    end
                end
            end
        elseif head === :throw_undef_if_not || # TODO: Terminate interpretation early if known false?
               head === :gc_preserve_begin ||
               head === :gc_preserve_end
            return false
        else
            ccall(:jl_, Cvoid, (Any,), inst)
            error("reprocess_instruction!: unhandled expression found")
        end
    elseif isa(inst, PhiNode)
        rt = abstract_eval_phi_stmt(interp, inst, idx, irsv)
    elseif isa(inst, ReturnNode)
        # Handled at the very end
        return false
    elseif isa(inst, PiNode)
        rt = tmeet(optimizer_lattice(interp), argextype(inst.val, ir), widenconst(inst.typ))
    elseif inst === nothing
        return false
    elseif isa(inst, GlobalRef)
        # GlobalRef is not refinable
    else
        ccall(:jl_, Cvoid, (Any,), inst)
        error()
    end
    if rt !== nothing && !⊑(optimizer_lattice(interp), typ, rt)
        ir.stmts[idx][:type] = rt
        return true
    end
    return false
end

# Process the terminator and add the successor to `ip`. Returns whether a backedge was seen.
function process_terminator!(ir::IRCode, idx::Int, bb::Int,
    all_rets::Vector{Int}, ip::BitSetBoundedMinPrioritySet)
    inst = ir.stmts[idx][:inst]
    if isa(inst, ReturnNode)
        if isdefined(inst, :val)
            push!(all_rets, idx)
        end
        return false
    elseif isa(inst, GotoNode)
        backedge = inst.label <= bb
        !backedge && push!(ip, inst.label)
        return backedge
    elseif isa(inst, GotoIfNot)
        backedge = inst.dest <= bb
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

default_reprocess(interp::AbstractInterpreter, irsv::IRInterpretationState) = nothing
function _ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState;
    extra_reprocess::Union{Nothing,BitSet} = default_reprocess(interp, irsv))
    (; ir, tpdum, ssa_refined) = irsv

    bbs = ir.cfg.blocks
    ip = BitSetBoundedMinPrioritySet(length(bbs))
    push!(ip, 1)
    all_rets = Int[]

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
            if extra_reprocess !== nothing
                if idx in extra_reprocess
                    pop!(extra_reprocess, idx)
                    any_refined = true
                end
            end
            for ur in userefs(inst)
                val = ur[]
                if isa(val, Argument)
                    any_refined |= irsv.argtypes_refined[val.n]
                elseif isa(val, SSAValue)
                    any_refined |= val.id in ssa_refined
                    count!(tpdum, val)
                end
            end
            if isa(inst, PhiNode) && idx in ssa_refined
                any_refined = true
                delete!(ssa_refined, idx)
            end
            if any_refined && reprocess_instruction!(interp,
                idx, bb, inst, typ, irsv, extra_reprocess)
                push!(ssa_refined, idx)
            end
            if idx == lstmt
                if process_terminator!(ir, idx, bb, all_rets, ip)
                    @goto residual_scan
                end
            end
            if typ === Bottom && !isa(inst, PhiNode)
                break
            end
        end
    end
    @goto compute_rt

    # Slow path
    begin @label residual_scan
        stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))
        if extra_reprocess !== nothing
            append!(stmt_ip, extra_reprocess)
        end

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
                        if irsv.argtypes_refined[val.n]
                            push!(stmt_ip, idx)
                        end
                    elseif isa(val, SSAValue)
                        count!(tpdum, val)
                    end
                end
                idx == lstmt && process_terminator!(ir, idx, bb, all_rets, ip)
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
                idx == lstmt && process_terminator!(ir, idx, bb, all_rets, ip)
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
            if reprocess_instruction!(interp,
                idx, nothing, inst, typ, irsv, stmt_ip)
                append!(stmt_ip, tpdum[idx])
            end
        end
    end

    begin @label compute_rt
        ultimate_rt = Union{}
        for idx in all_rets
            bb = block_for_inst(ir.cfg, idx)
            if bb != 1 && length(ir.cfg.blocks[bb].preds) == 0
                # Could have discovered this block is dead after the initial scan
                continue
            end
            inst = ir.stmts[idx][:inst]::ReturnNode
            rt = argextype(inst.val, ir)
            ultimate_rt = tmerge(optimizer_lattice(interp), ultimate_rt, rt)
        end
    end

    nothrow = true
    for i = 1:length(ir.stmts)
        if (ir.stmts[i][:flag] & IR_FLAG_NOTHROW) == 0
            nothrow = false
            break
        end
    end

    return Pair{Any, Bool}(maybe_singleton_const(ultimate_rt), nothrow)
end

function ir_abstract_constant_propagation(interp::AbstractInterpreter, irsv::IRInterpretationState)
    if __measure_typeinf__[]
        inf_frame = Timings.InferenceFrameInfo(irsv.mi, irsv.world, VarState[], Any[], length(irsv.ir.argtypes))
        Timings.enter_new_timer(inf_frame)
        v = _ir_abstract_constant_propagation(interp, irsv)
        append!(inf_frame.slottypes, irsv.ir.argtypes)
        Timings.exit_current_timer(inf_frame)
        return v
    else
        T = _ir_abstract_constant_propagation(interp, irsv)
        return T
    end
end
