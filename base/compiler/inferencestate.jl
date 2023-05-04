# This file is a part of Julia. License is MIT: https://julialang.org/license

# data structures
# ===============

mutable struct BitSetBoundedMinPrioritySet <: AbstractSet{Int}
    elems::BitSet
    min::Int
    # Stores whether min is exact or a lower bound
    # If exact, it is not set in elems
    min_exact::Bool
    max::Int
end

function BitSetBoundedMinPrioritySet(max::Int)
    bs = BitSet()
    bs.offset = 0
    BitSetBoundedMinPrioritySet(bs, max+1, true, max)
end

@noinline function _advance_bsbmp!(bsbmp::BitSetBoundedMinPrioritySet)
    @assert !bsbmp.min_exact
    bsbmp.min = _bits_findnext(bsbmp.elems.bits, bsbmp.min)::Int
    bsbmp.min < 0 && (bsbmp.min = bsbmp.max + 1)
    bsbmp.min_exact = true
    delete!(bsbmp.elems, bsbmp.min)
    return nothing
end

function isempty(bsbmp::BitSetBoundedMinPrioritySet)
    if bsbmp.min > bsbmp.max
        return true
    end
    bsbmp.min_exact && return false
    _advance_bsbmp!(bsbmp)
    return bsbmp.min > bsbmp.max
end

function popfirst!(bsbmp::BitSetBoundedMinPrioritySet)
    bsbmp.min_exact || _advance_bsbmp!(bsbmp)
    m = bsbmp.min
    m > bsbmp.max && throw(ArgumentError("BitSetBoundedMinPrioritySet must be non-empty"))
    bsbmp.min = m+1
    bsbmp.min_exact = false
    return m
end

function push!(bsbmp::BitSetBoundedMinPrioritySet, idx::Int)
    if idx <= bsbmp.min
        if bsbmp.min_exact && bsbmp.min < bsbmp.max && idx != bsbmp.min
            push!(bsbmp.elems, bsbmp.min)
        end
        bsbmp.min = idx
        bsbmp.min_exact = true
        return nothing
    end
    push!(bsbmp.elems, idx)
    return nothing
end

function in(idx::Int, bsbmp::BitSetBoundedMinPrioritySet)
    if bsbmp.min_exact && idx == bsbmp.min
        return true
    end
    return idx in bsbmp.elems
end

function append!(bsbmp::BitSetBoundedMinPrioritySet, itr)
    for val in itr
        push!(bsbmp, val)
    end
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

mutable struct LazyGenericDomtree{IsPostDom}
    ir::IRCode
    domtree::GenericDomTree{IsPostDom}
    LazyGenericDomtree{IsPostDom}(ir::IRCode) where {IsPostDom} = new{IsPostDom}(ir)
end
function get!(x::LazyGenericDomtree{IsPostDom}) where {IsPostDom}
    isdefined(x, :domtree) && return x.domtree
    return @timeit "domtree 2" x.domtree = IsPostDom ?
        construct_postdomtree(x.ir.cfg.blocks) :
        construct_domtree(x.ir.cfg.blocks)
end

const LazyDomtree = LazyGenericDomtree{false}
const LazyPostDomtree = LazyGenericDomtree{true}

# InferenceState
# ==============

"""
    const VarTable = Vector{VarState}

The extended lattice that maps local variables to inferred type represented as `AbstractLattice`.
Each index corresponds to the `id` of `SlotNumber` which identifies each local variable.
Note that `InferenceState` will maintain multiple `VarTable`s at each SSA statement
to enable flow-sensitive analysis.
"""
const VarTable = Vector{VarState}

mutable struct InferenceState
    #= information about this method instance =#
    linfo::MethodInstance
    world::UInt
    mod::Module
    sptypes::Vector{VarState}
    slottypes::Vector{Any}
    src::CodeInfo
    cfg::CFG
    method_info::MethodInfo

    #= intermediate states for local abstract interpretation =#
    currbb::Int
    currpc::Int
    ip::BitSet#=TODO BoundedMinPrioritySet=# # current active instruction pointers
    handler_at::Vector{Int} # current exception handler info
    ssavalue_uses::Vector{BitSet} # ssavalue sparsity and restart info
    # TODO: Could keep this sparsely by doing structural liveness analysis ahead of time.
    bb_vartables::Vector{Union{Nothing,VarTable}} # nothing if not analyzed yet
    ssavaluetypes::Vector{Any}
    stmt_edges::Vector{Union{Nothing,Vector{Any}}}
    stmt_info::Vector{CallInfo}

    #= intermediate states for interprocedural abstract interpretation =#
    pclimitations::IdSet{InferenceState} # causes of precision restrictions (LimitedAccuracy) on currpc ssavalue
    limitations::IdSet{InferenceState} # causes of precision restrictions (LimitedAccuracy) on return
    cycle_backedges::Vector{Tuple{InferenceState, Int}} # call-graph backedges connecting from callee to caller
    callers_in_cycle::Vector{InferenceState}
    dont_work_on_me::Bool
    parent # ::Union{Nothing,AbsIntState}

    #= results =#
    result::InferenceResult # remember where to put the result
    valid_worlds::WorldRange
    bestguess #::Type
    ipo_effects::Effects

    #= flags =#
    # Whether to restrict inference of abstract call sites to avoid excessive work
    # Set by default for toplevel frame.
    restrict_abstract_call_sites::Bool
    cached::Bool # TODO move this to InferenceResult?
    insert_coverage::Bool

    # The interpreter that created this inference state. Not looked at by
    # NativeInterpreter. But other interpreters may use this to detect cycles
    interp::AbstractInterpreter

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(result::InferenceResult, src::CodeInfo, cache::Symbol,
                            interp::AbstractInterpreter)
        linfo = result.linfo
        world = get_world_counter(interp)
        def = linfo.def
        mod = isa(def, Method) ? def.module : def
        sptypes = sptypes_from_meth_instance(linfo)
        code = src.code::Vector{Any}
        cfg = compute_basic_blocks(code)
        method_info = MethodInfo(src)

        currbb = currpc = 1
        ip = BitSet(1) # TODO BitSetBoundedMinPrioritySet(1)
        handler_at = compute_trycatch(code, BitSet())
        nssavalues = src.ssavaluetypes::Int
        ssavalue_uses = find_ssavalue_uses(code, nssavalues)
        nstmts = length(code)
        stmt_edges = Union{Nothing, Vector{Any}}[ nothing for i = 1:nstmts ]
        stmt_info = CallInfo[ NoCallInfo() for i = 1:nstmts ]

        nslots = length(src.slotflags)
        slottypes = Vector{Any}(undef, nslots)
        bb_vartables = Union{Nothing,VarTable}[ nothing for i = 1:length(cfg.blocks) ]
        bb_vartable1 = bb_vartables[1] = VarTable(undef, nslots)
        argtypes = result.argtypes
        nargtypes = length(argtypes)
        for i = 1:nslots
            argtyp = (i > nargtypes) ? Bottom : argtypes[i]
            slottypes[i] = argtyp
            bb_vartable1[i] = VarState(argtyp, i > nargtypes)
        end
        src.ssavaluetypes = ssavaluetypes = Any[ NOT_FOUND for i = 1:nssavalues ]

        pclimitations = IdSet{InferenceState}()
        limitations = IdSet{InferenceState}()
        cycle_backedges = Vector{Tuple{InferenceState,Int}}()
        callers_in_cycle = Vector{InferenceState}()
        dont_work_on_me = false
        parent = nothing

        valid_worlds = WorldRange(src.min_world, src.max_world == typemax(UInt) ? get_world_counter() : src.max_world)
        bestguess = Bottom
        ipo_effects = EFFECTS_TOTAL

        insert_coverage = should_insert_coverage(mod, src)
        if insert_coverage
            ipo_effects = Effects(ipo_effects; effect_free = ALWAYS_FALSE)
        end

        restrict_abstract_call_sites = isa(linfo.def, Module)
        @assert cache === :no || cache === :local || cache === :global
        cached = cache === :global

        # some more setups
        InferenceParams(interp).unoptimize_throw_blocks && mark_throw_blocks!(src, handler_at)
        cache !== :no && push!(get_inference_cache(interp), result)

        return new(
            linfo, world, mod, sptypes, slottypes, src, cfg, method_info,
            currbb, currpc, ip, handler_at, ssavalue_uses, bb_vartables, ssavaluetypes, stmt_edges, stmt_info,
            pclimitations, limitations, cycle_backedges, callers_in_cycle, dont_work_on_me, parent,
            result, valid_worlds, bestguess, ipo_effects,
            restrict_abstract_call_sites, cached, insert_coverage,
            interp)
    end
end

is_inferred(sv::InferenceState) = is_inferred(sv.result)
is_inferred(result::InferenceResult) = result.result !== nothing

was_reached(sv::InferenceState, pc::Int) = sv.ssavaluetypes[pc] !== NOT_FOUND

function compute_trycatch(code::Vector{Any}, ip::BitSet)
    # The goal initially is to record the frame like this for the state at exit:
    # 1: (enter 3) # == 0
    # 3: (expr)    # == 1
    # 3: (leave 1) # == 1
    # 4: (expr)    # == 0
    # then we can find all trys by walking backwards from :enter statements,
    # and all catches by looking at the statement after the :enter
    n = length(code)
    empty!(ip)
    ip.offset = 0 # for _bits_findnext
    push!(ip, n + 1)
    handler_at = fill(0, n)

    # start from all :enter statements and record the location of the try
    for pc = 1:n
        stmt = code[pc]
        if isexpr(stmt, :enter)
            l = stmt.args[1]::Int
            handler_at[pc + 1] = pc
            push!(ip, pc + 1)
            handler_at[l] = pc
            push!(ip, l)
        end
    end

    # now forward those marks to all :leave statements
    pc´´ = 0
    while true
        # make progress on the active ip set
        pc = _bits_findnext(ip.bits, pc´´)::Int
        pc > n && break
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            pc´ = pc + 1 # next program-counter (after executing instruction)
            if pc == pc´´
                pc´´ = pc´
            end
            delete!(ip, pc)
            cur_hand = handler_at[pc]
            @assert cur_hand != 0 "unbalanced try/catch"
            stmt = code[pc]
            if isa(stmt, GotoNode)
                pc´ = stmt.label
            elseif isa(stmt, GotoIfNot)
                l = stmt.dest::Int
                if handler_at[l] != cur_hand
                    @assert handler_at[l] == 0 "unbalanced try/catch"
                    handler_at[l] = cur_hand
                    if l < pc´´
                        pc´´ = l
                    end
                    push!(ip, l)
                end
            elseif isa(stmt, ReturnNode)
                @assert !isdefined(stmt, :val) "unbalanced try/catch"
                break
            elseif isa(stmt, Expr)
                head = stmt.head
                if head === :enter
                    cur_hand = pc
                elseif head === :leave
                    l = stmt.args[1]::Int
                    for i = 1:l
                        cur_hand = handler_at[cur_hand]
                    end
                    cur_hand == 0 && break
                end
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
            if handler_at[pc´] != cur_hand
                @assert handler_at[pc´] == 0 "unbalanced try/catch"
                handler_at[pc´] = cur_hand
            elseif !in(pc´, ip)
                break  # already visited
            end
            pc = pc´
        end
    end

    @assert first(ip) == n + 1
    return handler_at
end

# check if coverage mode is enabled
function should_insert_coverage(mod::Module, src::CodeInfo)
    coverage_enabled(mod) && return true
    JLOptions().code_coverage == 3 || return false
    # path-specific coverage mode: if any line falls in a tracked file enable coverage for all
    linetable = src.linetable
    if isa(linetable, Vector{Any})
        for line in linetable
            line = line::LineInfoNode
            if is_file_tracked(line.file)
                return true
            end
        end
    elseif isa(linetable, Vector{LineInfoNode})
        for line in linetable
            if is_file_tracked(line.file)
                return true
            end
        end
    end
    return false
end

function InferenceState(result::InferenceResult, cache::Symbol, interp::AbstractInterpreter)
    # prepare an InferenceState object for inferring lambda
    world = get_world_counter(interp)
    src = retrieve_code_info(result.linfo, world)
    src === nothing && return nothing
    validate_code_in_debug_mode(result.linfo, src, "lowered")
    return InferenceState(result, src, cache, interp)
end

"""
    constrains_param(var::TypeVar, sig, covariant::Bool, type_constrains::Bool)

Check if `var` will be constrained to have a definite value
in any concrete leaftype subtype of `sig`.

It is used as a helper to determine whether type intersection is guaranteed to be able to
find a value for a particular type parameter.
A necessary condition for type intersection to not assign a parameter is that it only
appears in a `Union[All]` and during subtyping some other union component (that does not
constrain the type parameter) is selected.

The `type_constrains` flag determines whether Type{T} is considered to be constraining
`T`. This is not true in general, because of the existence of types with free type
parameters, however, some callers would like to ignore this corner case.
"""
function constrains_param(var::TypeVar, @nospecialize(typ), covariant::Bool, type_constrains::Bool=false)
    typ === var && return true
    while typ isa UnionAll
        covariant && constrains_param(var, typ.var.ub, covariant, type_constrains) && return true
        # typ.var.lb doesn't constrain var
        typ = typ.body
    end
    if typ isa Union
        # for unions, verify that both options would constrain var
        ba = constrains_param(var, typ.a, covariant, type_constrains)
        bb = constrains_param(var, typ.b, covariant, type_constrains)
        (ba && bb) && return true
    elseif typ isa DataType
        # return true if any param constrains var
        fc = length(typ.parameters)
        if fc > 0
            if typ.name === Tuple.name
                # vararg tuple needs special handling
                for i in 1:(fc - 1)
                    p = typ.parameters[i]
                    constrains_param(var, p, covariant, type_constrains) && return true
                end
                lastp = typ.parameters[fc]
                vararg = unwrap_unionall(lastp)
                if vararg isa Core.TypeofVararg && isdefined(vararg, :N)
                    constrains_param(var, vararg.N, covariant, type_constrains) && return true
                    # T = vararg.parameters[1] doesn't constrain var
                else
                    constrains_param(var, lastp, covariant, type_constrains) && return true
                end
            else
                if typ.name === typename(Type) && typ.parameters[1] === var && var.ub === Any
                    # Types with free type parameters are <: Type cause the typevar
                    # to be unconstrained because Type{T} with free typevars is illegal
                    return type_constrains
                end
                for i in 1:fc
                    p = typ.parameters[i]
                    constrains_param(var, p, false, type_constrains) && return true
                end
            end
        end
    end
    return false
end

const EMPTY_SPTYPES = VarState[]

function sptypes_from_meth_instance(linfo::MethodInstance)
    def = linfo.def
    isa(def, Method) || return EMPTY_SPTYPES # toplevel
    sig = def.sig
    if isempty(linfo.sparam_vals)
        isa(sig, UnionAll) || return EMPTY_SPTYPES
        # linfo is unspecialized
        spvals = Any[]
        sig′ = sig
        while isa(sig′, UnionAll)
            push!(spvals, sig′.var)
            sig′ = sig′.body
        end
    else
        spvals = linfo.sparam_vals
    end
    nvals = length(spvals)
    sptypes = Vector{VarState}(undef, nvals)
    for i = 1:nvals
        v = spvals[i]
        if v isa TypeVar
            temp = sig
            for j = 1:i-1
                temp = temp.body
            end
            vᵢ = (temp::UnionAll).var
            sigtypes = (unwrap_unionall(temp)::DataType).parameters
            for j = 1:length(sigtypes)
                sⱼ = sigtypes[j]
                if isType(sⱼ) && sⱼ.parameters[1] === vᵢ
                    # if this parameter came from `arg::Type{T}`,
                    # then `arg` is more precise than `Type{T} where lb<:T<:ub`
                    ty = fieldtype(linfo.specTypes, j)
                    @goto ty_computed
                end
            end
            ub = unwraptv_ub(v)
            if has_free_typevars(ub)
                ub = Any
            end
            lb = unwraptv_lb(v)
            if has_free_typevars(lb)
                lb = Bottom
            end
            if Any === ub && lb === Bottom
                ty = Any
            else
                tv = TypeVar(v.name, lb, ub)
                ty = UnionAll(tv, Type{tv})
            end
            @label ty_computed
            undef = !(let sig=sig
                # if the specialized signature `linfo.specTypes` doesn't contain any free
                # type variables, we can use it for a more accurate analysis of whether `v`
                # is constrained or not, otherwise we should use `def.sig` which always
                # doesn't contain any free type variables
                if !has_free_typevars(linfo.specTypes)
                    sig = linfo.specTypes
                end
                @assert !has_free_typevars(sig)
                constrains_param(v, sig, #=covariant=#true)
            end)
        elseif isvarargtype(v)
            ty = Int
            undef = false
        else
            ty = Const(v)
            undef = false
        end
        sptypes[i] = VarState(ty, undef)
    end
    return sptypes
end

_topmod(sv::InferenceState) = _topmod(frame_module(sv))

function record_ssa_assign!(𝕃ᵢ::AbstractLattice, ssa_id::Int, @nospecialize(new), frame::InferenceState)
    ssavaluetypes = frame.ssavaluetypes
    old = ssavaluetypes[ssa_id]
    if old === NOT_FOUND || !⊑(𝕃ᵢ, new, old)
        # typically, we expect that old ⊑ new (that output information only
        # gets less precise with worse input information), but to actually
        # guarantee convergence we need to use tmerge here to ensure that is true
        ssavaluetypes[ssa_id] = old === NOT_FOUND ? new : tmerge(𝕃ᵢ, old, new)
        W = frame.ip
        for r in frame.ssavalue_uses[ssa_id]
            if was_reached(frame, r)
                usebb = block_for_inst(frame.cfg, r)
                # We're guaranteed to visit the statement if it's in the current
                # basic block, since SSA values can only ever appear after their
                # def.
                if usebb != frame.currbb
                    push!(W, usebb)
                end
            end
        end
    end
    return nothing
end

function add_cycle_backedge!(caller::InferenceState, frame::InferenceState, currpc::Int)
    update_valid_age!(caller, frame.valid_worlds)
    backedge = (caller, currpc)
    contains_is(frame.cycle_backedges, backedge) || push!(frame.cycle_backedges, backedge)
    add_backedge!(caller, frame.linfo)
    return frame
end

function get_stmt_edges!(caller::InferenceState, currpc::Int=caller.currpc)
    stmt_edges = caller.stmt_edges
    edges = stmt_edges[currpc]
    if edges === nothing
        edges = stmt_edges[currpc] = []
    end
    return edges
end

function empty_backedges!(frame::InferenceState, currpc::Int=frame.currpc)
    edges = frame.stmt_edges[currpc]
    edges === nothing || empty!(edges)
    return nothing
end

function print_callstack(sv::InferenceState)
    while sv !== nothing
        print(sv.linfo)
        !sv.cached && print("  [uncached]")
        println()
        for cycle in sv.callers_in_cycle
            print(' ', cycle.linfo)
            println()
        end
        sv = sv.parent
    end
end

function narguments(sv::InferenceState, include_va::Bool=true)
    def = sv.linfo.def
    nargs = length(sv.result.argtypes)
    if !include_va
        nargs -= isa(def, Method) && def.isva
    end
    return nargs
end

# IRInterpretationState
# =====================

# TODO add `result::InferenceResult` and put the irinterp result into the inference cache?
mutable struct IRInterpretationState
    const method_info::MethodInfo
    const ir::IRCode
    const mi::MethodInstance
    const world::UInt
    curridx::Int
    const argtypes_refined::Vector{Bool}
    const sptypes::Vector{VarState}
    const tpdum::TwoPhaseDefUseMap
    const ssa_refined::BitSet
    const lazydomtree::LazyDomtree
    valid_worlds::WorldRange
    const edges::Vector{Any}
    parent # ::Union{Nothing,AbsIntState}

    function IRInterpretationState(interp::AbstractInterpreter,
        method_info::MethodInfo, ir::IRCode, mi::MethodInstance, argtypes::Vector{Any},
        world::UInt, min_world::UInt, max_world::UInt)
        curridx = 1
        given_argtypes = Vector{Any}(undef, length(argtypes))
        for i = 1:length(given_argtypes)
            given_argtypes[i] = widenslotwrapper(argtypes[i])
        end
        given_argtypes = va_process_argtypes(optimizer_lattice(interp), given_argtypes, mi)
        argtypes_refined = Bool[!⊑(optimizer_lattice(interp), ir.argtypes[i], given_argtypes[i])
            for i = 1:length(given_argtypes)]
        empty!(ir.argtypes)
        append!(ir.argtypes, given_argtypes)
        tpdum = TwoPhaseDefUseMap(length(ir.stmts))
        ssa_refined = BitSet()
        lazydomtree = LazyDomtree(ir)
        valid_worlds = WorldRange(min_world, max_world == typemax(UInt) ? get_world_counter() : max_world)
        edges = Any[]
        parent = nothing
        return new(method_info, ir, mi, world, curridx, argtypes_refined, ir.sptypes, tpdum,
                   ssa_refined, lazydomtree, valid_worlds, edges, parent)
    end
end

function IRInterpretationState(interp::AbstractInterpreter,
    code::CodeInstance, mi::MethodInstance, argtypes::Vector{Any}, world::UInt)
    @assert code.def === mi
    src = @atomic :monotonic code.inferred
    if isa(src, Vector{UInt8})
        src = ccall(:jl_uncompress_ir, Any, (Any, Ptr{Cvoid}, Any), mi.def, C_NULL, src)::CodeInfo
    else
        isa(src, CodeInfo) || return nothing
    end
    method_info = MethodInfo(src)
    ir = inflate_ir(src, mi)
    return IRInterpretationState(interp, method_info, ir, mi, argtypes, world,
                                 src.min_world, src.max_world)
end

# AbsIntState
# ===========

const AbsIntState = Union{InferenceState,IRInterpretationState}

frame_instance(sv::InferenceState) = sv.linfo
frame_instance(sv::IRInterpretationState) = sv.mi

function frame_module(sv::AbsIntState)
    mi = frame_instance(sv)
    def = mi.def
    isa(def, Module) && return def
    return def.module
end

frame_parent(sv::InferenceState) = sv.parent::Union{Nothing,AbsIntState}
frame_parent(sv::IRInterpretationState) = sv.parent::Union{Nothing,AbsIntState}

is_constproped(sv::InferenceState) = any(sv.result.overridden_by_const)
is_constproped(::IRInterpretationState) = true

is_cached(sv::InferenceState) = sv.cached
is_cached(::IRInterpretationState) = false

method_info(sv::InferenceState) = sv.method_info
method_info(sv::IRInterpretationState) = sv.method_info

propagate_inbounds(sv::AbsIntState) = method_info(sv).propagate_inbounds
method_for_inference_limit_heuristics(sv::AbsIntState) = method_info(sv).method_for_inference_limit_heuristics

frame_world(sv::InferenceState) = sv.world
frame_world(sv::IRInterpretationState) = sv.world

callers_in_cycle(sv::InferenceState) = sv.callers_in_cycle
callers_in_cycle(sv::IRInterpretationState) = ()

is_effect_overridden(sv::AbsIntState, effect::Symbol) = is_effect_overridden(frame_instance(sv), effect)
function is_effect_overridden(linfo::MethodInstance, effect::Symbol)
    def = linfo.def
    return isa(def, Method) && is_effect_overridden(def, effect)
end
is_effect_overridden(method::Method, effect::Symbol) = is_effect_overridden(decode_effects_override(method.purity), effect)
is_effect_overridden(override::EffectsOverride, effect::Symbol) = getfield(override, effect)

has_conditional(𝕃::AbstractLattice, ::InferenceState) = has_conditional(𝕃)
has_conditional(::AbstractLattice, ::IRInterpretationState) = false

# work towards converging the valid age range for sv
function update_valid_age!(sv::AbsIntState, valid_worlds::WorldRange)
    valid_worlds = sv.valid_worlds = intersect(valid_worlds, sv.valid_worlds)
    @assert sv.world in valid_worlds "invalid age range update"
    return valid_worlds
end

"""
    AbsIntStackUnwind(sv::AbsIntState)

Iterate through all callers of the given `AbsIntState` in the abstract interpretation stack
(including the given `AbsIntState` itself), visiting children before their parents (i.e.
ascending the tree from the given `AbsIntState`).
Note that cycles may be visited in any order.
"""
struct AbsIntStackUnwind
    sv::AbsIntState
end
iterate(unw::AbsIntStackUnwind) = (unw.sv, (unw.sv, 0))
function iterate(unw::AbsIntStackUnwind, (sv, cyclei)::Tuple{AbsIntState, Int})
    # iterate through the cycle before walking to the parent
    if cyclei < length(callers_in_cycle(sv))
        cyclei += 1
        parent = callers_in_cycle(sv)[cyclei]
    else
        cyclei = 0
        parent = frame_parent(sv)
    end
    parent === nothing && return nothing
    return (parent, (parent, cyclei))
end

# temporarily accumulate our edges to later add as backedges in the callee
function add_backedge!(caller::InferenceState, mi::MethodInstance)
    isa(caller.linfo.def, Method) || return nothing # don't add backedges to toplevel method instance
    return push!(get_stmt_edges!(caller), mi)
end
function add_backedge!(irsv::IRInterpretationState, mi::MethodInstance)
    return push!(irsv.edges, mi)
end

function add_invoke_backedge!(caller::InferenceState, @nospecialize(invokesig::Type), mi::MethodInstance)
    isa(caller.linfo.def, Method) || return nothing # don't add backedges to toplevel method instance
    return push!(get_stmt_edges!(caller), invokesig, mi)
end
function add_invoke_backedge!(irsv::IRInterpretationState, @nospecialize(invokesig::Type), mi::MethodInstance)
    return push!(irsv.edges, invokesig, mi)
end

# used to temporarily accumulate our no method errors to later add as backedges in the callee method table
function add_mt_backedge!(caller::InferenceState, mt::MethodTable, @nospecialize(typ))
    isa(caller.linfo.def, Method) || return nothing # don't add backedges to toplevel method instance
    return push!(get_stmt_edges!(caller), mt, typ)
end
function add_mt_backedge!(irsv::IRInterpretationState, mt::MethodTable, @nospecialize(typ))
    return push!(irsv.edges, mt, typ)
end

get_curr_ssaflag(sv::InferenceState) = sv.src.ssaflags[sv.currpc]
get_curr_ssaflag(sv::IRInterpretationState) = sv.ir.stmts[sv.curridx][:flag]

add_curr_ssaflag!(sv::InferenceState, flag::UInt8) = sv.src.ssaflags[sv.currpc] |= flag
add_curr_ssaflag!(sv::IRInterpretationState, flag::UInt8) = sv.ir.stmts[sv.curridx][:flag] |= flag

sub_curr_ssaflag!(sv::InferenceState, flag::UInt8) = sv.src.ssaflags[sv.currpc] &= ~flag
sub_curr_ssaflag!(sv::IRInterpretationState, flag::UInt8) = sv.ir.stmts[sv.curridx][:flag] &= ~flag

merge_effects!(::AbstractInterpreter, caller::InferenceState, effects::Effects) =
    caller.ipo_effects = merge_effects(caller.ipo_effects, effects)
merge_effects!(::AbstractInterpreter, ::IRInterpretationState, ::Effects) = return

struct InferenceLoopState
    sig
    rt
    effects::Effects
    function InferenceLoopState(@nospecialize(sig), @nospecialize(rt), effects::Effects)
        new(sig, rt, effects)
    end
end

bail_out_toplevel_call(::AbstractInterpreter, state::InferenceLoopState, sv::InferenceState) =
    sv.restrict_abstract_call_sites && !isdispatchtuple(state.sig)
bail_out_toplevel_call(::AbstractInterpreter, ::InferenceLoopState, ::IRInterpretationState) = false

bail_out_call(::AbstractInterpreter, state::InferenceLoopState, ::InferenceState) =
    state.rt === Any && !is_foldable(state.effects)
bail_out_call(::AbstractInterpreter, state::InferenceLoopState, ::IRInterpretationState) =
    state.rt === Any && !is_foldable(state.effects)

bail_out_apply(::AbstractInterpreter, state::InferenceLoopState, ::InferenceState) =
    state.rt === Any
bail_out_apply(::AbstractInterpreter, state::InferenceLoopState, ::IRInterpretationState) =
    state.rt === Any

function should_infer_this_call(interp::AbstractInterpreter, sv::InferenceState)
    if InferenceParams(interp).unoptimize_throw_blocks
        # Disable inference of calls in throw blocks, since we're unlikely to
        # need their types. There is one exception however: If up until now, the
        # function has not seen any side effects, we would like to make sure there
        # aren't any in the throw block either to enable other optimizations.
        if is_stmt_throw_block(get_curr_ssaflag(sv))
            should_infer_for_effects(sv) || return false
        end
    end
    return true
end
function should_infer_for_effects(sv::InferenceState)
    effects = sv.ipo_effects
    return is_terminates(effects) && is_effect_free(effects)
end
should_infer_this_call(::AbstractInterpreter, ::IRInterpretationState) = true

add_remark!(::AbstractInterpreter, ::InferenceState, remark) = return
add_remark!(::AbstractInterpreter, ::IRInterpretationState, remark) = return
