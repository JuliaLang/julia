# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# OptimizationState #
#####################

mutable struct OptimizationState
    linfo::MethodInstance
    vararg_type_container #::Type
    backedges::Vector{Any}
    src::CodeInfo
    mod::Module
    nargs::Int
    next_label::Int # index of the current highest label for this function
    min_valid::UInt
    max_valid::UInt
    params::Params
    function OptimizationState(frame::InferenceState)
        s_edges = frame.stmt_edges[1]
        if s_edges === ()
            s_edges = []
            frame.stmt_edges[1] = s_edges
        end
        next_label = label_counter(frame.src.code) + 1
        return new(frame.linfo, frame.vararg_type_container,
                   s_edges::Vector{Any},
                   frame.src, frame.mod, frame.nargs,
                   next_label, frame.min_valid, frame.max_valid,
                   frame.params)
    end
    function OptimizationState(linfo::MethodInstance, src::CodeInfo,
                               params::Params)
        # prepare src for running optimization passes
        # if it isn't already
        nssavalues = src.ssavaluetypes
        if nssavalues isa Int
            src.ssavaluetypes = Any[ Any for i = 1:nssavalues ]
        end
        if src.slottypes === nothing
            nslots = length(src.slotnames)
            src.slottypes = Any[ Any for i = 1:nslots ]
        end
        s_edges = []
        # cache some useful state computations
        toplevel = !isa(linfo.def, Method)
        if !toplevel
            meth = linfo.def
            inmodule = meth.module
            nargs = meth.nargs
        else
            inmodule = linfo.def::Module
            nargs = 0
        end
        next_label = label_counter(src.code) + 1
        vararg_type_container = nothing # if you want something more accurate, set it yourself :P
        return new(linfo, vararg_type_container,
                   s_edges::Vector{Any},
                   src, inmodule, nargs,
                   next_label,
                   min_world(linfo), max_world(linfo),
                   params)
    end
end

function OptimizationState(linfo::MethodInstance, params::Params)
    src = retrieve_code_info(linfo)
    src === nothing && return nothing
    return OptimizationState(linfo, src, params)
end

_topmod(sv::OptimizationState) = _topmod(sv.mod)

genlabel(sv::OptimizationState) = LabelNode(sv.next_label += 1)

function newvar!(sv::OptimizationState, @nospecialize(typ))
    id = length(sv.src.ssavaluetypes)
    push!(sv.src.ssavaluetypes, typ)
    return SSAValue(id)
end

function update_valid_age!(min_valid::UInt, max_valid::UInt, sv::OptimizationState)
    sv.min_valid = max(sv.min_valid, min_valid)
    sv.max_valid = min(sv.max_valid, max_valid)
    @assert(!isa(sv.linfo.def, Method) ||
            (sv.min_valid == typemax(UInt) && sv.max_valid == typemin(UInt)) ||
            sv.min_valid <= sv.params.world <= sv.max_valid,
            "invalid age range update")
    nothing
end

update_valid_age!(li::MethodInstance, sv::OptimizationState) = update_valid_age!(min_world(li), max_world(li), sv)

function add_backedge!(li::MethodInstance, caller::OptimizationState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    push!(caller.backedges, li)
    update_valid_age!(li, caller)
    nothing
end

function is_specializable_vararg_slot(@nospecialize(arg), sv::OptimizationState)
    return (isa(arg, Slot) && slot_id(arg) == sv.nargs &&
            isa(sv.vararg_type_container, DataType))
end

###########
# structs #
###########

# This struct contains information about a use of certain value (`SSAValue` or `Slot`)
# This might be a toplevel use for `Slot` in which case the `expr` field is `#undef`,
# and it only happens if the slot might be used before it's defined.
struct ValueUse
    # The statement array where `expr` (or its parent assignment) appears in
    stmts::Vector{Any}
    # The position of the `expr` in the `stmts` array
    stmtidx::Int

    # The position the value appears in `expr`
    exidx::Int
    # The expression the value is used in.
    # If `expr` is undef, the use is a statement level use.
    # This must be one of the following:
    # 1. A statement level `Expr(:(=), ...)`.
    # 2. The RHS of a statement level `Expr(:(=), ...)`.
    # 3. A `&` ccall argument.
    expr::Expr

    ValueUse(stmts, stmtidx, expr, exidx) = new(stmts, stmtidx, exidx, expr)
    ValueUse(stmts, stmtidx) = new(stmts, stmtidx, 0)
end

# This struct contains information about a def of certain value (`SSAValue` or `Slot`)
# The `assign` field is usually an assignment but it might be a `NewvarNode` for `Slot`,
# which can happen if the slot might be used before it's defined.
struct ValueDef
    assign::Union{Expr,NewvarNode}
    # The statement array where `expr` (or its parent assignment) appears in
    stmts::Vector{Any}
    # The position of the `expr` in the `stmts` array
    stmtidx::Int
end

# Uses and defs of a value.
mutable struct ValueInfo
    uses::Vector{ValueUse}
    defs::Vector{ValueDef}
    has_method::Bool
    ValueInfo() = new(EMPTY_USES, EMPTY_DEFS, false)
end

# The def and use information of all variables in a function.
# `slots` is indexed by slot id and `ssas` is indexed by ssa id + 1.
# This structure is indexable by either a `Slot`, a `SSAValue` or a `id=>is_ssa` pair.
struct ValueInfoMap
    slots::Vector{ValueInfo}
    ssas::Vector{ValueInfo}
    ValueInfoMap() = new(ValueInfo[], ValueInfo[])
end

struct StructInfo
    defs::Vector{Any}
    names::Vector{Symbol}
    typ::DataType
    mutable::Bool
    isnew::Bool
end

struct InvokeData
    mt::Core.MethodTable
    entry::Core.TypeMapEntry
    types0
    fexpr
    texpr
end

struct AllocOptContext
    infomap::ValueInfoMap
    sv::OptimizationState
    todo::IdDict{Any,Any}
    changes::IdDict{Any,Any}
    sym_count::IdDict{Any,Any}
    all_fld::IdDict{Any,Any}
    setfield_typ::IdDict{Any,Any}
    undef_fld::IdDict{Any,Any}
    structinfos::Vector{StructInfo}
    function AllocOptContext(infomap::ValueInfoMap, sv::OptimizationState)
        todo = IdDict()
        for i in 1:length(infomap.ssas)
            isassigned(infomap.ssas, i) || continue
            todo[i=>true] = nothing
        end
        for i in 1:length(infomap.slots)
            isassigned(infomap.slots, i) || continue
            i > sv.nargs || continue
            todo[i=>false] = nothing
        end
        return new(infomap, sv, todo, IdDict(), IdDict(),
                   IdDict(), IdDict(), IdDict(), StructInfo[])
    end
end

#############
# constants #
#############

# The slot has uses that are not statically dominated by any assignment
# This is implied by `SLOT_USEDUNDEF`.
# If this is not set, all the uses are (statically) dominated by the defs.
# In particular, if a slot has `AssignedOnce && !StaticUndef`, it is an SSA.
const SLOT_STATICUNDEF  = 1

const SLOT_ASSIGNEDONCE = 16 # slot is assigned to only once

const SLOT_USEDUNDEF    = 32 # slot has uses that might raise UndefVarError

# const SLOT_CALLED      = 64

# known affect-free calls (also effect-free)
const _PURE_BUILTINS = Any[tuple, svec, fieldtype, apply_type, ===, isa, typeof, UnionAll, nfields]

# known effect-free calls (might not be affect-free)
const _PURE_BUILTINS_VOLATILE = Any[getfield, arrayref, isdefined, Core.sizeof]

const TOP_GETFIELD = GlobalRef(Core, :getfield)

const TOP_TUPLE = GlobalRef(Core, :tuple)

const META_POP_LOC = Expr(:meta, :pop_loc)

const ENABLE_VERIFY_VALUEINFO = (tmp = Array{Bool,0}(uninitialized); tmp[] = false; tmp)

# allocation optimization, must not be mutated.
const EMPTY_USES = ValueUse[]
const EMPTY_DEFS = ValueDef[]

#########
# logic #
#########

function isinlineable(m::Method, src::CodeInfo, mod::Module, params::Params, bonus::Int=0)
    # compute the cost (size) of inlining this code
    inlineable = false
    cost_threshold = params.inline_cost_threshold
    if m.module === _topmod(m.module)
        # a few functions get special treatment
        name = m.name
        sig = m.sig
        if ((name === :+ || name === :* || name === :min || name === :max) &&
            isa(sig,DataType) &&
            sig == Tuple{sig.parameters[1],Any,Any,Any,Vararg{Any}})
            inlineable = true
        elseif (name === :next || name === :done || name === :unsafe_convert ||
                name === :cconvert)
            cost_threshold *= 4
        end
    end
    if !inlineable
        inlineable = inline_worthy(src.code, src, mod, params, cost_threshold + bonus)
    end
    return inlineable
end

# converge the optimization work
function optimize(me::InferenceState)
    # annotate fulltree with type information
    type_annotate!(me)

    # run optimization passes on fulltree
    force_noinline = true
    if me.limited && me.cached && me.parent !== nothing
        # a top parent will be cached still, but not this intermediate work
        me.cached = false
        me.linfo.inInference = false
    elseif me.optimize
        opt = OptimizationState(me)
        # This pass is required for the AST to be valid in codegen
        # if any `SSAValue` is created by type inference. Ref issue #6068
        # This (and `reindex_labels!`) needs to be run for `!me.optimize`
        # if we start to create `SSAValue` in type inference when not
        # optimizing and use unoptimized IR in codegen.
        gotoifnot_elim_pass!(opt)
        inlining_pass!(opt, opt.src.propagate_inbounds)
        # Clean up after inlining
        gotoifnot_elim_pass!(opt)
        basic_dce_pass!(opt)
        void_use_elim_pass!(opt)
        copy_duplicated_expr_pass!(opt)
        split_undef_flag_pass!(opt)
        fold_constant_getfield_pass!(opt)
        # Compute escape information
        # and elide unnecessary allocations
        alloc_elim_pass!(opt)
        # Clean up for `alloc_elim_pass!`
        gotoifnot_elim_pass!(opt)
        basic_dce_pass!(opt)
        void_use_elim_pass!(opt)
        # Pop metadata before label reindexing
        let code = opt.src.code::Array{Any,1}
            meta_elim_pass!(code, coverage_enabled())
            filter!(x -> x !== nothing, code)
            force_noinline = peekmeta(code, :noinline)[1]
        end
        reindex_labels!(opt)
        me.min_valid = opt.min_valid
        me.max_valid = opt.max_valid
    end

    # convert all type information into the form consumed by the code-generator
    widen_all_consts!(me.src)

    # compute inlining and other related properties
    me.const_ret = (isa(me.bestguess, Const) || isconstType(me.bestguess))
    if me.const_ret
        proven_pure = false
        # must be proven pure to use const_api; otherwise we might skip throwing errors
        # (issue #20704)
        # TODO: Improve this analysis; if a function is marked @pure we should really
        # only care about certain errors (e.g. method errors and type errors).
        if length(me.src.code) < 10
            proven_pure = true
            for stmt in me.src.code
                if !statement_effect_free(stmt, me.src, me.mod)
                    proven_pure = false
                    break
                end
            end
            if proven_pure
                for fl in me.src.slotflags
                    if (fl & SLOT_USEDUNDEF) != 0
                        proven_pure = false
                        break
                    end
                end
            end
        end
        if proven_pure
            me.src.pure = true
        end

        if proven_pure && !coverage_enabled()
            # use constant calling convention
            # Do not emit `jlcall_api == 2` if coverage is enabled
            # so that we don't need to add coverage support
            # to the `jl_call_method_internal` fast path
            # Still set pure flag to make sure `inference` tests pass
            # and to possibly enable more optimization in the future
            if !(isa(me.bestguess, Const) && !is_inlineable_constant(me.bestguess.val))
                me.const_api = true
            end
            force_noinline || (me.src.inlineable = true)
        end
    end

    # determine and cache inlineability
    if !force_noinline
        # don't keep ASTs for functions specialized on a Union argument
        # TODO: this helps avoid a type-system bug mis-computing sparams during intersection
        sig = unwrap_unionall(me.linfo.specTypes)
        if isa(sig, DataType) && sig.name === Tuple.name
            for P in sig.parameters
                P = unwrap_unionall(P)
                if isa(P, Union)
                    force_noinline = true
                    break
                end
            end
        else
            force_noinline = true
        end
    end
    def = me.linfo.def
    if force_noinline
        me.src.inlineable = false
    elseif !me.src.inlineable && isa(def, Method)
        bonus = 0
        if me.bestguess ⊑ Tuple && !isbits(widenconst(me.bestguess))
            bonus = me.params.inline_tupleret_bonus
        end
        me.src.inlineable = isinlineable(def, me.src, me.mod, me.params, bonus)
    end
    me.src.inferred = true
    if me.optimize && !(me.limited && me.parent !== nothing)
        validate_code_in_debug_mode(me.linfo, me.src, "optimized")
    end
    nothing
end

# inference completed on `me`
# update the MethodInstance and notify the edges
function finish(me::InferenceState)
    if me.cached
        toplevel = !isa(me.linfo.def, Method)
        if !toplevel
            min_valid = me.min_valid
            max_valid = me.max_valid
        else
            min_valid = UInt(0)
            max_valid = UInt(0)
        end

        # check if the existing me.linfo metadata is also sufficient to describe the current inference result
        # to decide if it is worth caching it again (which would also clear any generated code)
        already_inferred = !me.linfo.inInference
        if isdefined(me.linfo, :inferred)
            inf = me.linfo.inferred
            if !isa(inf, CodeInfo) || (inf::CodeInfo).inferred
                if min_world(me.linfo) == min_valid && max_world(me.linfo) == max_valid
                    already_inferred = true
                end
            end
        end

        # don't store inferred code if we've decided to interpret this function
        if !already_inferred && me.linfo.jlcall_api != 4
            const_flags = (me.const_ret) << 1 | me.const_api
            if me.const_ret
                if isa(me.bestguess, Const)
                    inferred_const = (me.bestguess::Const).val
                else
                    @assert isconstType(me.bestguess)
                    inferred_const = me.bestguess.parameters[1]
                end
            else
                inferred_const = nothing
            end
            if me.const_api
                # use constant calling convention
                inferred_result = nothing
            else
                inferred_result = me.src
            end

            if !toplevel
                if !me.const_api
                    def = me.linfo.def::Method
                    keeptree = me.optimize &&
                        (me.src.inlineable ||
                         ccall(:jl_is_cacheable_sig, Int32, (Any, Any, Any), me.linfo.specTypes, def.sig, def) != 0)
                    if keeptree
                        # compress code for non-toplevel thunks
                        inferred_result = ccall(:jl_compress_ast, Any, (Any, Any), def, inferred_result)
                    else
                        inferred_result = nothing
                    end
                end
            end
            cache = ccall(:jl_set_method_inferred, Ref{MethodInstance}, (Any, Any, Any, Any, Int32, UInt, UInt),
                me.linfo, widenconst(me.bestguess), inferred_const, inferred_result,
                const_flags, min_valid, max_valid)
            if cache !== me.linfo
                me.linfo.inInference = false
                me.linfo = cache
                me.result.linfo = cache
            end
        end
        me.linfo.inInference = false
    end

    # finish updating the result struct
    if me.src.inlineable
        me.result.src = me.src # stash a copy of the code (for inlining)
    end
    me.result.result = me.bestguess # record type, and that wip is done and me.linfo can be used as a backedge

    # update all of the callers with real backedges by traversing the temporary list of backedges
    for (i, _) in me.backedges
        add_backedge!(me.linfo, i)
    end

    # finalize and record the linfo result
    me.inferred = true
    nothing
end

function maybe_widen_conditional(vt)
    if isa(vt, Conditional)
        if vt.vtype === Bottom
            vt = Const(false)
        elseif vt.elsetype === Bottom
            vt = Const(true)
        end
    end
    vt
end

function annotate_slot_load!(e::Expr, vtypes::VarTable, sv::InferenceState, undefs::Array{Bool,1})
    head = e.head
    i0 = 1
    e.typ = maybe_widen_conditional(e.typ)
    if is_meta_expr_head(head) || head === :const
        return
    end
    if head === :(=) || head === :method
        i0 = 2
    end
    for i = i0:length(e.args)
        subex = e.args[i]
        if isa(subex, Expr)
            annotate_slot_load!(subex, vtypes, sv, undefs)
        elseif isa(subex, Slot)
            id = slot_id(subex)
            s = vtypes[id]
            vt = maybe_widen_conditional(s.typ)
            if s.undef
                # find used-undef variables
                undefs[id] = true
            end
            #  add type annotations where needed
            if !(sv.src.slottypes[id] ⊑ vt)
                e.args[i] = TypedSlot(id, vt)
            end
        end
    end
end

function record_slot_assign!(sv::InferenceState)
    # look at all assignments to slots
    # and union the set of types stored there
    # to compute a lower bound on the storage required
    states = sv.stmt_types
    body = sv.src.code::Vector{Any}
    slottypes = sv.src.slottypes::Vector{Any}
    for i = 1:length(body)
        expr = body[i]
        st_i = states[i]
        # find all reachable assignments to locals
        if isa(st_i, VarTable) && isa(expr, Expr) && expr.head === :(=)
            lhs = expr.args[1]
            rhs = expr.args[2]
            if isa(lhs, Slot)
                id = slot_id(lhs)
                if isa(rhs, Slot)
                    # exprtype isn't yet computed for slots
                    vt = st_i[slot_id(rhs)].typ
                else
                    vt = exprtype(rhs, sv.src, sv.mod)
                end
                vt = widenconst(vt)
                if vt !== Bottom
                    otherTy = slottypes[id]
                    if otherTy === Bottom
                        slottypes[id] = vt
                    elseif otherTy === Any
                        slottypes[id] = Any
                    else
                        slottypes[id] = tmerge(otherTy, vt)
                    end
                end
            end
        end
    end
end

# annotate types of all symbols in AST
function type_annotate!(sv::InferenceState)
    # remove all unused ssa values
    gt = sv.src.ssavaluetypes
    for j = 1:length(gt)
        if gt[j] === NOT_FOUND
            gt[j] = Union{}
        end
    end

    # compute the required type for each slot
    # to hold all of the items assigned into it
    record_slot_assign!(sv)

    # annotate variables load types
    # remove dead code
    # and compute which variables may be used undef
    src = sv.src
    states = sv.stmt_types
    nargs = sv.nargs
    nslots = length(states[1])
    undefs = fill(false, nslots)
    body = src.code::Array{Any,1}
    nexpr = length(body)
    i = 1
    while i <= nexpr
        st_i = states[i]
        expr = body[i]
        if isa(st_i, VarTable)
            # st_i === ()  =>  unreached statement  (see issue #7836)
            if isa(expr, Expr)
                annotate_slot_load!(expr, st_i, sv, undefs)
            elseif isa(expr, Slot)
                id = slot_id(expr)
                if st_i[id].undef
                    # find used-undef variables in statement position
                    undefs[id] = true
                end
            end
        elseif sv.optimize
            if ((isa(expr, Expr) && is_meta_expr(expr)) ||
                 isa(expr, LineNumberNode))
                # keep any lexically scoped expressions
                i += 1
                continue
            end
            # This can create `Expr(:gotoifnot)` with dangling label, which we
            # will clean up by replacing them with the conditions later.
            deleteat!(body, i)
            deleteat!(states, i)
            nexpr -= 1
            continue
        end
        i += 1
    end

    # finish marking used-undef variables
    for j = 1:nslots
        if undefs[j]
            src.slotflags[j] |= SLOT_USEDUNDEF | SLOT_STATICUNDEF
        end
    end

    # The dead code elimination can delete the target of a reachable node. This
    # must mean that the target is unreachable. Later optimization passes will
    # assume that all branches lead to labels that exist, so we must replace
    # the node with the branch condition (which may have side effects).
    labelmap = get_label_map(body)
    for i in 1:length(body)
        expr = body[i]
        if isa(expr, Expr) && expr.head === :gotoifnot
            labelnum = labelmap[expr.args[2]::Int]
            if labelnum === 0
                body[i] = expr.args[1]
            end
        end
    end
    nothing
end

# widen all Const elements in type annotations
function _widen_all_consts!(e::Expr, untypedload::Vector{Bool}, slottypes::Vector{Any})
    e.typ = widenconst(e.typ)
    for i = 1:length(e.args)
        x = e.args[i]
        if isa(x, Expr)
            _widen_all_consts!(x, untypedload, slottypes)
        elseif isa(x, TypedSlot)
            vt = widenconst(x.typ)
            if !(vt === x.typ)
                if slottypes[x.id] ⊑ vt
                    x = SlotNumber(x.id)
                    untypedload[x.id] = true
                else
                    x = TypedSlot(x.id, vt)
                end
                e.args[i] = x
            end
        elseif isa(x, SlotNumber) && (i != 1 || e.head !== :(=))
            untypedload[x.id] = true
        end
    end
    nothing
end

function widen_all_consts!(src::CodeInfo)
    for i = 1:length(src.ssavaluetypes)
        src.ssavaluetypes[i] = widenconst(src.ssavaluetypes[i])
    end
    for i = 1:length(src.slottypes)
        src.slottypes[i] = widenconst(src.slottypes[i])
    end

    nslots = length(src.slottypes)
    untypedload = fill(false, nslots)
    e = Expr(:body)
    e.args = src.code
    _widen_all_consts!(e, untypedload, src.slottypes)
    for i = 1:nslots
        src.slottypes[i] = widen_slot_type(src.slottypes[i], untypedload[i])
    end
    return src
end

# widen all slots to their optimal storage layout
# we also need to preserve the type for any untyped load of a DataType
# since codegen optimizations of functions like `is` will depend on knowing it
function widen_slot_type(@nospecialize(ty), untypedload::Bool)
    if isa(ty, DataType)
        if untypedload || isbits(ty) || isdefined(ty, :instance)
            return ty
        end
    elseif isa(ty, Union)
        ty_a = widen_slot_type(ty.a, false)
        ty_b = widen_slot_type(ty.b, false)
        if ty_a !== Any || ty_b !== Any
            # TODO: better optimized codegen for unions?
            return ty
        end
    elseif isa(ty, UnionAll)
        if untypedload
            return ty
        end
    end
    return Any
end

# replace slots 1:na with argexprs, static params with spvals, and increment
# other slots by offset.
function substitute!(
        @nospecialize(e), na::Int, argexprs::Vector{Any},
        @nospecialize(spsig), spvals::Vector{Any},
        offset::Int, boundscheck::Symbol)
    if isa(e, Slot)
        id = slot_id(e)
        if 1 <= id <= na
            ae = argexprs[id]
            if isa(e, TypedSlot) && isa(ae, Slot)
                return TypedSlot(ae.id, e.typ)
            end
            return ae
        end
        if isa(e, SlotNumber)
            return SlotNumber(id + offset)
        else
            return TypedSlot(id + offset, e.typ)
        end
    end
    if isa(e, NewvarNode)
        return NewvarNode(substitute!(e.slot, na, argexprs, spsig, spvals, offset, boundscheck))
    end
    if isa(e, Expr)
        e = e::Expr
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
                elseif i == 4
                    @assert isa((e.args[4]::QuoteNode).value, Symbol)
                elseif i == 5
                    @assert isa(e.args[5], Int)
                else
                    e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset, boundscheck)
                end
            end
        elseif head === :boundscheck
            if boundscheck === :propagate
                return e
            elseif boundscheck === :off
                return false
            else
                return true
            end
        elseif !is_meta_expr_head(head)
            for i = 1:length(e.args)
                e.args[i] = substitute!(e.args[i], na, argexprs, spsig, spvals, offset, boundscheck)
            end
        end
    end
    return e
end

# whether `f` is pure for inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.sqrt_llvm ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.cglobal)  # cglobal lookup answer changes at runtime
end

# whether `f` is pure for optimizations
function is_pure_intrinsic_optim(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.checked_sdiv_int ||  # these may throw errors
             f === Intrinsics.checked_udiv_int ||
             f === Intrinsics.checked_srem_int ||
             f === Intrinsics.checked_urem_int ||
             f === Intrinsics.cglobal)  # cglobal throws an error for symbol-not-found
end

function is_pure_builtin(@nospecialize(f))
    if isa(f, IntrinsicFunction)
        return is_pure_intrinsic_optim(f)
    elseif isa(f, Builtin)
        return (contains_is(_PURE_BUILTINS, f) ||
                contains_is(_PURE_BUILTINS_VOLATILE, f))
    else
        return f === return_type
    end
end

function statement_effect_free(@nospecialize(e), src::CodeInfo, mod::Module)
    if isa(e, Expr)
        if e.head === :(=)
            return !isa(e.args[1], GlobalRef) && effect_free(e.args[2], src, mod, false)
        elseif e.head === :gotoifnot
            return effect_free(e.args[1], src, mod, false)
        end
    elseif isa(e, LabelNode) || isa(e, GotoNode)
        return true
    end
    return effect_free(e, src, mod, false)
end

# detect some important side-effect-free calls (allow_volatile=true)
# and some affect-free calls (allow_volatile=false) -- affect_free means the call
# cannot be affected by previous calls, except assignment nodes
function effect_free(@nospecialize(e), src::CodeInfo, mod::Module, allow_volatile::Bool)
    if isa(e, GlobalRef)
        return (isdefined(e.mod, e.name) && (allow_volatile || isconst(e.mod, e.name)))
    elseif isa(e, Symbol)
        return allow_volatile
    elseif isa(e, Slot)
        return src.slotflags[slot_id(e)] & SLOT_USEDUNDEF == 0
    elseif isa(e, Expr)
        e = e::Expr
        head = e.head
        if is_meta_expr_head(head)
            return true
        end
        if head === :static_parameter
            # if we aren't certain enough about the type, it might be an UndefVarError at runtime
            return isa(e.typ, Const) || issingletontype(widenconst(e.typ))
        end
        if e.typ === Bottom
            return false
        end
        ea = e.args
        if head === :call
            if is_known_call_p(e, is_pure_builtin, src, mod)
                if !allow_volatile
                    if is_known_call(e, arrayref, src, mod) || is_known_call(e, arraylen, src, mod)
                        return false
                    elseif is_known_call(e, getfield, src, mod)
                        nargs = length(ea)
                        (3 < nargs < 4) || return false
                        et = exprtype(e, src, mod)
                        # TODO: check ninitialized
                        if !isa(et, Const) && !isconstType(et)
                            # first argument must be immutable to ensure e is affect_free
                            a = ea[2]
                            typ = unwrap_unionall(widenconst(exprtype(a, src, mod)))
                            if isType(typ)
                                # all fields of subtypes of Type are effect-free
                                # (including the non-inferrable uid field)
                            elseif !isa(typ, DataType) || typ.abstract || (typ.mutable && length(typ.types) > 0)
                                return false
                            end
                        end
                    end
                end
                # fall-through
            elseif is_known_call(e, _apply, src, mod) && length(ea) > 1
                ft = exprtype(ea[2], src, mod)
                if !isa(ft, Const) || (!contains_is(_PURE_BUILTINS, ft.val) &&
                                       ft.val !== Core.sizeof)
                    return false
                end
                # fall-through
            else
                return false
            end
        elseif head === :new
            a = ea[1]
            typ = exprtype(a, src, mod)
            # `Expr(:new)` of unknown type could raise arbitrary TypeError.
            typ, isexact = instanceof_tfunc(typ)
            isexact || return false
            isconcretetype(typ) || return false
            !iskindtype(typ) || return false
            typ = typ::DataType
            if !allow_volatile && typ.mutable
                return false
            end
            fieldcount(typ) >= length(ea) - 1 || return false
            for fld_idx in 1:(length(ea) - 1)
                exprtype(ea[fld_idx + 1], src, mod) ⊑ fieldtype(typ, fld_idx) || return false
            end
            # fall-through
        elseif head === :return
            # fall-through
        elseif head === :isdefined
            return allow_volatile
        elseif head === :the_exception
            return allow_volatile
        elseif head === :copyast
            return true
        else
            return false
        end
        for a in ea
            if !effect_free(a, src, mod, allow_volatile)
                return false
            end
        end
    elseif isa(e, LabelNode) || isa(e, GotoNode)
        return false
    end
    return true
end

function inline_as_constant(@nospecialize(val), argexprs::Vector{Any}, sv::OptimizationState, @nospecialize(invoke_data))
    if invoke_data === nothing
        invoke_fexpr = nothing
        invoke_texpr = nothing
    else
        invoke_data = invoke_data::InvokeData
        invoke_fexpr = invoke_data.fexpr
        invoke_texpr = invoke_data.texpr
    end
    # check if any arguments aren't effect_free and need to be kept around
    stmts = invoke_fexpr === nothing ? [] : Any[invoke_fexpr]
    for i = 1:length(argexprs)
        arg = argexprs[i]
        if !effect_free(arg, sv.src, sv.mod, false)
            push!(stmts, arg)
        end
        if i == 1 && !(invoke_texpr === nothing)
            push!(stmts, invoke_texpr)
        end
    end
    return (quoted(val), stmts)
end

function countunionsplit(atypes)
    nu = 1
    for ti in atypes
        if isa(ti, Union)
            nu *= unionlen(ti::Union)
        end
    end
    return nu
end

function get_spec_lambda(@nospecialize(atypes), sv::OptimizationState, @nospecialize(invoke_data))
    if invoke_data === nothing
        return ccall(:jl_get_spec_lambda, Any, (Any, UInt), atypes, sv.params.world)
    else
        invoke_data = invoke_data::InvokeData
        atypes <: invoke_data.types0 || return nothing
        return ccall(:jl_get_invoke_lambda, Any, (Any, Any, Any, UInt),
                     invoke_data.mt, invoke_data.entry, atypes, sv.params.world)
    end
end

function invoke_NF(argexprs, @nospecialize(etype), atypes::Vector{Any}, sv::OptimizationState,
                   @nospecialize(atype_unlimited), @nospecialize(invoke_data))
    # converts a :call to :invoke
    nu = countunionsplit(atypes)
    nu > sv.params.MAX_UNION_SPLITTING && return NOT_FOUND
    if invoke_data === nothing
        invoke_fexpr = nothing
        invoke_texpr = nothing
    else
        invoke_data = invoke_data::InvokeData
        invoke_fexpr = invoke_data.fexpr
        invoke_texpr = invoke_data.texpr
    end

    if nu > 1
        stmts = []

        spec_miss = nothing
        error_label = nothing
        ex = Expr(:call)
        ex.typ = etype
        ex.args = copy(argexprs)
        invoke_texpr === nothing || insert!(stmts, 2, invoke_texpr)
        invoke_fexpr === nothing || pushfirst!(stmts, invoke_fexpr)

        local ret_var, merge, invoke_ex, spec_hit
        ret_var = add_slot!(sv.src, widenconst(etype), false)
        merge = genlabel(sv)
        invoke_ex = copy(ex)
        invoke_ex.head = :invoke
        pushfirst!(invoke_ex.args, nothing)
        spec_hit = false

        function splitunion(atypes::Vector{Any}, i::Int)
            if i == 0
                local sig = argtypes_to_type(atypes)
                local li = get_spec_lambda(sig, sv, invoke_data)
                li === nothing && return false
                add_backedge!(li, sv)
                local stmt = []
                invoke_ex = copy(invoke_ex)
                invoke_ex.args[1] = li
                push!(stmt, Expr(:(=), ret_var, invoke_ex))
                push!(stmt, GotoNode(merge.label))
                spec_hit = true
                return stmt
            else
                local ti = atypes[i]
                if isa(ti, Union)
                    local all = true
                    local stmts = []
                    local aei = ex.args[i]
                    for ty in uniontypes(ti::Union)
                        local ty
                        atypes[i] = ty
                        local match = splitunion(atypes, i - 1)
                        if match !== false
                            after = genlabel(sv)
                            isa_var = newvar!(sv, Bool)
                            isa_ty = Expr(:call, GlobalRef(Core, :isa), aei, ty)
                            isa_ty.typ = Bool
                            pushfirst!(match, Expr(:gotoifnot, isa_var, after.label))
                            pushfirst!(match, Expr(:(=), isa_var, isa_ty))
                            append!(stmts, match)
                            push!(stmts, after)
                        else
                            all = false
                        end
                    end
                    if all
                        error_label === nothing && (error_label = genlabel(sv))
                        push!(stmts, GotoNode(error_label.label))
                    else
                        spec_miss === nothing && (spec_miss = genlabel(sv))
                        push!(stmts, GotoNode(spec_miss.label))
                    end
                    atypes[i] = ti
                    return isempty(stmts) ? false : stmts
                else
                    return splitunion(atypes, i - 1)
                end
            end
        end
        local match = splitunion(atypes, length(atypes))
        if match !== false && spec_hit
            append!(stmts, match)
            if error_label !== nothing
                push!(stmts, error_label)
                error_call = Expr(:call, GlobalRef(_topmod(sv.mod), :error), "fatal error in type inference (type bound)")
                error_call.typ = Union{}
                push!(stmts, error_call)
            end
            if spec_miss !== nothing
                push!(stmts, spec_miss)
                push!(stmts, Expr(:(=), ret_var, ex))
                push!(stmts, GotoNode(merge.label))
            end
            push!(stmts, merge)
            return (ret_var, stmts)
        end
    else
        local cache_linfo = get_spec_lambda(atype_unlimited, sv, invoke_data)
        cache_linfo === nothing && return NOT_FOUND
        add_backedge!(cache_linfo, sv)
        argexprs = copy(argexprs)
        pushfirst!(argexprs, cache_linfo)
        ex = Expr(:invoke)
        ex.args = argexprs
        ex.typ = etype
        if invoke_texpr === nothing
            if invoke_fexpr === nothing
                return ex
            else
                return ex, Any[invoke_fexpr]
            end
        end
        newvar = newvar!(sv, atypes[1])
        stmts = Any[invoke_fexpr,
                    :($newvar = $(argexprs[2])),
                    invoke_texpr]
        argexprs[2] = newvar
        return ex, stmts
    end
    return NOT_FOUND
end

# inline functions whose bodies are "inline_worthy"
# where the function body doesn't contain any argument more than once.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
# `ft` is the type of the function. `f` is the exact function if known, or else `nothing`.
# `pending_stmts` is an array of statements from functions inlined so far, so
# we can estimate the total size of the enclosing function after inlining.
function inlineable(@nospecialize(f), @nospecialize(ft), e::Expr, atypes::Vector{Any},
                    pending_stmt::Vector{Any}, boundscheck::Symbol,
                    sv::OptimizationState)
    argexprs = e.args

    if (f === typeassert || ft ⊑ typeof(typeassert)) && length(atypes) == 3
        # typeassert(x::S, T) => x, when S<:T
        a3 = atypes[3]
        if (isType(a3) && !has_free_typevars(a3) && atypes[2] ⊑ a3.parameters[1]) ||
            (isa(a3, Const) && isa(a3.val, Type) && atypes[2] ⊑ a3.val)
            return (argexprs[2], ())
        end
    end
    topmod = _topmod(sv)
    # special-case inliners for known pure functions that compute types
    if sv.params.inlining
        if isa(e.typ, Const) # || isconstType(e.typ)
            val = e.typ.val
            if (f === apply_type || f === fieldtype || f === typeof || f === (===) ||
                f === Core.sizeof || f === isdefined ||
                istopfunction(topmod, f, :typejoin) ||
                istopfunction(topmod, f, :isbits) ||
                istopfunction(topmod, f, :promote_type) ||
                (f === Core.kwfunc && length(argexprs) == 2) ||
                (is_inlineable_constant(val) &&
                 (contains_is(_PURE_BUILTINS, f) ||
                  (f === getfield && effect_free(e, sv.src, sv.mod, false)) ||
                  (isa(f, IntrinsicFunction) && is_pure_intrinsic_optim(f)))))
                return inline_as_constant(val, argexprs, sv, nothing)
            end
        end
    end
    invoke_data = nothing
    invoke_fexpr = nothing
    invoke_texpr = nothing
    if f === Core.invoke && length(atypes) >= 3
        ft = widenconst(atypes[2])
        invoke_tt = widenconst(atypes[3])
        if !(isconcretetype(ft) || ft <: Type) || !isType(invoke_tt) ||
                has_free_typevars(invoke_tt) || has_free_typevars(ft) || (ft <: Builtin)
            # TODO: this is really aggressive at preventing inlining of closures. maybe drop `isconcretetype` requirement?
            return NOT_FOUND
        end
        if !(isa(invoke_tt.parameters[1], Type) &&
             invoke_tt.parameters[1] <: Tuple)
            return NOT_FOUND
        end
        invoke_tt = invoke_tt.parameters[1]
        invoke_types = rewrap_unionall(Tuple{ft, unwrap_unionall(invoke_tt).parameters...}, invoke_tt)
        invoke_entry = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt),
                             invoke_types, sv.params.world)
        invoke_entry === nothing && return NOT_FOUND
        invoke_fexpr = argexprs[1]
        invoke_texpr = argexprs[3]
        if effect_free(invoke_fexpr, sv.src, sv.mod, false)
            invoke_fexpr = nothing
        end
        if effect_free(invoke_texpr, sv.src, sv.mod, false)
            invoke_fexpr = nothing
        end
        invoke_data = InvokeData(ft.name.mt, invoke_entry,
                                 invoke_types, invoke_fexpr, invoke_texpr)
        atype0 = atypes[2]
        argexpr0 = argexprs[2]
        atypes = atypes[4:end]
        argexprs = argexprs[4:end]
        pushfirst!(atypes, atype0)
        pushfirst!(argexprs, argexpr0)
        f = isdefined(ft, :instance) ? ft.instance : nothing
    elseif isa(f, IntrinsicFunction) || ft ⊑ IntrinsicFunction ||
            isa(f, Builtin) || ft ⊑ Builtin
        return NOT_FOUND
    end

    atype_unlimited = argtypes_to_type(atypes)
    if !(invoke_data === nothing)
        invoke_data = invoke_data::InvokeData
        # TODO emit a type check and proceed for this case
        atype_unlimited <: invoke_data.types0 || return NOT_FOUND
    end
    if !sv.params.inlining
        return invoke_NF(argexprs, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    if length(atypes) == 3 && istopfunction(topmod, f, :!==)
        # special-case inliner for !== that precedes _methods_by_ftype union splitting
        # and that works, even though inference generally avoids inferring the `!==` Method
        if isa(e.typ, Const)
            return inline_as_constant(e.typ.val, argexprs, sv, nothing)
        end
        is_var = newvar!(sv, Bool)
        stmts = Any[ Expr(:(=), is_var, Expr(:call, GlobalRef(Core, :(===)), argexprs[2], argexprs[3])) ]
        stmts[1].args[2].typ = Bool
        not_is = Expr(:call, GlobalRef(Core.Intrinsics, :not_int), is_var)
        not_is.typ = Bool
        return (not_is, stmts)
    elseif length(atypes) == 3 && istopfunction(topmod, f, :(>:))
        # special-case inliner for issupertype
        # that works, even though inference generally avoids inferring the `>:` Method
        if isa(e.typ, Const)
            return inline_as_constant(e.typ.val, argexprs, sv, nothing)
        end
        arg_T1 = argexprs[2]
        arg_T2 = argexprs[3]
        issubtype_stmts = ()
        if !effect_free(arg_T2, sv.src, sv.mod, false)
            # spill first argument to preserve order-of-execution
            issubtype_vnew = newvar!(sv, widenconst(exprtype(arg_T1, sv.src, sv.mod)))
            issubtype_stmts = Any[ Expr(:(=), issubtype_vnew, arg_T1) ]
            arg_T1 = issubtype_vnew
        end
        issubtype_expr = Expr(:call, GlobalRef(Core, :(<:)), arg_T2, arg_T1)
        issubtype_expr.typ = Bool
        return (issubtype_expr, issubtype_stmts)
    end

    if length(atype_unlimited.parameters) - 1 > sv.params.MAX_TUPLETYPE_LEN
        atype = limit_tuple_type(atype_unlimited, sv.params)
    else
        atype = atype_unlimited
    end
    if invoke_data === nothing
        min_valid = UInt[typemin(UInt)]
        max_valid = UInt[typemax(UInt)]
        meth = _methods_by_ftype(atype, 1, sv.params.world, min_valid, max_valid)
        if meth === false || length(meth) != 1
            return invoke_NF(argexprs, e.typ, atypes, sv,
                             atype_unlimited, invoke_data)
        end
        meth = meth[1]::SimpleVector
        metharg = meth[1]::Type
        methsp = meth[2]::SimpleVector
        method = meth[3]::Method
    else
        invoke_data = invoke_data::InvokeData
        method = invoke_data.entry.func
        (metharg, methsp) = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                                  atype_unlimited, method.sig)::SimpleVector
        methsp = methsp::SimpleVector
    end

    methsig = method.sig
    if !(atype <: metharg)
        return invoke_NF(argexprs, e.typ, atypes, sv, atype_unlimited,
                         invoke_data)
    end

    # check whether call can be inlined to just a quoted constant value
    if isa(f, widenconst(ft)) && !isdefined(method, :generator)
        if f === return_type
            if isconstType(e.typ)
                return inline_as_constant(e.typ.parameters[1], argexprs, sv, invoke_data)
            elseif isa(e.typ, Const)
                return inline_as_constant(e.typ.val, argexprs, sv, invoke_data)
            end
        elseif method.pure && isa(e.typ, Const) && e.typ.actual && is_inlineable_constant(e.typ.val)
            return inline_as_constant(e.typ.val, argexprs, sv, invoke_data)
        end
    end

    argexprs0 = argexprs
    atypes0 = atypes
    na = Int(method.nargs)
    # check for vararg function
    isva = false
    if na > 0 && method.isva
        @assert length(argexprs) >= na - 1
        # construct tuple-forming expression for argument tail
        vararg = mk_tuplecall(argexprs[na:end], sv)
        argexprs = Any[argexprs[1:(na - 1)]..., vararg]
        atypes = Any[atypes[1:(na - 1)]..., vararg.typ]
        isva = true
    elseif na != length(argexprs)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return NOT_FOUND
    end

    @assert na == length(argexprs)

    for i = 1:length(methsp)
        isa(methsp[i], TypeVar) && return NOT_FOUND
    end

    # see if the method has been previously inferred (and cached)
    linfo = code_for_method(method, metharg, methsp, sv.params.world, true) # Union{Nothing, MethodInstance}
    isa(linfo, MethodInstance) || return invoke_NF(argexprs0, e.typ, atypes0, sv,
                                                   atype_unlimited, invoke_data)
    linfo = linfo::MethodInstance
    if linfo.jlcall_api == 2
        # in this case function can be inlined to a constant
        add_backedge!(linfo, sv)
        return inline_as_constant(linfo.inferred_const, argexprs, sv, invoke_data)
    end

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
                return inline_as_constant(inferred_const, argexprs, sv, invoke_data)
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

    # check that the code is inlineable
    if inferred === nothing
        src_inferred = src_inlineable = false
    else
        src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), inferred)
        src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), inferred)
    end
    if !src_inferred || !src_inlineable
        return invoke_NF(argexprs0, e.typ, atypes0, sv, atype_unlimited,
                         invoke_data)
    end

    # create the backedge
    add_backedge!(linfo, sv)

    # prepare the code object for mutation
    if isa(inferred, CodeInfo)
        src = inferred
        ast = copy_exprargs(inferred.code)
    else
        src = ccall(:jl_uncompress_ast, Any, (Any, Any), method, inferred::Vector{UInt8})::CodeInfo
        ast = src.code
    end
    ast = ast::Array{Any,1}
    nm = length(unwrap_unionall(metharg).parameters)
    body = Expr(:block)
    body.args = ast

    # see if each argument occurs only once in the body expression
    stmts = []
    prelude_stmts = []
    stmts_free = true # true = all entries of stmts are effect_free

    argexprs = copy(argexprs)
    if isva
        # move constructed vararg tuple to an ssavalue
        varargvar = newvar!(sv, atypes[na])
        push!(prelude_stmts, Expr(:(=), varargvar, argexprs[na]))
        argexprs[na] = varargvar
    end
    for i = na:-1:1 # stmts_free needs to be calculated in reverse-argument order
        #args_i = args[i]
        aei = argexprs[i]
        aeitype = argtype = widenconst(exprtype(aei, sv.src, sv.mod))
        if i == 1 && !(invoke_texpr === nothing)
            pushfirst!(prelude_stmts, invoke_texpr)
        end

        # ok for argument to occur more than once if the actual argument
        # is a symbol or constant, or is not affected by previous statements
        # that will exist after the inlining pass finishes
        affect_free = stmts_free  # false = previous statements might affect the result of evaluating argument
        occ = 0
        for j = length(body.args):-1:1
            b = body.args[j]
            if occ < 6
                occ += occurs_more(b, x->(isa(x, Slot) && slot_id(x) == i), 6)
            end
            if occ > 0 && affect_free && !effect_free(b, src, method.module, true)
                #TODO: we might be able to short-circuit this test better by memoizing effect_free(b) in the for loop over i
                affect_free = false
            end
            if occ > 5 && !affect_free
                break
            end
        end
        free = effect_free(aei, sv.src, sv.mod, true)
        if ((occ==0 && aeitype===Bottom) || (occ > 1 && !inline_worthy(aei, sv.src, sv.mod, sv.params)) ||
                (affect_free && !free) || (!affect_free && !effect_free(aei, sv.src, sv.mod, false)))
            if occ != 0
                vnew = newvar!(sv, aeitype)
                argexprs[i] = vnew
                pushfirst!(prelude_stmts, Expr(:(=), vnew, aei))
                stmts_free &= free
            elseif !free && !isType(aeitype)
                pushfirst!(prelude_stmts, aei)
                stmts_free = false
            end
        end
    end
    invoke_fexpr === nothing || pushfirst!(prelude_stmts, invoke_fexpr)

    # re-number the SSAValues and copy their type-info to the new ast
    ssavalue_types = src.ssavaluetypes
    if !isempty(ssavalue_types)
        incr = length(sv.src.ssavaluetypes)
        if incr != 0
            body = ssavalue_increment(body, incr)
        end
        append!(sv.src.ssavaluetypes, ssavalue_types)
    end

    # ok, substitute argument expressions for argument names in the body
    body = substitute!(body, na, argexprs, method.sig, Any[methsp...], length(sv.src.slotnames) - na, boundscheck)
    append!(sv.src.slotnames, src.slotnames[(na + 1):end])
    append!(sv.src.slottypes, src.slottypes[(na + 1):end])
    append!(sv.src.slotflags, src.slotflags[(na + 1):end])

    # make labels / goto statements unique
    # relocate inlining information
    newlabels = zeros(Int, label_counter(body.args))
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a, LabelNode)
            newlabel = genlabel(sv)
            newlabels[a.label] = newlabel.label
            body.args[i] = newlabel
        end
    end
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a, GotoNode)
            body.args[i] = GotoNode(newlabels[a.label])
        elseif isa(a, Expr)
            if a.head === :enter
                a.args[1] = newlabels[a.args[1]::Int]
            elseif a.head === :gotoifnot
                a.args[2] = newlabels[a.args[2]::Int]
            end
        end
    end

    # convert return statements into a series of goto's
    retstmt = genlabel(sv)
    local retval
    multiret = false
    lastexpr = pop!(body.args)
    if isa(lastexpr, LabelNode)
        push!(body.args, lastexpr)
        error_call = Expr(:call, GlobalRef(topmod, :error), "fatal error in type inference (lowering)")
        error_call.typ = Union{}
        push!(body.args, error_call)
        lastexpr = nothing
    elseif !(isa(lastexpr, Expr) && lastexpr.head === :return)
        # code sometimes ends with a meta node, e.g. inbounds pop
        push!(body.args, lastexpr)
        lastexpr = nothing
    end
    for a in body.args
        push!(stmts, a)
        if isa(a, Expr)
            if a.head === :return
                if !multiret
                    # create slot first time
                    retval = add_slot!(sv.src, rettype, false)
                end
                multiret = true
                pushfirst!(a.args, retval)
                a.head = :(=)
                push!(stmts, GotoNode(retstmt.label))
            end
        end
    end

    if multiret
        if lastexpr !== nothing
            pushfirst!(lastexpr.args, retval)
            lastexpr.head = :(=)
            push!(stmts, lastexpr)
        end
        push!(stmts, retstmt)
        expr = retval
    else
        # Dead code elimination can leave a non-return statement at the end
        if lastexpr === nothing
            expr = nothing
        else
            expr = lastexpr.args[1]
        end
    end

    inlining_ignore = function (@nospecialize(stmt),)
        isa(stmt, Expr) && return is_meta_expr(stmt::Expr)
        isa(stmt, LineNumberNode) && return true
        stmt === nothing && return true
        return false
    end

    do_coverage = coverage_enabled()
    line::Int = method.line
    file = method.file
    if !isempty(stmts)
        if !do_coverage && all(inlining_ignore, stmts)
            empty!(stmts)
        elseif isa(stmts[1], LineNumberNode)
            linenode = popfirst!(stmts)::LineNumberNode
            line = linenode.line
            isa(linenode.file, Symbol) && (file = linenode.file)
        end
    end
    if do_coverage
        # Check if we are switching module, which is necessary to catch user
        # code inlined into `Base` with `--code-coverage=user`.
        # Assume we are inlining directly into `enclosing` instead of another
        # function inlined in it
        mod = method.module
        if mod === sv.mod
            pushfirst!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line))
        else
            pushfirst!(stmts, Expr(:meta, :push_loc, file,
                                 method.name, line, mod))
        end
        push!(stmts, Expr(:meta, :pop_loc))
    elseif !isempty(stmts)
        pushfirst!(stmts, Expr(:meta, :push_loc, file,
                             method.name, line))
        if isa(stmts[end], LineNumberNode)
            stmts[end] = Expr(:meta, :pop_loc)
        else
            push!(stmts, Expr(:meta, :pop_loc))
        end
    end

    if isa(expr, Expr)
        old_t = e.typ
        if old_t ⊑ expr.typ
            # if we had better type information than the content being inlined,
            # change the return type now to use the better type
            expr.typ = old_t
        end
    end
    if !isempty(prelude_stmts)
        stmts = append!(prelude_stmts, stmts)
    end
    return (expr, stmts)
end

## Computing the cost of a function body

# saturating sum (inputs are nonnegative), prevents overflow with typemax(Int) below
plus_saturate(x, y) = max(x, y, x+y)

# known return type
isknowntype(@nospecialize T) = (T == Union{}) || isconcretetype(T)

function statement_cost(ex::Expr, line::Int, src::CodeInfo, mod::Module, params::Params)
    head = ex.head
    if is_meta_expr(ex) || head == :copyast # not sure if copyast is right
        return 0
    end
    argcost = 0
    for a in ex.args
        if a isa Expr
            argcost = plus_saturate(argcost, statement_cost(a, line, src, mod, params))
        end
    end
    if head == :return || head == :(=)
        return argcost
    end
    if head == :call
        extyp = exprtype(ex.args[1], src, mod)
        if isa(extyp, Type)
            return argcost
        end
        if isa(extyp, Const)
            f = (extyp::Const).val
            if isa(f, IntrinsicFunction)
                iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
                if !isassigned(T_IFUNC_COST, iidx)
                    # unknown/unhandled intrinsic
                    return plus_saturate(argcost, params.inline_nonleaf_penalty)
                end
                return plus_saturate(argcost, T_IFUNC_COST[iidx])
            end
            if isa(f, Builtin)
                # The efficiency of operations like a[i] and s.b
                # depend strongly on whether the result can be
                # inferred, so check ex.typ
                if f == Main.Core.getfield || f == Main.Core.tuple
                    # we might like to penalize non-inferrability, but
                    # tuple iteration/destructuring makes that
                    # impossible
                    # return plus_saturate(argcost, isknowntype(ex.typ) ? 1 : params.inline_nonleaf_penalty)
                    return argcost
                elseif f == Main.Core.arrayref
                    return plus_saturate(argcost, isknowntype(ex.typ) ? 4 : params.inline_nonleaf_penalty)
                end
                fidx = findfirst(x->x===f, T_FFUNC_KEY)
                if fidx === nothing
                    # unknown/unhandled builtin or anonymous function
                    # Use the generic cost of a direct function call
                    return plus_saturate(argcost, 20)
                end
                return plus_saturate(argcost, T_FFUNC_COST[fidx])
            end
        end
        return plus_saturate(argcost, params.inline_nonleaf_penalty)
    elseif head == :foreigncall || head == :invoke
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        return ex.typ == Union{} ? 0 : plus_saturate(20, argcost)
    elseif head == :llvmcall
        return plus_saturate(10, argcost) # a wild guess at typical cost
    elseif head == :enter
        # try/catch is a couple function calls,
        # but don't inline functions with try/catch
        # since these aren't usually performance-sensitive functions,
        # and llvm is more likely to miscompile them when these functions get large
        return typemax(Int)
    elseif head == :gotoifnot
        target = ex.args[2]::Int
        # loops are generally always expensive
        # but assume that forward jumps are already counted for from
        # summing the cost of the not-taken branch
        return target < line ? plus_saturate(40, argcost) : argcost
    end
    return argcost
end

function inline_worthy(body::Array{Any,1}, src::CodeInfo, mod::Module,
                       params::Params,
                       cost_threshold::Integer=params.inline_cost_threshold)
    bodycost = 0
    for line = 1:length(body)
        stmt = body[line]
        if stmt isa Expr
            thiscost = statement_cost(stmt, line, src, mod, params)::Int
        elseif stmt isa GotoNode
            # loops are generally always expensive
            # but assume that forward jumps are already counted for from
            # summing the cost of the not-taken branch
            thiscost = stmt.label < line ? 40 : 0
        else
            continue
        end
        bodycost = plus_saturate(bodycost, thiscost)
        bodycost == typemax(Int) && return false
    end
    return bodycost <= cost_threshold
end

function inline_worthy(body::Expr, src::CodeInfo, mod::Module, params::Params,
                       cost_threshold::Integer=params.inline_cost_threshold)
    bodycost = statement_cost(body, typemax(Int), src, mod, params)
    return bodycost <= cost_threshold
end

function inline_worthy(@nospecialize(body), src::CodeInfo, mod::Module, params::Params,
                       cost_threshold::Integer=params.inline_cost_threshold)
    newbody = exprtype(body, src, mod)
    !isa(newbody, Expr) && return true
    return inline_worthy(newbody, src, mod, params, cost_threshold)
end

ssavalue_increment(@nospecialize(body), incr) = body
ssavalue_increment(body::SSAValue, incr) = SSAValue(body.id + incr)
function ssavalue_increment(body::Expr, incr)
    if is_meta_expr(body)
        return body
    end
    for i in 1:length(body.args)
        body.args[i] = ssavalue_increment(body.args[i], incr)
    end
    return body
end

function mk_getfield(texpr, i, T)
    e = Expr(:call, TOP_GETFIELD, texpr, i)
    e.typ = T
    return e
end

function mk_tuplecall(args, sv::OptimizationState)
    e = Expr(:call, TOP_TUPLE, args...)
    e.typ = tuple_tfunc(Tuple{Any[widenconst(exprtype(x, sv.src, sv.mod)) for x in args]...})
    return e
end

function inlining_pass!(sv::OptimizationState, propagate_inbounds::Bool)
    # Also handles bounds check elision:
    #
    #    1. If check_bounds is always on, set `Expr(:boundscheck)` true
    #    2. If check_bounds is always off, set `Expr(:boundscheck)` false
    #    3. If check_bounds is default, figure out whether each boundscheck
    #         is true, false, or propagate based on the enclosing inbounds directives
    _opt_check_bounds = JLOptions().check_bounds
    opt_check_bounds = (_opt_check_bounds == 0 ? :default :
                        _opt_check_bounds == 1 ? :on :
                        :off)
    # Number of stacked inbounds
    inbounds_depth = 0

    eargs = sv.src.code
    i = 1
    stmtbuf = []
    while i <= length(eargs)
        ei = eargs[i]
        if isa(ei, Expr)
            if ei.head === :inbounds
                eargs[i] = nothing
                arg1 = ei.args[1]
                if arg1 === true # push
                    inbounds_depth += 1
                elseif arg1 === false # clear
                    inbounds_depth = 0
                elseif inbounds_depth > 0 # pop
                    inbounds_depth -= 1
                end
            else
                if opt_check_bounds === :off
                     boundscheck = :off
                elseif opt_check_bounds === :on
                     boundscheck = :on
                elseif inbounds_depth > 0
                     boundscheck = :off
                elseif propagate_inbounds
                     boundscheck = :propagate
                else
                     boundscheck = :on
                end
                eargs[i] = inline_expr(ei, sv, stmtbuf, boundscheck)
                if !isempty(stmtbuf)
                    splice!(eargs, i:(i - 1), stmtbuf)
                    i += length(stmtbuf)
                    empty!(stmtbuf)
                end
            end
        end
        i += 1
    end
end


function inline_expr(e::Expr, sv::OptimizationState, stmts::Vector{Any}, boundscheck::Symbol)
    if e.head === :call
        return inline_call(e, sv, stmts, boundscheck)
    elseif e.head === :isdefined
        isa(e.typ, Const) && return e.typ.val
    elseif e.head === :(=) && isa(e.args[2], Expr)
        e.args[2] = inline_expr(e.args[2], sv, stmts, boundscheck)
    elseif e.head === :return && isa(e.args[1], Expr)
        e.args[1] = inline_expr(e.args[1], sv, stmts, boundscheck)
    end
    return e
end

function finddef(v::SSAValue, stmts::Vector{Any})
    for s in stmts
        if isa(s,Expr) && s.head === :(=) && s.args[1] === v
            return s
        end
    end
    return nothing
end

# return inlined replacement for call `e`, inserting new needed statements in `stmts`.
function inline_call(e::Expr, sv::OptimizationState, stmts::Vector{Any}, boundscheck::Symbol)
    eargs = e.args
    if length(eargs) < 1
        return e
    end
    arg1 = eargs[1]

    ft = exprtype(arg1, sv.src, sv.mod)
    if isa(ft, Const)
        f = ft.val
    elseif isa(ft, Conditional)
        f = nothing
        ft = Bool
    else
        f = nothing
        if !(isconcretetype(ft) || (widenconst(ft) <: Type)) || has_free_typevars(ft)
            # TODO: this is really aggressive at preventing inlining of closures. maybe drop `isconcretetype` requirement?
            return e
        end
    end

    ins = 1

    # TODO: determine whether this is really necessary
#=
    if sv.params.inlining
        if isdefined(Main, :Base) &&
            ((isdefined(Main.Base, :^) && f === Main.Base.:^) ||
             (isdefined(Main.Base, :.^) && f === Main.Base.:.^)) &&
            length(e.args) == 3

            a2 = e.args[3]
            if isa(a2, Symbol) || isa(a2, Slot) || isa(a2, SSAValue)
                ta2 = exprtype(a2, sv.src, sv.mod)
                if isa(ta2, Const)
                    a2 = ta2.val
                end
            end

            square = (a2 === Int32(2) || a2 === Int64(2))
            triple = (a2 === Int32(3) || a2 === Int64(3))
            if square || triple
                a1 = e.args[2]
                basenumtype = Union{CoreNumType, Main.Base.ComplexF32, Main.Base.ComplexF64, Main.Base.Rational}
                if isa(a1, basenumtype) || ((isa(a1, Symbol) || isa(a1, Slot) || isa(a1, SSAValue)) &&
                                           exprtype(a1, sv.src, sv.mod) ⊑ basenumtype)
                    if square
                        e.args = Any[GlobalRef(Main.Base,:*), a1, a1]
                        res = inlining_pass(e, sv, stmts, ins, boundscheck)
                    else
                        e.args = Any[GlobalRef(Main.Base,:*), Expr(:call, GlobalRef(Main.Base,:*), a1, a1), a1]
                        e.args[2].typ = e.typ
                        res = inlining_pass(e, sv, stmts, ins, boundscheck)
                    end
                    return res
                end
            end
        end
    end
=#
    for ninline = 1:100
        ata = Vector{Any}(uninitialized, length(e.args))
        ata[1] = ft
        for i = 2:length(e.args)
            a = exprtype(e.args[i], sv.src, sv.mod)
            (a === Bottom || isvarargtype(a)) && return e
            ata[i] = a
        end
        res = inlineable(f, ft, e, ata, stmts, boundscheck, sv)
        if isa(res,Tuple)
            if isa(res[2],Array) && !isempty(res[2])
                splice!(stmts, ins:ins-1, res[2])
                ins += length(res[2])
            end
            res = res[1]
        end

        if res !== NOT_FOUND
            # iteratively inline apply(f, tuple(...), tuple(...), ...) in order
            # to simplify long vararg lists as in multi-arg +
            if isa(res,Expr) && is_known_call(res, _apply, sv.src, sv.mod)
                e = res::Expr
                f = _apply; ft = AbstractEvalConstant(f)
            else
                return res
            end
        end

        if f === _apply
            na = length(e.args)
            newargs = Vector{Any}(uninitialized, na-2)
            newstmts = Any[]
            effect_free_upto = 0
            for i = 3:na
                aarg = e.args[i]
                argt = exprtype(aarg, sv.src, sv.mod)
                t = widenconst(argt)
                if isa(argt, Const) && (isa(argt.val, Tuple) || isa(argt.val, SimpleVector)) &&
                        effect_free(aarg, sv.src, sv.mod, true)
                    val = argt.val
                    newargs[i - 2] = Any[ quoted(val[i]) for i in 1:(length(val)::Int) ] # avoid making a tuple Generator here!
                elseif isa(aarg, Tuple) || (isa(aarg, QuoteNode) && (isa(aarg.value, Tuple) || isa(aarg.value, SimpleVector)))
                    if isa(aarg, QuoteNode)
                        aarg = aarg.value
                    end
                    newargs[i - 2] = Any[ quoted(aarg[i]) for i in 1:(length(aarg)::Int) ] # avoid making a tuple Generator here!
                elseif isa(t, DataType) && t.name === Tuple.name && !isvatuple(t) &&
                         length(t.parameters) <= sv.params.MAX_TUPLE_SPLAT
                    for k = (effect_free_upto + 1):(i - 3)
                        as = newargs[k]
                        for kk = 1:length(as)
                            ak = as[kk]
                            if !effect_free(ak, sv.src, sv.mod, true)
                                tmpv = newvar!(sv, widenconst(exprtype(ak, sv.src, sv.mod)))
                                push!(newstmts, Expr(:(=), tmpv, ak))
                                as[kk] = tmpv
                            end
                        end
                    end
                    effect_free_upto = i - 3
                    if effect_free(aarg, sv.src, sv.mod, true)
                        # apply(f,t::(x,y)) => f(t[1],t[2])
                        tmpv = aarg
                    else
                        # apply(f,t::(x,y)) => tmp=t; f(tmp[1],tmp[2])
                        tmpv = newvar!(sv, t)
                        push!(newstmts, Expr(:(=), tmpv, aarg))
                    end
                    if is_specializable_vararg_slot(aarg, sv)
                        tp = sv.vararg_type_container.parameters
                    else
                        tp = t.parameters
                    end
                    ntp = length(tp)::Int
                    if isa(aarg,SSAValue) && any(p->(p === DataType || p === UnionAll || p === Union || p === Type), tp)
                        # replace element type from Tuple{DataType} with more specific type if possible
                        def = finddef(aarg, sv.src.code)
                        if def !== nothing
                            defex = def.args[2]
                            if isa(defex, Expr) && is_known_call(defex, tuple, sv.src, sv.mod)
                                tp = collect(Any, tp)
                                for j = 1:ntp
                                    specific_type = exprtype(defex.args[j+1], sv.src, sv.mod)
                                    if (tp[j] <: Type) && specific_type ⊑ tp[j]
                                        tp[j] = specific_type
                                    end
                                end
                            end
                        end
                    end
                    fldvars = Any[ newvar!(sv, tp[j]) for j in 1:ntp ]
                    for j = 1:ntp
                        push!(newstmts, Expr(:(=), fldvars[j], mk_getfield(tmpv, j, tp[j])))
                    end
                    newargs[i - 2] = fldvars
                else
                    # not all args expandable
                    return e
                end
            end
            splice!(stmts, ins:(ins - 1), newstmts)
            ins += length(newstmts)
            e.args = [Any[e.args[2]]; newargs...]

            # now try to inline the simplified call
            ft = exprtype(e.args[1], sv.src, sv.mod)
            if isa(ft, Const)
                f = ft.val
            elseif isa(ft, Conditional)
                f = nothing
                ft = Bool
            else
                f = nothing
                if !(isconcretetype(ft) || (widenconst(ft) <: Type)) || has_free_typevars(ft)
                    # TODO: this is really aggressive at preventing inlining of closures. maybe drop `isconcretetype` requirement?
                    return e
                end
            end
        else
            return e
        end
    end
    return e
end


function add_slot!(src::CodeInfo, @nospecialize(typ), is_sa::Bool, name::Symbol=COMPILER_TEMP_SYM)
    @assert !isa(typ, Const) && !isa(typ, Conditional)
    id = length(src.slotnames) + 1
    push!(src.slotnames, name)
    push!(src.slottypes, typ)
    push!(src.slotflags, is_sa * SLOT_ASSIGNEDONCE)
    return SlotNumber(id)
end

function is_known_call(e::Expr, @nospecialize(func), src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return isa(f, Const) && f.val === func
end

function is_known_call_p(e::Expr, @nospecialize(pred), src::CodeInfo, mod::Module)
    if e.head !== :call
        return false
    end
    f = exprtype(e.args[1], src, mod)
    return (isa(f, Const) && pred(f.val)) || (isType(f) && pred(f.parameters[1]))
end

symequal(x::SSAValue, y::SSAValue) = x.id === y.id
symequal(x::Slot    , y::Slot)     = x.id === y.id
symequal(@nospecialize(x)     , @nospecialize(y))      = x === y

function void_use_elim_pass!(sv::OptimizationState)
    # Remove top level SSAValue and slots that is `!usedUndef`.
    # Also remove some `nothing` while we are at it....
    not_void_use = function (@nospecialize(ex),)
        if isa(ex, SSAValue)
            # Explicitly listed here for clarity
            return false
        elseif isa(ex, Slot)
            return sv.src.slotflags[slot_id(ex)] & SLOT_USEDUNDEF != 0
        elseif isa(ex, GlobalRef)
            ex = ex::GlobalRef
            return !isdefined(ex.mod, ex.name)
        elseif isa(ex, Expr)
            h = ex.head
            if h === :return || h === :(=) || h === :gotoifnot || is_meta_expr_head(h)
                return true
            elseif h === :isdefined || h === :copyast || h === :the_exception
                return false
            end
            return !effect_free(ex, sv.src, sv.mod, false)
        elseif (isa(ex, GotoNode) || isa(ex, LineNumberNode) ||
                isa(ex, NewvarNode) || isa(ex, Symbol) || isa(ex, LabelNode))
            # This is a list of special types handled by the compiler
            return true
        end
        return false
    end
    filter!(not_void_use, sv.src.code::Array{Any,1})
    nothing
end

function meta_elim_pass!(code::Array{Any,1}, do_coverage::Bool)
    # 1. Remove place holders
    #
    # 2. If coverage is off, remove line number nodes that don't mark any
    #    real expressions.
    #
    # 3. Remove top level SSAValue
    #

    # Position of the last line number node without any non-meta expressions
    # in between.
    prev_dbg_stack = Int[0] # always non-empty
    # Whether there's any non-meta exprs after the enclosing `push_loc`
    push_loc_pos_stack = Int[0] # always non-empty

    for i in 1:length(code)
        ex = code[i]
        if ex === nothing
            continue
        elseif isa(ex, SSAValue)
            code[i] = nothing
            continue
        elseif isa(ex, LabelNode)
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            continue
        elseif !do_coverage && isa(ex, LineNumberNode)
            prev_label = prev_dbg_stack[end]
            if prev_label != 0 && code[prev_label].file === ex.file
                code[prev_label] = nothing
            end
            prev_dbg_stack[end] = i
            continue
        elseif !isa(ex, Expr)
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            continue
        end
        ex = ex::Expr
        args = ex.args
        head = ex.head
        if head !== :meta
            prev_dbg_stack[end] = 0
            push_loc_pos_stack[end] = 0
            continue
        end
        nargs = length(args)
        if do_coverage || nargs == 0
            continue
        end
        arg1 = args[1]
        if arg1 === :push_loc
            push!(prev_dbg_stack, 0)
            push!(push_loc_pos_stack, i)
        elseif arg1 === :pop_loc
            npops = (nargs > 1 ? args[2]::Int : 1)
            for pop in 1:npops
                prev_dbg = if length(prev_dbg_stack) > 1
                    pop!(prev_dbg_stack)
                else
                    prev_dbg_stack[end]
                end
                if prev_dbg > 0
                    code[prev_dbg] = nothing
                end
                push_loc = if length(push_loc_pos_stack) > 1
                    pop!(push_loc_pos_stack)
                else
                    push_loc_pos_stack[end]
                end
                if push_loc > 0
                    code[push_loc] = nothing
                    npops -= 1
                else
                    prev_dbg_stack[end] = 0
                    push_loc_pos_stack[end] = 0
                end
            end
            if npops > 1
                code[i] = Expr(:meta, :pop_loc, npops)
            elseif npops == 1
                code[i] = Expr(:meta, :pop_loc)
            else
                code[i] = nothing
            end
        else
            continue
        end
    end

    # combine consecutive :pop_loc instructions
    lastpop = nothing
    npops = 0
    for i in 1:length(code)
        ex = code[i]
        if isa(ex, Expr) && ex.head === :meta && length(ex.args) > 0 && ex.args[1] == :pop_loc
            npops += (length(ex.args) > 1 ? ex.args[2]::Int : 1)
            if lastpop === nothing
                lastpop = i
            else
                code[i] = nothing
            end
        elseif ex !== nothing && lastpop !== nothing
            if npops > 1
                popex = code[lastpop]
                if length(popex.args) == 1
                    code[lastpop] = Expr(:meta, :pop_loc, npops)
                else
                    popex.args[2] = npops
                end
            end
            lastpop = nothing
            npops = 0
        end
    end
end

# Replace branches with constant conditions with unconditional branches
function gotoifnot_elim_pass!(sv::OptimizationState)
    body = sv.src.code
    i = 1
    while i < length(body)
        expr = body[i]
        i += 1
        isa(expr, Expr) || continue
        expr.head === :gotoifnot || continue
        cond = expr.args[1]
        condt = exprtype(cond, sv.src, sv.mod)
        isa(condt, Const) || continue
        val = (condt::Const).val
        # Codegen should emit an unreachable if val is not a Bool so
        # we don't need to do anything (also, type inference currently
        # doesn't recognize the error for strictly non-Bool condition)
        if isa(val, Bool)
            # in case there's side effects... (like raising `UndefVarError`)
            body[i - 1] = cond
            if val === false
                insert!(body, i, GotoNode(expr.args[2]))
                i += 1
            end
        end
    end
end

# basic dead-code-elimination of unreachable statements
function basic_dce_pass!(sv::OptimizationState)
    body = sv.src.code
    labelmap = get_label_map(body)
    reachable = BitSet()
    W = BitSet()
    push!(W, 1)
    while !isempty(W)
        pc = pop!(W)
        pc in reachable && continue
        push!(reachable, pc)
        expr = body[pc]
        pc´ = pc + 1 # next program-counter (after executing instruction)
        if isa(expr, GotoNode)
            pc´ = labelmap[expr.label]
        elseif isa(expr, Expr)
            if expr.head === :gotoifnot
                push!(W, labelmap[expr.args[2]::Int])
            elseif expr.head === :enter
                push!(W, labelmap[expr.args[1]::Int])
            elseif expr.head === :return
                continue
            end
        end
        pc´ <= length(body) && push!(W, pc´)
    end
    for i in 1:length(body)
        expr = body[i]
        if !(i in reachable ||
             (isa(expr, Expr) && is_meta_expr(expr)) ||
             isa(expr, LineNumberNode))
            body[i] = nothing
        end
    end
end

# Run on linear IR before the `alloc_elim_pass!` to fold away
# getfield on constant immutable objects. Any chained getfield will then be optimized
# away by the `alloc_elim_pass!`.
# Also expect undefined variable access check to be lifted into flags.
function fold_constant_getfield_pass!(sv::OptimizationState)
    body = sv.src.code
    nexpr = length(body)
    for i in 1:nexpr
        ex = body[i]
        isa(ex, Expr) || continue
        head = ex.head
        parent = body
        pidx = i
        if head === :(=)
            parent = ex.args
            pidx = 2
            ex = ex.args[2]
            isa(ex, Expr) || continue
            head = ex.head
        end
        (head === :call && 2 < length(ex.args) < 5) || continue
        is_known_call(ex, getfield, sv.src, sv.mod) || continue
        # No side effect can happen for linear IR
        obj = exprtype(ex.args[2], sv.src, sv.mod)
        isa(obj, Const) || continue
        obj = obj.val
        is_inlineable_constant(obj) || continue
        fld = ex.args[3]
        if isa(fld, Int)
            fld = fld
        elseif isa(fld, QuoteNode)
            fld = fld.value
        else
            continue
        end
        isa(fld, Symbol) || isa(fld, Int) || continue
        if isdefined(obj, fld)
            parent[pidx] = quoted(getfield(obj, fld))
        end
    end
end

function get_undef_flag_slot(src::CodeInfo, flagslots, id)
    flag_id = flagslots[id]
    flag_id != 0 && return SlotNumber(flag_id)
    slot = add_slot!(src, Nothing, src.slotflags[id] & SLOT_ASSIGNEDONCE != 0, src.slotnames[id])
    flag_id = slot_id(slot)
    src.slotflags[flag_id] |= SLOT_STATICUNDEF | SLOT_USEDUNDEF
    flagslots[id] = flag_id
    return slot
end

# Run on linear IR before the `alloc_elim_pass!` to make sure all expression arguments are
# effect-free while maintaining the original def-use chain so that assignments of allocation
# to a UsedUndef slot can still be optimized.
# After this pass, we are sure that no `Expr` arguments have side-effects.
function split_undef_flag_pass!(sv::OptimizationState)
    body = sv.src.code
    len = length(body)
    next_i = 1
    norigslots = length(sv.src.slotflags)
    flagslots = zeros(Int, norigslots)
    while next_i <= len
        i = next_i
        next_i += 1
        ex = body[i]
        if isa(ex, Slot)
            id = slot_id(ex)
            var_has_undef(sv.src, id) || continue
            body[i] = get_undef_flag_slot(sv.src, flagslots, id)
            continue
        elseif isa(ex, NewvarNode)
            id = slot_id(ex.slot)
            var_has_undef(sv.src, id) || continue
            body[i] = NewvarNode(get_undef_flag_slot(sv.src, flagslots, id))
            continue
        elseif !isa(ex, Expr)
            continue
        end
        head = ex.head
        if head === :(=)
            rhs = ex.args[2]
            lhs = ex.args[1]
            if isa(lhs, Slot)
                id = slot_id(lhs)
                if var_has_undef(sv.src, id)
                    flagslot = get_undef_flag_slot(sv.src, flagslots, id)
                    insert!(body, i + 1, :($flagslot = $nothing))
                    next_i += 1
                    len += 1
                end
            end
            if isa(rhs, Expr)
                ex = rhs
                head = ex.head
            else
                if isa(rhs, Slot)
                    id = slot_id(rhs)
                    if var_has_undef(sv.src, id)
                        insert!(body, i, get_undef_flag_slot(sv.src, flagslots, id))
                        i += 1
                        next_i += 1
                        len += 1
                    end
                end
                continue
            end
        end
        eargs = ex.args
        if head === :const || is_meta_expr_head(head)
            continue
        elseif head === :isdefined
            @assert length(eargs) == 1
            ea1 = eargs[1]
            if isa(ea1, Slot)
                id = slot_id(ea1)
                if var_has_undef(sv.src, id)
                    eargs[1] = get_undef_flag_slot(sv.src, flagslots, id)
                end
            end
            continue
        end
        isccall = head === :foreigncall
        for j in 1:length(eargs)
            ea = eargs[j]
            if isccall && isa(ea, Expr) && ea.head === :&
                ea = ea.args[1]
            end
            if isa(ea, Slot)
                id = slot_id(ea)
                if j == 1 && head === :method
                    flagslot = get_undef_flag_slot(sv.src, flagslots, id)
                    insert!(body, i + 1, :($flagslot = $nothing))
                    next_i += 1
                    len += 1
                    continue
                end
                if var_has_undef(sv.src, id)
                    insert!(body, i, get_undef_flag_slot(sv.src, flagslots, id))
                    i += 1
                    next_i += 1
                    len += 1
                end
            end
        end
    end
    for i in 1:norigslots
        if flagslots[i] != 0
            sv.src.slotflags[i] = (sv.src.slotflags[i] | SLOT_STATICUNDEF) & ~UInt8(SLOT_USEDUNDEF)
        end
    end
end


# Check if the use is still valid.
# The code that invalidate this use is responsible for adding new use(s) if any.
function check_valid(use::ValueUse, changes::IdDict)
    haskey(changes, use.stmts=>use.stmtidx) && return false
    isdefined(use, :expr) && haskey(changes, use.expr) && return false
    return true
end


# Check if the use is still valid.
# The code that invalidate this use is responsible for adding new def(s) if any.
check_valid(def::ValueDef, changes::IdDict) = !haskey(changes, def.stmts=>def.stmtidx)


function remove_invalid!(info::ValueInfo, changes::IdDict)
    if isempty(changes)
        return
    end
    cb = x->check_valid(x, changes)
    filter!(cb, info.uses)
    filter!(cb, info.defs)
    return
end

function add_def(info::ValueInfo, def::ValueDef)
    if info.defs === EMPTY_DEFS
        info.defs = [def]
    else
        push!(info.defs, def)
    end
    return
end
function add_use(info::ValueInfo, use::ValueUse)
    if info.uses === EMPTY_USES
        info.uses = [use]
    else
        push!(info.uses, use)
    end
    return
end

@inline get_info_entry(infomap::ValueInfoMap, slot::Slot) = (infomap.slots, slot_id(slot))
@inline get_info_entry(infomap::ValueInfoMap, ssa::SSAValue) = (infomap.ssas, ssa.id + 1)
@inline get_info_entry(infomap::ValueInfoMap, pair::Pair{Int,Bool}) =
    (pair.second ? infomap.ssas : infomap.slots, pair.first)
isassigned(infomap::ValueInfoMap, var) = isassigned(get_info_entry(infomap, var)...)
function delete!(infomap::ValueInfoMap, var)
    infos, idx = get_info_entry(infomap, var)
    ccall(:jl_arrayunset, Cvoid, (Any, Csize_t), infos, (idx - 1) % UInt)
end
function getindex(infomap::ValueInfoMap, var)
    infos, idx = get_info_entry(infomap, var)
    if isassigned(infos, idx)
        return infos[idx]
    else
        idx > length(infos) && resize!(infos, idx)
        info = ValueInfo()
        infos[idx] = info
        return info
    end
end
function setindex!(infomap::ValueInfoMap, info::ValueInfo, var)
    ary, idx = get_info_entry(infomap, var)
    idx > length(ary) && resize!(ary, idx)
    ary[idx] = info
end
add_def(infomap::ValueInfoMap, var, def::ValueDef) = add_def(infomap[var], def)
add_use(infomap::ValueInfoMap, var, use::ValueUse) = add_use(infomap[var], use)

@inline function var_has_static_undef(src, id, ssa=false)
    ssa && return false
    flags = src.slotflags[id]
    # The check for `UsedUndef` shouldn't be necessary but doesn't hurt
    return flags & SLOT_USEDUNDEF != 0 || flags & SLOT_STATICUNDEF != 0
end

@inline function var_has_undef(src, id, ssa=false)
    ssa && return false
    flags = src.slotflags[id]
    return flags & SLOT_USEDUNDEF != 0
end

@inline function var_is_ssa(src, id, ssa=false)
    ssa && return true
    flags = src.slotflags[id]
    # The check for `UsedUndef` shouldn't be necessary but doesn't hurt
    return flags & (SLOT_USEDUNDEF | SLOT_STATICUNDEF) == 0 && flags & SLOT_ASSIGNEDONCE != 0
end

function scan_expr_use!(infomap, body, i, ex, src)
    if isa(ex, Vector{Any})
        # Shouldn't happen but just to be safe since we treat this as nested blocks
        # in the `alloc_elim_pass!`
        body[i] = nothing
        return
    elseif isa(ex, SSAValue)
        body[i] = nothing
        return
    elseif isa(ex, Slot)
        if var_has_undef(src, slot_id(ex))
            add_use(infomap, ex, ValueUse(body, i))
        else
            body[i] = nothing
        end
        return
    elseif isa(ex, NewvarNode)
        # Clear newvarnode if the variable is never used undefined
        if var_has_undef(src, slot_id(ex.slot))
            add_def(infomap, ex.slot, ValueDef(ex, body, i))
        else
            body[i] = nothing
        end
        return
    elseif !isa(ex, Expr)
        return
    end
    head = ex.head
    is_meta_expr_head(head) && return
    if head === :(=)
        rhs = ex.args[2]
        lhs = ex.args[1]
        lhs_slot = isa(lhs, Slot)
        rhs_slot = isa(rhs, Slot)
        if lhs_slot && rhs_slot && symequal(lhs, rhs)
            # Self assignment
            if var_has_undef(src, slot_id(lhs))
                body[i] = rhs
                add_use(infomap, rhs, ValueUse(body, i))
            else
                body[i] = nothing
            end
            return
        end
        if lhs_slot || isa(lhs, SSAValue)
            add_def(infomap, lhs, ValueDef(ex, body, i))
        end
        if isa(rhs, Expr)
            ex = rhs
            head = ex.head
        else
            if rhs_slot || isa(rhs, SSAValue)
                add_use(infomap, rhs, ValueUse(body, i, ex, 2))
            end
            return
        end
    end
    eargs = ex.args
    isccall = head === :foreigncall
    for j in 1:length(eargs)
        ea = eargs[j]
        if j == 1 && head === :method
            if isa(ea, Slot)
                infomap[ea].has_method = true
                continue
            end
        end
        if isccall && isa(ea, Expr) && ea.head === :&
            ea2 = ea.args[1]
            if isa(ea2, Slot) || isa(ea2, SSAValue)
                add_use(infomap, ea2, ValueUse(body, i, ea, 1))
            end
        elseif isa(ea, Slot) || isa(ea, SSAValue)
            add_use(infomap, ea, ValueUse(body, i, ex, j))
        end
    end
end

# Collect infomation about all the defs and uses of variables in the function
# Also do simple cleanups as we do a linear scan over the code
function collect_value_infos(body::Vector{Any}, src::CodeInfo, nargs::Int)
    infomap = ValueInfoMap()
    nexpr = length(body)
    for i in 1:nexpr
        ex = body[i]
        scan_expr_use!(infomap, body, i, ex, src)
    end

    # Remove arguments from slot uses since they are useless
    slotsinfo = infomap.slots
    for i in 1:length(slotsinfo)
        if isassigned(slotsinfo, i)
            info = slotsinfo[i]
            if i > nargs && length(info.defs) == 1 && isa(info.defs[1].assign, Expr)
                src.slotflags[i] |= SLOT_ASSIGNEDONCE
            end
        end
    end

    return infomap
end

function delete_valueinfo!(ctx::AllocOptContext, key)
    # Slot
    if !key.second
        ctx.sv.src.slotflags[key.first] = 0
    end
    delete!(ctx.infomap, key)
    return
end

function add_allocopt_todo(ctx::AllocOptContext, id, is_ssa)
    ctx.todo[id=>is_ssa] = nothing
end

@inline function add_allocopt_todo(ctx::AllocOptContext, @nospecialize(var))
    if isa(var, SSAValue)
        ctx.todo[(var.id + 1)=>true] = nothing
    elseif isa(var, Pair{Int,Bool})
        ctx.todo[var] = nothing
    else
        ctx.todo[slot_id(var)=>false] = nothing
    end
end

@inline function maybe_add_allocopt_todo(ctx::AllocOptContext, @nospecialize(var))
    if isa(var, SSAValue)
        ctx.todo[(var.id + 1)=>true] = nothing
    elseif isa(var, Slot)
        ctx.todo[slot_id(var)=>false] = nothing
    end
end

function delete_value!(ctx::AllocOptContext, info, key)
    # No use is left for this value, delete the assignment
    for def in info.defs
        ctx.changes[def.assign] = nothing
        assign = def.assign
        if isa(assign, NewvarNode)
            def.stmts[def.stmtidx] = nothing
            continue
        end
        defex = assign.args[2]
        if isa(defex, SSAValue)
            add_allocopt_todo(ctx, defex)
            def.stmts[def.stmtidx] = nothing
        elseif isa(defex, Slot)
            if var_has_undef(ctx.sv.src, slot_id(defex))
                add_use(ctx.infomap, defex, ValueUse(def.stmts, def.stmtidx))
                def.stmts[def.stmtidx] = defex
            else
                add_allocopt_todo(ctx, defex)
                def.stmts[def.stmtidx] = nothing
            end
        elseif isa(defex, Vector{Any})
            def.stmts[def.stmtidx] = nothing
        else
            # Update this to add todo
            # when adding cases where this might enable further optimizations.
            def.stmts[def.stmtidx] = defex
        end
    end
    delete_valueinfo!(ctx, key)
end

function merge_value_ssa!(ctx::AllocOptContext, info, key)
    # There are other cases that we can merge
    # but those require control flow analysis.
    var_has_static_undef(ctx.sv.src, key.first, key.second) && return false
    local defkey
    for def in info.defs
        # No NewvarNode def for variables that aren't used undefined
        defex = (def.assign::Expr).args[2]
        if isa(defex, SSAValue)
            new_key = (defex.id + 1)=>true
            if @isdefined(defkey) && defkey != new_key
                return true
            else
                defkey = new_key
            end
        elseif isa(defex, Slot)
            id = slot_id(defex)
            new_key = id=>false
            defslot_is_ssa = id <= ctx.sv.nargs || var_is_ssa(ctx.sv.src, id)
            if !defslot_is_ssa || (@isdefined(defkey) && defkey != new_key)
                return true
            else
                defkey = new_key
            end
        else
            return @isdefined(defkey)
        end
    end

    if defkey.second || defkey.first > ctx.sv.nargs
        add_allocopt_todo(ctx, defkey)
    end

    # don't replace TypedSlots with SSAValues
    # TODO: introduce extra SSAValue for each differently-typed use.
    if defkey.second
        for use in info.uses
            if isdefined(use, :expr) && use.expr.args[use.exidx] isa TypedSlot
                return false
            end
        end
    end

    definfo = ctx.infomap[defkey]
    for def in info.defs
        ctx.changes[def.assign] = nothing
        def.stmts[def.stmtidx] = nothing
    end
    if defkey.second
        # SSAValue def
        replace_v = SSAValue(defkey.first - 1)
    else
        replace_v = SlotNumber(defkey.first)
    end
    for use in info.uses
        if isdefined(use, :expr)
            this_use = use.expr.args[use.exidx]
            if !defkey.second && this_use isa TypedSlot
                use.expr.args[use.exidx] = TypedSlot(replace_v.id, this_use.typ)
            else
                use.expr.args[use.exidx] = replace_v
            end
            add_use(definfo, use)
        else
            # This variable is never used undef, ignore statement level use
            use.stmts[use.stmtidx] = nothing
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function replace_use_expr_with!(ctx::AllocOptContext, use::ValueUse, expr,
                                delete_old=true, update_var_use=false)
    # Not supported on ccall & expression
    oldexpr = use.expr
    stmt = use.stmts[use.stmtidx]::Expr
    if stmt === oldexpr
        use.stmts[use.stmtidx] = expr
        if update_var_use && (isa(expr, Slot) || isa(expr, SSAValue))
            add_use(ctx.infomap, expr, ValueUse(use.stmts, use.stmtidx))
        end
    else
        @assert stmt.head === :(=) && stmt.args[2] === oldexpr
        stmt.args[2] = expr
        if update_var_use && (isa(expr, Slot) || isa(expr, SSAValue))
            add_use(ctx.infomap, expr, ValueUse(use.stmts, use.stmtidx, stmt, 2))
        end
        maybe_add_allocopt_todo(ctx, stmt.args[1])
    end
    if delete_old
        ctx.changes[oldexpr] = nothing
    end
end

function propagate_const_def!(ctx::AllocOptContext, info, key)
    local constv
    for def in info.defs
        # No NewvarNode def for variables that aren't used undefined
        defex = (def.assign::Expr).args[2]
        # We don't want to probagate this constant since this is a token.
        isa(defex, Expr) && defex.head === :gc_preserve_begin && return false
        v = exprtype(defex, ctx.sv.src, ctx.sv.mod)
        (isa(v, Const) && is_inlineable_constant(v.val)) || return false
        v = v.val
        @isdefined(constv) && constv !== v && return false
        constv = v
    end
    for def in info.defs
        defex = (def.assign::Expr).args[2]
        if isa(defex, Expr)
            # Expr def can have side effects
            def.stmts[def.stmtidx] = defex
        else
            def.stmts[def.stmtidx] = nothing
        end
        ctx.changes[def.assign] = nothing
    end
    replace_ex = quoted(constv)
    is_immutable = !typeof(constv).mutable || isa(constv, DataType)
    for use in info.uses
        if !isdefined(use, :expr)
            use.stmts[use.stmtidx] = nothing
            continue
        elseif is_immutable
            if use.exidx == 2 && is_known_call(use.expr, getfield, ctx.sv.src, ctx.sv.mod) &&
                2 < length(use.expr.args) < 5

                fld = use.expr.args[3]
                if isa(fld, Int)
                    fld = fld
                elseif isa(fld, QuoteNode)
                    fld = fld.value
                else
                    fld = nothing
                end
                if isa(fld, Symbol) || isa(fld, Int)
                    if isdefined(constv, fld)
                        fieldv = getfield(constv, fld)
                        replace_use_expr_with!(ctx, use, quoted(fieldv))
                        continue
                    end
                end
            end
        end
        use.expr.args[use.exidx] = replace_ex
        if use.expr.head === :(=)
            maybe_add_allocopt_todo(ctx, use.expr.args[1])
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function split_disjoint_assign!(ctx::AllocOptContext, info, key)
    key.second && return false
    isdispatchelem(widenconst(ctx.sv.src.slottypes[key.first])) && return false # no splitting can be necessary
    alltypes = IdDict()
    ndefs = length(info.defs)
    deftypes = Vector{Any}(uninitialized, ndefs)
    for i in 1:ndefs
        def = info.defs[i]
        defex = (def.assign::Expr).args[2]
        rhstyp = widenconst(exprtype(defex, ctx.sv.src, ctx.sv.mod))
        isdispatchelem(rhstyp) || return false
        alltypes[rhstyp] = nothing
        deftypes[i] = rhstyp
    end
    need_tag = false
    for use in info.uses
        usex = use.expr
        slot = usex.args[use.exidx]
        if isa(slot, TypedSlot)
            usetyp = widenconst(slot.typ)
            if isdispatchelem(usetyp)
                alltypes[usetyp] = nothing
                continue
            end
        end
        if usex.head === :(=) || usex.head === :&
            return false
        else
            ty = exprtype(usex, ctx.sv.src, ctx.sv.mod)
            if isa(ty, Const)
            elseif isa(ty, Conditional) && isa(ty.var, Slot) && slot_id(ty.var) == key.first
                if isa(ty.vtype, Const) && !isdefined(typeof(ty.vtype.val), :instance)
                    return false
                end
                need_tag = true
            else
                return false
            end
            effect_free(usex, ctx.sv.src, ctx.sv.mod, false) || return false
        end
    end
    name = ctx.sv.src.slotnames[key.first]
    flags = ctx.sv.src.slotflags[key.first]
    if need_tag
        tag_var = add_slot!(ctx.sv.src, Int, false, name)
        ctx.sv.src.slotflags[tag_var.id] = flags
    end
    for i in 1:ndefs
        def = info.defs[i]
        assign = def.assign::Expr
        defex = assign.args[2]
        rhstyp = deftypes[i]
        new_slot = alltypes[rhstyp]
        if !isa(new_slot, SlotNumber)
            new_slot = add_slot!(ctx.sv.src, rhstyp, false, name)
            ctx.sv.src.slotflags[new_slot.id] = flags | SLOT_STATICUNDEF
            alltypes[rhstyp] = new_slot
            add_allocopt_todo(ctx, new_slot)
        end
        assign.args[1] = new_slot
        if need_tag
            stmts = Any[:($tag_var = $(new_slot.id)), assign]
            add_def(ctx.infomap, tag_var, ValueDef(stmts[1], stmts, 1))
            scan_expr_use!(ctx.infomap, stmts, 2, assign, ctx.sv.src)
            ctx.changes[def.stmts=>def.stmtidx] = nothing
            def.stmts[def.stmtidx] = stmts
        else
            add_def(ctx.infomap, new_slot, def)
        end
    end
    for use in info.uses
        usex = use.expr
        slot = usex.args[use.exidx]
        if isa(slot, TypedSlot)
            usetyp = widenconst(slot.typ)
            if isdispatchelem(usetyp)
                usetyp = widenconst(slot.typ)
                new_slot = alltypes[usetyp]
                if !isa(new_slot, SlotNumber)
                    new_slot = add_slot!(ctx.sv.src, usetyp, false, name)
                    ctx.sv.src.slotflags[new_slot.id] = flags | SLOT_STATICUNDEF
                    alltypes[usetyp] = new_slot
                end
                new_slot = new_slot::SlotNumber
                use.expr.args[use.exidx] = new_slot
                add_use(ctx.infomap, new_slot, use)
                continue
            end
        end
        ty = exprtype(usex, ctx.sv.src, ctx.sv.mod)
        if isa(ty, Const) && is_inlineable_constant(ty.val)
            replace_use_expr_with!(ctx, use, quoted(ty.val))
        elseif isa(ty, Conditional)
            exprs = []
            vars = []
            for (t, v) in alltypes
                isa(v, SlotNumber) || continue
                if t ⊑ widenconst(ty.vtype)
                    new_var = newvar!(ctx.sv, Bool)
                    new_val = :($(GlobalRef(Core, :(===)))($tag_var, $(v.id)))
                    new_val.typ = Bool
                    new_ex = :($new_var = $new_val)
                    push!(exprs, new_ex)
                    push!(vars, new_var)
                    add_def(ctx.infomap, new_var, ValueDef(new_ex, exprs, length(exprs)))
                    add_use(ctx.infomap, tag_var, ValueUse(exprs, length(exprs), new_val, 2))
                end
            end
            if isempty(vars)
                replace_use_expr_with!(ctx, use, quoted(false))
            else
                var = vars[1]
                for var_i in 2:length(vars)
                    new_var = newvar!(ctx.sv, Bool)
                    new_val = :($(GlobalRef(Core.Intrinsics, :and_int))($var, $(vars[var_i])))
                    new_val.typ = Bool
                    new_ex = :($new_var = $new_val)
                    push!(exprs, new_ex)
                    add_def(ctx.infomap, new_var, ValueDef(new_ex, exprs, length(exprs)))
                    add_use(ctx.infomap, var, ValueUse(exprs, length(exprs), new_val, 2))
                    add_use(ctx.infomap, vars[var_i], ValueUse(exprs, length(exprs), new_val, 3))
                    var = new_var
                end
                replace_use_expr_with!(ctx, use, var, false)
                old_expr = use.stmts[use.stmtidx]
                push!(exprs, old_expr)
                use.stmts[use.stmtidx] = exprs
                scan_expr_use!(ctx.infomap, exprs, length(exprs), old_expr, ctx.sv.src)
                ctx.changes[use.stmts=>use.stmtidx] = nothing
            end
        end
    end
    delete_valueinfo!(ctx, key)
    return true
end

function show_info(io::IO, use::ValueUse, ctx)
    if isdefined(use, :expr)
        print(io, "ValueUse(expr=`", use.expr, "`, exidx=", use.exidx,
              ", stmts[", use.stmtidx, "]=`", use.stmts[use.stmtidx], "`)")
    else
        print(io, "ValueUse(stmts[", use.stmtidx, "]=", use.stmts[use.stmtidx], ")")
    end
    if isa(ctx, AllocOptContext) && !check_valid(use, ctx.changes)
        print(io, " Invalidated")
    end
end
function show_info(io::IO, def::ValueDef, ctx)
    print(io, "ValueDef(assign=`", def.assign, "`, stmts=..., stmtidx=", def.stmtidx, ")")
    if isa(ctx, AllocOptContext) && !check_valid(def, ctx.changes)
        print(io, " Invalidated")
    end
end
function show_info(io::IO, info::ValueInfo, ctx, prefix="")
    println(io, prefix, "ValueInfo:")
    if !isempty(info.defs)
        println(io, prefix, "  Defs:")
        for def in info.defs
            print(io, prefix, "    ")
            show_info(io, def, ctx)
            println(io)
        end
    end
    if !isempty(info.uses)
        println(io, prefix, "  Uses:")
        for use in info.uses
            print(io, prefix, "    ")
            show_info(io, use, ctx)
            println(io)
        end
    end
end
function show_info(io::IO, info::ValueInfoMap, ctx)
    for i in 1:length(info.ssas)
        isassigned(info.ssas, i) || continue
        println(io, "SSAValue(", i - 1, "):")
        show_info(io, info.ssas[i], ctx, "  ")
    end
    for i in 1:length(info.slots)
        isassigned(info.slots, i) || continue
        println(io, "Slot(", i, "):")
        show_info(io, info.slots[i], ctx, "  ")
    end
end

function structinfo_constant(ctx::AllocOptContext, @nospecialize(v), vt::DataType)
    nf = fieldcount(vt)
    if vt <: Tuple
        si = StructInfo(Vector{Any}(uninitialized, nf), Symbol[], vt, false, false)
    else
        si = StructInfo(Vector{Any}(uninitialized, nf), collect(Symbol, fieldnames(vt)),
                        vt, false, false)
    end
    for i in 1:nf
        if isdefined(v, i)
            si.defs[i] = quoted(getfield(v, i))
        else
            ctx.undef_fld[i] = nothing
            ctx.undef_fld[si.names[i]] = nothing
        end
    end
    return si
end

structinfo_tuple(ex::Expr) = StructInfo(ex.args[2:end], Symbol[], Tuple, false, false)
function structinfo_new(ctx::AllocOptContext, ex::Expr, vt::DataType)
    nf = fieldcount(vt)
    si = StructInfo(Vector{Any}(uninitialized, nf), collect(Symbol, fieldnames(vt)),
                    vt, vt.mutable, true)
    ninit = length(ex.args) - 1
    for i in 1:nf
        if i <= ninit
            si.defs[i] = ex.args[i + 1]
        else
            ft = fieldtype(vt, i)
            if isbits(ft)
                ex = Expr(:new, ft)
                ex.typ = ft
                si.defs[i] = ex
            else
                ctx.undef_fld[i] = nothing
                ctx.undef_fld[si.names[i]] = nothing
            end
        end
    end
    return si
end

function split_struct_alloc!(ctx::AllocOptContext, info, key)
    # Collect information about each struct/tuple allocation
    min_nf = typemax(Int)
    max_nf = 0
    n_immut = 0
    ndef = length(info.defs)
    empty!(ctx.sym_count)
    empty!(ctx.undef_fld)
    empty!(ctx.structinfos)
    for def in info.defs
        local defex, si, def_nf
        defex = (def.assign::Expr).args[2]
        if isa(defex, Expr)
            defex = defex::Expr
            if is_known_call(defex, tuple, ctx.sv.src, ctx.sv.mod)
                si = structinfo_tuple(defex)
            elseif defex.head === :new
                typ = widenconst(exprtype(defex, ctx.sv.src, ctx.sv.mod))
                # typ <: Tuple shouldn't happen but just in case someone generated invalid AST
                if !isa(typ, DataType) || !isdispatchelem(typ) || typ <: Tuple
                    return false
                end
                si = structinfo_new(ctx, defex, typ)
            else
                return false
            end
        else
            v = exprtype(defex, ctx.sv.src, ctx.sv.mod)
            isa(v, Const) || return false
            v = v.val
            vt = typeof(v)
            if (vt.mutable && !isa(v, DataType)) || fieldcount(vt) == 0
                # allowing DataType as immutable can change the behavior if the user calls
                # `setfield!` on it but that's invalid anyway and throwing an error is actually
                # better than letting it work.
                return false
            end
            si = structinfo_constant(ctx, v, vt)
        end
        if !si.mutable
            n_immut += 1
        end
        def_nf = length(si.defs)
        if def_nf < min_nf
            min_nf = def_nf
        end
        if def_nf > max_nf
            max_nf = def_nf
        end
        for fld_idx in 1:length(si.names)
            sym = si.names[fld_idx]
            prev_count = get(ctx.sym_count, sym, 0=>0)
            if isa(prev_count, Pair{Int,Int})
                if first(prev_count) == 0
                    ctx.sym_count[sym] = 1=>fld_idx
                elseif last(prev_count) == fld_idx
                    ctx.sym_count[sym] = (first(prev_count) + 1)=>fld_idx
                else
                    ctx.sym_count[sym] = (first(prev_count) + 1)=>[last(prev_count), fld_idx]
                end
            else
                prev_count = prev_count::Pair{Int,Vector{Int}}
                ctx.sym_count[sym] = (first(prev_count) + 1)=>push!(last(prev_count), fld_idx)
            end
        end
        push!(ctx.structinfos, si)
    end
    if ndef > 1
        allowed_idx = trues(min_nf)
        idx_count = zeros(Int, min_nf)
        for v in values(ctx.sym_count)
            # Ignore field names that will not be allowed to be optimized
            if isa(v, Pair{Int,Int})
                first(v) == ndef || continue
                if last(v) <= min_nf
                    idx_count[last(v)] += 1
                end
            else
                v = v::Pair{Int,Vector{Int}}
                first(v) == ndef || continue
                for fld_idx in last(v)
                    allowed_idx[fld_idx] = false
                end
            end
        end
        for fld_idx in 1:min_nf
            if idx_count[fld_idx] > 1
                allowed_idx[fld_idx] = false
            end
        end
    end

    # Find the set of valid operations for each kind of use
    #
    # * `isdefined`
    #
    #     Field name or index must be constant and unambiguous.
    #     Field that are known or unknown for all defs are allowed.
    #     Unambiguous means that each field must be only accessed by index or only by name
    #     that maps to the same index for all defs.
    #     Currently this requirement is raised to be only based on the def and not the use
    #
    # * `getfield`
    #
    #     Requires all conditions for `isdefined`.
    #     Field index must be within the intersect of all the defs.
    #     Field name must be known for all defs.
    #
    # * `setfield!`
    #
    #     Requires all conditions for `getfield`.
    #     Additionally require all defs to be mutable.
    #
    # * preserved objects (`GC.@preserve` and `ccall` roots)
    #
    #     No `setfield!` should be called for mutable defs on the NULL sites.
    #     This is because it's currently unclear how conditional undefined root slots
    #     can be represented. It's possible that we can just change the requirement in codegen
    #     for preserved objects.
    #     Currently also require single assignment.
    #     Lifting this requirement is certainly possible but harder to implement....

    has_preserve = false
    has_setfield_undef = false
    empty!(ctx.all_fld)
    empty!(ctx.setfield_typ)
    for use in info.uses
        isdefined(use, :expr) || continue
        local fld
        local expr = use.expr
        local head = expr.head
        if head === :isdefined
            # May or may not be needed but trivial to handle
            continue
        elseif head === :gc_preserve_begin
            if ndef > 1
                # This is certainly overkill, but too hard for initial version ;-p
                return false
            end
            has_setfield_undef && return false
            has_preserve = true
            continue
        elseif head === :foreigncall
            if ndef > 1
                # This is certainly overkill, but too hard for initial version ;-p
                return false
            end
            nccallargs = expr.args[5]::Int
            if use.exidx <= 5 + nccallargs
                return false
            end
            has_setfield_undef && return false
            has_preserve = true
            continue
        elseif head === :call
            if use.exidx != 2 || length(expr.args) < 3
                return false
            end
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                if n_immut != 0 || length(expr.args) < 4
                    return false
                end
                use_kind = 2
            else
                return false
            end
        else
            return false
        end
        if isa(fld, Int)
            fld = fld
        elseif isa(fld, QuoteNode)
            fld = fld.value
        else
            return false
        end
        if isa(fld, Int)
            if 0 < fld <= min_nf
                ndef == 1 || allowed_idx[fld] || return false
            elseif min_nf < fld <= max_nf
                return false
            elseif use_kind == 0
                # We can handle this but don't record in `all_fld`
                continue
            else
                return false
            end
        elseif isa(fld, Symbol)
            v = get(ctx.sym_count, fld, 0=>0)
            fld_count = isa(v, Pair{Int,Int}) ? first(v) : first(v::Pair{Int,Vector})
            if use_kind == 0 && fld_count == 0
                # We can handle this but don't record in `all_fld`
                continue
            elseif fld_count != ndef
                return false
            end
        else
            return false
        end
        if use_kind == 2
            if haskey(ctx.undef_fld, fld)
                has_preserve && return false
                has_setfield_undef = true
            end
            fld_typ = widenconst(exprtype(expr.args[4], ctx.sv.src, ctx.sv.mod))
            ctx.setfield_typ[fld] = Union{fld_typ, get(ctx.setfield_typ, fld, Union{})}
        end
        ctx.all_fld[fld] = nothing
    end
    if ndef == 1
        split_struct_alloc_single!(ctx, info, key, min_nf, has_preserve, has_setfield_undef)
    else
        split_struct_alloc_multi!(ctx, info, key)
    end
    delete_valueinfo!(ctx, key)
    return true
end

function split_struct_alloc_multi!(ctx::AllocOptContext, info, key)
    # If there are multiple assignments, we have to create slots for each values
    # We currently only allow ccall root with a single assignment so we only need to handle
    # isdefined, setfield! and getfield here.

    # First, assign a slot to each variable.
    # The slot types at this point is determined only by the setfield that are applied.
    # We'll include the initialization type as we go through the defs
    vars = IdDict()
    create_struct_field_slots!(ctx, key, vars)

    # Now, for each def. Assign all the slots.
    # Also `Union` the RHS of the assignments into the slot type as we scan through the defs.
    replace_struct_defs!(ctx, info, vars)

    # Finally replace all uses
    replace_struct_uses!(ctx, info, vars)
end

function replace_struct_uses!(ctx, info, vars)
    for use in info.uses
        if !isdefined(use, :expr)
            # Top level dummy use, remove
            use.stmts[use.stmtidx] = nothing
            continue
        end
        local fld
        local use_kind
        expr = use.expr
        head = expr.head
        if head === :isdefined
            replace_use_expr_with!(ctx, use, quoted(true))
            continue
        elseif head === :call
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                use_kind = 2
            end
        end
        if isa(fld, QuoteNode)
            fld = fld.value
        end
        slot_id = get(vars, fld, 0)
        if slot_id == 0
            @assert use_kind == 0
            replace_use_expr_with!(ctx, use, quoted(false))
            continue
        end

        if use_kind == 0
            # isdefined
            if haskey(ctx.undef_fld, fld)
                replace_use_expr_with!(ctx, use, SlotNumber(slot_id + 1), true, true)
            else
                replace_use_expr_with!(ctx, use, quoted(true))
            end
        elseif use_kind == 1
            # getfield
            if !haskey(ctx.undef_fld, fld)
                replace_use_expr_with!(ctx, use, SlotNumber(slot_id), true, true)
            else
                replace_var = SlotNumber(slot_id)
                flag_slot = SlotNumber(slot_id + 1)
                check_expr = Expr(:call, throw_undefreferror_ifnot, flag_slot)
                stmts = Any[check_expr]
                add_use(ctx.infomap, flag_slot, ValueUse(stmts, 1, check_expr, 2))
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt !== expr
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = replace_var
                    push!(stmts, stmt)
                    scan_expr_use!(ctx.infomap, stmts, length(stmts), stmt, ctx.sv.src)
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
                use.stmts[use.stmtidx] = stmts
            end
        else
            @assert use_kind == 2
            # setfield!
            replace_var = SlotNumber(slot_id)
            val = expr.args[4]
            stmts = Any[]
            if haskey(ctx.undef_fld, fld)
                flag_slot = SlotNumber(slot_id + 1)
                assign_flag_ex = :($flag_slot = true)
                push!(stmts, assign_flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(assign_flag_ex, stmts, length(stmts)))
            end
            assign_ex = :($replace_var = $val)
            push!(stmts, assign_ex)
            scan_expr_use!(ctx.infomap, stmts, length(stmts), assign_ex, ctx.sv.src)
            stmt = use.stmts[use.stmtidx]::Expr
            if stmt !== expr
                @assert stmt.head === :(=) && stmt.args[2] === expr
                extra_assign_ex = :($(stmt.args[1]) = $val)
                push!(stmts, extra_assign_ex)
                scan_expr_use!(ctx.infomap, stmts, length(stmts), extra_assign_ex, ctx.sv.src)
            end
            use.stmts[use.stmtidx] = stmts
            ctx.changes[use.stmts=>use.stmtidx] = nothing
        end
    end
end

function check_new_field_type(@nospecialize(val), @nospecialize(typ))
    if !isa(val, typ)
        ccall(:jl_type_error_new_expr, Union{}, (Any, Any), typ, val)
    end
end

function replace_struct_defs!(ctx, info, vars)
    for i in 1:length(info.defs)
        si = ctx.structinfos[i]
        has_name = !isempty(si.names)
        exprs = []
        for fld_idx in 1:length(si.defs)
            local slot_id
            local fld_def
            need_flag = false
            if isassigned(si.defs, fld_idx)
                fld_def = si.defs[fld_idx]
            end
            has_def = @isdefined(fld_def)
            # First check field name
            if has_name
                fname = si.names[fld_idx]
                if haskey(vars, fname)
                    slot_id = vars[fname]
                    need_flag = haskey(ctx.undef_fld, fname)
                end
            end
            if !@isdefined(slot_id)
                # Then check field index
                if haskey(vars, fld_idx)
                    slot_id = vars[fld_idx]
                    need_flag = haskey(ctx.undef_fld, fld_idx)
                else
                    # The field is not used and we don't need to deal with any side effects
                    if has_def
                        if si.isnew && !(exprtype(fld_def, ctx.sv.src,
                                                  ctx.sv.mod) ⊑ fieldtype(si.typ, fld_idx))
                            check_ex = Expr(:call, check_new_field_type, fld_def,
                                            quoted(fieldtype(si.typ, fld_idx)))
                            push!(exprs, check_ex)
                            scan_expr_use!(ctx.infomap, exprs, length(exprs), check_ex, ctx.sv.src)
                        else
                            maybe_add_allocopt_todo(ctx, fld_def)
                        end
                    end
                    continue
                end
            end

            if need_flag
                flag_slot = SlotNumber(slot_id + 1)
                flag_ex = :($flag_slot = $has_def)
                push!(exprs, flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(flag_ex, exprs, length(exprs)))
            end
            if has_def
                fld_ty = widenconst(exprtype(fld_def, ctx.sv.src, ctx.sv.mod))
                if si.isnew && !(fld_ty ⊑ fieldtype(si.typ, fld_idx))
                    struct_fld_ty = fieldtype(si.typ, fld_idx)
                    check_ex = Expr(:call, check_new_field_type, fld_def,
                                    quoted(struct_fld_ty))
                    push!(exprs, check_ex)
                    scan_expr_use!(ctx.infomap, exprs, length(exprs), check_ex, ctx.sv.src)
                    fld_ty = typeintersect(struct_fld_ty, fld_ty)
                else
                    maybe_add_allocopt_todo(ctx, fld_def)
                end
                assign_ex = :($(SlotNumber(slot_id)) = $fld_def)
                push!(exprs, assign_ex)
                scan_expr_use!(ctx.infomap, exprs, length(exprs), assign_ex, ctx.sv.src)
                slot_type = ctx.sv.src.slottypes[slot_id]
                ctx.sv.src.slottypes[slot_id] = Union{slot_type,fld_ty}
            end
        end
        def = info.defs[i]
        ctx.changes[def.stmts=>def.stmtidx] = nothing
        def.stmts[def.stmtidx] = exprs
    end
end

function create_struct_field_slots!(ctx, key, vars)
    slot_flag = var_has_static_undef(ctx.sv.src, key.first, key.second) * SLOT_STATICUNDEF
    for fld in keys(ctx.all_fld)
        haskey(vars, fld) && continue
        local fldidx
        local slot_id
        slot_type = get(ctx.setfield_typ, fld, Union{})
        # We shouldn't need to check both field index and field name for undef
        # since the two should agree for unambiguous fields.
        if isa(fld, Symbol)
            slot_name = fld
            v = get(ctx.sym_count, fld, 0=>0)
            if isa(v, Pair{Int,Int}) && first(v) != 0
                # single field index
                fldidx = last(v)
                slot_type = Union{slot_type,get(ctx.setfield_typ, fldidx, Union{})}
                if haskey(vars, fldidx)
                    slot_id = first(vars[fldidx]::Int)
                    ctx.sv.src.slottypes[slot_id] = slot_type
                    ctx.sv.src.slotnames[slot_id] = slot_name
                end
            end
        else
            slot_name = :field_slot
        end

        if !@isdefined(slot_id)
            slot_id = add_slot!(ctx.sv.src, slot_type, false, slot_name).id
            add_allocopt_todo(ctx, slot_id, false)
            if haskey(ctx.undef_fld, fld)
                ctx.sv.src.slotflags[end] = SLOT_STATICUNDEF
                undef_slot = add_slot!(ctx.sv.src, Bool, false, :field_flag)
                ctx.sv.src.slotflags[end] = slot_flag
                add_allocopt_todo(ctx, undef_slot)
                @assert undef_slot.id == slot_id + 1
            else
                ctx.sv.src.slotflags[end] = slot_flag
            end
            if @isdefined(fldidx)
                vars[fldidx] = slot_id
            end
        end
        vars[fld] = slot_id
    end
end

function throw_undefreferror_ifnot(cond::Bool)
    if !cond
        throw(UndefRefError())
    end
    return
end

function split_struct_alloc_single!(ctx::AllocOptContext, info, key, nf, has_preserve,
                                    has_setfield_undef)
    # If there's only a single def, there's a lot more tricks we can do that's impossible
    # when there are multiple ones.
    #
    # 1. Using SSAValue instead of Slot if the variable itself is SSA and there isn't
    #    any setfield on the value
    # 2. Forward SSAValue or constant field value to use directly when there isn't setfield
    # 3. (For now) Splitting preserved objects
    def = info.defs[1]
    si = ctx.structinfos[1]
    if !isempty(ctx.undef_fld) && has_setfield_undef
        flag_vars = Vector{SlotNumber}(uninitialized, nf)
    end
    vars = Vector{Any}(uninitialized, nf)
    is_ssa = !var_has_static_undef(ctx.sv.src, key.first, key.second)
    def_exprs = Any[]
    if has_preserve
        preserved_vars = Any[]
    end
    # First go through each field and decide the variable name and create assignments
    # expressions.
    for i in 1:nf
        local fld_name, orig_def
        if !isempty(si.names)
            fld_name = si.names[i]
        end
        has_fld_use = haskey(ctx.all_fld, i) || (@isdefined(fld_name) &&
                                                 haskey(ctx.all_fld, fld_name))
        has_def = isassigned(si.defs, i)
        has_setfld_use = false
        if has_def
            orig_def = si.defs[i]
            field_typ = widenconst(exprtype(orig_def, ctx.sv.src, ctx.sv.mod))
            if si.isnew && !(field_typ ⊑ fieldtype(si.typ, i))
                struct_fld_ty = fieldtype(si.typ, i)
                check_ex = Expr(:call, check_new_field_type, orig_def,
                                quoted(struct_fld_ty))
                push!(def_exprs, check_ex)
                scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), check_ex, ctx.sv.src)
                field_typ = typeintersect(struct_fld_ty, field_typ)
            else
                maybe_add_allocopt_todo(ctx, orig_def)
            end
        else
            field_typ = Union{}
        end
        if has_fld_use && !isempty(ctx.setfield_typ)
            if haskey(ctx.setfield_typ, i)
                has_setfld_use = true
                field_typ = Union{field_typ,ctx.setfield_typ[i]}
            end
            if @isdefined(fld_name) && haskey(ctx.setfield_typ, fld_name)
                has_setfld_use = true
                field_typ = Union{field_typ,ctx.setfield_typ[fld_name]}
            end
        end
        if !@isdefined(fld_name)
            fld_name = :struct_field
        end
        need_preserved_root = has_preserve && !isbits(field_typ)
        local var_slot
        if !has_def
            # If there's no direct use of the field
            # (and we know that there's no use in preserved root) we can ignore this field
            # Similarly, unless someone set the field, it is always #undef and we can
            # replace all uses with that
            has_setfld_use || continue
            # OK, so someone calls setfield! on this =(
            # We need to allocate the variable **AND** the undef tag variable
            flag_slot = add_slot!(ctx.sv.src, Bool, false, fld_name)
            ctx.sv.src.slotflags[end] = is_ssa ? 0 : SLOT_STATICUNDEF
            flag_vars[i] = flag_slot
            add_allocopt_todo(ctx, flag_slot)

            var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
            ctx.sv.src.slotflags[end] = SLOT_STATICUNDEF
            vars[i] = var_slot
            add_allocopt_todo(ctx, var_slot)

            newvar_flag = :($flag_slot = false)
            push!(def_exprs, newvar_flag)
            add_def(ctx.infomap, flag_slot, ValueDef(newvar_flag, def_exprs, length(def_exprs)))
            continue
        elseif has_setfld_use
            var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
            ctx.sv.src.slotflags[end] = is_ssa ? 0 : SLOT_STATICUNDEF
            def_ex = :($var_slot = $orig_def)
            push!(def_exprs, def_ex)
            scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), def_ex, ctx.sv.src)
            vars[i] = var_slot
            add_allocopt_todo(ctx, var_slot)
        else
            # OK so no setfield
            # First check if the field was a constant
            cv = exprtype(orig_def, ctx.sv.src, ctx.sv.mod)
            if isa(cv, Const) && is_inlineable_constant(cv.val)
                vars[i] = quoted(cv.val)
                # Constants don't need to be preserved
                continue
            end
            if has_fld_use || need_preserved_root
                need_assign = true
                if is_ssa
                    if isa(orig_def, SSAValue)
                        var_slot = orig_def
                        need_assign = false
                    else
                        var_slot = newvar!(ctx.sv, field_typ)
                    end
                else
                    var_slot = add_slot!(ctx.sv.src, field_typ, false, fld_name)
                    ctx.sv.src.slotflags[end] = SLOT_STATICUNDEF
                end
                if need_assign
                    def_ex = :($var_slot = $orig_def)
                    push!(def_exprs, def_ex)
                    scan_expr_use!(ctx.infomap, def_exprs, length(def_exprs), def_ex, ctx.sv.src)
                end
                vars[i] = var_slot
                add_allocopt_todo(ctx, var_slot)
            end
            # No side effect allowed in `Expr` arguments at this point
        end
        if need_preserved_root
            push!(preserved_vars, var_slot)
        end
    end
    # OK, so now we've created all the variables and assignments needed, replace the def.
    ctx.changes[def.stmts=>def.stmtidx] = nothing
    def.stmts[def.stmtidx] = def_exprs
    # Now fix up uses =)
    for use in info.uses
        local fld
        local use_kind
        if !isdefined(use, :expr)
            # Top level dummy use, remove
            use.stmts[use.stmtidx] = nothing
            continue
        end
        expr = use.expr
        head = expr.head
        if head === :isdefined
            replace_use_expr_with!(ctx, use, quoted(true))
            continue
        elseif head === :foreigncall || head === :gc_preserve_begin
            # Splicing in the new ccall roots without moving existing other roots
            npreserved_vars = length(preserved_vars)
            if npreserved_vars == 0
                use.expr.args[use.exidx] = nothing
            else
                cvar = preserved_vars[1]
                expr.args[use.exidx] = cvar
                add_use(ctx.infomap, cvar, use)
                if npreserved_vars > 1
                    old_nargs = length(expr.args)
                    resize!(expr.args, old_nargs + npreserved_vars - 1)
                    for i in 2:npreserved_vars
                        cvar = preserved_vars[i]
                        expr.args[old_nargs + i - 1] = cvar
                        add_use(ctx.infomap, cvar, ValueUse(use.stmts, use.stmtidx,
                                                            expr, old_nargs + i - 1))
                    end
                end
            end
            continue
        elseif head === :call
            fld = expr.args[3]
            if is_known_call(expr, isdefined, ctx.sv.src, ctx.sv.mod)
                use_kind = 0
            elseif is_known_call(expr, getfield, ctx.sv.src, ctx.sv.mod)
                use_kind = 1
            elseif is_known_call(expr, setfield!, ctx.sv.src, ctx.sv.mod)
                use_kind = 2
            end
        end
        if isa(fld, QuoteNode)
            fld = fld.value
        end
        if !isa(fld, Int)
            v = get(ctx.sym_count, fld, 0=>0)::Pair{Int,Int}
            if first(v) == 0
                @assert use_kind == 0
                replace_use_expr_with!(ctx, use, quoted(false))
                continue
            end
            fld = last(v)
        end
        if fld > nf || fld <= 0
            @assert use_kind == 0
            replace_use_expr_with!(ctx, use, quoted(false))
            continue
        end
        if use_kind == 0
            # isdefined
            if isassigned(si.defs, fld)
                replace_use_expr_with!(ctx, use, quoted(true))
                continue
            end
            flag_slot = flag_vars[fld]
            replace_use_expr_with!(ctx, use, flag_slot, true, true)
        elseif use_kind == 1
            # getfield
            if isassigned(si.defs, fld)
                replace_var = vars[fld]
                replace_use_expr_with!(ctx, use, replace_var, true, true)
            elseif haskey(ctx.setfield_typ, fld) || (!isempty(si.names) &&
                                                     haskey(ctx.setfield_typ, si.names[fld]))
                replace_var = vars[fld]
                flag_slot = flag_vars[fld]
                check_expr = Expr(:call, throw_undefreferror_ifnot, flag_slot)
                stmts = Any[check_expr]
                add_use(ctx.infomap, flag_slot, ValueUse(stmts, 1, check_expr, 2))
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt !== expr
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = replace_var
                    push!(stmts, stmt)
                    scan_expr_use!(ctx.infomap, stmts, length(stmts), stmt, ctx.sv.src)
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
                use.stmts[use.stmtidx] = stmts
            else
                throw_err = Expr(:call, GlobalRef(Core, :throw), UndefRefError())
                throw_err.typ = Union{}
                # We can't simply delete the assignment since it can break assumptions downstream
                # If we are assigning to anything, just assign the throw instead.
                stmt = use.stmts[use.stmtidx]::Expr
                if stmt === expr
                    use.stmts[use.stmtidx] = Any[throw_err]
                else
                    @assert stmt.head === :(=) && stmt.args[2] === expr
                    stmt.args[2] = throw_err
                    stmts = Any[stmt]
                    use.stmts[use.stmtidx] = stmts
                    lhs = stmt.args[1]
                    if isa(lhs, SSAValue) || isa(lhs, Slot)
                        add_def(ctx.infomap, lhs, ValueDef(stmt, stmts, 1))
                    end
                end
                ctx.changes[use.stmts=>use.stmtidx] = nothing
            end
        else
            @assert use_kind == 2
            # setfield!
            replace_var = vars[fld]
            val = expr.args[4]
            stmts = Any[]
            if !isassigned(si.defs, fld)
                flag_slot = flag_vars[fld]
                assign_flag_ex = :($flag_slot = true)
                push!(stmts, assign_flag_ex)
                add_def(ctx.infomap, flag_slot, ValueDef(assign_flag_ex, stmts, length(stmts)))
            end
            assign_ex = :($replace_var = $val)
            push!(stmts, assign_ex)
            scan_expr_use!(ctx.infomap, stmts, length(stmts), assign_ex, ctx.sv.src)
            stmt = use.stmts[use.stmtidx]::Expr
            if stmt !== expr
                @assert stmt.head === :(=) && stmt.args[2] === expr
                extra_assign_ex = :($(stmt.args[1]) = $val)
                push!(stmts, extra_assign_ex)
                scan_expr_use!(ctx.infomap, stmts, length(stmts), extra_assign_ex, ctx.sv.src)
            end
            use.stmts[use.stmtidx] = stmts
            ctx.changes[use.stmts=>use.stmtidx] = nothing
        end
    end
end

macro check_ast(ctx, ex)
    eex = esc(ex)
    ectx = esc(ctx)
    qex = QuoteNode(ex)
    quote
        ctx = $ectx
        if !$eex
            println("Check failed: ", $qex)
            println("Code:")
            println(ctx.sv.src)
            println("Value Info Map:")
            show_info(stdout, ctx.infomap, ctx)
            ccall(:abort, Union{}, ())
        end
    end
end

function verify_value_infomap(ctx::AllocOptContext)
    seen = IdDict()
    in_methods = IdDict()
    infomap = ctx.infomap
    all_stmts = IdDict()
    for i in 1:length(infomap.ssas)
        isassigned(infomap.ssas, i) || continue
        info = infomap.ssas[i]
        ssav = SSAValue(i - 1)
        @check_ast(ctx, !info.has_method)
        ndef = 0
        for def in info.defs
            check_valid(def, ctx.changes) || continue
            ndef += 1
            @check_ast(ctx, !haskey(seen, def))
            seen[def] = nothing
            all_stmts[def.stmts] = nothing

            assign = def.assign
            @check_ast(ctx, isa(assign, Expr))
            @check_ast(ctx, assign.head === :(=))
            @check_ast(ctx, def.stmts[def.stmtidx] === assign)
            @check_ast(ctx, assign.args[1] === ssav)
        end
        @check_ast(ctx, ndef <= 1)
        nuse = 0
        for use in info.uses
            check_valid(use, ctx.changes) || continue
            nuse += 1
            @check_ast(ctx, !haskey(seen, use))
            seen[use] = nothing
            all_stmts[use.stmts] = nothing
            stmt = use.stmts[use.stmtidx]
            if !isdefined(use, :expr)
                @check_ast(ctx, stmt === ssav)
            else
                expr = use.expr
                @check_ast(ctx, expr.args[use.exidx] === ssav)
                if stmt === expr
                    if expr.head === :(=)
                        @check_ast(ctx, use.exidx != 1)
                    end
                elseif expr.head === :&
                    @check_ast(ctx, use.exidx === 1)
                    ccall_expr = stmt::Expr
                    if ccall_expr.head === :(=)
                        ccall_expr = ccall_expr.args[2]::Expr
                    end
                    @check_ast(ctx, ccall_expr.head === :foreigncall)
                    nccallargs = ccall_expr.args[5]::Int
                    found_ccallarg = false
                    for argi in 6:(5 + nccallargs)
                        if ccall_expr.args[argi] === expr
                            found_ccallarg = true
                            break
                        end
                    end
                    @check_ast(ctx, found_ccallarg)
                else
                    @check_ast(ctx, stmt.head === :(=) && stmt.args[2] === expr)
                end
            end
        end
        if ndef == 0
            @check_ast(ctx, nuse == 0)
        end
    end
    for i in 1:length(infomap.slots)
        isassigned(infomap.slots, i) || continue
        info = infomap.slots[i]
        slotv = SlotNumber(i)
        if info.has_method
            in_methods[slotv] = nothing
        end
        ndef = 0
        for def in info.defs
            check_valid(def, ctx.changes) || continue
            @check_ast(ctx, !haskey(seen, def))
            seen[def] = nothing
            all_stmts[def.stmts] = nothing

            assign = def.assign
            @check_ast(ctx, def.stmts[def.stmtidx] === assign)
            if isa(assign, NewvarNode)
                @check_ast(ctx, var_has_undef(ctx.sv.src, i))
                @check_ast(ctx, assign.slot === slotv)
            else
                ndef += 1
                @check_ast(ctx, assign.head === :(=))
                @check_ast(ctx, assign.args[1] === slotv)
            end
        end
        if ctx.sv.src.slotflags[i] & SLOT_ASSIGNEDONCE != 0
            @check_ast(ctx, ndef <= 1)
        end
        for use in info.uses
            check_valid(use, ctx.changes) || continue
            @check_ast(ctx, !haskey(seen, use))
            seen[use] = nothing
            all_stmts[use.stmts] = nothing
            stmt = use.stmts[use.stmtidx]

            if !isdefined(use, :expr)
                @check_ast(ctx, symequal(stmt, slotv))
            else
                expr = use.expr
                @check_ast(ctx, symequal(expr.args[use.exidx], slotv))
                if stmt === expr
                    if expr.head === :(=)
                        @check_ast(ctx, use.exidx != 1)
                    end
                elseif expr.head === :&
                    @check_ast(ctx, use.exidx === 1)
                    ccall_expr = stmt::Expr
                    if ccall_expr.head === :(=)
                        ccall_expr = ccall_expr.args[2]::Expr
                    end
                    @check_ast(ctx, ccall_expr.head === :foreigncall)
                    nccallargs = ccall_expr.args[5]::Int
                    found_ccallarg = false
                    for argi in 6:(5 + nccallargs)
                        if ccall_expr.args[argi] === expr
                            found_ccallarg = true
                            break
                        end
                    end
                    @check_ast(ctx, found_ccallarg)
                else
                    @check_ast(ctx, stmt.head === :(=) && stmt.args[2] === expr)
                end
            end
        end
    end
    verify_value_infomap_rescan(ctx, ctx.sv.src.code, seen, in_methods, all_stmts)
    @check_ast(ctx, isempty(all_stmts))
    @check_ast(ctx, isempty(seen))
end

function verify_seen_info(ctx::AllocOptContext, seen, def_or_use)
    @check_ast(ctx, haskey(seen, def_or_use))
    delete!(seen, def_or_use)
end

function verify_value_infomap_rescan(ctx::AllocOptContext, stmts, seen, in_methods, all_stmts)
    if haskey(all_stmts, stmts)
        delete!(all_stmts, stmts)
    end
    for i in 1:length(stmts)
        ex = stmts[i]
        if isa(ex, Vector{Any})
            verify_value_infomap_rescan(ctx, ex, seen, in_methods, all_stmts)
            continue
        elseif isa(ex, SSAValue)
            verify_seen_info(ctx, seen, ValueUse(stmts, i))
            continue
        elseif isa(ex, Slot)
            verify_seen_info(ctx, seen, ValueUse(stmts, i))
            continue
        elseif isa(ex, NewvarNode)
            verify_seen_info(ctx, seen, ValueDef(ex, stmts, i))
            continue
        elseif !isa(ex, Expr)
            continue
        end
        head = ex.head
        is_meta_expr_head(head) && continue
        if head === :(=)
            rhs = ex.args[2]
            lhs = ex.args[1]
            lhs_slot = isa(lhs, Slot)
            rhs_slot = isa(rhs, Slot)
            if lhs_slot && rhs_slot
                @check_ast(ctx, !symequal(lhs, rhs))
            end
            if lhs_slot || isa(lhs, SSAValue)
                verify_seen_info(ctx, seen, ValueDef(ex, stmts, i))
            end
            if isa(rhs, Expr)
                ex = rhs
                head = ex.head
            else
                if rhs_slot || isa(rhs, SSAValue)
                    verify_seen_info(ctx, seen, ValueUse(stmts, i, ex, 2))
                end
                continue
            end
        end
        eargs = ex.args
        isccall = head === :foreigncall
        for j in 1:length(eargs)
            ea = eargs[j]
            if j == 1 && head === :method
                if isa(ea, Slot)
                    @check_ast(ctx, haskey(in_methods, SlotNumber(slot_id(ea))))
                    continue
                end
            end
            if isccall && isa(ea, Expr)
                @check_ast(ctx, ea.head === :& || ea.head === :boundscheck)
                ea2 = ea.args[1]
                if isa(ea2, Slot) || isa(ea2, SSAValue)
                    verify_seen_info(ctx, seen, ValueUse(stmts, i, ea, 1))
                end
            elseif isa(ea, Slot) || isa(ea, SSAValue)
                verify_seen_info(ctx, seen, ValueUse(stmts, i, ex, j))
            end
        end
    end
end

function optimize_value!(ctx::AllocOptContext, key)
    info = ctx.infomap[key]
    if info.has_method
        return
    end
    remove_invalid!(info, ctx.changes)
    if isempty(info.uses)
        delete_value!(ctx, info, key)
        return
    elseif isempty(info.defs)
        # Undefined but used variable these can only be variables whose uses are guarded by
        # conditions that will never be true. Ignore them for now.
        # TODO: slots introduced by inlining multiple-`return` functions might have their
        # defs removed by DCE but not be marked StaticUndef.
        #@assert ((!key.second && key.first <= ctx.sv.nargs) ||
        #         var_has_static_undef(ctx.sv.src, key.first, key.second))
        return
    elseif var_has_undef(ctx.sv.src, key.first, key.second)
        return
    end
    # Split assignments of leaftypes that do no have overlaping uses with each other
    split_disjoint_assign!(ctx, info, key) && return
    # If we've found SSAValue or Slot as def, no need to try other optimizations
    merge_value_ssa!(ctx, info, key) && return
    # Check if it's just a constant
    propagate_const_def!(ctx, info, key) && return
    # Split/eliminate allocation
    split_struct_alloc!(ctx, info, key) && return
    return
end



# Simplify the AST and eliminate unnecessary allocations
# This does the following optimizations iteratively until there's no changes to be made
# 1. Remove def of variables that is never used
# 2. Merge variables that have identical SSA definitions
# 3. Replace variables that always take the same constant definition with the constant
# 4. Deconstruct variables with defs that are only constant or inlined allocations
#    and with only known non-escape uses into its fields.
# This pass requires the IR to be linear, in particular, the only allowed nested `Expr` are
# RHS of a `Expr(:(=))` and the `Expr(:(&))` argument of a `ccall`.
function alloc_elim_pass!(sv::OptimizationState)
    body = sv.src.code
    infomap = collect_value_infos(body, sv.src, sv.nargs)
    ctx = AllocOptContext(infomap, sv)
    ENABLE_VERIFY_VALUEINFO[] && verify_value_infomap(ctx)
    while !isempty(ctx.todo)
        k, v = first(ctx.todo)
        k = k::Pair{Int,Bool}
        delete!(ctx.todo, k)
        optimize_value!(ctx, k)
        ENABLE_VERIFY_VALUEINFO[] && verify_value_infomap(ctx)
    end
    len = length(body)
    next_i = 1
    while next_i <= len
        i = next_i
        next_i += 1
        ex = body[i]
        if isa(ex, Vector{Any})
            len += length(ex) - 1
            next_i = i
            splice!(body, i, ex)
        elseif (isa(ex, Expr) && ex.head === :call && length(ex.args) == 2 &&
                ex.args[1] === throw_undefreferror_ifnot)
            len += 3
            next_i = i + 4
            flag_slot = ex.args[2]
            _growat!(body, i, 3)

            not_flag = newvar!(ctx.sv, Bool)
            not_flag_val = :($(GlobalRef(Core.Intrinsics, :not_int))($flag_slot))
            not_flag_val.typ = Bool
            body[i] = :($not_flag = $not_flag_val)

            after_lbl = genlabel(ctx.sv)
            body[i + 1] = Expr(:gotoifnot, not_flag, after_lbl.label)

            throw_err = Expr(:call, GlobalRef(Core, :throw), UndefRefError())
            throw_err.typ = Union{}
            body[i + 2] = throw_err
            body[i + 3] = after_lbl
        elseif (isa(ex, Expr) && ex.head === :call && length(ex.args) == 3 &&
                ex.args[1] === check_new_field_type)
            val_ex = ex.args[2]
            typ_ex = ex.args[3]

            len += 4
            next_i = i + 5
            _growat!(body, i, 4)

            flag = newvar!(ctx.sv, Bool)
            flag_val = :($(GlobalRef(Core, :isa))($val_ex, $typ_ex))
            flag_val.typ = Bool
            body[i] = :($flag = $flag_val)

            not_flag = newvar!(ctx.sv, Bool)
            not_flag_val = :($(GlobalRef(Core.Intrinsics, :not_int))($flag))
            not_flag_val.typ = Bool
            body[i + 1] = :($not_flag = $not_flag_val)

            after_lbl = genlabel(ctx.sv)
            body[i + 2] = Expr(:gotoifnot, not_flag, after_lbl.label)

            throw_err = Expr(:foreigncall, QuoteNode(:jl_type_error_new_expr),
                             Union{}, Core.svec(Any, Any), QuoteNode(:ccall), 2, typ_ex, val_ex)
            throw_err.typ = Union{}
            body[i + 3] = throw_err
            body[i + 4] = after_lbl
        end
    end
end


function copy_expr_in_array!(ary, seen)
    for i in 1:length(ary)
        ex = ary[i]
        isa(ex, Expr) || continue
        ex = ex::Expr
        if ex.head === :meta
            # Try to save some memory by using the same object for all `:pop_loc` meta node
            if ex !== META_POP_LOC && length(ex.args) == 1 && ex.args[1] === :pop_loc
                ary[i] = META_POP_LOC
            end
            continue # No need to copy meta expressions
        end
        if haskey(seen, ex)
            newex = Expr(ex.head)
            append!(newex.args, ex.args)
            newex.typ = ex.typ
            ary[i] = ex = newex
            # No need to add to `seen`, there's no way there can be another one of the copied
            # version in the AST....
        else
            seen[ex] = nothing
            if haskey(seen, ex.args)
                # Haven't actually seen this happen but it's pretty easy to check
                ex.args = copy(ex.args)
            else
                seen[ex.args] = nothing
            end
        end
        copy_expr_in_array!(ex.args, seen)
    end
end

# Clone expressions that appears multiple times in the code
function copy_duplicated_expr_pass!(sv::OptimizationState)
    copy_expr_in_array!(sv.src.code, IdDict())
end

# fix label numbers to always equal the statement index of the label
function reindex_labels!(sv::OptimizationState)
    body = sv.src.code
    mapping = get_label_map(body)
    for i = 1:length(body)
        el = body[i]
        # For goto and enter, the statement and the target has to be
        # both reachable or both not.
        if isa(el, LabelNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = LabelNode(labelnum)
        elseif isa(el, GotoNode)
            labelnum = mapping[el.label]
            @assert labelnum !== 0
            body[i] = GotoNode(labelnum)
        elseif isa(el, Expr)
            if el.head === :gotoifnot
                labelnum = mapping[el.args[2]::Int]
                @assert labelnum !== 0
                el.args[2] = labelnum
            elseif el.head === :enter
                labelnum = mapping[el.args[1]::Int]
                @assert labelnum !== 0
                el.args[1] = labelnum
            end
        end
    end
end

function return_type(@nospecialize(f), @nospecialize(t))
    params = Params(ccall(:jl_get_tls_world_age, UInt, ()))
    rt = Union{}
    if isa(f, Builtin)
        rt = builtin_tfunction(f, Any[t.parameters...], nothing, params)
        if isa(rt, TypeVar)
            rt = rt.ub
        else
            rt = widenconst(rt)
        end
    else
        for m in _methods(f, t, -1, params.world)
            ty = typeinf_type(m[3], m[1], m[2], true, params)
            ty === nothing && return Any
            rt = tmerge(rt, ty)
            rt === Any && break
        end
    end
    return rt
end
