# This file is a part of Julia. License is MIT: https://julialang.org/license

###########
# generic #
###########

# avoid cycle due to over-specializing `any` when used by inference
function _any(@nospecialize(f), a)
    for x in a
        f(x) && return true
    end
    return false
end

function contains_is(itr, @nospecialize(x))
    for y in itr
        if y === x
            return true
        end
    end
    return false
end

anymap(f::Function, a::Array{Any,1}) = Any[ f(a[i]) for i in 1:length(a) ]

###########
# scoping #
###########

_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(topmod, @nospecialize(f), sym)
    if isdefined(Main, :Base) && isdefined(Main.Base, sym) && isconst(Main.Base, sym) && f === getfield(Main.Base, sym)
        return true
    elseif isdefined(topmod, sym) && isconst(topmod, sym) && f === getfield(topmod, sym)
        return true
    end
    return false
end

#######
# AST #
#######

# Meta expression head, these generally can't be deleted even when they are
# in a dead branch but can be ignored when analyzing uses/liveness.
is_meta_expr_head(head::Symbol) = (head === :inbounds || head === :boundscheck || head === :meta || head === :simdloop)
is_meta_expr(ex::Expr) = is_meta_expr_head(ex.head)

sym_isless(a::Symbol, b::Symbol) = ccall(:strcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}), a, b) < 0

function is_self_quoting(@nospecialize(x))
    return isa(x,Number) || isa(x,AbstractString) || isa(x,Tuple) || isa(x,Type) ||
        isa(x,Char) || x === nothing || isa(x,Function)
end

function quoted(@nospecialize(x))
    return is_self_quoting(x) ? x : QuoteNode(x)
end

function is_inlineable_constant(@nospecialize(x))
    x isa Type && return true
    return isbits(x) && Core.sizeof(x) <= MAX_INLINE_CONST_SIZE
end

# count occurrences up to n+1
function occurs_more(@nospecialize(e), pred, n)
    if isa(e,Expr)
        e = e::Expr
        head = e.head
        is_meta_expr_head(head) && return 0
        c = 0
        for a = e.args
            c += occurs_more(a, pred, n)
            if c>n
                return c
            end
        end
        return c
    end
    if pred(e)
        return 1
    end
    return 0
end

###########################
# MethodInstance/CodeInfo #
###########################

function get_staged(li::MethodInstance)
    try
        # user code might throw errors – ignore them
        return ccall(:jl_code_for_staged, Any, (Any,), li)::CodeInfo
    catch
        return nothing
    end
end

# create copies of the CodeInfo definition, and any fields that type-inference might modify
function copy_code_info(c::CodeInfo)
    cnew = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), c)
    cnew.code = copy_exprargs(cnew.code)
    cnew.slotnames = copy(cnew.slotnames)
    cnew.slotflags = copy(cnew.slotflags)
    return cnew
end

function retrieve_code_info(linfo::MethodInstance)
    m = linfo.def::Method
    if isdefined(m, :generator)
        # user code might throw errors – ignore them
        return get_staged(linfo)
    else
        # TODO: post-inference see if we can swap back to the original arrays?
        if isa(m.source, Array{UInt8,1})
            c = ccall(:jl_uncompress_ast, Any, (Any, Any), m, m.source)
        else
            c = copy_code_info(m.source)
        end
    end
    return c
end

function code_for_method(method::Method, @nospecialize(atypes), sparams::SimpleVector, world::UInt, preexisting::Bool=false)
    if world < min_world(method)
        return nothing
    end
    if isdefined(method, :generator) && !isdispatchtuple(atypes)
        # don't call staged functions on abstract types.
        # (see issues #8504, #10230)
        # we can't guarantee that their type behavior is monotonic.
        return nothing
    end
    if preexisting
        if method.specializations !== nothing
            # check cached specializations
            # for an existing result stored there
            return ccall(:jl_specializations_lookup, Any, (Any, Any, UInt), method, atypes, world)
        end
        return nothing
    end
    return ccall(:jl_specializations_get_linfo, Ref{MethodInstance}, (Any, Any, Any, UInt), method, atypes, sparams, world)
end

# TODO: Use these functions instead of directly manipulating
# the "actual" method for appropriate places in inference (see #24676)
function method_for_inference_heuristics(cinfo, default)
    if isa(cinfo, CodeInfo)
        # appropriate format for `sig` is svec(ftype, argtypes, world)
        sig = cinfo.signature_for_inference_heuristics
        if isa(sig, SimpleVector) && length(sig) == 3
            methods = _methods(sig[1], sig[2], -1, sig[3])
            if length(methods) == 1
                _, _, m = methods[]
                if isa(m, Method)
                    return m
                end
            end
        end
    end
    return default
end

function method_for_inference_heuristics(method::Method, @nospecialize(sig), sparams, world)
    if isdefined(method, :generator) && method.generator.expand_early
        method_instance = code_for_method(method, sig, sparams, world, false)
        if isa(method_instance, MethodInstance)
            return method_for_inference_heuristics(get_staged(method_instance), method)
        end
    end
    return method
end

function exprtype(@nospecialize(x), src, mod::Module)
    if isa(x, Expr)
        return (x::Expr).typ
    elseif isa(x, SlotNumber)
        return src.slottypes[(x::SlotNumber).id]
    elseif isa(x, TypedSlot)
        return (x::TypedSlot).typ
    elseif isa(x, SSAValue)
        return abstract_eval_ssavalue(x::SSAValue, src)
    elseif isa(x, Argument)
        return isa(src, IncrementalCompact) ? src.ir.argtypes[x.n] : src.argtypes[x.n]
    elseif isa(x, Symbol)
        return abstract_eval_global(mod, x::Symbol)
    elseif isa(x, QuoteNode)
        return AbstractEvalConstant((x::QuoteNode).value)
    elseif isa(x, GlobalRef)
        return abstract_eval_global(x.mod, (x::GlobalRef).name)
    elseif isa(x, PhiNode)
        return Any
    elseif isa(x, PiNode)
        return x.typ
    else
        return AbstractEvalConstant(x)
    end
end

###################
# SSAValues/Slots #
###################

function find_ssavalue_uses(body::Vector{Any}, nvals::Int)
    uses = BitSet[ BitSet() for i = 1:nvals ]
    for line in 1:length(body)
        e = body[line]
        isa(e, Expr) && find_ssavalue_uses(e, uses, line)
    end
    return uses
end

function find_ssavalue_uses(e::Expr, uses::Vector{BitSet}, line::Int)
    head = e.head
    is_meta_expr_head(head) && return
    skiparg = (head === :(=))
    for a in e.args
        if skiparg
            skiparg = false
        elseif isa(a, SSAValue)
            push!(uses[a.id + 1], line)
        elseif isa(a, Expr)
            find_ssavalue_uses(a, uses, line)
        end
    end
end

function find_ssavalue_defs(body::Vector{Any}, nvals::Int)
    defs = zeros(Int, nvals)
    for line in 1:length(body)
        e = body[line]
        if isa(e, Expr) && e.head === :(=)
            lhs = e.args[1]
            if isa(lhs, SSAValue)
                defs[lhs.id + 1] = line
            end
        end
    end
    return defs
end

# using a function to ensure we can infer this
@inline slot_id(s) = isa(s, SlotNumber) ? (s::SlotNumber).id : (s::TypedSlot).id

##############
# LabelNodes #
##############

# scan body for the value of the largest referenced label
# so that we won't accidentally re-use it
function label_counter(body::Vector{Any}, comefrom=true)
    l = 0
    for b in body
        label = 0
        if isa(b, LabelNode) && comefrom
            label = b.label::Int
        elseif isa(b, GotoNode)
            label = b.label::Int
        elseif isa(b, Expr)
            if b.head == :gotoifnot
                label = b.args[2]::Int
            elseif b.head == :enter
                label = b.args[1]::Int
            elseif b.head === :(=) && comefrom
                rhs = b.args[2]
                if isa(rhs, PhiNode)
                    for edge in rhs.edges
                        edge = edge::Int + 1
                        if edge > l
                            l = edge
                        end
                    end
                end
            end
        end
        if label > l
            l = label
        end
    end
    return l
end

function get_label_map(body::Vector{Any})
    nlabels = label_counter(body)
    labelmap = zeros(Int, nlabels)
    for i = 1:length(body)
        el = body[i]
        if isa(el, LabelNode)
            # @assert labelmap[el.label] == 0
            labelmap[el.label] = i
        end
    end
    return labelmap
end
