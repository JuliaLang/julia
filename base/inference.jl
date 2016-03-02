# This file is a part of Julia. License is MIT: http://julialang.org/license

# parameters limiting potentially-infinite types
const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 7
const MAX_TUPLETYPE_LEN  = 42
const MAX_TUPLE_DEPTH = 4

# avoid cycle due to over-specializing `any` when used by inference
function _any(f::ANY, a)
    for x in a
        f(x) && return true
    end
    return false
end

immutable NotFound
end

const NF = NotFound()

type VarInfo
    sp::SimpleVector     # static parameters
    vars::Array{Any,1}   # names of args and locals
    gensym_types::Array{Any,1} # types of the GenSym's in this function
    vinfo::Array{Any,1}  # variable properties
    label_counter::Int   # index of the current highest label for this function
    fedbackvars::ObjectIdDict
    mod::Module
    linfo::LambdaInfo
end

function VarInfo(linfo::LambdaInfo, ast=linfo.ast)
    if !isa(ast,Expr)
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, ast)
    end
    vinflist = ast.args[2][1]::Array{Any,1}
    vars = map(vi->vi[1], vinflist)
    body = (ast.args[3].args)::Array{Any,1}
    ngs = ast.args[2][3]
    if !isa(ngs,Int)
        ngs = length(ngs::Array)
    end
    gensym_types = Any[ NF for i = 1:(ngs::Int) ]
    nl = label_counter(body)+1
    sp = linfo.sparam_vals
    VarInfo(sp, vars, gensym_types, vinflist, nl, ObjectIdDict(), linfo.module, linfo)
end

type VarState
    typ
    undef::Bool
end

type EmptyCallStack
end

type CallStack
    ast
    types::Type
    recurred::Bool
    cycleid::Int
    result
    prev::Union{EmptyCallStack,CallStack}
    sv::VarInfo

    CallStack(ast, types::ANY, prev) = new(ast, types, false, 0, Bottom, prev)
end

inference_stack = EmptyCallStack()

function is_static_parameter(sv::VarInfo, s::Symbol)
    sp = sv.linfo.sparam_syms
    for i=1:length(sp)
        if is(sp[i],s)
            return true
        end
    end
    return false
end

function contains_is(itr, x::ANY)
    for y in itr
        if is(y,x)
            return true
        end
    end
    return false
end

is_local(sv::VarInfo, s::GenSym) = true
is_local(sv::VarInfo, s::Symbol) = contains_is(sv.vars, s)
is_global(sv::VarInfo, s::Symbol) = !is_local(sv,s) && !is_static_parameter(sv,s)

function _iisconst(s::Symbol, sv)
    m = sv.mod
    isdefined(m,s) && (ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0)
end

_ieval(x::ANY, sv) =
    ccall(:jl_interpret_toplevel_expr_in, Any, (Any, Any, Any, Any),
          sv.mod, x, svec(), svec())

_topmod(sv::VarInfo) = _topmod(sv.mod)
_topmod(m::Module) = ccall(:jl_base_relative_to, Any, (Any,), m)::Module

function istopfunction(topmod, f::ANY, sym)
    if isdefined(Main, :Base) && isdefined(Main.Base, sym) && f === getfield(Main.Base, sym)
        return true
    elseif isdefined(topmod, sym) && f === getfield(topmod, sym)
        return true
    end
    return false
end

isknownlength(t::DataType) = !isvatuple(t) && !(t.name===NTuple.name && !isa(t.parameters[1],Int))

cmp_tfunc = (x,y)->Bool

isType(t::ANY) = isa(t,DataType) && is((t::DataType).name,Type.name)

const IInf = typemax(Int) # integer infinity
const n_ifunc = reinterpret(Int32,arraylen)+1
const t_ifunc = Array{Tuple{Int,Int,Any},1}(n_ifunc)
const t_ffunc_key = Array{Function,1}(0)
const t_ffunc_val = Array{Tuple{Int,Int,Any},1}(0)
function add_tfunc(f::IntrinsicFunction, minarg::Int, maxarg::Int, tfunc::ANY)
    t_ifunc[reinterpret(Int32,f)+1] = (minarg, maxarg, tfunc)
end
function add_tfunc(f::Function, minarg::Int, maxarg::Int, tfunc::ANY)
    push!(t_ffunc_key, f)
    push!(t_ffunc_val, (minarg, maxarg, tfunc))
end
add_tfunc(throw, 1, 1, x->Bottom)
add_tfunc(box, 2, 2, (t,v)->(isType(t) ? t.parameters[1] : Any))
add_tfunc(eq_int, 2, 2, cmp_tfunc)
add_tfunc(ne_int, 2, 2, cmp_tfunc)
add_tfunc(slt_int, 2, 2, cmp_tfunc)
add_tfunc(ult_int, 2, 2, cmp_tfunc)
add_tfunc(sle_int, 2, 2, cmp_tfunc)
add_tfunc(ule_int, 2, 2, cmp_tfunc)
add_tfunc(eq_float, 2, 2, cmp_tfunc)
add_tfunc(ne_float, 2, 2, cmp_tfunc)
add_tfunc(lt_float, 2, 2, cmp_tfunc)
add_tfunc(le_float, 2, 2, cmp_tfunc)
add_tfunc(fpiseq, 2, 2, cmp_tfunc)
add_tfunc(fpislt, 2, 2, cmp_tfunc)
add_tfunc(nan_dom_err, 2, 2, (a, b)->a)
add_tfunc(getfield(Core.Intrinsics,:ccall), 3, IInf,
    function(fptr, rt, at, a...)
        if !isType(rt)
            return Any
        end
        t = rt.parameters[1]
        if isa(t,DataType) && is((t::DataType).name,Ref.name)
            t = t.parameters[1]
            if is(t,Any)
                return Union{} # a return type of Box{Any} is invalid
            end
            return t
        end
        return t
    end)
add_tfunc(eval(Core.Intrinsics,:llvmcall), 3, IInf,
    (fptr, rt, at, a...)->(isType(rt) ? rt.parameters[1] : Any))
add_tfunc(eval(Core.Intrinsics,:cglobal), 1, 2,
    (fptr, t...)->(isempty(t) ? Ptr{Void} :
                   isType(t[1]) ? Ptr{t[1].parameters[1]} : Ptr))
add_tfunc(eval(Core.Intrinsics,:select_value), 3, 3,
    # TODO: return Bottom if cnd is definitely not a Bool
    (cnd, x, y)->Union{x,y})
add_tfunc(eval(Core.Intrinsics,:arraylen), 1, 1, x->Int)
add_tfunc(is, 2, 2, cmp_tfunc)
add_tfunc(issubtype, 2, 2, cmp_tfunc)
add_tfunc(isa, 2, 2, cmp_tfunc)
add_tfunc(isdefined, 1, IInf, (args...)->Bool)
add_tfunc(Core.sizeof, 1, 1, x->Int)
add_tfunc(nfields, 1, 1, x->Int)
add_tfunc(_expr, 1, IInf, (args...)->Expr)
add_tfunc(applicable, 1, IInf, (f, args...)->Bool)
#add_tfunc(arrayref, 2,IInf,(a,i...)->(isa(a,DataType) && a<:Array ?
#                                     a.parameters[1] : Any))
#add_tfunc(arrayset, 3, IInf, (a,v,i...)->a)
add_tfunc(arraysize, 2, 2, (a,d)->Int)
add_tfunc(pointerref, 2, 2, (a,i)->(isa(a,DataType) && a<:Ptr && isa(a.parameters[1],Union{Type,TypeVar}) ? a.parameters[1] : Any))
add_tfunc(pointerset, 3, 3, (a,v,i)->a)

const typeof_tfunc = function (t::ANY)
    if isType(t)
        t = t.parameters[1]
        if isa(t,TypeVar)
            DataType
        else
            Type{typeof(t)}
        end
    elseif isa(t,DataType)
        if isleaftype(t)
            Type{t}
        elseif t === Any
            DataType
        else
            Type{TypeVar(:_,t)}
        end
    elseif isa(t,Union)
        Union{map(typeof_tfunc, t.types)...}
    elseif isa(t,TypeVar) && !(Any <: t.ub)
        Type{t}
    else
        DataType
    end
end
add_tfunc(typeof, 1, 1, typeof_tfunc)
# involving constants: typeassert, getfield, fieldtype, apply_type
# therefore they get their arguments unevaluated
add_tfunc(typeassert, 2, 2,
    (A, v, t)->(isType(t) ? typeintersect(v,t.parameters[1]) : Any))

function type_depth(t::ANY, d::Int=0)
    if isa(t,Union)
        t === Bottom && return d
        return maximum(x->type_depth(x, d+1), t.types)
    elseif isa(t,DataType)
        P = t.parameters
        isempty(P) && return d
        return maximum(x->type_depth(x, d+1), P)
    end
    return d
end

function limit_type_depth(t::ANY, d::Int, cov::Bool, vars)
    if isa(t,TypeVar) || isa(t,TypeConstructor)
        return t
    end
    inexact = !cov && d > MAX_TYPE_DEPTH
    if isa(t,Union)
        t === Bottom && return t
        if d > MAX_TYPE_DEPTH
            R = Any
        else
            R = Union{map(x->limit_type_depth(x, d+1, cov, vars), t.types)...}
        end
    elseif isa(t,DataType)
        P = t.parameters
        isempty(P) && return t
        if d > MAX_TYPE_DEPTH
            R = t.name.primary
        else
            stillcov = cov && (t.name === Tuple.name)
            Q = map(x->limit_type_depth(x, d+1, stillcov, vars), P)
            if !cov && _any(p->contains_is(vars,p), Q)
                R = t.name.primary
                inexact = true
            else
                R = t.name.primary{Q...}
            end
        end
    else
        return t
    end
    if inexact
        R = TypeVar(:_,R)
        push!(vars, R)
    end
    return R
end

const getfield_tfunc = function (A, s0::ANY, name)
    s = s0
    if isType(s)
        s = typeof(s.parameters[1])
        if s === TypeVar
            return Any, false
        end
    end
    if isa(s,Union)
        return reduce(tmerge, Bottom, map(t->getfield_tfunc(A, t, name)[1], s.types)), false
    end
    if !isa(s,DataType)
        return Any, false
    end
    if is(s.name,NTuple.name)
        return (name == Symbol ? Bottom : s.parameters[2]), true
    end
    if s.abstract
        return Any, false
    end
    if s <: Tuple && name === Symbol
        return Bottom, true
    end
    haveargs = A !== nothing && length(A)>1
    if haveargs && isa(A[2],QuoteNode) && isa(A[2].value,Symbol)
        fld = A[2].value
        A1 = A[1]
        if isa(A1,Module) && isdefined(A1,fld) && isconst(A1, fld)
            return abstract_eval_constant(eval(A1,fld)), true
        end
        if s === Module
            return Any, false
        end
        if isType(s0)
            sp = s0.parameters[1]
            if isa(sp,DataType)
                # TODO
                #if fld === :parameters
                #    return Type{sp.parameters}, true
                #end
                #if fld === :types
                #    return Type{sp.types}, true
                #end
                if fld === :super
                    return Type{sp.super}, isleaftype(s)
                end
            end
        end
        snames = s.name.names
        for i=1:length(snames)
            if is(snames[i],fld)
                R = s.types[i]
                if isempty(s.parameters)
                    return R, true
                else
                    typ = limit_type_depth(R, 0, true,
                                           filter!(x->isa(x,TypeVar), Any[s.parameters...]))
                    return typ, isleaftype(s) && typeseq(typ, R)
                end
            end
        end
        return Bottom, true
    elseif haveargs && isa(A[2],Int)
        if isa(A[1],Module) || s === Module
            return Bottom, true
        end
        i::Int = A[2]
        nf = s.types.length
        if isvatuple(s) && i >= nf
            return s.types[nf].parameters[1], false
        end
        if i < 1 || i > nf
            return Bottom, true
        end
        return s.types[i], false
    else
        return reduce(tmerge, Bottom, map(unwrapva,s.types)) #=Union{s.types...}=#, false
    end
end
add_tfunc(getfield, 2, 2, (A,s,name)->getfield_tfunc(A,s,name)[1])
add_tfunc(setfield!, 3, 3, (o, f, v)->v)
const fieldtype_tfunc = function (A, s::ANY, name)
    if isType(s)
        s = s.parameters[1]
    else
        return Type
    end
    t, exact = getfield_tfunc(A, s, name)
    if is(t,Bottom)
        return t
    end
    Type{exact || isleaftype(t) || isa(t,TypeVar) ? t : TypeVar(:_, t)}
end
add_tfunc(fieldtype, 2, 2, fieldtype_tfunc)

function valid_tparam(x::ANY)
    if isa(x,Tuple)
        for t in x
            !valid_tparam(t) && return false
        end
        return true
    end
    return isa(x,Int) || isa(x,Symbol) || isa(x,Bool) || (!isa(x,Type) && isbits(x))
end

function extract_simple_tparam(Ai)
    if !isa(Ai,Symbol) && !isa(Ai,GenSym) && valid_tparam(Ai)
        return Ai
    elseif isa(Ai,QuoteNode) && valid_tparam(Ai.value)
        return Ai.value
    elseif isa(inference_stack,CallStack) && isa(Ai,Expr) &&
            is_known_call(Ai,tuple,inference_stack.sv)
        tup = ()
        for arg in Ai.args[2:end]
            val = extract_simple_tparam(arg)
            if val === Bottom
                return val
            end
            tup = tuple(tup...,val)
        end
        return tup
    end
    return Bottom
end

has_typevars(t::ANY, all=false) = ccall(:jl_has_typevars_, Cint, (Any,Cint), t, all)!=0

# TODO: handle e.g. apply_type(T, R::Union{Type{Int32},Type{Float64}})
const apply_type_tfunc = function (A::ANY, args...)
    if !isType(args[1])
        return Any
    end
    headtype = args[1].parameters[1]
    if isa(headtype,Union) || isa(headtype,TypeVar)
        return args[1]
    end
    largs = length(args)
    if headtype === Union
        largs == 1 && return Type{Bottom}
        largs == 2 && return args[2]
        args = args[2:end]
        if all(isType, args)
            try
                return Type{Union{map(t->t.parameters[1],args)...}}
            catch
                return Any
            end
        else
            return Any
        end
    elseif isa(headtype, Union)
        return Any
    end
    istuple = (headtype === Tuple)
    uncertain = false
    lA = length(A)
    tparams = Any[]
    for i=2:max(lA,largs)
        ai = args[i]
        if isType(ai)
            aip1 = ai.parameters[1]
            uncertain |= has_typevars(aip1)
            push!(tparams, aip1)
        else
            if i<=lA
                val = extract_simple_tparam(A[i])
                if val !== Bottom
                    push!(tparams, val)
                    continue
                elseif isa(inference_stack,CallStack) && isa(A[i],Symbol)
                    sp = inference_stack.sv.linfo.sparam_syms
                    s = A[i]
                    found = false
                    for j=1:length(sp)
                        if is(sp[j],s)
                            # static parameter
                            val = inference_stack.sv.sp[j]
                            if valid_tparam(val)
                                push!(tparams, val)
                                found = true
                                break
                            end
                        end
                    end
                    if found
                        continue
                    end
                end
            end
            if !istuple && i-1 > length(headtype.parameters)
                # too many parameters for type
                return Bottom
            end
            uncertain = true
            if istuple
                push!(tparams, Any)
            else
                push!(tparams, headtype.parameters[i-1])
            end
        end
    end
    local appl
    # good, all arguments understood
    try
        appl = apply_type(headtype, tparams...)
    catch
        # type instantiation might fail if one of the type parameters
        # doesn't match, which could happen if a type estimate is too coarse
        appl = headtype
        uncertain = true
    end
    !uncertain && return Type{appl}
    if type_too_complex(appl,0)
        return Type{TypeVar(:_,headtype)}
    end
    !isa(appl,TypeVar) ? Type{TypeVar(:_,appl)} : Type{appl}
end
add_tfunc(apply_type, 1, IInf, apply_type_tfunc)

function tuple_tfunc(argtype::ANY)
    if isa(argtype,DataType) && argtype.name === Tuple.name
        p = map(x->(isType(x) && !isa(x.parameters[1],TypeVar) ? typeof(x.parameters[1]) : x),
                argtype.parameters)
        return Tuple{p...}
    end
    argtype
end

function builtin_tfunction(f::ANY, args::ANY, argtype::ANY)
    isva = isvatuple(argtype)
    argtypes = argtype.parameters
    if is(f,tuple)
        return tuple_tfunc(limit_tuple_depth(argtype))
    elseif is(f,svec)
        return SimpleVector
    elseif is(f,arrayset)
        if length(argtypes) < 3 && !isva
            return Bottom
        end
        a1 = argtypes[1]
        if isvarargtype(a1)
            return a1.parameters[1]
        end
        return a1
    elseif is(f,arrayref)
        if length(argtypes) < 2 && !isva
            return Bottom
        end
        a = argtypes[1]
        return (isa(a,DataType) && a<:Array && isa(a.parameters[1],Union{Type,TypeVar}) ?
                a.parameters[1] : Any)
    elseif is(f,Expr)
        if length(argtypes) < 1 && !isva
            return Bottom
        end
        return Expr
    end
    if isa(f, IntrinsicFunction)
        iidx = Int(reinterpret(Int32, f::IntrinsicFunction))+1
        if !isdefined(t_ifunc, iidx)
            # unknown/unhandled intrinsic (most fall in this category since most return an unboxed value)
            return Any
        end
        tf = t_ifunc[iidx]
    else
        fidx = findfirst(t_ffunc_key, f::Function)
        if fidx == 0
            # unknown/unhandled builtin or anonymous function
            return Any
        end
        tf = t_ffunc_val[fidx]
    end
    tf = tf::Tuple{Real, Real, Any}
    if isva
        # only some t-funcs can handle varargs  (TODO)
        #if !is(f, apply_type)
        return Any
        #end
    elseif !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    if is(f,typeassert) || is(f,getfield) || is(f,apply_type) || is(f,fieldtype)
        # TODO: case of apply(), where we do not have the args
        return tf[3](args, argtypes...)
    end
    return tf[3](argtypes...)
end

function isconstantref(f::ANY, sv::VarInfo)
    if isa(f,TopNode)
        m = _topmod(sv)
        return isconst(m, f.name) && isdefined(m, f.name) && f
    end
    if isa(f,GlobalRef)
        M = f.mod; s = f.name
        return isdefined(M,s) && isconst(M,s) && f
    end
    if isa(f,Expr)
        if is(f.head,:call)
            if length(f.args) == 3 && isa(f.args[1], TopNode) &&
                is(f.args[1].name,:getfield) && isa(f.args[3],QuoteNode)
                s = f.args[3].value
                if isa(f.args[2],Module)
                    M = f.args[2]
                else
                    M = isconstantref(f.args[2], sv)
                    if M === false
                        return false
                    end
                    M = _ieval(M, sv)
                    if !isa(M,Module)
                        return false
                    end
                end
                return isdefined(M,s) && isconst(M,s) && f
            end
        elseif is(f.head,:inert)
            return f
        end
        return false
    end
    if isa(f,QuoteNode)
        return f
    elseif isa(f,GenSym)
        return false
    elseif isa(f,SymbolNode)
        f = f.name
    end
    if isa(f,Symbol)
        return is_global(sv, f) && _iisconst(f, sv) && f
    elseif !isa(f,Expr)
        return f
    end
    return false
end

const isconstantfunc = isconstantref

const limit_tuple_depth = t->limit_tuple_depth_(t,0)

const limit_tuple_depth_ = function (t::ANY,d::Int)
    if isa(t,Union)
        # also limit within Union types.
        # may have to recur into other stuff in the future too.
        return Union{map(x->limit_tuple_depth_(x,d+1), t.types)...}
    end
    if isa(t,TypeVar)
        return limit_tuple_depth_(t.ub, d)
    end
    if !(isa(t,DataType) && t.name === Tuple.name)
        return t
    end
    if d > MAX_TUPLE_DEPTH
        return Tuple
    end
    p = map(x->limit_tuple_depth_(x,d+1), t.parameters)
    Tuple{p...}
end

limit_tuple_type = (t::ANY) -> limit_tuple_type_n(t, MAX_TUPLETYPE_LEN)

const limit_tuple_type_n = function (t::ANY, lim::Int)
    p = t.parameters
    n = length(p)
    if n > lim
        tail = reduce(typejoin, Bottom, Any[p[lim:(n-1)]..., unwrapva(p[n])])
        return Tuple{p[1:(lim-1)]..., Vararg{tail}}
    end
    return t
end

let stagedcache=Dict{Any,Any}()
    global func_for_method
    function func_for_method(m::Method, tt, env)
        if !m.isstaged
            return m.func
        elseif haskey(stagedcache, (m, tt))
            return stagedcache[(m, tt)]
        else
            if !isleaftype(tt)
                # don't call staged functions on abstract types.
                # (see issues #8504, #10230)
                # we can't guarantee that their type behavior is monotonic.
                return NF
            end
            f = ccall(:jl_instantiate_staged, Any, (Any, Any, Any), m.func, tt, env)
            stagedcache[(m, tt)] = f
            return f
        end
    end
end

function abstract_call_gf(f::ANY, fargs, argtype::ANY, e)
    argtypes = argtype.parameters
    tm = _topmod((inference_stack::CallStack).sv)  # TODO pass in sv instead
    if length(argtypes)>2 && argtypes[3]===Int &&
        (argtypes[2] <: Tuple ||
         (isa(argtypes[2], DataType) && isdefined(Main, :Base) && isdefined(Main.Base, :Pair) &&
          (argtypes[2]::DataType).name === Main.Base.Pair.name))
        # allow tuple indexing functions to take advantage of constant
        # index arguments.
        if istopfunction(tm, f, :getindex)
            return getfield_tfunc(fargs, argtypes[2], argtypes[3])[1]
        elseif istopfunction(tm, f, :next)
            t1 = getfield_tfunc(fargs, argtypes[2], argtypes[3])[1]
            return t1===Bottom ? Bottom : Tuple{t1, Int}
        elseif istopfunction(tm, f, :indexed_next)
            t1 = getfield_tfunc(fargs, argtypes[2], argtypes[3])[1]
            return t1===Bottom ? Bottom : Tuple{t1, Int}
        end
    end
    if istopfunction(tm, f, :promote_type) || istopfunction(tm, f, :typejoin)
        return Type
    end
    return abstract_call_gf_by_type(f, argtype, e)
end

function abstract_call_gf_by_type(f::ANY, argtype::ANY, e)
    tm = _topmod((inference_stack::CallStack).sv)  # TODO pass in sv instead
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    # here I picked 4.
    argtype = limit_tuple_type(argtype)
    argtypes = argtype.parameters
    applicable = _methods_by_ftype(argtype, 4)
    rettype = Bottom
    if is(applicable,false)
        # this means too many methods matched
        isa(e,Expr) && (e.head = :call)
        return Any
    end
    x::Array{Any,1} = applicable
    if isempty(x)
        # no methods match
        # TODO: it would be nice to return Bottom here, but during bootstrap we
        # often compile code that calls methods not defined yet, so it is much
        # safer just to fall back on dynamic dispatch.
        return Any
    end
    for (m::SimpleVector) in x
        sig = m[1]
        local linfo
        linfo = try
            func_for_method(m[3],sig,m[2])
        catch
            NF
        end
        if linfo === NF
            rettype = Any
            break
        end
        linfo = linfo::LambdaInfo
        lsig = length(m[3].sig.parameters)
        # limit argument type tuple based on size of definition signature.
        # for example, given function f(T, Any...), limit to 3 arguments
        # instead of the default (MAX_TUPLETYPE_LEN)
        sp = inference_stack
        limit = false
        # look at the stack to detect recursive calls with growing argument lists
        while sp !== EmptyCallStack()
            if linfo.ast === sp.ast && length(argtypes) > length(sp.types.parameters)
                limit = true; break
            end
            sp = sp.prev
        end
        ls = length(sig.parameters)
        if limit && ls > lsig+1
            if !istopfunction(tm, f, :promote_typeof)
                fst = sig.parameters[lsig+1]
                allsame = true
                # allow specializing on longer arglists if all the trailing
                # arguments are the same, since there is no exponential
                # blowup in this case.
                for i = lsig+2:ls
                    if sig.parameters[i] != fst
                        allsame = false
                        break
                    end
                end
                if !allsame
                    sig = limit_tuple_type_n(sig, lsig+1)
                end
            end
        end
        #print(m,"\n")
        (_tree,rt) = typeinf(linfo, sig, m[2], linfo)
        rettype = tmerge(rettype, rt)
        if is(rettype,Any)
            break
        end
    end
    # if rettype is Bottom we've found a method not found error
    #print("=> ", rettype, "\n")
    return rettype
end

function invoke_tfunc(f::ANY, types::ANY, argtype::ANY)
    argtype = typeintersect(types,limit_tuple_type(argtype))
    if is(argtype,Bottom)
        return Bottom
    end
    ft = type_typeof(f)
    types = Tuple{ft, types.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    meth = ccall(:jl_gf_invoke_lookup, Any, (Any,), types)
    if is(meth, nothing)
        return Any
    end
    (ti, env) = ccall(:jl_match_method, Any, (Any, Any, Any),
                      argtype, meth.sig, meth.tvars)::SimpleVector
    linfo = try
        func_for_method(meth, types, env)
    catch
        NF
    end
    if linfo === NF
        return Any
    end
    return typeinf(linfo::LambdaInfo, ti, env, linfo)[2]
end

# `types` is an array of inferred types for expressions in `args`.
# if an expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types. returns an array of
# arrays of types, or `nothing`.
function precise_container_types(args, types, vtypes, sv)
    n = length(args)
    assert(n == length(types))
    result = cell(n)
    for i = 1:n
        ai = args[i]; ti = types[i]
        if isa(ai,Expr) && (is_known_call(ai, svec, sv) || is_known_call(ai, tuple, sv))
            aa = ai.args
            result[i] = Any[ (isa(aa[j],Expr) ? aa[j].typ : abstract_eval(aa[j],vtypes,sv)) for j=2:length(aa) ]
            if _any(isvarargtype, result[i])
                return nothing
            end
        elseif isa(ti, Union)
            return nothing
        elseif ti<:Tuple
            if i == n
                if ti.name === NTuple.name
                    result[i] = Any[Vararg{ti.parameters[2]}]
                else
                    result[i] = ti.parameters
                end
            elseif isknownlength(ti)
                result[i] = ti.parameters
            else
                return nothing
            end
        elseif ti<:AbstractArray && i==n
            result[i] = Any[Vararg{eltype(ti)}]
        else
            return nothing
        end
    end
    return result
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(af::ANY, fargs, aargtypes::Vector{Any}, vtypes, sv, e)
    ctypes = precise_container_types(fargs, aargtypes, vtypes, sv)
    if ctypes !== nothing
        # apply with known func with known tuple types
        # can be collapsed to a call to the applied func
        at = append_any(Any[type_typeof(af)], ctypes...)
        n = length(at)
        if n-1 > MAX_TUPLETYPE_LEN
            tail = foldl((a,b)->tmerge(a,unwrapva(b)), Bottom, at[MAX_TUPLETYPE_LEN+1:n])
            at = vcat(at[1:MAX_TUPLETYPE_LEN], Any[Vararg{tail}])
        end
        return abstract_call(af, (), at, vtypes, sv, ())
    end
    # apply known function with unknown args => f(Any...)
    return abstract_call(af, (), Any[type_typeof(af), Vararg{Any}], vtypes, sv, ())
end

function isconstantargs(args, argtypes::Vector{Any}, sv::VarInfo)
    if length(argtypes) == 1 # just the function
        return true
    end
    if isvarargtype(argtypes[end])
        return false
    end
    for i = 2:length(argtypes)
        t = argtypes[i]
        if !isType(t) || has_typevars(t.parameters[1])
            args === () && return false
            arg = args[i]
            if isconstantref(arg, sv) === false
                return false
            end
        end
    end
    return true
end

function _ieval_args(args, argtypes::Vector{Any}, sv::VarInfo)
    c = cell(length(argtypes) - 1)
    for i = 2:length(argtypes)
        t = argtypes[i]
        if isType(t) && !has_typevars(t.parameters[1])
            c[i - 1] = t.parameters[1]
        else
            c[i - 1] = _ieval(isconstantref(args[i], sv), sv)
        end
    end
    return c
end

@pure function type_typeof(v::ANY)
    if isa(v, Type)
        return Type{v}
    end
    return typeof(v)
end

function pure_eval_call(f::ANY, fargs, argtypes::ANY, sv, e)
    if !isconstantargs(fargs, argtypes, sv)
        return false
    end

    args = _ieval_args(fargs, argtypes, sv)
    atype = Tuple{type_typeof(f), Any[type_typeof(a) for a in args]...}
    meth = _methods_by_ftype(atype, 1)
    if meth === false || length(meth) != 1
        return false
    end
    meth = meth[1]::SimpleVector
    linfo = try
        func_for_method(meth[3], meth[1], meth[2])
    catch
        NF
    end
    if linfo === NF
        return false
    end
    if !linfo.pure
        typeinf(linfo, meth[1], meth[2], linfo)
        if !linfo.pure
            return false
        end
    end

    try
        v = f(args...)
        return type_typeof(v)
    catch
        return false
    end
end


function abstract_call(f::ANY, fargs, argtypes::Vector{Any}, vtypes, sv::VarInfo, e)
    t = pure_eval_call(f, fargs, argtypes, sv, e)
    t !== false && return t
    if is(f,_apply) && length(fargs)>1
        af = isconstantfunc(fargs[2], sv)
        if af === false
            aft = argtypes[2]
            if isType(aft) && !isa(aft.parameters[1],TypeVar)
                af = aft.parameters[1]
            elseif isleaftype(aft) && isdefined(aft,:instance)
                af = aft.instance
            else
                # TODO jb/functions: take advantage of case where non-constant `af`'s type is known
                return Any
            end
        else
            af = _ieval(af, sv)
        end
        return abstract_apply(af, fargs[3:end], argtypes[3:end], vtypes, sv, e)
    end
    for i=2:(length(argtypes)-1)
        if isvarargtype(argtypes[i])
            return Any
        end
    end
    if is(f,invoke) && length(fargs)>2
        af = isconstantfunc(fargs[2], sv)
        if !is(af,false)
            af = _ieval(af,sv)
            sig = argtypes[3]
            if isType(sig) && sig.parameters[1] <: Tuple
                return invoke_tfunc(af, sig.parameters[1], Tuple{argtypes[4:end]...})
            end
        end
    end
    if is(f,getfield)
        val = isconstantref(e, sv)
        if !is(val,false)
            return abstract_eval_constant(_ieval(val,sv))
        end
    end
    if is(f,Core.kwfunc) && length(fargs)==2
        ft = argtypes[2]
        if isa(ft,DataType) && !ft.abstract
            if isdefined(ft.name.mt, :kwsorter)
                return typeof(ft.name.mt.kwsorter)
            end
        end
        return Any
    end
    fargs = fargs[2:end]
    if isa(f,Builtin) || isa(f,IntrinsicFunction)
        rt = builtin_tfunction(f, fargs, Tuple{argtypes[2:end]...})
        return isa(rt, TypeVar) ? rt.ub : rt
    end
    return abstract_call_gf(f, fargs, Tuple{argtypes...}, e)
end

function abstract_eval_call(e, vtypes, sv::VarInfo)
    argtypes = Any[abstract_eval(a, vtypes, sv) for a in e.args]
    #print("call ", e.args[1], argtypes, "\n\n")
    for x in argtypes
        x === Bottom && return Bottom
    end
    called = e.args[1]
    func = isconstantfunc(called, sv)
    if is(func,false)
        ft = argtypes[1]
        if isType(ft) && !isa(ft.parameters[1],TypeVar)
            f = ft.parameters[1]
        elseif isleaftype(ft) && isdefined(ft,:instance)
            f = ft.instance
        else
            for i = 2:(length(argtypes)-1)
                if isvarargtype(argtypes[i])
                    return Any
                end
            end
            # non-constant function, but type is known
            if (isleaftype(ft) || ft <: Type) && !(ft <: Builtin) && !(ft <: IntrinsicFunction)
                return abstract_call_gf_by_type(nothing, Tuple{argtypes...}, e)
            end
            return Any
        end
    else
        f = _ieval(func,sv)
    end
    if isa(called, Expr)
        # if called thing is a constant, still make sure it gets annotated with a type.
        # issue #11997
        called.typ = abstract_eval_constant(f)
    end
    return abstract_call(f, e.args, argtypes, vtypes, sv, e)
end

function abstract_eval(e::ANY, vtypes, sv::VarInfo)
    if isa(e,QuoteNode)
        v = (e::QuoteNode).value
        return type_typeof(v)
    elseif isa(e,TopNode)
        return abstract_eval_global(_topmod(sv), (e::TopNode).name)
    elseif isa(e,Symbol)
        return abstract_eval_symbol(e::Symbol, vtypes, sv)
    elseif isa(e,SymbolNode)
        return abstract_eval_symbol((e::SymbolNode).name, vtypes, sv)
    elseif isa(e,GenSym)
        return abstract_eval_gensym(e::GenSym, sv)
    elseif isa(e,GlobalRef)
        return abstract_eval_global(e.mod, e.name)
    end

    if !isa(e,Expr)
        return abstract_eval_constant(e)
    end
    e = e::Expr
    # handle:
    # call  null  new  &  static_typeof
    if is(e.head,:call)
        t = abstract_eval_call(e, vtypes, sv)
    elseif is(e.head,:null)
        t = Void
    elseif is(e.head,:new)
        t = abstract_eval(e.args[1], vtypes, sv)
        if isType(t)
            t = t.parameters[1]
        else
            t = Any
        end
        for i = 2:length(e.args)
            abstract_eval(e.args[i], vtypes, sv)
        end
    elseif is(e.head,:&)
        abstract_eval(e.args[1], vtypes, sv)
        t = Any
    elseif is(e.head,:static_typeof)
        var = e.args[1]
        t = abstract_eval(var, vtypes, sv)
        if isa(t,DataType) && typeseq(t,t.name.primary)
            # remove unnecessary typevars
            t = t.name.primary
        end
        if is(t,Bottom)
            # if we haven't gotten fed-back type info yet, return Bottom. otherwise
            # Bottom is the actual type of the variable, so return Type{Bottom}.
            if haskey(sv.fedbackvars, var)
                t = Type{Bottom}
            end
        elseif isleaftype(t)
            t = Type{t}
        elseif isleaftype(inference_stack.types)
            if isa(t,TypeVar)
                t = Type{t.ub}
            else
                t = Type{t}
            end
        else
            # if there is any type uncertainty in the arguments, we are
            # effectively predicting what static_typeof will say when
            # the function is compiled with actual arguments. in that case
            # abstract types yield Type{<:T} instead of Type{T}.
            # this doesn't really model the situation perfectly, but
            # "isleaftype(inference_stack.types)" should be good enough.
            if isa(t,TypeVar)
                t = Type{t}
            else
                t = Type{TypeVar(:_,t)}
            end
        end
    elseif is(e.head,:method)
        t = (length(e.args) == 1) ? Any : Void
    elseif is(e.head,:copyast)
        t = abstract_eval(e.args[1], vtypes, sv)
    elseif is(e.head,:inert)
        v = e.args[1]
        return type_typeof(v)
    else
        t = Any
    end
    if isa(t,TypeVar)
        # no need to use a typevar as the type of an expression
        t = t.ub
    end
    e.typ = t
    return t
end

const Type_Array = Type{Array}

function abstract_eval_constant(x::ANY)
    if isa(x,Type)
        if is(x,Array)
            return Type_Array
        end
        return Type{x}
    end
    return typeof(x)
end

function abstract_eval_global(M::Module, s::Symbol)
    if isdefined(M,s) && isconst(M,s)
        return abstract_eval_constant(getfield(M,s))
    end
    return Any
end

function abstract_eval_gensym(s::GenSym, sv::VarInfo)
    typ = sv.gensym_types[s.id+1]
    if typ === NF
        return Bottom
    end
    return typ
end

function abstract_eval_symbol(s::Symbol, vtypes::ObjectIdDict, sv::VarInfo)
    t = get(vtypes,s,NF)
    if is(t,NF)
        sp = sv.linfo.sparam_syms
        for i=1:length(sp)
            if is(sp[i],s)
                # static parameter
                val = sv.sp[i]
                if isa(val,TypeVar)
                    # static param bound to typevar
                    if Any <: val.ub
                        # if the tvar does not refer to anything more specific
                        # than Any, the static param might actually be an
                        # integer, symbol, etc.
                        return Any
                    end
                    return Type{val}
                end
                return abstract_eval_constant(val)
            end
        end
        if s in sv.vars
            # local variable use not reached
            return Bottom
        end
        # global
        return abstract_eval_global(sv.mod, s)
    end
    return t.typ
end

typealias VarTable ObjectIdDict

type StateUpdate
    var::Union{Symbol,GenSym}
    vtype
    state::VarTable
end

function getindex(x::StateUpdate, s::Symbol)
    if is(x.var,s)
        return x.vtype
    end
    return get(x.state,s,NF)
end

function abstract_interpret(e::ANY, vtypes, sv::VarInfo)
    !isa(e,Expr) && return vtypes
    # handle assignment
    if is(e.head,:(=))
        t = abstract_eval(e.args[2], vtypes, sv)
        lhs = e.args[1]
        if isa(lhs,SymbolNode)
            lhs = lhs.name
        end
        if isa(lhs,Symbol) || isa(lhs,GenSym)
            # don't bother for GlobalRef
            return StateUpdate(lhs, VarState(t,false), vtypes)
        end
    elseif is(e.head,:call)
        abstract_eval(e, vtypes, sv)
    elseif is(e.head,:gotoifnot)
        abstract_eval(e.args[1], vtypes, sv)
    elseif is(e.head,:method)
        fname = e.args[1]
        if isa(fname,Symbol)
            return StateUpdate(fname, VarState(Any,false), vtypes)
        end
    end
    return vtypes
end

function type_too_complex(t::ANY, d)
    if d > MAX_TYPE_DEPTH
        return true
    end
    if isa(t,Union)
        p = t.types
    elseif isa(t,DataType)
        p = t.parameters
    elseif isa(t,TypeVar)
        return type_too_complex(t.lb,d+1) || type_too_complex(t.ub,d+1)
    else
        return false
    end
    for x in (p::SimpleVector)
        if type_too_complex(x, d+1)
            return true
        end
    end
    return false
end

function tmerge(typea::ANY, typeb::ANY)
    is(typea, NF)  && return typeb
    is(typeb, NF)  && return typea
    typea <: typeb && return typeb
    typeb <: typea && return typea
    if (typea <: Tuple) && (typeb <: Tuple)
        if length(typea.parameters) == length(typeb.parameters) && !isvatuple(typea) && !isvatuple(typeb)
            return typejoin(typea, typeb)
        end
        return Tuple
    end
    u = Union{typea, typeb}
    if length(u.types) > MAX_TYPEUNION_LEN || type_too_complex(u, 0)
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return Any
    end
    return u
end

issubstate(a::VarState,b::VarState) = (a.typ <: b.typ && a.undef <= b.undef)

function smerge(sa::Union{NotFound,VarState}, sb::Union{NotFound,VarState})
    is(sa, NF) && return sb
    is(sb, NF) && return sa
    issubstate(sa,sb) && return sb
    issubstate(sb,sa) && return sa
    VarState(tmerge(sa.typ, sb.typ), sa.undef | sb.undef)
end

tchanged(n::ANY, o::ANY) = is(o,NF) || (!is(n,NF) && !(n <: o))
schanged(n::ANY, o::ANY) = is(o,NF) || (!is(n,NF) && !issubstate(n, o))

stupdate(state::Tuple{}, changes::VarTable, vars) = copy(changes)
stupdate(state::Tuple{}, changes::StateUpdate, vars) = stupdate(ObjectIdDict(), changes, vars)

function stupdate(state::ObjectIdDict, changes::Union{StateUpdate,VarTable}, vars)
    for i = 1:length(vars)
        v = vars[i]
        newtype = changes[v]
        oldtype = get(state::ObjectIdDict,v,NF)
        if schanged(newtype, oldtype)
            state[v] = smerge(oldtype, newtype)
        end
    end
    state
end

function stchanged(new::Union{StateUpdate,VarTable}, old, vars)
    if is(old,())
        return true
    end
    for v in vars
        if schanged(new[v], get(old,v,NF))
            return true
        end
    end
    return false
end

function findlabel(labels, l)
    i = l+1 > length(labels) ? 0 : labels[l+1]
    if i == 0
        error("label ",l," not found")
    end
    return i
end

function label_counter(body)
    l = -1
    for b in body
        if isa(b,LabelNode) && (b::LabelNode).label > l
            l = (b::LabelNode).label
        end
    end
    return l
end
genlabel(sv) = LabelNode(sv.label_counter += 1)

function find_gensym_uses(body)
    uses = IntSet[]
    for line = 1:length(body)
        find_gensym_uses(body[line], uses, line)
    end
    return uses
end
function find_gensym_uses(e::ANY, uses, line)
    if isa(e,GenSym)
        id = (e::GenSym).id+1
        while length(uses) < id
            push!(uses, IntSet())
        end
        push!(uses[id], line)
    elseif isa(e,Expr)
        b = e::Expr
        head = b.head
        if head === :line
            return
        end
        if head === :(=)
            if isa(b.args[1],GenSym)
                id = (b.args[1]::GenSym).id+1
                while length(uses) < id
                    push!(uses, IntSet())
                end
            end
            find_gensym_uses(b.args[2], uses, line)
            return
        end
        for a in b.args
            find_gensym_uses(a, uses, line)
        end
    end
end

function newvar!(sv::VarInfo, typ)
    id = length(sv.gensym_types)
    push!(sv.gensym_types, typ)
    return GenSym(id)
end

f_argnames(ast) =
    Any[(isa(x,Expr) ? x.args[1] : x) for x in ast.args[1]::Array{Any,1}]

is_rest_arg(arg::ANY) = (ccall(:jl_is_rest_arg,Int32,(Any,), arg) != 0)

function typeinf_ext(linfo, atypes::ANY, def)
    global inference_stack
    last = inference_stack
    inference_stack = EmptyCallStack()
    result = typeinf(linfo, atypes, svec(), def, true, true)
    inference_stack = last
    return result
end

typeinf(linfo,atypes::ANY,sparams::ANY) = typeinf(linfo,atypes,sparams,linfo,true,false)
typeinf(linfo,atypes::ANY,sparams::ANY,def) = typeinf(linfo,atypes,sparams,def,true,false)

CYCLE_ID = 1

#trace_inf = false
#enable_trace_inf(on) = (global trace_inf=on)

# def is the original unspecialized version of a method. we aggregate all
# saved type inference data there.
function typeinf(linfo::LambdaInfo, atypes::ANY, sparams::SimpleVector, def, cop, needtree)
    if linfo.module === Core && isempty(sparams) && isempty(linfo.sparam_vals)
        atypes = Tuple
    end
    #dbg =
    #dotrace = true
    local ast::Expr, tfunc_idx = -1
    curtype = Bottom
    redo = false
    # check cached t-functions
    tf = def.tfunc
    if !is(tf,nothing)
        tfarr = tf::Array{Any,1}
        for i = 1:3:length(tfarr)
            if typeseq(tfarr[i],atypes)
                code = tfarr[i+1]
                if tfarr[i+2]
                    redo = true
                    tfunc_idx = i+1
                    curtype = code
                    break
                end
                if isa(code,Type)
                    curtype = code::Type
                    # sometimes just a return type is stored here. if a full AST
                    # is not needed, we can return it.
                    if !needtree
                        return (nothing, code)
                    end
                else
                    return code  # else code is a tuple (ast, type)
                end
            end
        end
    end
    # TODO: typeinf currently gets stuck without this
    if linfo.name === :abstract_interpret || linfo.name === :alloc_elim_pass || linfo.name === :abstract_call_gf
        return (linfo.ast, Any)
    end

    (fulltree, result, rec) = typeinf_uncached(linfo, atypes, sparams, def, curtype, cop, true)
    if fulltree === ()
        return (fulltree, result::Type)
    end

    if !redo
        if is(def.tfunc,nothing)
            def.tfunc = Any[]
        end
        tfarr = def.tfunc::Array{Any,1}
        idx = -1
        for i = 1:3:length(tfarr)
            if typeseq(tfarr[i],atypes)
                idx = i; break
            end
        end
        if idx == -1
            l = length(tfarr)
            idx = l+1
            resize!(tfarr, l+3)
        end
        tfarr[idx] = atypes
        # in the "rec" state this tree will not be used again, so store
        # just the return type in place of it.
        tfarr[idx+1] = rec ? result : (fulltree,result)
        tfarr[idx+2] = rec
    else
        def.tfunc[tfunc_idx] = rec ? result : (fulltree,result)
        def.tfunc[tfunc_idx+1] = rec
    end

    return (fulltree, result::Type)
end

typeinf_uncached(linfo, atypes::ANY, sparams::ANY; optimize=true) =
    typeinf_uncached(linfo, atypes, sparams, linfo, Bottom, true, optimize)

# t[n:end]
tupletype_tail(t::ANY, n) = Tuple{t.parameters[n:end]...}

# compute an inferred (optionally optimized) AST without global effects (i.e. updating the cache)
function typeinf_uncached(linfo::LambdaInfo, atypes::ANY, sparams::SimpleVector, def, curtype, cop, optimize)
    ast0 = def.ast
    #if dbg
    #    print("typeinf ", linfo.name, " ", object_id(ast0), "\n")
    #end
    # if isdefined(:STDOUT)
    #     write(STDOUT, "typeinf ")
    #     write(STDOUT, string(linfo.name))
    #     write(STDOUT, string(atypes))
    #     write(STDOUT, '\n')
    # end
    #print("typeinf ", ast0, " ", sparams, " ", atypes, "\n")

    global inference_stack, CYCLE_ID

    f = inference_stack
    while !isa(f,EmptyCallStack)
        if is(f.ast,ast0)
            # impose limit if we recur and the argument types grow beyond MAX_TYPE_DEPTH
            td = type_depth(atypes)
            if td > type_depth(f.types)
                if td > MAX_TYPE_DEPTH
                    atypes = limit_type_depth(atypes, 0, true, [])
                    break
                else
                    p1, p2 = atypes.parameters, f.types.parameters
                    n = length(p1)
                    if length(p2) == n
                        limited = false
                        newatypes = Array(Any, n)
                        for i = 1:n
                            if p1[i] <: Function && type_depth(p1[i]) > type_depth(p2[i]) &&
                                isa(p1[i],DataType)
                                # if a Function argument is growing (e.g. nested closures)
                                # then widen to the outermost function type. without this
                                # inference fails to terminate on do_quadgk.
                                newatypes[i] = p1[i].name.primary
                                limited = true
                            else
                                newatypes[i] = p1[i]
                            end
                        end
                        if limited
                            atypes = Tuple{newatypes...}
                            break
                        end
                    end
                end
            end
        end
        f = f.prev
    end

    # check for recursion
    f = inference_stack
    while !isa(f,EmptyCallStack)
        if (is(f.ast,ast0) || f.ast==ast0) && typeseq(f.types, atypes)
            # return best guess so far
            (f::CallStack).recurred = true
            (f::CallStack).cycleid = CYCLE_ID
            r = inference_stack
            while !is(r, f)
                # mark all frames that are part of the cycle
                r.recurred = true
                r.cycleid = CYCLE_ID
                r = r.prev
            end
            CYCLE_ID += 1
            #print("*==> ", f.result,"\n")
            return ((),f.result,true)
        end
        f = f.prev
    end

    #if trace_inf
    #    print("typeinf ", linfo.name, " ", atypes, " ", linfo.file,":",linfo.line,"\n")
    #end

    #if dbg print("typeinf ", linfo.name, " ", atypes, "\n") end

    if cop
        ast = ccall(:jl_prepare_ast, Any, (Any,), linfo)::Expr
    else
        ast = linfo.ast
    end

    sv = VarInfo(linfo, ast)

    if length(linfo.sparam_vals) > 0
        # handled by VarInfo constructor
    elseif isempty(sparams) && !isempty(linfo.sparam_syms)
        sv.sp = svec(Any[ TypeVar(sym, Any, true) for sym in linfo.sparam_syms ]...)
    else
        sv.sp = sparams
    end

    args = f_argnames(ast)
    la = length(args)
    assert(is(ast.head,:lambda))
    vinflist = sv.vinfo
    vars = sv.vars
    body = (ast.args[3].args)::Array{Any,1}
    n = length(body)

    labels = zeros(Int, sv.label_counter)
    for i=1:length(body)
        b = body[i]
        if isa(b,LabelNode)
            labels[b.label+1] = i
        end
    end

    # our stack frame
    frame = CallStack(ast0, atypes, inference_stack)
    frame.sv = sv
    inference_stack = frame
    frame.result = curtype

    rec = false
    toprec = false

    s = Any[ () for i=1:n ]
    # initial types
    s[1] = ObjectIdDict()
    for v in vars
        s[1][v] = VarState(Bottom,true)
    end
    if la > 0
        lastarg = ast.args[1][la]
        if is_rest_arg(lastarg)
            if atypes === Tuple
                if la > 1
                    atypes = Tuple{Any[Any for i=1:la-1]..., Tuple.parameters[1]}
                end
                s[1][args[la]] = VarState(Tuple,false)
            else
                s[1][args[la]] = VarState(limit_tuple_depth(tupletype_tail(atypes,la)),false)
            end
            la -= 1
        else
            if atypes === Tuple
                atypes = Tuple{Any[Any for i=1:la]..., Tuple.parameters[1]}
            end
        end
    end

    laty = length(atypes.parameters)
    if laty > 0
        lastatype = atypes.parameters[laty]
        if isvarargtype(lastatype)
            lastatype = lastatype.parameters[1]
            laty -= 1
        end
        if isa(lastatype, TypeVar)
            lastatype = lastatype.ub
        end
        if laty > la
            laty = la
        end
        for i=1:laty
            atyp = atypes.parameters[i]
            if isa(atyp, TypeVar)
                atyp = atyp.ub
            end
            s[1][args[i]] = VarState(atyp, false)
        end
        for i=laty+1:la
            s[1][args[i]] = VarState(lastatype, false)
        end
    elseif la != 0
        return ((), Bottom, false) # wrong number of arguments
    end

    gensym_uses = find_gensym_uses(body)
    if length(sv.gensym_types) != length(gensym_uses)
        sv.gensym_types = Any[ NF for i=1:length(gensym_uses) ]
    end
    gensym_types = sv.gensym_types
    gensym_init = copy(gensym_types)

    recpts = IntSet()  # statements that depend recursively on our value
    W = IntSet()

    @label typeinf_top

    typegotoredo = false

    # exception handlers
    cur_hand = ()
    handler_at = Any[ () for i=1:n ]
    n_handlers = 0

    push!(W,1) #initial pc to visit

    while !isempty(W)
        pc = first(W)
        while true
            #print(pc,": ",s[pc],"\n")
            delete!(W, pc)
            if is(handler_at[pc],())
                handler_at[pc] = cur_hand
            else
                cur_hand = handler_at[pc]
            end
            stmt = body[pc]
            changes = abstract_interpret(stmt, s[pc]::ObjectIdDict, sv)
            if frame.recurred
                rec = true
                if !(isa(frame.prev,CallStack) && frame.prev.cycleid == frame.cycleid)
                    toprec = true
                end
                push!(recpts, pc)
                #if dbg
                #    show(pc); print(" recurred\n")
                #end
                frame.recurred = false
            end
            if !is(cur_hand,())
                # propagate type info to exception handler
                l = cur_hand[1]::Int
                if stchanged(changes, s[l], vars)
                    push!(W, l)
                    s[l] = stupdate(s[l], changes, vars)
                end
            end
            pc´ = pc+1
            if isa(changes,StateUpdate) && isa((changes::StateUpdate).var, GenSym)
                changes = changes::StateUpdate
                id = (changes.var::GenSym).id+1
                new = changes.vtype.typ
                old = gensym_types[id]
                if old===NF || !(new <: old)
                    gensym_types[id] = tmerge(old, new)
                    for r in gensym_uses[id]
                        if !is(s[r],()) # s[r] === () => unreached statement
                            push!(W, r)
                        end
                    end
                end
            elseif isa(stmt,GotoNode)
                pc´ = findlabel(labels,stmt.label)
            elseif isa(stmt,Expr)
                hd = stmt.head
                if is(hd,:gotoifnot)
                    condexpr = stmt.args[1]
                    l = findlabel(labels,stmt.args[2])
                    # constant conditions
                    if is(condexpr,true)
                    elseif is(condexpr,false)
                        pc´ = l
                    else
                        # general case
                        handler_at[l] = cur_hand
                        if stchanged(changes, s[l], vars)
                            push!(W, l)
                            s[l] = stupdate(s[l], changes, vars)
                        end
                    end
                elseif is(hd,:type_goto)
                    for i = 2:length(stmt.args)
                        var = stmt.args[i]::GenSym
                        # Store types that need to be fed back via type_goto
                        # in gensym_init. After finishing inference, if any
                        # of these types changed, start over with the fed-back
                        # types known from the beginning.
                        # See issue #3821 (using !typeseq instead of !subtype),
                        # and issue #7810.
                        id = var.id+1
                        vt = gensym_types[id]
                        ot = gensym_init[id]
                        if ot===NF || !typeseq(vt,ot)
                            gensym_init[id] = vt
                            typegotoredo = true
                        end
                        sv.fedbackvars[var] = true
                    end
                elseif is(hd,:return)
                    pc´ = n+1
                    rt = abstract_eval(stmt.args[1], s[pc], sv)
                    if frame.recurred
                        rec = true
                        if !(isa(frame.prev,CallStack) && frame.prev.cycleid == frame.cycleid)
                            toprec = true
                        end
                        push!(recpts, pc)
                        #if dbg
                        #    show(pc); print(" recurred\n")
                        #end
                        frame.recurred = false
                    end
                    #if dbg
                    #    print("at "); show(pc)
                    #    print(" result is "); show(frame.result)
                    #    print(" and rt is "); show(rt)
                    #    print("\n")
                    #end
                    if tchanged(rt, frame.result)
                        frame.result = tmerge(frame.result, rt)
                        # revisit states that recursively depend on this
                        for r in recpts
                            #if dbg
                            #    print("will revisit ")
                            #    show(r)
                            #    print("\n")
                            #end
                            push!(W, r)
                        end
                    end
                elseif is(hd,:enter)
                    l = findlabel(labels,stmt.args[1]::Int)
                    cur_hand = (l,cur_hand)
                    if handler_at[l] === ()
                        n_handlers += 1
                        if n_handlers > 25
                            # too many exception handlers slows down inference a lot.
                            # for an example see test/libgit2.jl on 0.5-pre master
                            # around e.g. commit c072d1ce73345e153e4fddf656cda544013b1219
                            inference_stack = (inference_stack::CallStack).prev
                            return (ast0, Any, false)
                        end
                    end
                    handler_at[l] = cur_hand
                elseif is(hd,:leave)
                    for i=1:((stmt.args[1])::Int)
                        cur_hand = cur_hand[2]
                    end
                end
            end
            if pc´<=n && (handler_at[pc´] = cur_hand; true) &&
               stchanged(changes, s[pc´], vars)
                s[pc´] = stupdate(s[pc´], changes, vars)
                pc = pc´
            elseif pc´ in W
                pc = pc´
            else
                break
            end
        end
    end

    if typegotoredo
        # if any type_gotos changed, clear state and restart.
        for ll = 2:length(s)
            s[ll] = ()
        end
        empty!(W)
        gensym_types[:] = gensym_init
        frame.result = curtype
        @goto typeinf_top
    end
    for i = 1:length(gensym_types)
        if gensym_types[i] === NF
            gensym_types[i] = Union{}
        end
    end

    #print("\n",ast,"\n")
    #if dbg print("==> ", frame.result,"\n") end
    if (toprec && typeseq(curtype, frame.result)) || !isa(frame.prev,CallStack)
        rec = false
    end
    fulltree = type_annotate(ast, s, sv, frame.result, args)

    if !rec
        @assert fulltree.args[3].head === :body
        if optimize
            if JLOptions().can_inline == 1
                fulltree.args[3] = inlining_pass(fulltree.args[3], sv, fulltree)[1]
                # inlining can add variables
                sv.vars = append_any(f_argnames(fulltree), map(vi->vi[1], fulltree.args[2][1]))
                inbounds_meta_elim_pass(fulltree.args[3])
            end
            alloc_elim_pass(fulltree, sv)
            getfield_elim_pass(fulltree.args[3], sv)
        end
        linfo.inferred = true
        body = Expr(:block)
        body.args = fulltree.args[3].args::Array{Any,1}
        linfo.pure = popmeta!(body, :pure)[1]
        fulltree = ccall(:jl_compress_ast, Any, (Any,Any), def, fulltree)
    end

    inference_stack = (inference_stack::CallStack).prev
    return (fulltree, frame.result, rec)
end

function record_var_type(e::Symbol, t::ANY, decls)
    otherTy = get(decls::ObjectIdDict, e, false)
    # keep track of whether a variable is always the same type
    if !is(otherTy,false)
        if !typeseq(otherTy, t)
            decls[e] = Any
        end
    else
        decls[e] = t
    end
end

function eval_annotate(e::ANY, vtypes::ANY, sv::VarInfo, decls, undefs)
    if isa(e, Symbol)
        e = e::Symbol

        if !is_local(sv, e)
            # can get types of globals and static params from the environment
            return e
        end
        t = abstract_eval(e, vtypes, sv)
        s = get(vtypes, e, NF)
        if s !== NF && s.undef
            undefs[e] = true
        end
        record_var_type(e, t, decls)
        return (is(t,Any) || is(t,IntrinsicFunction)) ? e : SymbolNode(e, t)
    end

    if isa(e, SymbolNode)
        e = e::SymbolNode
        curtype = e.typ
        t = abstract_eval(e.name, vtypes, sv)
        s = get(vtypes, e.name, NF)
        if s !== NF && s.undef
            undefs[e] = true
        end
        if !(curtype <: t) || typeseq(curtype, t)
            record_var_type(e.name, t, decls)
            e.typ = t
        end
        return e
    end

    if !isa(e,Expr)
        return e
    end

    e = e::Expr
    head = e.head
    if is(head,:static_typeof) || is(head,:line) || is(head,:const)
        return e
    #elseif is(head,:gotoifnot) || is(head,:return)
    #    e.typ = Any
    elseif is(head,:(=))
    #    e.typ = Any
        s = e.args[1]
        # assignment LHS not subject to all-same-type variable checking,
        # but the type of the RHS counts as one of its types.
        if isa(s,SymbolNode)
            # we don't use types on assignment LHS
            s = s.name
        end
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, decls, undefs)
        if isa(s,Symbol)
            # TODO: if this def does not reach any uses, maybe don't do this
            rhstype = exprtype(e.args[2], sv)
            if !is(rhstype,Bottom)
                record_var_type(s, rhstype, decls)
            end
        end
        return e
    end
    i0 = is(head,:method) ? 2 : 1
    for i=i0:length(e.args)
        subex = e.args[i]
        if !(isa(subex,Number) || isa(subex,AbstractString))
            e.args[i] = eval_annotate(subex, vtypes, sv, decls, undefs)
        end
    end
    return e
end

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array{Any,1}, sv::ANY, rettype::ANY, args)
    decls = ObjectIdDict()
    undefs = ObjectIdDict()
    # initialize decls with argument types
    for arg in args
        decls[arg] = states[1][arg].typ
    end
    body = ast.args[3].args::Array{Any,1}
    for i=1:length(body)
        st_i = states[i]
        if st_i !== ()
            # st_i === ()  =>  unreached statement  (see issue #7836)
            body[i] = eval_annotate(body[i], st_i, sv, decls, undefs)
        end
    end
    ast.args[3].typ = rettype

    # add declarations for variables that are always the same type
    for vi in ast.args[2][1]::Array{Any,1}
        vi[2] = get(decls, vi[1], vi[2])
        if haskey(undefs, vi[1])
            vi[3] |= 32
        end
    end
    for vi in ast.args[2][2]::Array{Any,1}
        vi[2] = get(decls, vi[1], vi[2])
        if haskey(undefs, vi[1])
            vi[3] |= 32
        end
    end
    ast.args[2][3] = sv.gensym_types

    return ast
end

function sym_replace(e::ANY, from1, from2, to1, to2)
    if isa(e,Symbol) || isa(e,GenSym)
        return _sym_repl(e::Union{Symbol,GenSym}, from1, from2, to1, to2, e)
    end
    if isa(e,SymbolNode)
        e2 = _sym_repl(e.name, from1, from2, to1, to2, e)
        if isa(e2, SymbolNode) || !isa(e2, Symbol)
            return e2
        else
            return SymbolNode(e2, e.typ)
        end
    end
    if isa(e,NewvarNode)
        e2 = _sym_repl(e.name::Symbol, from1, from2, to1, to2, e)
        if isa(e2, NewvarNode) || !isa(e2, Symbol)
             return e2
        else
            return NewvarNode(e2)
        end
    end
    if !isa(e,Expr)
        return e
    end
    e = e::Expr
    if e.head === :(=)
        s = e.args[1]
        if isa(s, Symbol) || isa(s, GenSym)
            e2 = _sym_repl(s, from1, from2, to1, to2, s)
            # remove_redundant_temp_vars can only handle Symbols
            # on the LHS of assignments, so we make sure not to put
            # something else there
            if isa(e2, SymbolNode)
                e2 = e2.name
            end
            e.args[1] = e2::Union{Symbol,GenSym}
        end
        e.args[2] = sym_replace(e.args[2], from1, from2, to1, to2)
    elseif e.head !== :line
        for i=1:length(e.args)
            e.args[i] = sym_replace(e.args[i], from1, from2, to1, to2)
        end
    end
    return e
end

function _sym_repl(s::Union{Symbol,GenSym}, from1, from2, to1, to2, deflt)
    for i=1:length(from1)
        if is(from1[i],s)
            return to1[i]
        end
    end
    for i=1:length(from2)
        if is(from2[i],s)
            return to2[i]
        end
    end
    return deflt
end

# count occurrences up to n+1
function occurs_more(e::ANY, pred, n)
    if isa(e,Expr)
        e = e::Expr
        e.head === :line && return 0
        c = 0
        for a = e.args
            c += occurs_more(a, pred, n)
            if c>n
                return c
            end
        end
        return c
    end
    if pred(e) || (isa(e,SymbolNode) && pred(e.name))
        return 1
    end
    return 0
end

const emptydict = ObjectIdDict()

function exprtype(x::ANY, sv::VarInfo)
    if isa(x,Expr)
        return (x::Expr).typ
    elseif isa(x,SymbolNode)
        return (x::SymbolNode).typ
    elseif isa(x,GenSym)
        return abstract_eval_gensym(x::GenSym, sv)
    elseif isa(x,TopNode)
        return abstract_eval_global(_topmod(sv), (x::TopNode).name)
    elseif isa(x,Symbol)
        if is_local(sv, x::Symbol)
            return Any
        end
        return abstract_eval(x::Symbol, emptydict, sv)
    elseif isa(x,QuoteNode)
        v = (x::QuoteNode).value
        return type_typeof(v)
    elseif isa(x,Type)
        return Type{x}
    elseif isa(x,GlobalRef)
        return abstract_eval_global(x.mod, (x::GlobalRef).name)
    else
        return typeof(x)
    end
end

# known affect-free calls (also effect-free)
const _pure_builtins = Any[tuple, svec, fieldtype, apply_type, is, isa, typeof, typeassert]

# known effect-free calls (might not be affect-free)
const _pure_builtins_volatile = Any[getfield, arrayref]

function is_pure_builtin(f::ANY)
    if contains_is(_pure_builtins, f)
        return true
    end
    if contains_is(_pure_builtins_volatile, f)
        return true
    end
    if isa(f,IntrinsicFunction)
        if !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.ccall ||      # this one is never effect-free
             f === Intrinsics.llvmcall)     # this one is never effect-free
            return true
        end
    end
    return false
end

# detect some important side-effect-free calls (allow_volatile=true)
# and some affect-free calls (allow_volatile=false) -- affect_free means the call
# cannot be affected by previous calls, except assignment nodes
function effect_free(e::ANY, sv, allow_volatile::Bool)
    if isa(e,SymbolNode)
        allow_volatile && return true
        if is_global(sv, (e::SymbolNode).name)
            return false
        end
        return true
    end
    if isa(e,Symbol)
        allow_volatile && return true
        if is_global(sv, e::Symbol)
            return false
        end
        return true
    end
    if isa(e,Number) || isa(e,AbstractString) || isa(e,GenSym) ||
        isa(e,TopNode) || isa(e,QuoteNode) || isa(e,Type) || isa(e,Tuple)
        return true
    end
    if isa(e,GlobalRef)
        allow_volatile && return true
        return isconst(e.mod, e.name)
    end
    if isconstantref(e, sv) !== false
        return true
    end
    if isa(e,Expr)
        e = e::Expr
        if e.head === :static_typeof
            return true
        end
        ea = e.args
        if e.head === :call
            if is_known_call_p(e, is_pure_builtin, sv)
                if !allow_volatile
                    if is_known_call(e, arrayref, sv) || is_known_call(e, arraylen, sv)
                        return false
                    elseif is_known_call(e, getfield, sv)
                        # arguments must be immutable to ensure e is affect_free
                        first = true
                        for a in ea
                            if first # first "arg" is the function name
                                first = false
                                continue
                            end
                            if isa(a,Symbol)
                                return false
                            end
                            if isa(a,SymbolNode)
                                typ = (a::SymbolNode).typ
                                if !isa(typ,DataType) || typ.mutable
                                    return false
                                end
                            end
                            if isa(a,GenSym)
                                typ = exprtype(a,sv)
                                if !isa(typ,DataType) || typ.mutable
                                    return false
                                end
                            end
                            if !effect_free(a,sv,allow_volatile)
                                return false
                            end
                        end
                        return true
                    end
                end
                # fall-through
            else
                return false
            end
        elseif e.head === :new
            if !allow_volatile
                a = ea[1]
                typ = exprtype(a,sv)
                if !isType(typ) || !isa((typ::Type).parameters[1],DataType) || ((typ::Type).parameters[1]::DataType).mutable
                    return false
                end
            end
            # fall-through
        elseif e.head === :return
            # fall-through
        else
            return false
        end
        for a in ea
            if !effect_free(a,sv,allow_volatile)
                return false
            end
        end
        return true
    end
    return false
end

function ast_localvars(ast)
    args = ObjectIdDict()
    for argname in (ast.args[1]::Array{Any,1})
        args[argname] = true
    end
    locals = Any[]
    for vi in (ast.args[2][1]::Array{Any,1})
        if !haskey(args, vi[1])
            push!(locals, vi[1])
        end
    end
    locals
end

# inline functions whose bodies are "inline_worthy"
# where the function body doesn't contain any argument more than once.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
# `ft` is the type of the function. `f` is the exact function if known, or else `nothing`.
function inlineable(f::ANY, ft::ANY, e::Expr, atypes::Vector{Any}, sv::VarInfo, enclosing_ast::Expr)
    local linfo,
        metharg::Type,
        argexprs = e.args,
        incompletematch = false

    if (is(f, typeassert) || is(ft, typeof(typeassert))) && length(atypes)==3
        # typeassert(x::S, T) => x, when S<:T
        if isType(atypes[3]) && isleaftype(atypes[3]) &&
            atypes[2] <: atypes[3].parameters[1]
            return (e.args[2],())
        end
    end
    if length(atypes)==3 && is(f,unbox) && isa(atypes[3],DataType) && !atypes[3].mutable && atypes[3].pointerfree
        # remove redundant unbox
        return (e.args[3],())
    end
    topmod = _topmod(sv)
    if istopfunction(topmod, f, :isbits) && length(atypes)==2 && isType(atypes[2]) &&
        effect_free(argexprs[2],sv,true) && isleaftype(atypes[2].parameters[1])
        return (isbits(atypes[2].parameters[1]),())
    end
    # special-case inliners for known pure functions that compute types
    if isType(e.typ) && !has_typevars(e.typ.parameters[1],true)
        if (is(f,apply_type) || is(f,fieldtype) || is(f,typeof) ||
            istopfunction(topmod, f, :typejoin) ||
            istopfunction(topmod, f, :promote_type))
            if effect_free(argexprs[2], sv, true)
                return (e.typ.parameters[1],())
            else
                return (e.typ.parameters[1], Any[argexprs[2]])
            end
        end
    end
    if isa(f,IntrinsicFunction) || ft === IntrinsicFunction
        return NF
    end

    local methfunc
    atype = Tuple{atypes...}
    if length(atype.parameters)-1 > MAX_TUPLETYPE_LEN
        atype = limit_tuple_type(atype)
    end
    meth = _methods_by_ftype(atype, 1)
    if meth === false || length(meth) != 1
        return NF
    end
    meth = meth[1]::SimpleVector
    metharg = meth[1]
    methsp = meth[2]
    linfo = try
        func_for_method(meth[3],metharg,methsp)
    catch
        NF
    end
    if linfo === NF
        return NF
    end
    if linfo.pure && isconstantargs(argexprs, atypes, sv)
        # check if any arguments aren't effect_free and need to be kept around
        stmts = Any[]
        for i = 1:length(argexprs)
            arg = argexprs[i]
            if !effect_free(arg, sv, false)
                push!(stmts, arg)
            end
        end

        if isType(e.typ) && !has_typevars(e.typ.parameters[1])
            return (QuoteNode(e.typ.parameters[1]), stmts)
        end

        constargs = _ieval_args(argexprs, atypes, sv)
        try
            v = f(constargs...)
            return (QuoteNode(v), stmts)
        catch ex
            thrw = Expr(:call, TopNode(:throw), QuoteNode(ex))
            thrw.typ = Bottom
            return (thrw, stmts)
        end
    end

    methfunc = meth[3].func
    methsig = meth[3].sig
    if !(atype <: metharg)
        incompletematch = true
        if !inline_incompletematch_allowed || !isdefined(Main,:Base)
            # provide global disable if this optimization is not desirable
            # need Main.Base defined for MethodError
            return NF
        end
    end
    linfo = linfo::LambdaInfo

    spnames = Any[s for s in linfo.sparam_syms]
    if length(linfo.sparam_vals) > 0
        spvals = Any[x for x in linfo.sparam_vals]
    else
        spvals = Any[]
        for i = 1:length(spnames)
            si = methsp[i]
            if isa(si, TypeVar)
                return NF
            end
            push!(spvals, si)
        end
    end
    for i=1:length(spvals)
        si = spvals[i]
        if isa(si,Symbol) || isa(si,GenSym)
            spvals[i] = QuoteNode(si)
        end
    end

    ## This code tries to limit the argument list length only when it is
    ## growing due to recursion.
    ## It might be helpful for some things, but turns out not to be
    ## necessary to get max performance from recursive varargs functions.
    # if length(atypes) > MAX_TUPLETYPE_LEN
    #     # check call stack to see if this argument list is growing
    #     st = inference_stack
    #     while !isa(st, EmptyCallStack)
    #         if st.ast === linfo.def.ast && length(atypes) > length(st.types)
    #             atypes = limit_tuple_type(atypes)
    #             meth = _methods(f, atypes, 1)
    #             if meth === false || length(meth) != 1
    #                 return NF
    #             end
    #             meth = meth[1]::Tuple
    #             linfo2 = meth[3].func.code
    #             if linfo2 !== linfo
    #                 return NF
    #             end
    #             linfo = linfo2
    #             break
    #         end
    #         st = st.prev
    #     end
    # end

    methargs = metharg.parameters
    nm = length(methargs)

    (ast, ty) = typeinf(linfo, metharg, methsp, linfo, true, true)
    if is(ast,())
        return NF
    end
    needcopy = true
    if !isa(ast,Expr)
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, ast)
        needcopy = false
    end
    ast = ast::Expr
    vinflist = ast.args[2][1]::Array{Any,1}

    body = Expr(:block)
    body.args = ast.args[3].args::Array{Any,1}
    propagate_inbounds, _ = popmeta!(body, :propagate_inbounds)
    cost::Int = 1000
    if incompletematch
        cost *= 4
    end
    if (istopfunction(topmod, f, :next) || istopfunction(topmod, f, :done) ||
       istopfunction(topmod, f, :unsafe_convert) || istopfunction(topmod, f, :cconvert))
        cost ÷= 4
    end
    inline_op = (istopfunction(topmod, f, :+) || istopfunction(topmod, f, :*) ||
        istopfunction(topmod, f, :min) || istopfunction(topmod, f, :max)) &&
        (4 <= length(argexprs) <= 10) && methsig == Tuple{ft,Any,Any,Any,Vararg{Any}}
    if !inline_op && !inline_worthy(body, cost)
        if incompletematch
            # inline a typeassert-based call-site, rather than a
            # full generic lookup, using the inliner to handle
            # all the fiddly details
            numarg = length(argexprs)
            newnames = unique_names(ast,numarg)
            spnames = []
            spvals = []
            locals = []
            newcall = Expr(:call, e.args[1])
            newcall.typ = ty
            for i = 1:numarg
                name = newnames[i]
                argtype = exprtype(argexprs[i],sv)
                push!(locals, Any[name,argtype,0])
                push!(newcall.args, argtype===Any ? name : SymbolNode(name, argtype))
            end
            body.args = Any[Expr(:return, newcall)]
            ast = Expr(:lambda, newnames, Any[[], locals, [], 0], body)
            needcopy = false
        else
            return NF
        end
    end
    # remove empty meta
    body.args = filter(x->!(isa(x,Expr) && x.head === :meta && isempty(x.args)),
                       body.args)

    enc_vinflist = enclosing_ast.args[2][1]::Array{Any,1}
    enc_locllist = ast_localvars(enclosing_ast)
    locllist = ast_localvars(ast)

    # check for vararg function
    args = f_argnames(ast)
    na = length(args)

    isva = false
    if na > 0 && is_rest_arg(ast.args[1][na])
        if length(argexprs) < na - 1
            return (Expr(:call, TopNode(:error), "too few arguments"), [])
        end
        vaname = args[na]
        len_argexprs = length(argexprs)
        valen = len_argexprs-na+1
        if valen>0 && !occurs_outside_getfield(body, vaname, sv, valen, ())
            # argument tuple is not used as a whole, so convert function body
            # to one accepting the exact number of arguments we have.
            newnames = unique_names(ast,valen)
            if needcopy
                body = astcopy(body)
                needcopy = false
            end
            replace_getfield!(ast, body, vaname, newnames, (), sv, 1)
            args = vcat(args[1:na-1], newnames)
            na = length(args)

            # if the argument name is also used as a local variable,
            # we need to keep it around as a variable name
            for vi in vinflist
                if vi[1] === vaname
                    if vi[3] != 0
                        vnew = unique_name(enclosing_ast, ast)
                        push!(enc_vinflist, Any[vnew, vi[2], vi[3]])
                        push!(spnames, vaname)
                        push!(spvals, vnew)
                        push!(enc_locllist, vnew)
                    end
                    break
                end
            end
        else
            # construct tuple-forming expression for argument tail
            vararg = mk_tuplecall(argexprs[na:end], sv)
            argexprs = Any[argexprs[1:(na-1)]..., vararg]
            isva = true
        end
    elseif na != length(argexprs)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return NF
    end

    @assert na == length(argexprs)

    if argexprs === e.args
        argexprs = copy(argexprs)
    end
    if needcopy
        body = astcopy(body)
    end

    # avoid capturing free variables in enclosing function with the same name as in our function
    for localval in locllist
        localval = localval::Symbol
        vnew = gensym(localval)
        push!(spnames, localval)
        push!(spvals, vnew)
        push!(enc_locllist, vnew)
        for vi in vinflist
            if vi[1] === localval
                push!(enc_vinflist, Any[vnew, vi[2], vi[3]])
                break
            end
        end
    end

    # see if each argument occurs only once in the body expression
    stmts = Any[]
    stmts_free = true # true = all entries of stmts are effect_free

    # when 1 method matches the inferred types, there is still a chance
    # of a no-method error at run time, unless the inferred types are a
    # subset of the method signature.
    if incompletematch
        t = Expr(:call) # tuple(args...)
        t.typ = Tuple
        argexprs2 = t.args
        icall = LabelNode(label_counter(body.args)+1)
        partmatch = Expr(:gotoifnot, false, icall.label)
        thrw = Expr(:call, :throw, Expr(:call, GlobalRef(Main.Base,:MethodError), Expr(:call, top_tuple, e.args[1], QuoteNode(:inline)), t))
        thrw.typ = Bottom
    end

    for i=na:-1:1 # stmts_free needs to be calculated in reverse-argument order
        args_i = args[i]
        aei = argexprs[i]
        aeitype = argtype = exprtype(aei,sv)
        needtypeassert = false
        if incompletematch
            if isva
                if nm == 0
                    methitype = Tuple{}
                elseif i > nm
                    methitype = methargs[end]
                    if isvarargtype(methitype)
                        methitype = Tuple{methitype}
                    else
                        methitype = Tuple{}
                    end
                else
                    methitype = tupletype_tail(metharg,i)
                end
                isva = false
            else
                if i < nm
                    methitype = methargs[i]
                else
                    methitype = methargs[end]
                    if isvarargtype(methitype)
                        methitype = methitype.parameters[1]
                    else
                        @assert i==nm
                    end
                end
            end
            if isa(methitype, TypeVar)
                methitype = methitype.ub
            end
            if !(aeitype <: methitype)
                #TODO: make Undef a faster special-case?
                needtypeassert = true
                aeitype = methitype
            end
        end

        islocal = false # if the argument name is also used as a local variable,
                        # we need to keep it as a variable name
        for vi in vinflist
            if vi[1] === args_i && vi[3] != 0
                islocal = true
                aeitype = tmerge(aeitype, vi[2])
                if aeitype === Any
                    break
                end
            end
        end

        # ok for argument to occur more than once if the actual argument
        # is a symbol or constant, or is not affected by previous statements
        # that will exist after the inlining pass finishes
        if needtypeassert
            vnew1 = unique_name(enclosing_ast, ast)
            add_variable(enclosing_ast, vnew1, aeitype, !islocal)
            v1 = (aeitype===Any ? vnew1 : SymbolNode(vnew1,aeitype))
            push!(spnames, args_i)
            push!(spvals, v1)
            vnew2 = unique_name(enclosing_ast, ast)
            v2 = (argtype===Any ? vnew2 : SymbolNode(vnew2,argtype))
            unshift!(body.args, Expr(:(=), args_i, v2))
            args[i] = args_i = vnew2
            islocal = false
            aeitype = argtype
            affect_free = stmts_free
            occ = 3
            # it's really late in codegen, so we expand the typeassert manually: cond = !isa(vnew2, methitype) | cond
            cond = Expr(:call, Intrinsics.isa, v2, methitype)
            cond.typ = Bool
            cond = Expr(:call, Intrinsics.not_int, cond)
            cond.typ = Bool
            cond = Expr(:call, Intrinsics.or_int, cond, partmatch.args[1])
            cond.typ = Bool
            cond = Expr(:call, Intrinsics.box, Bool, cond)
            cond.typ = Bool
            partmatch.args[1] = cond
        else
            affect_free = stmts_free && !islocal # false = previous statements might affect the result of evaluating argument
            occ = 0
            for j = length(body.args):-1:1
                b = body.args[j]
                if occ < 6
                    occ += occurs_more(b, x->is(x,args_i), 6)
                end
                if occ > 0 && affect_free && !effect_free(b, sv, true) #TODO: we could short-circuit this test better by memoizing effect_free(b) in the for loop over i
                    affect_free = false
                end
                if occ > 5 && !affect_free
                    break
                end
            end
        end
        free = effect_free(aei,sv,true)
        if ((occ==0 && is(aeitype,Bottom)) || islocal || (occ > 1 && !inline_worthy(aei, occ*2000)) ||
                (affect_free && !free) || (!affect_free && !effect_free(aei,sv,false)))
            if occ != 0 # islocal=true is implied by occ!=0
                if !islocal
                    vnew = newvar!(sv, aeitype)
                    argexprs[i] = vnew
                else
                    vnew = unique_name(enclosing_ast, ast)
                    add_variable(enclosing_ast, vnew, aeitype, #=SSA=#false)
                    argexprs[i] = aeitype===Any ? vnew : SymbolNode(vnew,aeitype)
                end
                unshift!(stmts, Expr(:(=), vnew, aei))
                stmts_free &= free
            elseif !free && !isType(aeitype)
                unshift!(stmts, aei)
                stmts_free = false
            end
        end
        if incompletematch
            unshift!(argexprs2, (argtype===Any ? args_i : SymbolNode(a,argtype)))
        end
    end
    if incompletematch && partmatch.args[1] != false
        unshift!(body.args, icall)
        unshift!(body.args, thrw)
        unshift!(body.args, partmatch)
        unshift!(argexprs2, top_tuple)
    end

    # re-number the GenSyms and copy their type-info to the new ast
    gensym_types = ast.args[2][3]
    if gensym_types != 0
        if (isa(gensym_types,Integer))
            gensym_types = Any[Any for i = 1:ast.args[2][3]]
        end
        if !isempty(gensym_types)
            incr = length(sv.gensym_types)
            if incr != 0
                body = gensym_increment(body, incr)
            end
            append!(sv.gensym_types, ast.args[2][3])
        end
    end

    # ok, substitute argument expressions for argument names in the body
    body = sym_replace(body, args, spnames, argexprs, spvals)

    # make labels / goto statements unique
    newlabels = zeros(Int,label_counter(body.args)+1)
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a,LabelNode)
            a = a::LabelNode
            newlabel = genlabel(sv)
            newlabels[a.label+1] = newlabel.label
            body.args[i] = newlabel
        end
    end
    for i = 1:length(body.args)
        a = body.args[i]
        if isa(a,GotoNode)
            a = a::GotoNode
            body.args[i] = GotoNode(newlabels[a.label+1])
        elseif isa(a,Expr)
            a = a::Expr
            if a.head === :enter
                a.args[1] = newlabels[a.args[1]+1]
            elseif a.head === :gotoifnot
                a.args[2] = newlabels[a.args[2]+1]
            end
        end
    end

    # convert return statements into a series of goto's
    retstmt = genlabel(sv)
    retval = unique_name(enclosing_ast, ast)
    multiret = false
    lastexpr = pop!(body.args)
    if isa(lastexpr,LabelNode)
        push!(body.args, lastexpr)
        push!(body.args, Expr(:call, TopNode(:error), "fatal error in type inference"))
        lastexpr = nothing
    else
        @assert isa(lastexpr,Expr) "inference.jl:1774"
        @assert is(lastexpr.head,:return) "inference.jl:1775"
    end
    for a in body.args
        push!(stmts, a)
        if isa(a,Expr)
            a = a::Expr
            if a.head === :return
                multiret = true
                unshift!(a.args, retval)
                a.head = :(=)
                push!(stmts, GotoNode(retstmt.label))
            end
        end
    end

    if multiret
        rettype = (ast.args[3]::Expr).typ
        add_variable(enclosing_ast, retval, rettype, #=SSA=#false)
        if lastexpr !== nothing
            unshift!(lastexpr.args, retval)
            lastexpr.head = :(=)
            push!(stmts, lastexpr)
        end
        push!(stmts, retstmt)
        expr = rettype===Any ? retval : SymbolNode(retval,rettype)
    else
        expr = lastexpr.args[1]
    end

    if length(stmts) == 1
        # remove line number when inlining a single expression. see issue #13725
        s = stmts[1]
        if isa(s,Expr)&&is(s.head,:line) || isa(s,LineNumberNode)
            pop!(stmts)
        end
    end

    if !isempty(stmts) && !propagate_inbounds
        # inlined statements are out-of-bounds by default
        unshift!(stmts, Expr(:inbounds, false))
        push!(stmts, Expr(:inbounds, :pop))
    end

    if isa(expr,Expr)
        old_t = e.typ
        if old_t <: expr.typ
            expr.typ = old_t
        end
    end
    return (expr, stmts)
end
# The inlining incomplete matches optimization currently
# doesn't work on Tuples of TypeVars
const inline_incompletematch_allowed = false

inline_worthy(body, cost::Integer) = true
function inline_worthy(body::Expr, cost::Integer=1000) # precondition: 0 < cost; nominal cost = 1000
    if popmeta!(body, :inline)[1]
        return true
    end
    if popmeta!(body, :noinline)[1]
        return false
    end
    symlim = 1000 + 5_000_000 ÷ cost
    nargs = 0
    for arg in body.args
        if (!isa(arg, LineNumberNode) &&
            !(isa(arg, Expr) && (arg::Expr).head === :line))
            nargs += 1
        end
    end
    if nargs < (symlim + 500) ÷ 1000
        symlim *= 16
        symlim ÷= 1000
        if occurs_more(body, e->(!isa(e, LineNumberNode)), symlim) < symlim
            return true
        end
    end
    return false
end

gensym_increment(body, incr) = body
gensym_increment(body::GenSym, incr) = GenSym(body.id + incr)
function gensym_increment(body::Expr, incr)
    if body.head === :line
        return body
    end
    for i in 1:length(body.args)
        body.args[i] = gensym_increment(body.args[i], incr)
    end
    return body
end

const top_setfield = TopNode(:setfield)
const top_getfield = TopNode(:getfield)
const top_tuple = TopNode(:tuple)

function mk_getfield(texpr, i, T)
    e = :(($top_getfield)($texpr, $i))
    e.typ = T
    e
end

function mk_tuplecall(args, sv::VarInfo)
    e = Expr(:call, top_tuple, args...)
    e.typ = tuple_tfunc(Tuple{Any[exprtype(x,sv) for x in args]...})
    e
end

const corenumtype = Union{Int32,Int64,Float32,Float64}

function inlining_pass(e::Expr, sv, ast)
    if e.head === :method
        # avoid running the inlining pass on function definitions
        return (e,())
    end
    eargs = e.args
    if length(eargs)<1
        return (e,())
    end
    stmts = []
    if e.head === :body
        i = 1
        while i <= length(eargs)
            ei = eargs[i]
            if isa(ei,Expr)
                res = inlining_pass(ei, sv, ast)
                eargs[i] = res[1]
                if isa(res[2],Array)
                    sts = res[2]::Array{Any,1}
                    for j = 1:length(sts)
                        insert!(eargs, i, sts[j])
                        i += 1
                    end
                end
            end
            i += 1
        end
        return (e, stmts)
    end
    arg1 = eargs[1]
    # don't inline first (global) arguments of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall (or try to move the variables out into the function)
    if is_known_call(e, Core.Intrinsics.ccall, sv)
        i0 = 5
        isccall = true
    elseif is_known_call(e, Core.Intrinsics.llvmcall, sv)
        i0 = 5
        isccall = false
    else
        i0 = 1
        isccall = false
    end
    has_stmts = false # needed to preserve order-of-execution
    for i=length(eargs):-1:i0
        ei = eargs[i]
        if isa(ei,Expr)
            if ei.head === :&
                argloc = (ei::Expr).args
                i = 1
                ei = argloc[1]
                if !isa(ei,Expr)
                    continue
                end
            else
                argloc = eargs
            end
            res = inlining_pass(ei::Expr, sv, ast)
            res1 = res[1]
            if has_stmts && !effect_free(res1, sv, false)
                restype = exprtype(res1,sv)
                vnew = newvar!(sv, restype)
                argloc[i] = vnew
                unshift!(stmts, Expr(:(=), vnew, res1))
            else
                argloc[i] = res1
            end
            if isa(res[2],Array)
                res2 = res[2]::Array{Any,1}
                if !isempty(res2)
                    prepend!(stmts,res2)
                    if !has_stmts
                        for stmt in res2
                            if !effect_free(stmt, sv, true)
                                has_stmts = true
                            end
                        end
                    end
                end
            end
        end
    end
    if e.head !== :call
        return (e, stmts)
    end
    if isccall
        le = length(eargs)
        for i=5:2:le-1
            if eargs[i] === eargs[i+1]
                eargs[i+1] = 0
            end
        end
    end

    f = isconstantfunc(arg1, sv)
    if !is(f,false)
        f = _ieval(f,sv); ft = abstract_eval_constant(f)
    else
        f = nothing
        ft = exprtype(arg1, sv)
        if !( isleaftype(ft) || ft<:Type )
            return (e, stmts)
        end
    end

    if isdefined(Main, :Base) &&
        ((isdefined(Main.Base, :^) && is(f, Main.Base.(:^))) ||
         (isdefined(Main.Base, :.^) && is(f, Main.Base.(:.^))))
        if length(e.args) == 3 && isa(e.args[3],Union{Int32,Int64})
            a1 = e.args[2]
            basenumtype = Union{corenumtype, Main.Base.Complex64, Main.Base.Complex128, Main.Base.Rational}
            if isa(a1,basenumtype) || ((isa(a1,Symbol) || isa(a1,SymbolNode) || isa(a1,GenSym)) &&
                                       exprtype(a1,sv) <: basenumtype)
                if e.args[3]==2
                    e.args = Any[GlobalRef(Main.Base,:*), a1, a1]
                    f = Main.Base.(:*); ft = abstract_eval_constant(f)
                elseif e.args[3]==3
                    e.args = Any[GlobalRef(Main.Base,:*), a1, a1, a1]
                    f = Main.Base.(:*); ft = abstract_eval_constant(f)
                end
            end
        end
    end

    for ninline = 1:100
        ata = cell(length(e.args))
        ata[1] = ft
        for i = 2:length(e.args)
            a = exprtype(e.args[i], sv)
            (a === Bottom || isvarargtype(a)) && return (e, stmts)
            ata[i] = a
        end
        res = inlineable(f, ft, e, ata, sv, ast)
        if isa(res,Tuple)
            if isa(res[2],Array) && !isempty(res[2])
                append!(stmts,res[2])
            end
            res = res[1]
        end

        if !is(res,NF)
            # iteratively inline apply(f, tuple(...), tuple(...), ...) in order
            # to simplify long vararg lists as in multi-arg +
            if isa(res,Expr) && is_known_call(res, _apply, sv)
                e = res::Expr
                f = _apply; ft = abstract_eval_constant(f)
            else
                return (res,stmts)
            end
        end

        if is(f,_apply)
            na = length(e.args)
            newargs = cell(na-2)
            for i = 3:na
                aarg = e.args[i]
                t = exprtype(aarg,sv)
                if isa(aarg,Expr) && (is_known_call(aarg, tuple, sv) || is_known_call(aarg, svec, sv))
                    # apply(f,tuple(x,y,...)) => f(x,y,...)
                    newargs[i-2] = aarg.args[2:end]
                elseif isa(aarg, Tuple)
                    newargs[i-2] = Any[ QuoteNode(x) for x in aarg ]
                elseif isa(t,DataType) && t.name===Tuple.name && !isvatuple(t) && effect_free(aarg,sv,true)
                    # apply(f,t::(x,y)) => f(t[1],t[2])
                    tp = t.parameters
                    newargs[i-2] = Any[ mk_getfield(aarg,j,tp[j]) for j=1:length(tp) ]
                else
                    # not all args expandable
                    return (e,stmts)
                end
            end
            e.args = [Any[e.args[2]]; newargs...]

            # now try to inline the simplified call
            f = isconstantfunc(e.args[1], sv)
            if !is(f,false)
                f = _ieval(f,sv); ft = abstract_eval_constant(f)
            else
                f = nothing
                ft = exprtype(e.args[1], sv)
                if !( isleaftype(ft) || ft<:Type )
                    return (e,stmts)
                end
            end
        else
            return (e,stmts)
        end
    end
    return (e,stmts)
end

function add_variable(ast, name, typ, is_sa)
    vinf = Any[name, typ, 2+16*is_sa]
    vinflist = ast.args[2][1]::Array{Any,1}
    push!(vinflist, vinf)
end

const some_names = Symbol[:_var0, :_var1, :_var2, :_var3, :_var4, :_var5, :_var6,
                          :_var7, :_var8, :_var9, :_var10, :_var11, :_var12,
                          :_var13, :_var14, :_var15, :_var16, :_var17, :_var18,
                          :_var19, :_var20, :_var21, :_var22, :_var23, :_var24]
function contains_is1(vinflist::Array{Any,1}, x::Symbol)
    for y in vinflist
        if is(y[1],x)
            return true
        end
    end
    return false
end
function unique_name(ast)
    locllist = ast.args[2][1]::Array{Any,1}
    for g in some_names
        if !contains_is1(locllist, g)
            return g
        end
    end
    g = gensym()
    while contains_is1(locllist, g)
        g = gensym()
    end
    g
end
function unique_name(ast1, ast2)
    locllist1 = ast1.args[2][1]::Array{Any,1}
    locllist2 = ast2.args[2][1]::Array{Any,1}
    for g in some_names
        if !contains_is1(locllist1, g) &&
           !contains_is1(locllist2, g)
            return g
        end
    end
    g = gensym()
    while contains_is1(locllist1, g) |
          contains_is1(locllist2, g)
        g = gensym()
    end
    g
end

function unique_names(ast, n)
    ns = []
    locllist = ast.args[2][1]::Array{Any,1}
    for g in some_names
        if !contains_is1(locllist, g)
            push!(ns, g)
            if length(ns)==n
                return ns
            end
        end
    end
    while length(ns)<n
        g = gensym()
        while contains_is1(locllist, g) || contains_is(ns, g)
            g = gensym()
        end
        push!(ns, g)
    end
    ns
end

function is_known_call(e::Expr, func, sv)
    if e.head !== :call
        return false
    end
    f = isconstantfunc(e.args[1], sv)
    return !is(f,false) && is(_ieval(f,sv), func)
end

function is_known_call_p(e::Expr, pred, sv)
    if e.head !== :call
        return false
    end
    f = isconstantfunc(e.args[1], sv)
    return !is(f,false) && pred(_ieval(f,sv))
end

function is_var_assigned(ast, v)
    for vi in ast.args[2][1]
        if symequal(vi[1], v) && (vi[3]&2)!=0
            return true
        end
    end
    return false
end

function delete_var!(ast, v)
    if !isa(v, GenSym)
        filter!(vi->!symequal(vi[1],v), ast.args[2][1])
    end
    filter!(x->!(isa(x,Expr) && (x.head === :(=) || x.head === :const) &&
                 symequal(x.args[1],v)),
            ast.args[3].args)
    ast
end

# remove all single-assigned vars v in "v = x" where x is an argument
# and not assigned.
# "sa" is the result of find_sa_vars
function remove_redundant_temp_vars(ast, sa)
    varinfo = ast.args[2][1]
    gensym_types = ast.args[2][3]
    body = ast.args[3]
    for (v,init) in sa
        if ((isa(init,Symbol) || isa(init,SymbolNode)) &&
            _any(vi->symequal(vi[1],init), varinfo) &&
            !is_var_assigned(ast, init))

            # this transformation is not valid for vars used before def.
            # we need to preserve the point of assignment to know where to
            # throw errors (issue #4645).
            if !occurs_undef(v, body, varinfo)

                # the transformation is not ideal if the assignment
                # is present for the auto-unbox functionality
                # (from inlining improved type inference information)
                # and this transformation would worsen the type information
                # everywhere later in the function
                if (isa(init,SymbolNode) ? (init.typ <: (isa(v,GenSym)?gensym_types[(v::GenSym).id+1]:local_typeof(v, varinfo))) : true)
                    delete_var!(ast, v)
                    sym_replace(body, Any[v], Void[], Any[init], Void[])
                end
            end
        end
    end
    ast
end

function local_typeof(v, varinfo)
    for (v2, typ, info) in varinfo
        v === v2 && return typ
    end
    @assert false "v not in varinfo"
end
function var_infobits(v, varinfo)
    for (v2, typ, info) in varinfo
        v === v2 && return info
    end
    @assert false "v not in varinfo"
end

occurs_undef(var::GenSym, expr, varinfo) = false

occurs_undef(var, expr, varinfo) =
    occurs_more(expr, e->(isa(e,SymbolNode) && symequal(var,e) &&
                          ((var_infobits(e.name,varinfo)&32)!=0)), 0)>0

# compute set of vars assigned once
function find_sa_vars(ast)
    body = ast.args[3].args
    av = ObjectIdDict()
    av2 = ObjectIdDict()
    vinfos = ast.args[2][1]::Array{Any,1}
    args = ast.args[1]
    for i = 1:length(body)
        e = body[i]
        if isa(e,Expr) && is(e.head,:(=))
            lhs = e.args[1]
            if isa(lhs,GenSym)
                av[lhs] = e.args[2]
            elseif isa(lhs,SymbolNode)
                av2[(lhs::SymbolNode).name] = true
            elseif isa(lhs, Symbol)
                lhs = lhs::Symbol
                if contains_is1(vinfos,lhs) && !contains_is(args,lhs) # exclude globals & args
                    if !haskey(av, lhs)
                        av[lhs] = e.args[2]
                    else
                        av2[lhs] = true
                    end
                end
            end
        end
    end
    filter!((var,_)->!haskey(av2,var), av)
    for vi in vinfos
        if (vi[3]&1)!=0
            # remove captured vars
            delete!(av, vi[1])
        end
    end
    av
end

symequal(x::SymbolNode, y::SymbolNode) = is(x.name,y.name)
symequal(x::SymbolNode, y::Symbol)     = is(x.name,y)
symequal(x::Symbol    , y::SymbolNode) = is(x,y.name)
symequal(x::GenSym    , y::GenSym)     = is(x.id,y.id)
symequal(x::ANY       , y::ANY)        = is(x,y)

function occurs_outside_getfield(e::ANY, sym::ANY, sv::VarInfo, field_count, field_names)
    if is(e, sym) || (isa(e, SymbolNode) && is(e.name, sym))
        return true
    end
    if isa(e,Expr)
        e = e::Expr
        if is_known_call(e, getfield, sv) && symequal(e.args[2],sym)
            idx = e.args[3]
            if isa(idx,QuoteNode) && (idx.value in field_names)
                return false
            end
            if isa(idx,Int) && (1 <= idx <= field_count)
                return false
            end
            return true
        end
        if is(e.head,:(=))
            return occurs_outside_getfield(e.args[2], sym, sv, field_count, field_names)
        else
            for a in e.args
                if occurs_outside_getfield(a, sym, sv, field_count, field_names)
                    return true
                end
            end
        end
    end
    return false
end

# removes inbounds metadata if we never encounter an inbounds=true or
# boundscheck context in the method body
function inbounds_meta_elim_pass(e::Expr)
    if findfirst(x -> isa(x, Expr) &&
                      ((x.head === :inbounds && x.args[1] == true) || x.head === :boundscheck),
                 e.args) == 0
        filter!(x -> !(isa(x, Expr) && x.head === :inbounds), e.args)
    end
end

# does the same job as alloc_elim_pass for allocations inline in getfields
# TODO can probably be removed when we switch to a linear IR
function getfield_elim_pass(e::Expr, sv)
    for i = 1:length(e.args)
        ei = e.args[i]
        if isa(ei,Expr)
            getfield_elim_pass(ei, sv)
            if is_known_call(ei, getfield, sv) && length(ei.args)==3 &&
                (isa(ei.args[3],Int) || isa(ei.args[3],QuoteNode))
                e1 = ei.args[2]
                j = ei.args[3]
                if isa(e1,Expr)
                    alloc = is_immutable_allocation(e1, sv)
                    if !is(alloc, false)
                        flen, fnames = alloc
                        if isa(j,QuoteNode)
                            j = findfirst(fnames, j.value)
                        end
                        if 1 <= j <= flen
                            ok = true
                            for k = 2:length(e1.args)
                                k == j+1 && continue
                                if !effect_free(e1.args[k], sv, true)
                                    ok = false; break
                                end
                            end
                            if ok
                                e.args[i] = e1.args[j+1]
                            end
                        end
                    end
                elseif isa(e1,Tuple) && isa(j,Int) && (1 <= j <= length(e1))
                    e1j = e1[j]
                    if !(isa(e1j,Number) || isa(e1j,AbstractString) || isa(e1j,Tuple) ||
                         isa(e1j,Type))
                        e1j = QuoteNode(e1j)
                    end
                    e.args[i] = e1j
                elseif isa(e1,QuoteNode) && isa(e1.value,Tuple) && isa(j,Int) && (1 <= j <= length(e1.value))
                    e.args[i] = QuoteNode(e1.value[j])
                end
            end
        end
    end
end

# check if e is a successful allocation of an immutable struct
# if it is, returns (n,f) such that it is always valid to call
# getfield(..., 1 <= x <= n) or getfield(..., x in f) on the result
function is_immutable_allocation(e :: ANY, sv::VarInfo)
    isa(e, Expr) || return false
    if is_known_call(e, tuple, sv)
        return (length(e.args)-1,())
    elseif e.head === :new
        typ = exprtype(e, sv)
        if isleaftype(typ) && !typ.mutable
            @assert(isa(typ,DataType))
            nf = length(e.args)-1
            names = fieldnames(typ)
            @assert(nf <= nfields(typ))
            if nf < nfields(typ)
                # some fields were left undef
                # we could potentially propagate Bottom
                # for pointer fields
                names = names[1:nf]
            end
            return (nf, names)
        end
    end
    false
end
# eliminate allocation of unnecessary immutables
# that are only used as arguments to safe getfield calls
function alloc_elim_pass(ast::Expr, sv::VarInfo)
    bexpr = ast.args[3]::Expr
    body = (ast.args[3].args)::Array{Any,1}
    vs = find_sa_vars(ast)
    remove_redundant_temp_vars(ast, vs)
    i = 1
    while i < length(body)
        e = body[i]
        if !(isa(e,Expr) && is(e.head,:(=)) && (isa(e.args[1], GenSym) || haskey(vs, e.args[1])))
            i += 1
            continue
        end
        var = e.args[1]
        rhs = e.args[2]
        alloc = is_immutable_allocation(rhs, sv)
        if !is(alloc,false)
            nv, field_names = alloc
            tup = rhs.args
            if occurs_outside_getfield(bexpr, var, sv, nv, field_names) || !is_local(sv, var)
                i += 1
                continue
            end

            deleteat!(body, i)  # remove tuple allocation
            # convert tuple allocation to a series of local var assignments
            vals = cell(nv)
            n_ins = 0
            for j=1:nv
                tupelt = tup[j+1]
                if isa(tupelt,Number) || isa(tupelt,AbstractString) || isa(tupelt,QuoteNode)
                    vals[j] = tupelt
                else
                    elty = exprtype(tupelt,sv)
                    tmpv = newvar!(sv, elty)
                    tmp = Expr(:(=), tmpv, tupelt)
                    insert!(body, i+n_ins, tmp)
                    vals[j] = tmpv
                    n_ins += 1
                end
            end
            i += n_ins
            replace_getfield!(ast, bexpr, var, vals, field_names, sv, i)
        else
            i += 1
        end
    end
end

function replace_getfield!(ast, e::ANY, tupname, vals, field_names, sv, i0)
    if !isa(e,Expr)
        return
    end
    for i = i0:length(e.args)
        a = e.args[i]
        if isa(a,Expr) && is_known_call(a, getfield, sv) &&
            symequal(a.args[2],tupname)
            idx = if isa(a.args[3], Int)
                a.args[3]
            else
                @assert isa(a.args[3], QuoteNode)
                findfirst(field_names, a.args[3].value)
            end
            @assert(idx > 0) # clients should check that all getfields are valid
            val = vals[idx]
            # original expression might have better type info than
            # the tuple element expression that's replacing it.
            if isa(val,SymbolNode)
                val = val::SymbolNode
                if a.typ <: val.typ && !typeseq(a.typ,val.typ)
                    val.typ = a.typ
                    for vi in ast.args[2][1]::Array{Any,1}
                        if vi[1] === val.name
                            vi[2] = a.typ
                            break
                        end
                    end
                end
            elseif isa(val,GenSym)
                val = val::GenSym
                typ = exprtype(val, sv)
                if a.typ <: typ && !typeseq(a.typ,typ)
                    sv.gensym_types[val.id+1] = a.typ
                end
            end
            e.args[i] = val
        else
            replace_getfield!(ast, a, tupname, vals, field_names, sv, 1)
        end
    end
end

#tfunc(f,t) = methods(f,t)[1].func.code.tfunc

ccall(:jl_set_typeinf_func, Void, (Any,), typeinf_ext)
