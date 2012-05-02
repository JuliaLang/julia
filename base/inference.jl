# parameters limiting potentially-infinite types
const MAX_TYPEUNION_LEN = 2
const MAX_TYPEUNION_DEPTH = 2
const MAX_TUPLETYPE_LEN  = 8
const MAX_TUPLE_DEPTH = 4

type NotFound
end

const NF = NotFound()

type EmptyCallStack
end

type CallStack
    ast
    mod::Module
    types::Tuple
    recurred::Bool
    result
    prev::Union(EmptyCallStack,CallStack)

    CallStack(ast, mod, types, prev) = new(ast, mod, types, false, None, prev)
end

# TODO thread local
inference_stack = EmptyCallStack()

tintersect(a::ANY,b::ANY) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)
tmatch(a::ANY,b::ANY) = ccall(:jl_type_match, Any, (Any,Any), a, b)

getmethods(f,t) = getmethods(f,t,-1)::Array{Any,1}
getmethods(f,t,lim) = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, t, lim)

typeseq(a,b) = subtype(a,b)&&subtype(b,a)

isbuiltin(f) = ccall(:jl_is_builtin, Int32, (Any,), f) != 0
isgeneric(f) = (isa(f,Function)||isa(f,CompositeKind)) && isa(f.env,MethodTable)
isleaftype(t) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

isconst(s::Symbol) =
    ccall(:jl_is_const, Int32, (Ptr{Void}, Any), C_NULL, s) != 0

function _iisconst(s::Symbol)
    m = (inference_stack::CallStack).mod
    isbound(m,s) && (ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0)
end

_ieval(x) = eval((inference_stack::CallStack).mod, x)
_iisbound(x) = isbound((inference_stack::CallStack).mod, x)

_iisconst(s::SymbolNode) = _iisconst(s.name)
_iisconst(s::TopNode) = _iisconst(s.name)
_iisconst(x::Expr) = false
_iisconst(x) = true

cmp_tfunc = (x,y)->Bool

isType(t::ANY) = isa(t,AbstractKind) && is((t::AbstractKind).name,Type.name)

isseqtype(t::ANY) = isa(t,AbstractKind) && is((t::AbstractKind).name.name,:...)

const t_func = ObjectIdDict()
#t_func[tuple] = (0, Inf, (args...)->limit_tuple_depth(args))
t_func[throw] = (1, 1, x->None)
t_func[boxsi8] = (1, 1, x->Int8)
t_func[boxui8] = (1, 1, x->Uint8)
t_func[boxsi16] = (1, 1, x->Int16)
t_func[boxui16] = (1, 1, x->Uint16)
t_func[boxsi32] = (1, 1, x->Int32)
t_func[boxui32] = (1, 1, x->Uint32)
t_func[boxsi64] = (1, 1, x->Int64)
t_func[boxui64] = (1, 1, x->Uint64)
t_func[boxf32] = (1, 1, x->Float32)
t_func[boxf64] = (1, 1, x->Float64)
t_func[box] = (2, 2, (t,v)->(isType(t) ? t.parameters[1] : Any))
t_func[eq_int] = (2, 2, cmp_tfunc)
t_func[ne_int] = (2, 2, cmp_tfunc)
t_func[slt_int] = (2, 2, cmp_tfunc)
t_func[ult_int] = (2, 2, cmp_tfunc)
t_func[sle_int] = (2, 2, cmp_tfunc)
t_func[ule_int] = (2, 2, cmp_tfunc)
t_func[eq_float] = (2, 2, cmp_tfunc)
t_func[ne_float] = (2, 2, cmp_tfunc)
t_func[lt_float] = (2, 2, cmp_tfunc)
t_func[le_float] = (2, 2, cmp_tfunc)
t_func[eqfsi64] = (2, 2, cmp_tfunc)
t_func[eqfui64] = (2, 2, cmp_tfunc)
t_func[ltfsi64] = (2, 2, cmp_tfunc)
t_func[ltfui64] = (2, 2, cmp_tfunc)
t_func[lefsi64] = (2, 2, cmp_tfunc)
t_func[lefui64] = (2, 2, cmp_tfunc)
t_func[ltsif64] = (2, 2, cmp_tfunc)
t_func[ltuif64] = (2, 2, cmp_tfunc)
t_func[lesif64] = (2, 2, cmp_tfunc)
t_func[leuif64] = (2, 2, cmp_tfunc)
t_func[fpiseq32] = (2, 2, cmp_tfunc)
t_func[fpiseq64] = (2, 2, cmp_tfunc)
t_func[fpislt32] = (2, 2, cmp_tfunc)
t_func[fpislt64] = (2, 2, cmp_tfunc)
t_func[eval(Core,:ccall)] =
    (3, Inf, (fptr, rt, at, a...)->(is(rt,Type{Void}) ? Nothing :
                                    isType(rt) ? rt.parameters[1] : Any))
t_func[is] = (2, 2, cmp_tfunc)
t_func[subtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[isbound] = (1, 2, (args...)->Bool)
t_func[Union] = (0, Inf,
                 (args...)->(if allp(isType,args)
                                 Type{Union(map(t->t.parameters[1],args)...)}
                             else
                                 Type
                             end))
t_func[method_exists] = (2, 2, cmp_tfunc)
t_func[applicable] = (1, Inf, (f, args...)->Bool)
t_func[tuplelen] = (1, 1, x->Int)
t_func[arraylen] = (1, 1, x->Int)
t_func[arrayref] = (2, 2, (a,i)->(isa(a,CompositeKind) && subtype(a,Array) ?
                                  a.parameters[1] : Any))
t_func[arrayset] = (3, 3, (a,i,v)->a)
_jl_arraysize_tfunc(a, d) = Int
function _jl_arraysize_tfunc(a)
    if isa(a,CompositeKind) && subtype(a,Array)
        return NTuple{a.parameters[2],Int}
    else
        return NTuple{Array.parameters[2],Int}
    end
end
t_func[arraysize] = (1, 2, _jl_arraysize_tfunc)

function static_convert(to::ANY, from::ANY)
    if !isa(to,Tuple) || !isa(from,Tuple)
        return tintersect(from,to)
    end
    if is(to,Tuple)
        return from
    end
    pl = length(to); cl = length(from)
    pseq = false
    result = Array(Any, cl)
    for i=1:cl
        ce = from[i]
        if pseq
        elseif i <= pl
            pe = to[i]
            if isseqtype(pe)
                pe = pe.parameters[1]
                pseq = true
            end
        else
            return None
        end
        # tuple conversion calls convert recursively
        if isseqtype(ce)
            #R = abstract_call_gf(convert, (), (Type{pe}, ce.parameters[1]), ())
            R = static_convert(pe, ce.parameters[1])
            isType(R) && (R = R.parameters[1])
            result[i] = ...{R}
        else
            #R = abstract_call_gf(convert, (), (Type{pe}, ce), ())
            R = static_convert(pe, ce)
            isType(R) && (R = R.parameters[1])
            result[i] = R
        end
    end
    a2t(result)
end
t_func[convert_default] =
    (3, 3, (t,x,f)->(isType(t) ? static_convert(t.parameters[1],x) : Any))
t_func[convert_tuple] =
    (3, 3, (t,x,f)->(if isa(t,Tuple) && allp(isType,t)
                         t = Type{map(t->t.parameters[1],t)}
                     end;
                     isType(t) ? static_convert(t.parameters[1],x) :
                     Any))
typeof_tfunc = function (t)
    if isType(t)
        t = t.parameters[1]
        if isa(t,TypeVar)
            Type
        else
            Type{typeof(t)}
        end
    elseif isa(t,AbstractKind) || isa(t,CompositeKind) || isa(t,BitsKind)
        if isleaftype(t)
            Type{t}
        else
            Type{typevar(:_,t)}
        end
    elseif isa(t,UnionKind)
        Union(map(typeof_tfunc, t.types)...)
    elseif isa(t,Tuple)
        map(typeof_tfunc, t)
    elseif isa(t,TypeVar)
        Type{t}
    else
        Type
    end
end
t_func[typeof] = (1, 1, typeof_tfunc)
# involving constants: typeassert, tupleref, getfield, fieldtype, apply_type
# therefore they get their arguments unevaluated
t_func[typeassert] =
    (2, 2, (A, v, t)->(isType(t) ? tintersect(v,t.parameters[1]) :
                       isa(t,Tuple) && allp(isType,t) ?
                           tintersect(v,map(t->t.parameters[1],t)) :
                       Any))

tupleref_tfunc = function (A, t, i)
    if is(t,())
        return None
    end
    if isa(t,AbstractKind) && is(t.name,NTuple.name)
        return t.parameters[2]
    end
    if !isa(t,Tuple)
        return Any
    end
    n = length(t)
    last = tupleref(t,n)
    vararg = isseqtype(last)
    if isa(A[2],Integer)
        # index is a constant
        i = A[2]
        if i > n
            if vararg
                return last.parameters[1]
            else
                return None
            end
        elseif i == n && vararg
            return last.parameters[1]
        else
            return tupleref(t,i)
        end
    else
        # index unknown, could be anything from the tuple
        if vararg
            types = tuple(t[1:(n-1)]..., last.parameters[1])
        else
            types = t
        end
        return reduce(tmerge, None, types)
    end
end
t_func[tupleref] = (2, 2, tupleref_tfunc)

getfield_tfunc = function (A, s, name)
    if !isa(s,CompositeKind)
        return Any
    end
    if isa(A[2],QuoteNode) && isa(A[2].value,Symbol)
        fld = A[2].value
        for i=1:length(s.names)
            if is(s.names[i],fld)
                return s.types[i]
            end
        end
        return None
    else
        return reduce(tmerge, None, s.types)#Union(s.types...)
    end
end
t_func[getfield] = (2, 2, getfield_tfunc)
t_func[_setfield] = (3, 3, (o, f, v)->v)
fieldtype_tfunc = function (A, s, name)
    if !isa(s,CompositeKind)
        return Type
    end
    t = getfield_tfunc(A, s, name)
    if is(t,None)
        return t
    end
    Type{t}
end
t_func[fieldtype] = (2, 2, fieldtype_tfunc)
t_func[Expr] = (3, 3, (a,b,c)->Expr)
t_func[Box] = (1, 1, (a,)->Box)

# TODO: handle e.g. apply_type(T, R::Union(Type{Int32},Type{Float64}))
apply_type_tfunc = function (A, args...)
    if !isType(args[1])
        return Any
    end
    headtype = args[1].parameters[1]
    if isa(headtype,UnionKind) || isa(headtype,Tuple)
        return args[1]
    end
    tparams = ()
    uncertain = false
    for i=2:length(A)
        if isType(args[i])
            tparams = append(tparams, (args[i].parameters[1],))
        elseif isa(A[i],Int)
            tparams = append(tparams, (A[i],))
        else
            uncertain = true
            tparams = append(tparams, (headtype.parameters[i-1],))
        end
    end
    local appl
    # good, all arguments understood
    try
        appl = apply_type(headtype, tparams...)
    catch
        # type instantiation might fail if one of the type parameters
        # doesn't match, which could happen if a type estimate is too coarse
        appl = args[1]
        uncertain = true
    end
    uncertain ? Type{typevar(:_,appl)} : Type{appl}
end
t_func[apply_type] = (1, Inf, apply_type_tfunc)

# other: apply

function builtin_tfunction(f::ANY, args::ANY, argtypes::ANY)
    if is(f,tuple)
        return limit_tuple_depth(argtypes)
    end
    tf = get(t_func::ObjectIdDict, f, false)
    if is(tf,false)
        # struct constructor
        if isa(f, CompositeKind)
            return f
        end
        # unknown/unhandled builtin
        return Any
    end
    tf = tf::(Real, Real, Function)
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return None
    end
    if is(f,typeassert) || is(f,tupleref) || is(f,getfield) ||
       is(f,apply_type) || is(f,fieldtype)
        # TODO: case of apply(), where we do not have the args
        return tf[3](args, argtypes...)
    end
    return tf[3](argtypes...)
end

type StaticVarInfo
    sp::Tuple
    cenv::ObjectIdDict   # types of closed vars
end

function a2t(a::AbstractVector)
    n = length(a)
    if n==2 return (a[1],a[2]) end
    if n==1 return (a[1],) end
    if n==3 return (a[1],a[2],a[3]) end
    if n==0 return () end
    return tuple(a...)
end

function a2t_butfirst(a::AbstractVector)
    n = length(a)
    if n==2 return (a[2],) end
    if n<=1 return () end
    if n==3 return (a[2],a[3]) end
    t = (a[2],a[3],a[4])
    if n==4 return t end
    for i=5:n
        t = tuple(t..., a[i])
    end
    t
end

function isconstantfunc(f, vtypes, sv::StaticVarInfo)
    if isa(f,TopNode)
        return _iisconst(f.name) && f.name
    end
    if isa(f,SymbolNode)
        f = f.name
    end
    return isa(f,Symbol) && !has(vtypes,f) && !has(sv.cenv,f) &&
           _iisconst(f) && f
end

isvatuple(t::Tuple) = (n = length(t); n > 0 && isseqtype(t[n]))

limit_tuple_depth(t) = limit_tuple_depth(t,0)

function limit_tuple_depth(t,d)
    if isa(t,UnionKind)
        # also limit within Union types.
        # may have to recur into other stuff in the future too.
        return Union(limit_tuple_depth(t.types,d)...)
    end
    if !isa(t,Tuple)
        return t
    end
    if d > MAX_TUPLE_DEPTH
        return Tuple
    end
    map(x->limit_tuple_depth(x,d+1), t)
end

function limit_tuple_type(t)
    n = length(t)
    if n > MAX_TUPLETYPE_LEN
        last = t[n]
        if isseqtype(last)
            last = last.parameters[1]
        end
        tail = tuple(t[MAX_TUPLETYPE_LEN:(n-1)]..., last)
        tail = tintersect(reduce(tmerge, None, tail), Any)
        return tuple(t[1:(MAX_TUPLETYPE_LEN-1)]..., ...{tail})
    end
    return t
end

function abstract_call_gf(f, fargs, argtypes, e)
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always None.
    # here I picked 4.
    argtypes = limit_tuple_type(argtypes)
    applicable = getmethods(f, argtypes, 4)
    rettype = None
    if is(applicable,false)
        # this means too many methods matched
        if isa(e,Expr)
            e.head = :call
        end
        return Any
    end
    x::Array{Any,1} = applicable
    if isempty(x)
        # no methods match
        if is(f,method_missing)
            # match failure due to None (error) argument, propagate
            return None
        end
        return abstract_call_gf(method_missing, tuple(f, fargs...),
                                tuple(Function, argtypes...), ())
    end
    if isa(e,Expr)
        if length(x)==1
            # method match is unique; mark it
            e.head = :call1
        else
            e.head = :call
        end
    end
    for (m::Tuple) in x
        #print(m,"\n")
        if isa(m[3],Type)
            # constructor
            rt = m[3]
        else
            (_tree,rt) = typeinf(m[3], m[1], m[2], m[3])
        end
        rettype = tmerge(rettype, rt)
        if is(rettype,Any)
            break
        end
    end
    # if rettype is None we've found a method not found error
    #print("=> ", rettype, "\n")
    return rettype
end

function _jl_invoke_tfunc(f, types, argtypes)
    argtypes = tintersect(types,limit_tuple_type(argtypes))
    if is(argtypes,None)
        return None
    end
    applicable = getmethods(f, types)
    if isempty(applicable)
        return Any
    end
    for (m::Tuple) in applicable
        if typeseq(m[1],types)
            tvars = m[2][1:2:end]
            (ti, env) = ccall(:jl_match_method, Any, (Any,Any,Any),
                              argtypes, m[1], tvars)::(Any,Any)
            (_tree,rt) = typeinf(m[3], ti, env, m[3])
            return rt
        end
    end
    return Any
end

function abstract_call(f, fargs, argtypes, vtypes, sv::StaticVarInfo, e)
    if isbuiltin(f)
        if is(f,apply) && length(fargs)>0
            af = isconstantfunc(fargs[1], vtypes, sv)
            if !is(af,false) && _iisbound(af)
                aargtypes = argtypes[2:]
                if allp(x->isa(x,Tuple), aargtypes) &&
                   !anyp(isvatuple, aargtypes[1:(length(aargtypes)-1)])
                    e.head = :call1
                    # apply with known func with known tuple types
                    # can be collapsed to a call to the applied func
                    at = length(aargtypes) > 0 ?
                         limit_tuple_type(append(aargtypes...)) : ()
                    return abstract_call(_ieval(af), (), at, vtypes, sv, ())
                end
            end
        end
        if is(f,invoke) && length(fargs)>1
            af = isconstantfunc(fargs[1], vtypes, sv)
            if !is(af,false) && _iisbound(af) && (af=_ieval(af);isgeneric(af))
                sig = argtypes[2]
                if isa(sig,Tuple) && allp(isType, sig)
                    sig = map(t->t.parameters[1], sig)
                    return _jl_invoke_tfunc(af, sig, argtypes[3:])
                end
            end
        end
        if !is(f,apply) && isa(e,Expr) && (isa(f,Function) || isa(f,IntrinsicFunction))
            e.head = :call1
        end
        rt = builtin_tfunction(f, fargs, argtypes)
        #print("=> ", rt, "\n")
        return rt
    elseif isgeneric(f)
        return abstract_call_gf(f, fargs, argtypes, e)
    else
        #print("=> ", Any, "\n")
        return Any
    end
end

function abstract_eval_call(e, vtypes, sv::StaticVarInfo)
    fargs = a2t_butfirst(e.args)
    argtypes = map(x->abstract_eval(x,vtypes,sv), fargs)
    if anyp(x->is(x,None), argtypes)
        return None
    end
    func = isconstantfunc(e.args[1], vtypes, sv)
    if is(func,false)
        # TODO: lambda expression (let)
        ft = abstract_eval(e.args[1], vtypes, sv)
        if isType(ft) && isa(ft.parameters[1],CompositeKind)
            st = ft.parameters[1]
            if isgeneric(st) && isleaftype(st)
                return abstract_call_gf(st, fargs, argtypes, e)
            end
            # struct constructor
            return st
        end
        return Any
    end
    #print("call ", e.args[1], argtypes, " ")
    if !_iisbound(func)
        #print("=> ", Any, "\n")
        return Any
    end
    f = _ieval(func)
    return abstract_call(f, fargs, argtypes, vtypes, sv, e)
end

function abstract_eval(e::Expr, vtypes, sv::StaticVarInfo)
    # handle:
    # call  lambda  null  isbound static_typeof
    if is(e.head,:call) || is(e.head,:call1)
        t = abstract_eval_call(e, vtypes, sv)
    #elseif is(e.head,:isbound)
    #    t = Bool
    elseif is(e.head,:null)
        t = Nothing
    elseif is(e.head,:new)
        t = abstract_eval(e.args[1], vtypes, sv)
        if isType(t)
            t = t.parameters[1]
        else
            t = Any
        end
    elseif is(e.head,:&)
        abstract_eval(e.args[1], vtypes, sv)
        t = Any
    elseif is(e.head,:static_typeof)
        t = abstract_eval(e.args[1], vtypes, sv)
        # intersect with Any to remove Undef
        t = tintersect(t, Any)
        if isleaftype(t)
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
                t = Type{typevar(:_,t)}
            end
        end
    else
        t = Any
    end
    e.typ = t
    return t
end

function abstract_eval(e::QuoteNode, vtypes, sv::StaticVarInfo)
    return typeof(e.value)
end

function abstract_eval(e::TopNode, vtypes, sv::StaticVarInfo)
    return abstract_eval_global(e.name)
end

const _jl_Type_Array = Type{Array}

function abstract_eval_constant(x::ANY)
    if isa(x,AbstractKind) || isa(x,BitsKind) || isa(x,CompositeKind) ||
        isa(x,UnionKind) || isa(x,TypeConstructor)
        if is(x,Array)
            return _jl_Type_Array
        end
        return Type{x}
    end
    if isa(x,LambdaStaticData)
        return Function
    end
    return typeof(x)
end

# Undef is the static type of a value location (e.g. variable) that is
# undefined. The corresponding run-time type is None, since accessing an
# undefined location is an error. A non-lvalue expression cannot have
# type Undef, only None.
# typealias Top Union(Any,Undef)

function abstract_eval_global(s::Symbol)
    if !_iisbound(s)
        return Top
    end
    if _iisconst(s)
        return abstract_eval_constant(_ieval(s))
    else
        # TODO: change to Undef if there's a way to clear variables
        return Any
    end
end

function abstract_eval(s::Symbol, vtypes, sv::StaticVarInfo)
    if has(sv.cenv,s)
        # consider closed vars to always have their propagated (declared) type
        return sv.cenv[s]
    end
    t = is(vtypes,()) ? NF : get(vtypes,s,NF)
    if is(t,NF)
        sp = sv.sp
        for i=1:2:length(sp)
            if is(sp[i].name,s)
                # static parameter
                val = sp[i+1]
                if isa(val,TypeVar)
                    # static param bound to typevar
                    return Type{val}
                end
                return abstract_eval_constant(val)
            end
        end
        # global
        return abstract_eval_global(s)
    end
    return t
end

abstract_eval(s::SymbolNode, vtypes, sv::StaticVarInfo) =
    abstract_eval(s.name, vtypes, sv)

abstract_eval(x, vtypes, sv::StaticVarInfo) = abstract_eval_constant(x)

typealias VarTable ObjectIdDict

type StateUpdate
    var::Symbol
    vtype
    state::VarTable
end

function ref(x::StateUpdate, s::Symbol)
    if is(x.var,s)
        return x.vtype
    end
    return get(x.state,s,NF)
end

_jl_abstract_interpret(e, vtypes, sv::StaticVarInfo) = vtypes

function _jl_abstract_interpret(e::Expr, vtypes, sv::StaticVarInfo)
    # handle assignment
    if is(e.head,:(=))
        t = abstract_eval(e.args[2], vtypes, sv)
        lhs = e.args[1]
        if isa(lhs,SymbolNode)
            lhs = lhs.name
        end
        assert(isa(lhs,Symbol), "inference.jl:579")
        return StateUpdate(lhs, t, vtypes)
    elseif is(e.head,:call) || is(e.head,:call1)
        abstract_eval(e, vtypes, sv)
    elseif is(e.head,:gotoifnot)
        abstract_eval(e.args[1], vtypes, sv)
    elseif is(e.head,:method)
        return StateUpdate(e.args[1], Function, vtypes)
    end
    return vtypes
end

tchanged(n::ANY, o::ANY) = is(o,NF) || (!is(n,NF) && !subtype(n,o))

function stchanged(new::Union(StateUpdate,VarTable), old, vars)
    if is(old,())
        return true
    end
    for i = 1:length(vars)
        v = vars[i]
        if tchanged(new[v], get(old,v,NF))
            return true
        end
    end
    return false
end

function type_too_complex(t, d)
    if d > MAX_TYPEUNION_DEPTH
        return true
    end
    if isa(t,UnionKind)
        p = t.types
    elseif isa(t,CompositeKind) || isa(t,AbstractKind) || isa(t,BitsKind)
        p = t.parameters
    elseif isa(t,Tuple)
        p = t
    elseif isa(t,TypeVar)
        return type_too_complex(t.lb,d+1) || type_too_complex(t.ub,d+1)
    else
        return false
    end
    for x = (p::Tuple)
        if type_too_complex(x, d+1)
            return true
        end
    end
    return false
end

function tmerge(typea::ANY, typeb::ANY)
    if is(typea,NF)
        return typeb
    end
    if is(typeb,NF)
        return typea
    end
    if subtype(typea,typeb)
        return typeb
    end
    if subtype(typeb,typea)
        return typea
    end
    u = Union(typea, typeb)
    if length(u.types) > MAX_TYPEUNION_LEN || type_too_complex(u, 0)
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return subtype(Undef,u) ? Top : Any
    end
    return u
end

function stupdate(state, changes::Union(StateUpdate,VarTable), vars)
    if is(state,())
        state = ObjectIdDict()
    end
    for i = 1:length(vars)
        v = vars[i]
        newtype = changes[v]
        oldtype = get(state::ObjectIdDict,v,NF)
        if tchanged(newtype, oldtype)
            state[v] = tmerge(oldtype, newtype)
        end
    end
    state
end

function findlabel(body, l)
    for i=1:length(body)
        b = body[i]
        if isa(b,LabelNode) && b.label==l
            return i
        end
    end
    error("label ",l," not found")
end

f_argnames(ast) =
    map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1]::Array{Any,1})

is_rest_arg(arg) = (ccall(:jl_is_rest_arg,Int32,(Any,), arg) != 0)

# function typeinf_task(caller)
#     result = ()
#     while true
#         (caller, args) = yieldto(caller, result)
#         result = typeinf_ext_(args...)
#     end
# end

#Inference_Task = Task(typeinf_task, 2097152)
#yieldto(Inference_Task, current_task())

#function typeinf_ext(linfo, atypes, sparams, cop)
    #C = current_task()
    #args = (linfo, atypes, sparams, cop)
    #if is(C, Inference_Task)
    #    return typeinf_ext_(args...)
    #end
    #return yieldto(Inference_Task, C, args)
#end

function typeinf_ext(linfo, atypes, sparams, def)
    global inference_stack
    last = inference_stack
    inference_stack = EmptyCallStack()
    result = typeinf(linfo, atypes, sparams, def, true)
    inference_stack = last
    return result
end

typeinf(linfo,atypes,sparams) = typeinf(linfo,atypes,sparams,linfo,true)
typeinf(linfo,atypes,sparams,linfo) = typeinf(linfo,atypes,sparams,linfo,true)

ast_rettype(ast) = ast.args[3].typ

# def is the original unspecialized version of a method. we aggregate all
# saved type inference data there.
function typeinf(linfo::LambdaStaticData,atypes::Tuple,sparams::Tuple, def, cop)
    #dbg = 
    #dotrace = true
    local ast::Expr, tfunc_idx
    curtype = None
    redo = false
    # check cached t-functions
    tf = def.tfunc
    if !is(tf,())
        typearr = tf[1]::Array{Any,1}
        codearr = tf[2]::Array{Any,1}
        for i = 1:length(codearr)
            if typeseq(typearr[i],atypes)
                code = codearr[i]
                assert(isa(code, Tuple))
                if code[5]
                    curtype = code[3]
                    redo = true
                    tfunc_idx = i
                    break
                end
                return (code, code[3])
            end
        end
    end

    ast0 = def.ast

    #if dbg
    #    print("typeinf ", linfo.name, " ", uid(ast0), "\n")
    #end
    #print("typeinf ", linfo.name, " ", atypes, "\n")
    # if isbound(:stdout_stream)
    #     write(stdout_stream, "typeinf ")
    #     write(stdout_stream, string(linfo.name))
    #     write(stdout_stream, string(atypes))
    #     write(stdout_stream, '\n')
    # end
    #print("typeinf ", ast0, " ", sparams, " ", atypes, "\n")

    global inference_stack
    # check for recursion
    f = inference_stack
    while !isa(f,EmptyCallStack)
        if is(f.ast,ast0) && typeseq(f.types, atypes)
            # return best guess so far
            (f::CallStack).recurred = true
            r = inference_stack
            while !is(r, f)
                # mark all frames that are part of the cycle
                r.recurred = true
                r = r.prev
            end
            #print("*==> ", f.result,"\n")
            return ((),f.result)
        end
        f = f.prev
    end

    #print("typeinf ", linfo.name, " ", atypes, "\n")

    if cop
        sparams = append(sparams, linfo.sparams)
        ast = ccall(:jl_prepare_ast, Any, (Any,Any), linfo, sparams)::Expr
    else
        ast = linfo.ast
    end

    args = f_argnames(ast)
    la = length(args)
    assert(is(ast.head,:lambda), "inference.jl:745")
    locals = (ast.args[2][1])::Array{Any,1}
    vars = append(args, locals)
    body = (ast.args[3].args)::Array{Any,1}
    n = length(body)

    # our stack frame
    frame = CallStack(ast0, linfo.module, atypes, inference_stack)
    inference_stack = frame
    frame.result = curtype

    rec = false

    s = { () for i=1:n }
    recpts = IntSet(n+1)  # statements that depend recursively on our value
    W = IntSet(n+1)
    # initial set of pc
    add(W,1)
    # initial types
    s[1] = ObjectIdDict()
    for v in vars
        s[1][v] = Undef
    end
    if la > 0
        if is(atypes,Tuple)
            atypes = tuple(NTuple{la,Any}..., Tuple[1])
        end
        lastarg = ast.args[1][la]
        if is_rest_arg(lastarg)
            s[1][args[la]] = limit_tuple_depth(atypes[la:])
            la -= 1
        end
    end
    for i=1:la
        s[1][args[i]] = atypes[i]
    end
    # types of closed vars
    cenv = ObjectIdDict()
    for vi = ((ast.args[2][3])::Array{Any,1})
        vi::Array{Any,1}
        vname = vi[1]
        vtype = vi[2]
        cenv[vname] = vtype
        s[1][vname] = vtype
    end
    for vi = ((ast.args[2][2])::Array{Any,1})
        vi::Array{Any,1}
        if (vi[3]&4)!=0
            # variables assigned by inner functions are treated like
            # closed variables; we only use the declared type
            vname = vi[1]
            vtype = vi[2]
            cenv[vname] = vtype
            s[1][vname] = vtype
        end
    end
    sv = StaticVarInfo(sparams, cenv)

    # exception handlers
    cur_hand = ()
    handler_at = { () for i=1:n }

    while !isempty(W)
        pc = choose(W)
        while true
            #print(pc,": ",s[pc],"\n")
            del(W, pc)
            if is(handler_at[pc],())
                handler_at[pc] = cur_hand
            else
                cur_hand = handler_at[pc]
            end
            stmt = body[pc]
            changes = _jl_abstract_interpret(stmt, s[pc], sv)
            if frame.recurred
                if isa(frame.prev,CallStack) && frame.prev.recurred
                    rec = true
                end
                add(recpts, pc)
                #if dbg
                #    show(pc); print(" recurred\n")
                #end
                frame.recurred = false
            end
            if !is(cur_hand,())
                # propagate type info to exception handler
                l = cur_hand[1]::Int
                if stchanged(changes, s[l], vars)
                    add(W, l)
                    s[l] = stupdate(s[l], changes, vars)
                end
            end
            pc´ = pc+1
            if isa(stmt,GotoNode)
                pc´ = findlabel(body,stmt.label)
            elseif isa(stmt,Expr)
                hd = stmt.head
                if is(hd,:gotoifnot)
                    condexpr = stmt.args[1]
                    l = findlabel(body,stmt.args[2])
                    # constant conditions
                    if is(condexpr,true)
                    elseif is(condexpr,false)
                        pc´ = l
                    else
                        # general case
                        handler_at[l] = cur_hand
                        if stchanged(changes, s[l], vars)
                            add(W, l)
                            s[l] = stupdate(s[l], changes, vars)
                        end
                    end
                elseif is(hd,:return)
                    pc´ = n+1
                    rt = abstract_eval(stmt.args[1], s[pc], sv)
                    if frame.recurred
                        if isa(frame.prev,CallStack) && frame.prev.recurred
                            rec = true
                        end
                        add(recpts, pc)
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
                            add(W,r)
                        end
                    end
                elseif is(hd,:enter)
                    l = findlabel(body,stmt.args[1]::Int)
                    cur_hand = (l,cur_hand)
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
            else
                break
            end
        end
    end
    #print("\n",ast,"\n")
    #print("==> ", frame.result,"\n")
    if redo && typeseq(curtype, frame.result)
        rec = false
    end
    
    fulltree = type_annotate(ast, s, sv, frame.result, vars)
    
    if !rec
        fulltree.args[3] = inlining_pass(fulltree.args[3], vars)
        tuple_elim_pass(fulltree)
        linfo.inferred = true
    end
    
    compr = ccall(:jl_compress_ast, Any, (Any, Any,), def, fulltree)
    
    if !redo
        if is(def.tfunc,())
            def.tfunc = ({},{})
        end
        compr = (compr[1],compr[2],compr[3],compr[4],rec)
        push(def.tfunc[1]::Array{Any,1}, atypes)
        push(def.tfunc[2]::Array{Any,1}, compr)
    elseif !rec
        codearr = def.tfunc[2]
        compr = codearr[tfunc_idx]
        codearr[tfunc_idx] = (compr[1],compr[2],compr[3],compr[4],false)
    end
    
    inference_stack = (inference_stack::CallStack).prev
    return (compr, frame.result)
end

function record_var_type(e::Symbol, t, decls)
    otherTy = get(decls::ObjectIdDict, e, false)
    # keep track of whether a variable is always the same type
    if !is(otherTy,false)
        if !is(otherTy, t)
            decls[e] = Any
        end
    else
        decls[e] = t
    end
end

function eval_annotate(e::Expr, vtypes, sv, decls, clo)
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
            #s.typ = abstract_eval(s.name, vtypes, sv)
            s = s.name
        else
            #e.args[1] = SymbolNode(s, abstract_eval(s, vtypes, sv))
        end
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, decls, clo)
        # TODO: if this def does not reach any uses, maybe don't do this
        rhstype = exprtype(e.args[2])
        if !is(rhstype,None)
            record_var_type(s, rhstype, decls)
        end
        return e
    end
    i0 = is(head,:method) ? 2 : 1
    for i=i0:length(e.args)
        e.args[i] = eval_annotate(e.args[i], vtypes, sv, decls, clo)
    end
    e
end

function eval_annotate(e::Symbol, vtypes, sv, decls, clo)
    t = abstract_eval(e, vtypes, sv)
    record_var_type(e, t, decls)
    return (is(t,Any) || is(t,IntrinsicFunction)) ? e : SymbolNode(e, t)
end

function eval_annotate(e::SymbolNode, vtypes, sv, decls, clo)
    t = abstract_eval(e.name, vtypes, sv)
    record_var_type(e.name, t, decls)
    e.typ = t
    e
end

eval_annotate(s, vtypes, sv, decls, clo) = s

function eval_annotate(l::LambdaStaticData, vtypes, sv, decls, clo)
    push(clo, l)
    l
end

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array{Any,1},
                       sv::ANY, rettype::ANY, vnames::ANY)
    decls = ObjectIdDict()
    closures = LambdaStaticData[]
    body = ast.args[3].args::Array{Any,1}
    for i=1:length(body)
        body[i] = eval_annotate(body[i], states[i], sv, decls, closures)
    end
    ast.args[3].typ = rettype

    vinf = append(ast.args[2][2], ast.args[2][3])::Array{Any,1}
    # add declarations for variables that are always the same type
    for vi in vinf
        if has(decls,vi[1])
            vi[2] = decls[vi[1]]
        end
    end

    for li in closures
        if !li.inferred
            a = li.ast
            # pass on declarations of captured vars
            vinf = a.args[2][3]::Array{Any,1}
            for vi in vinf
                if has(decls,vi[1])
                    vi[2] = decls[vi[1]]
                end
            end
            na = length(a.args[1])
            typeinf(li, ntuple(na+1, i->(i>na ? Tuple[1] : Any)),
                    li.sparams, li, false)
        end
    end

    ast
end

function sym_replace(e::Expr, from, to)
    head = e.head
    if is(head,:line)
        return e
    end
    for i=1:length(e.args)
        e.args[i] = sym_replace(e.args[i], from, to)
    end
    e
end

function sym_replace(e::SymbolNode, from, to)
    s = e.name
    for i=1:length(from)
        if is(from[i],s)
            return to[i]
        end
    end
    return e
end

function sym_replace(s::Symbol, from, to)
    for i=1:length(from)
        if is(from[i],s)
            return to[i]
        end
    end
    s
end

sym_replace(x, from, to) = x

# count occurrences up to n+1
function occurs_more(e::Expr, pred, n)
    c = 0
    for a = e.args
        c += occurs_more(a, pred, n)
        if c>n
            return c
        end
    end
    c
end

occurs_more(e::SymbolNode, pred, n) = occurs_more(e.name, pred, n)
occurs_more(e, pred, n) = pred(e) ? 1 : 0

function contains_is(arr, item::ANY)
    for i = 1:length(arr)
        if is(arr[i],item)
            return true
        end
    end
    return false
end

function exprtype(x::ANY)
    if isa(x,Expr)
        return x.typ
    elseif isa(x,SymbolNode)
        return x.typ
    elseif isa(x,TopNode)
        return abstract_eval_global(x.name)
    elseif isa(x,Symbol)
        return Any
    elseif isa(x,Type)
        return Type{x}
    else
        return typeof(x)
    end
end

function without_linenums(a::Array{Any,1})
    l = {}
    for x in a
        if (isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)
        else
            push(l, x)
        end
    end
    l
end

# for now, only inline functions whose bodies are of the form "return <expr>"
# where <expr> doesn't contain any argument more than once.
# functions with closure environments or varargs are also excluded.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
function inlineable(f, e::Expr, vars)
    if !(isa(f,Function)||isa(f,CompositeKind))
        return NF
    end
    argexprs = a2t_butfirst(e.args)
    atypes = limit_tuple_type(map(exprtype, argexprs))

    if is(f, convert_default) && length(atypes)==3
        # builtin case of convert. convert(T,x::S) => x, when S<:T
        if isType(atypes[1]) && isleaftype(atypes[1]) &&
            subtype(atypes[2],atypes[1].parameters[1])
            # todo: if T expression has side effects??!
            return e.args[3]
        end
    end
    if (is(f,apply_type) || is(f,fieldtype)) &&
        isType(e.typ) && isleaftype(e.typ.parameters[1])
        return e.typ.parameters[1]
    end
    if length(atypes)==1 && isa(atypes[1],BitsKind) &&
        (is(f,unbox8) || is(f,unbox16) || is(f,unbox32) || is(f,unbox64) || is(f,unbox))
        return e.args[2]
    end

    meth = getmethods(f, atypes)
    if length(meth) != 1
        return NF
    end
    meth = meth[1]::Tuple
    # when 1 method matches the inferred types, there is still a chance
    # of a no-method error at run time, unless the inferred types are a
    # subset of the method signature.
    if !subtype(atypes, meth[1])
        return NF
    end
    if !isa(meth[3],LambdaStaticData) || !is(meth[4],())
        return NF
    end
    sp = meth[2]::Tuple
    spvals = sp[2:2:]
    for i=1:length(spvals)
        if isa(spvals[i],TypeVar)
            return NF
        end
    end
    (ast, ty) = typeinf(meth[3], meth[1], meth[2], meth[3])
    if is(ast,())
        return NF
    end
    if isa(ast,Tuple)
        ast = ccall(:jl_uncompress_ast, Any, (Any,), ast)
    end
    ast = ast::Expr
    for vi = ast.args[2][2]
        if (vi[3]&1)!=0
            # captures variables (TODO)
            return NF
        end
    end
    body = without_linenums(ast.args[3].args)::Array{Any,1}
    # see if body is only "return <expr>"
    if length(body) != 1
        return NF
    end
    assert(isa(body[1],Expr), "inference.jl:1050")
    assert(is(body[1].head,:return), "inference.jl:1051")
    # check for vararg function
    args = f_argnames(ast)
    na = length(args)
    if na>0 && is_rest_arg(ast.args[1][na])
        return NF
    end
    # see if each argument occurs only once in the body expression
    # TODO: make sure side effects aren't skipped if argument doesn't occur
    expr = body[1].args[1]
    for i=1:length(args)
        a = args[i]
        occ = occurs_more(expr, x->is(x,a), 1)
        if occ > 1
            aei = argexprs[i]
            # ok for argument to occur more than once if the actual argument
            # is a symbol or constant
            if !isa(aei,Symbol) && !isa(aei,Number) && !isa(aei,SymbolNode)
                return NF
            end
        elseif occ == 0
            aei = argexprs[i]
            if is(exprtype(aei),None)
                # if an argument does not occur in the function and its
                # actual argument is an error, make sure the error is not
                # skipped.
                return NF
            end
        end
    end
    # avoid capture if the function has free variables with the same name
    # as our vars
    if occurs_more(expr, x->(contains_is(vars,x)&&!contains_is(args,x)), 0) > 0
        return NF
    end
    # ok, substitute argument expressions for argument names in the body
    spnames = { sp[i].name for i=1:2:length(sp) }
    return sym_replace(copy(expr), append(args,spnames),
                       append(argexprs,spvals))
end

_jl_tn(sym::Symbol) =
    ccall(:jl_new_struct, Any, (Any,Any...), TopNode, sym, Any)

const _jl_top_tupleref = _jl_tn(:tupleref)

function _jl_mk_tupleref(texpr, i)
    e = :(($_jl_top_tupleref)($texpr, $i))
    e.typ = exprtype(texpr)[i]
    e
end

function inlining_pass(e::Expr, vars)
    # don't inline first argument of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall.
    eargs = e.args
    if length(eargs)<1
        return e
    end
    arg1 = eargs[1]
    if is(e.head,:call1) && (is(arg1, :ccall) ||
                             (isa(arg1,SymbolNode) && is(arg1.name, :ccall)) ||
                             (isa(arg1,TopNode) && is(arg1.name, :ccall)))
        i0 = 3
        isccall = true
    else
        i0 = 1
        isccall = false
    end
    for i=i0:length(eargs)
        ei = eargs[i]
        if isa(ei,Expr)
            eargs[i] = inlining_pass(ei, vars)
        end
    end
    if isccall
        for i=5:2:length(eargs)
            if isa(eargs[i],Symbol) || isa(eargs[i],SymbolNode)
                eargs[i+1] = 0
            end
        end
    end
    if is(e.head,:call1)
        e.head = :call
        ET = exprtype(arg1)
        if isType(ET)
            f = ET.parameters[1]
        else
            f = _ieval(arg1)
        end

        if is(f, ^) || is(f, .^)
            if length(e.args) == 3 && isa(e.args[3],Integer)
                a1 = e.args[2]
                if isa(a1,Number) || ((isa(a1,Symbol) || isa(a1,SymbolNode)) &&
                                      exprtype(a1) <: Number)
                    if e.args[3]==2
                        e.args = {_jl_tn(:*), a1, a1}
                        f = *
                    elseif e.args[3]==3
                        e.args = {_jl_tn(:*), a1, a1, a1}
                        f = *
                    end
                end
            end
        end

        body = inlineable(f, e, vars)
        if !is(body,NF)
            #print("inlining ", e, " => ", body, "\n")
            return body
        end
        if is(f,apply)
            na = length(e.args)
            newargs = cell(na-2)
            for i = 3:na
                aarg = e.args[i]
                t = exprtype(aarg)
                if isa(aarg,Expr) && is_top_call(aarg, :tuple)
                    # apply(f,tuple(x,y,...)) => f(x,y,...)
                    newargs[i-2] = aarg.args[2:]
                elseif isa(t,Tuple) && isleaftype(t)
                    # apply(f,t::(x,y)) => f(t[1],t[2])
                    newargs[i-2] = { _jl_mk_tupleref(aarg,j) for j=1:length(t) }
                else
                    # not all args expandable
                    return e
                end
            end
            e.args = append({e.args[2]}, newargs...)

            # now try to inline the simplified call
            body = inlineable(_ieval(e.args[1]), e, vars)
            if !is(body,NF)
                return body
            end
            return e
        end
    end
    e
end

function add_variable(ast, name, typ)
    vinf = {name,typ,2}
    locllist = ast.args[2][1]::Array{Any,1}
    vinflist = ast.args[2][2]::Array{Any,1}
    push(locllist, name)
    push(vinflist, vinf)
end

function unique_name(ast)
    locllist = ast.args[2][1]::Array{Any,1}
    g = gensym()
    while contains_is(locllist, g)
        g = gensym()
    end
    g
end

function is_top_call(e::Expr, fname)
    return is(e.head,:call) && isa(e.args[1],TopNode) &&
        is(e.args[1].name,fname)
end

# eliminate allocation of tuples used to return multiple values
function tuple_elim_pass(ast::Expr)
    body = (ast.args[3].args)::Array{Any,1}
    i = 1
    while i < length(body)-1
        e = body[i]
        if isa(e,Expr) && is(e.head,:multiple_value)
            i_start = i
            ret = body[i+1]
            # look for t = top(tuple)(...)
            if isa(ret,Expr) && is(ret.head,:(=))
                rhs = ret.args[2]
                if isa(rhs,Expr) && is_top_call(rhs,:tuple)
                    tup = rhs.args
                    tupname = ret.args[1]
                    nv = length(tup)-1
                    if nv > 0
                        del(body, i)  # remove (multiple_value)
                        del(body, i)  # remove tuple allocation
                        vals = { unique_name(ast) for j=1:nv }
                        # convert tuple allocation to a series of assignments
                        # to local variables
                        for j=1:nv
                            tupelt = tup[j+1]
                            tmp = Expr(:(=), {vals[j],tupelt}, Any)
                            add_variable(ast, vals[j], exprtype(tupelt))
                            insert(body, i+j-1, tmp)
                        end
                        i = i+nv
                        i0 = i
                        j = 1; k = 1
                        while k <= nv
                            stmt = body[i+j-1]
                            if isa(stmt,Expr) && is(stmt.head,:(=))
                                rhs = stmt.args[2]
                                if isa(rhs,Expr)
                                    if is_top_call(rhs,:tupleref) &&
                                        isequal(rhs.args[2],tupname)
                                        r = vals[k]
                                        if isa(r,Symbol)
                                            r = SymbolNode(r, exprtype(tup[k+1]))
                                        end
                                        stmt.args[2] = r
                                        k += 1
                                    elseif length(rhs.args)>2 &&
                                        isa(rhs.args[3],Expr) &&
                                        is_top_call(rhs.args[3],:tupleref) &&
                                        isequal(rhs.args[3].args[2],tupname)
                                        # assignment with conversion
                                        r = vals[k]
                                        if isa(r,Symbol)
                                            r = SymbolNode(r, exprtype(tup[k+1]))
                                        end
                                        rhs.args[3] = r
                                        k += 1
                                    end
                                end
                            end
                            j += 1
                        end
                        i = i_start
                    end
                end
            end
        end
        i += 1
    end
    ast
end

function finfer(f, types)
    x = getmethods(f,types)[1]
    (tree, ty) = typeinf(x[3], x[1], x[2])
    if isa(tree,Tuple)
        return ccall(:jl_uncompress_ast, Any, (Any,), tree)
    end
    tree
end

#tfunc(f,t) = (getmethods(f,t)[1][3]).tfunc

ccall(:jl_enable_inference, Void, ())
