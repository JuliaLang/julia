# parameters limiting potentially-infinite types
const MAX_TYPEUNION_LEN = 2
const MAX_TYPE_DEPTH = 2
const MAX_TUPLETYPE_LEN  = 8
const MAX_TUPLE_DEPTH = 4

type NotFound
end

const NF = NotFound()

type StaticVarInfo
    sp::Tuple            # static parameters tuple
    cenv::ObjectIdDict   # types of closed vars
    vars::Array{Any,1}   # names of args and locals
end

type EmptyCallStack
end

type CallStack
    ast
    mod::Module
    types::Tuple
    recurred::Bool
    result
    prev::Union(EmptyCallStack,CallStack)
    sv::StaticVarInfo

    CallStack(ast, mod, types, prev) = new(ast, mod, types, false, None, prev)
end

inference_stack = EmptyCallStack()

function is_static_parameter(sv::StaticVarInfo, s::Symbol)
    sp = sv.sp
    for i=1:2:length(sp)
        if is(sp[i].name,s)
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

is_local(sv::StaticVarInfo, s::Symbol) = contains_is(sv.vars, s)
is_closed(sv::StaticVarInfo, s::Symbol) = has(sv.cenv, s)
is_global(sv::StaticVarInfo, s::Symbol) =
    !is_local(sv,s) && !is_closed(sv,s) && !is_static_parameter(sv,s)

typeintersect(a::ANY,b::ANY) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)

methods(f::Union(Function,CompositeKind),t) = methods(f,t,-1)::Array{Any,1}
methods(f::Union(Function,CompositeKind),t,lim) = ccall(:jl_matching_methods, Any, (Any,Any,Int32), f, t, lim)

typeseq(a::ANY,b::ANY) = subtype(a,b)&&subtype(b,a)

isgeneric(f) = (isa(f,Function)||isa(f,CompositeKind)) && isa(f.env,MethodTable)
isleaftype(t) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

isconst(s::Symbol) =
    ccall(:jl_is_const, Int32, (Ptr{Void}, Any), C_NULL, s) != 0

isconst(m::Module, s::Symbol) =
    ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0

function _iisconst(s::Symbol)
    m = (inference_stack::CallStack).mod
    isdefined(m,s) && (ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0)
end

_ieval(x) = eval((inference_stack::CallStack).mod, x)
_iisdefined(x) = isdefined((inference_stack::CallStack).mod, x)

_iisconst(s::SymbolNode) = _iisconst(s.name)
_iisconst(s::TopNode) = isconst(_basemod(), s.name)
_iisconst(x::Expr) = false
_iisconst(x) = true

function _basemod()
    m = (inference_stack::CallStack).mod
    if m === Core || m === Base
        return m
    end
    return Main.Base
end

cmp_tfunc = (x,y)->Bool

isType(t::ANY) = isa(t,AbstractKind) && is((t::AbstractKind).name,Type.name)

isvarargtype(t::ANY) = isa(t,AbstractKind)&&is((t::AbstractKind).name,Vararg.name)

const t_func = ObjectIdDict()
#t_func[tuple] = (0, Inf, (args...)->limit_tuple_depth(args))
t_func[throw] = (1, 1, x->None)
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
t_func[nan_dom_err] = (2, 2, (a, b)->a)
t_func[eval(Core,:ccall)] =
    (3, Inf, (fptr, rt, at, a...)->(is(rt,Type{Void}) ? Nothing :
                                    isType(rt) ? rt.parameters[1] : Any))
t_func[is] = (2, 2, cmp_tfunc)
t_func[subtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[isdefined] = (1, 2, (args...)->Bool)
t_func[Union] = (0, Inf,
                 (args...)->(if all(isType,args)
                                 Type{Union(map(t->t.parameters[1],args)...)}
                             else
                                 Type
                             end))
t_func[method_exists] = (2, 2, cmp_tfunc)
t_func[applicable] = (1, Inf, (f, args...)->Bool)
t_func[tuplelen] = (1, 1, x->Int)
t_func[arraylen] = (1, 1, x->Int)
#t_func[arrayref] = (2,Inf,(a,i...)->(isa(a,CompositeKind) && subtype(a,Array) ?
#                                     a.parameters[1] : Any))
#t_func[arrayset] = (3, Inf, (a,v,i...)->a)
arraysize_tfunc(a, d) = Int
function arraysize_tfunc(a)
    if isa(a,CompositeKind) && subtype(a,Array)
        N = a.parameters[2]
        return isa(N,Int) ? NTuple{N,Int} : (Int...)
    else
        return (Int...)
    end
end
t_func[arraysize] = (1, 2, arraysize_tfunc)
t_func[pointerref] = (2,2,(a,i)->(subtype(a,Ptr) ? a.parameters[1] : Any))
t_func[pointerset] = (3, 3, (a,v,i)->a)

function static_convert(to::ANY, from::ANY)
    if !isa(to,Tuple) || !isa(from,Tuple)
        if isa(to,TypeVar)
            return to
        end
        if subtype(from, to)
            return from
        end
        t = typeintersect(from,to)
        return is(t,None) ? to : t
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
            if isvarargtype(pe)
                pe = pe.parameters[1]
                pseq = true
            elseif isa(pe,TypeVar) && isvarargtype(pe.ub)
                pe = pe.ub.parameters[1]
                pseq = true
            end
        else
            return None
        end
        # tuple conversion calls convert recursively
        if isvarargtype(ce)
            R = abstract_call_gf(convert, (), (Type{pe}, ce.parameters[1]), ())
            #R = static_convert(pe, ce.parameters[1])
            isType(R) && (R = R.parameters[1])
            result[i] = Vararg{R}
        else
            R = abstract_call_gf(convert, (), (Type{pe}, ce), ())
            #R = static_convert(pe, ce)
            isType(R) && (R = R.parameters[1])
            result[i] = R
        end
    end
    a2t(result)
end
t_func[convert_default] =
    (3, 3, (t,x,f)->(isType(t) ? static_convert(t.parameters[1],x) : Any))
t_func[convert_tuple] =
    (3, 3, (t,x,f)->(if isa(t,Tuple) && all(isType,t)
                         t = Type{map(t->t.parameters[1],t)}
                     end;
                     isType(t) ? static_convert(t.parameters[1],x) :
                     Any))
const typeof_tfunc = function (t)
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
            Type{TypeVar(:_,t)}
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
    (2, 2, (A, v, t)->(isType(t) ? typeintersect(v,t.parameters[1]) :
                       isa(t,Tuple) && all(isType,t) ?
                           typeintersect(v,map(t->t.parameters[1],t)) :
                       Any))

const tupleref_tfunc = function (A, t, i)
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
    vararg = isvarargtype(last)
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

const getfield_tfunc = function (A, s, name)
    if isType(s)
        s = typeof(s.parameters[1])
        if s === TypeVar
            return Any
        end
    end
    if !isa(s,CompositeKind)
        return Any
    end
    if isa(A[2],QuoteNode) && isa(A[2].value,Symbol)
        fld = A[2].value
        A1 = A[1]
        if isa(A1,Module) && isdefined(A1,fld) && isconst(A1, fld)
            return abstract_eval_constant(eval(A1,fld))
        end
        if s === Module
            return Top
        end
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
t_func[setfield] = (3, 3, (o, f, v)->v)
const fieldtype_tfunc = function (A, s, name)
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
const apply_type_tfunc = function (A, args...)
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
            tparams = tuple(tparams..., args[i].parameters[1])
        elseif isa(A[i],Int)
            tparams = tuple(tparams..., A[i])
        else
            uncertain = true
            tparams = tuple(tparams..., headtype.parameters[i-1])
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
    uncertain ? Type{TypeVar(:_,appl)} : Type{appl}
end
t_func[apply_type] = (1, Inf, apply_type_tfunc)

# other: apply

function builtin_tfunction(f::ANY, args::ANY, argtypes::ANY)
    if is(f,tuple)
        return limit_tuple_depth(argtypes)
    end
    if is(f,arrayset)
        if length(argtypes) < 3
            return None
        end
        return argtypes[1]
    end
    if is(f,arrayref)
        if length(argtypes) < 2
            return None
        end
        a = argtypes[1]
        return (isa(a,CompositeKind) && subtype(a,Array) ?
                a.parameters[1] : Any)
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

function a2t(a::AbstractVector)
    n = length(a)
    if n==2 return (a[1],a[2]) end
    if n==1 return (a[1],) end
    if n==3 return (a[1],a[2],a[3]) end
    if n==0 return () end
    return tuple(a...)
end

function isconstantfunc(f, sv::StaticVarInfo)
    if isa(f,TopNode)
        m = _basemod()
        return isconst(m, f.name) && isdefined(m, f.name) && f
    end
    if isa(f,GetfieldNode) && isa(f.value,Module)
        M = f.value; s = f.name
        return isdefined(M,s) && isconst(M,s) && f
    end
    if isa(f,Expr) && (is(f.head,:call) || is(f.head,:call1))
        if length(f.args) == 3 && isa(f.args[1], TopNode) &&
            is(f.args[1].name,:getfield) && isa(f.args[3],QuoteNode)
            s = f.args[3].value
            if isa(f.args[2],Module)
                M = f.args[2]
            else
                M = isconstantfunc(f.args[2], sv)
                if M === false
                    return false
                end
                M = _ieval(M)
                if !isa(M,Module)
                    return false
                end
            end
            return isdefined(M,s) && isconst(M,s) && f
        end
    end

    if isa(f,SymbolNode)
        f = f.name
    end
    return isa(f,Symbol) && is_global(sv, f) && _iisconst(f) && f
end

const isconstantref = isconstantfunc

isvatuple(t::Tuple) = (n = length(t); n > 0 && isvarargtype(t[n]))

const limit_tuple_depth = t->limit_tuple_depth_(t,0)

const limit_tuple_depth_ = function (t,d::Int)
    if isa(t,UnionKind)
        # also limit within Union types.
        # may have to recur into other stuff in the future too.
        return Union(limit_tuple_depth_(t.types,d)...)
    end
    if !isa(t,Tuple)
        return t
    end
    if d > MAX_TUPLE_DEPTH
        return Tuple
    end
    map(x->limit_tuple_depth_(x,d+1), t)
end

const limit_tuple_type = function (t::Tuple)
    n = length(t)
    if n > MAX_TUPLETYPE_LEN
        last = t[n]
        if isvarargtype(last)
            last = last.parameters[1]
        end
        tail = tuple(t[MAX_TUPLETYPE_LEN:(n-1)]..., last)
        tail = typeintersect(reduce(tmerge, None, tail), Any)
        return tuple(t[1:(MAX_TUPLETYPE_LEN-1)]..., Vararg{tail})
    end
    return t
end

function abstract_call_gf(f, fargs, argtypes, e)
    if length(argtypes)>1 && isa(argtypes[1],Tuple) && argtypes[2]===Int
        # allow tuple indexing functions to take advantage of constant
        # index arguments.
        if f === Main.Base.ref
            e.head = :call1
            return tupleref_tfunc(fargs, argtypes[1], argtypes[2])
        elseif f === Main.Base.next
            e.head = :call1
            return (tupleref_tfunc(fargs, argtypes[1], argtypes[2]), Int)
        elseif f === Main.Base.indexed_next
            e.head = :call1
            return (tupleref_tfunc(fargs, argtypes[1], argtypes[2]), Int)
        end
    end
    if f === Main.Base.promote_type || f === Main.Base.typejoin
        la = length(argtypes)
        c = cell(la)
        for i = 1:la
            t = argtypes[i]
            if isType(t) && !isa(t.parameters[1],TypeVar)
                c[i] = t.parameters[1]
            else
                return Type
            end
        end
        return Type{f(c...)}
    end
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always None.
    # here I picked 4.
    argtypes = limit_tuple_type(argtypes)
    applicable = methods(f, argtypes, 4)
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
        return None
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

function invoke_tfunc(f, types, argtypes)
    argtypes = typeintersect(types,limit_tuple_type(argtypes))
    if is(argtypes,None)
        return None
    end
    applicable = methods(f, types)
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
    if is(f,apply) && length(fargs)>0
        af = isconstantfunc(fargs[1], sv)
        if !is(af,false)
            aargtypes = argtypes[2:]
            if all(x->isa(x,Tuple), aargtypes) &&
                !any(isvatuple, aargtypes[1:(length(aargtypes)-1)])
                e.head = :call1
                # apply with known func with known tuple types
                # can be collapsed to a call to the applied func
                at = length(aargtypes) > 0 ?
                     limit_tuple_type(apply(tuple,aargtypes...)) : ()
                return abstract_call(_ieval(af), (), at, vtypes, sv, ())
            end
            af = _ieval(af)
            if is(af,tuple) && length(fargs)==2
                # tuple(xs...)
                aat = aargtypes[1]
                if aat <: AbstractArray
                    # tuple(array...)
                    # TODO: > 1 array of the same type
                    tn = AbstractArray.name
                    while isa(aat, AbstractKind) || isa(aat, BitsKind) ||
                        isa(aat, CompositeKind)
                        if is(aat.name, tn)
                            et = aat.parameters[1]
                            if !isa(et,TypeVar)
                                return (et...)
                            end
                        end
                        if is(aat, Any)
                            break
                        end
                        aat = aat.super
                    end
                end
                return Tuple
            end
        else
            ft = abstract_eval(fargs[1], vtypes, sv)
            if isType(ft)
                # TODO: improve abstract_call_constructor
                st = ft.parameters[1]
                if isa(st,TypeVar) && isa(st.ub,CompositeKind)
                    return st.ub
                end
                if isa(st,CompositeKind)
                    return st
                end
            end
        end
    end
    if isgeneric(f)
        return abstract_call_gf(f, fargs, argtypes, e)
    end
    if is(f,invoke) && length(fargs)>1
        af = isconstantfunc(fargs[1], sv)
        if !is(af,false) && (af=_ieval(af);isgeneric(af))
            sig = argtypes[2]
            if isa(sig,Tuple) && all(isType, sig)
                sig = map(t->t.parameters[1], sig)
                return invoke_tfunc(af, sig, argtypes[3:])
            end
        end
    end
    if !is(f,apply) && isa(e,Expr) && (isa(f,Function) || isa(f,IntrinsicFunction))
        e.head = :call1
    end
    if is(f,getfield)
        val = isconstantref(e, sv)
        if !is(val,false)
            return abstract_eval_constant(_ieval(val))
        end
    end
    rt = builtin_tfunction(f, fargs, argtypes)
    #print("=> ", rt, "\n")
    return rt
end

function abstract_eval_arg(a, vtypes, sv)
    t = abstract_eval(a, vtypes, sv)
    if isa(a,Symbol) || isa(a,SymbolNode)
        t = typeintersect(t,Any)  # remove Undef
    end
    return t
end

function abstract_eval_call(e, vtypes, sv::StaticVarInfo)
    fargs = e.args[2:]
    argtypes = tuple([abstract_eval_arg(a, vtypes, sv) for a in fargs]...)
    if any(x->is(x,None), argtypes)
        return None
    end
    called = e.args[1]
    func = isconstantfunc(called, sv)
    if is(func,false)
        if isa(called, LambdaStaticData)
            # called lambda expression (let)
            (_, result) = typeinf(called, argtypes, called.sparams, called,
                                  false)
            return result
        end
        ft = abstract_eval(called, vtypes, sv)
        if isType(ft)
            st = ft.parameters[1]
            if isa(st,TypeVar) && isa(st.ub,CompositeKind)
                return st.ub
            end
            if isa(st,CompositeKind)
                if isgeneric(st) && isleaftype(st)
                    return abstract_call_gf(st, fargs, argtypes, e)
                end
                # struct constructor
                return st
            end
        end
        return Any
    end
    #print("call ", e.args[1], argtypes, " ")
    f = _ieval(func)
    return abstract_call(f, fargs, argtypes, vtypes, sv, e)
end

function abstract_eval(e::Expr, vtypes, sv::StaticVarInfo)
    # handle:
    # call  null  new  &  static_typeof
    if is(e.head,:call) || is(e.head,:call1)
        t = abstract_eval_call(e, vtypes, sv)
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
        t = typeintersect(t, Any)
        if is(t,None)
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
        t = Function
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
    return abstract_eval_global(_basemod(), e.name)
end

const Type_Array = Type{Array}

function abstract_eval_constant(x::ANY)
    if isa(x,AbstractKind) || isa(x,BitsKind) || isa(x,CompositeKind) ||
        isa(x,UnionKind) || isa(x,TypeConstructor)
        if is(x,Array)
            return Type_Array
        end
        return Type{x}
    end
    return typeof(x)
end

# Undef is the static type of a value location (e.g. variable) that is
# undefined. The corresponding run-time type is None, since accessing an
# undefined location is an error. A non-lvalue expression cannot have
# type Undef, only None.
# typealias Top Union(Any,Undef)

abstract_eval_global(s::Symbol) =
    abstract_eval_global((inference_stack::CallStack).mod, s)

function abstract_eval_global(M, s::Symbol)
    if isconst(M,s)
        return abstract_eval_constant(eval(M,s))
    end
    if !isdefined(M,s)
        return Top
    end
    # TODO: change to Undef if there's a way to clear variables
    return Any
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
        # global
        return abstract_eval_global(s)
    end
    return t
end

abstract_eval(s::SymbolNode, vtypes, sv::StaticVarInfo) =
    abstract_eval(s.name, vtypes, sv)

abstract_eval(x, vtypes, sv::StaticVarInfo) = abstract_eval_constant(x)

abstract_eval(x::LambdaStaticData, vtypes, sv::StaticVarInfo) = Function

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

abstract_interpret(e, vtypes, sv::StaticVarInfo) = vtypes

function abstract_interpret(e::Expr, vtypes, sv::StaticVarInfo)
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

function type_too_complex(t::ANY, d)
    if d > MAX_TYPE_DEPTH
        return true
    end
    if isa(t,UnionKind)
        p = t.types
    elseif isa(t,CompositeKind) || isa(t,AbstractKind) || isa(t,BitsKind)
        return false
    elseif isa(t,Tuple)
        p = t
    elseif isa(t,TypeVar)
        return type_too_complex(t.lb,d+1) || type_too_complex(t.ub,d+1)
    else
        return false
    end
    for x in (p::Tuple)
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

is_rest_arg(arg::ANY) = (ccall(:jl_is_rest_arg,Int32,(Any,), arg) != 0)

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

function typeinf_ext(linfo, atypes::ANY, sparams::ANY, def)
    global inference_stack
    last = inference_stack
    inference_stack = EmptyCallStack()
    result = typeinf(linfo, atypes, sparams, def, true)
    inference_stack = last
    return result
end

typeinf(linfo,atypes::ANY,sparams::ANY) = typeinf(linfo,atypes,sparams,linfo,true)
typeinf(linfo,atypes::ANY,sparams::ANY,def) = typeinf(linfo,atypes,sparams,def,true)

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
        tfarr = tf::Array{Any,1}
        for i = 1:3:length(tfarr)
            if typeseq(tfarr[i],atypes)
                code = tfarr[i+1]
                curtype = ccall(:jl_ast_rettype, Any, (Any,Any), def, code)
                if tfarr[i+2]
                    redo = true
                    tfunc_idx = i+1
                    break
                end
                return (code, curtype)
            end
        end
    end

    ast0 = def.ast

    #if dbg
    #    print("typeinf ", linfo.name, " ", object_id(ast0), "\n")
    #end
    #print("typeinf ", linfo.name, " ", atypes, "\n")
    # if isdefined(:STDOUT)
    #     write(STDOUT, "typeinf ")
    #     write(STDOUT, string(linfo.name))
    #     write(STDOUT, string(atypes))
    #     write(STDOUT, '\n')
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

    #if dbg print("typeinf ", linfo.name, " ", atypes, "\n") end

    if cop
        sparams = tuple(sparams..., linfo.sparams...)
        ast = ccall(:jl_prepare_ast, Any, (Any,Any), linfo, sparams)::Expr
    else
        ast = linfo.ast
    end

    args = f_argnames(ast)
    la = length(args)
    assert(is(ast.head,:lambda), "inference.jl:745")
    locals = (ast.args[2][1])::Array{Any,1}
    vars = [args, locals]
    body = (ast.args[3].args)::Array{Any,1}
    n = length(body)

    # our stack frame
    frame = CallStack(ast0, linfo.module, atypes, inference_stack)
    inference_stack = frame
    frame.result = curtype

    rec = false
    toprec = false

    s = { () for i=1:n }
    recpts = IntSet()  # statements that depend recursively on our value
    W = IntSet()
    # initial set of pc
    add!(W,1)
    # initial types
    s[1] = ObjectIdDict()
    for v in vars
        s[1][v] = Undef
    end
    if la > 0
        lastarg = ast.args[1][la]
        if is_rest_arg(lastarg)
	    if atypes === Tuple
                if la > 1
                    atypes = tuple(NTuple{la-1,Any}..., Tuple[1])
                end
                s[1][args[la]] = Tuple
            else
                s[1][args[la]] = limit_tuple_depth(atypes[la:])
            end
            la -= 1
        else
            if atypes === Tuple
                atypes = tuple(NTuple{la,Any}..., Tuple[1])
            end
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
    sv = StaticVarInfo(sparams, cenv, vars)
    frame.sv = sv

    # exception handlers
    cur_hand = ()
    handler_at = { () for i=1:n }

    while !isempty(W)
        pc = first(W)
        while true
            #print(pc,": ",s[pc],"\n")
            delete!(W, pc, 0)
            if is(handler_at[pc],())
                handler_at[pc] = cur_hand
            else
                cur_hand = handler_at[pc]
            end
            stmt = body[pc]
            changes = abstract_interpret(stmt, s[pc], sv)
            if frame.recurred
                rec = true
                if !(isa(frame.prev,CallStack) && frame.prev.recurred)
                    toprec = true
                end
                add!(recpts, pc)
                #if dbg
                #    show(pc); print(" recurred\n")
                #end
                frame.recurred = false
            end
            if !is(cur_hand,())
                # propagate type info to exception handler
                l = cur_hand[1]::Int
                if stchanged(changes, s[l], vars)
                    add!(W, l)
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
                            add!(W, l)
                            s[l] = stupdate(s[l], changes, vars)
                        end
                    end
                elseif is(hd,:type_goto)
                    l = findlabel(body,stmt.args[1])
                    if stchanged(changes, s[l], vars)
                        add!(W, l)
                        s[l] = stupdate(s[l], changes, vars)
                    end
                elseif is(hd,:return)
                    pc´ = n+1
                    rt = abstract_eval_arg(stmt.args[1], s[pc], sv)
                    if frame.recurred
                        rec = true
                        if !(isa(frame.prev,CallStack) && frame.prev.recurred)
                            toprec = true
                        end
                        add!(recpts, pc)
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
                            add!(W,r)
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
    #if dbg print("==> ", frame.result,"\n") end
    if (toprec && typeseq(curtype, frame.result)) || !isa(frame.prev,CallStack)
        rec = false
    end
    
    fulltree = type_annotate(ast, s, sv, frame.result, args)
    
    if !rec
        fulltree.args[3] = inlining_pass(fulltree.args[3], sv, fulltree)[1]
        # inlining can add variables
        sv.vars = append_any(f_argnames(fulltree), fulltree.args[2][1])
        tuple_elim_pass(fulltree)
        linfo.inferred = true
        fulltree = ccall(:jl_compress_ast, Any, (Any,Any), def, fulltree)
    end
    
    if !redo
        if is(def.tfunc,())
            def.tfunc = {}
        end
        push!(def.tfunc::Array{Any,1}, atypes)
        push!(def.tfunc::Array{Any,1}, fulltree)
        push!(def.tfunc::Array{Any,1}, rec)
    else
        def.tfunc[tfunc_idx] = fulltree
        def.tfunc[tfunc_idx+1] = rec
    end
    
    inference_stack = (inference_stack::CallStack).prev
    return (fulltree, frame.result)
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
        subex = e.args[i]
        if !(isa(subex,Number) || isa(subex,String))
            e.args[i] = eval_annotate(subex, vtypes, sv, decls, clo)
        end
    end
    e
end

function eval_annotate(e::Symbol, vtypes, sv, decls, clo)
    if !is_local(sv, e) && !is_closed(sv, e)
        # can get types of globals and static params from the environment
        return e
    end
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
    push!(clo, l)
    l
end

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array{Any,1}, sv::ANY, rettype::ANY,
                       args)
    decls = ObjectIdDict()
    # initialize decls with argument types
    for arg in args
        decls[arg] = states[1][arg]
    end
    closures = {}
    body = ast.args[3].args::Array{Any,1}
    for i=1:length(body)
        body[i] = eval_annotate(body[i], states[i], sv, decls, closures)
    end
    ast.args[3].typ = rettype

    # add declarations for variables that are always the same type
    for vi in ast.args[2][2]::Array{Any,1}
        if (vi[3]&4)==0
            vi[2] = get(decls, vi[1], vi[2])
        end
    end
    for vi in ast.args[2][3]::Array{Any,1}
        if (vi[3]&4)==0
            vi[2] = get(decls, vi[1], vi[2])
        end
    end

    for (li::LambdaStaticData) in closures
        if !li.inferred
            a = li.ast
            # pass on declarations of captured vars
            for vi in a.args[2][3]::Array{Any,1}
                if (vi[3]&4)==0
                    vi[2] = get(decls, vi[1], vi[2])
                end
            end
            na = length(a.args[1])
            li.ast, _ = typeinf(li, ntuple(na+1, i->(i>na ? (Tuple)[1] : Any)),
                                li.sparams, li, false)
        end
    end

    ast
end

function sym_replace(e::Expr, from1, from2, to1, to2)
    if !is(e.head,:line)
        for i=1:length(e.args)
            e.args[i] = sym_replace(e.args[i], from1, from2, to1, to2)
        end
    end
    e
end

function _sym_repl(s::Symbol, from1, from2, to1, to2, deflt)
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

function sym_replace(e::SymbolNode, from1, from2, to1, to2)
    return _sym_repl(e.name, from1, from2, to1, to2, e)
end

function sym_replace(s::Symbol, from1, from2, to1, to2)
    return _sym_repl(s, from1, from2, to1, to2, s)
end

sym_replace(x, from1, from2, to1, to2) = x

# return an expr to evaluate "from.sym" in module "to"
function resolve_relative(sym, from, to, typ, orig)
    if is(from,to)
        return orig
    end
    const_from = (isconst(from,sym) && isdefined(from,sym))
    const_to   = (isconst(to,sym) && isdefined(to,sym))
    if const_from
        if const_to && is(eval(from,sym), eval(to,sym))
            return orig
        end
        m = _basemod()
        if is(from,m) || is(from,Core)
            return tn(sym)
        end
    end
    return GetfieldNode(from, sym, typ)
end

# annotate symbols with their original module for inlining
function resolve_globals(e::Expr, from, to, env1, env2)
    if !is(e.head,:line)
        for i=1:length(e.args)
            subex = e.args[i]
            if !(isa(subex,Number) || isa(subex,String))
                e.args[i] = resolve_globals(subex, from, to, env1, env2)
            end
        end
    end
    e
end

function resolve_globals(s::Symbol, from, to, env1, env2)
    if contains_is(env1, s) || contains_is(env2, s)
        return s
    end
    resolve_relative(s, from, to, Any, s)
end

function resolve_globals(s::SymbolNode, from, to, env1, env2)
    name = s.name
    if contains_is(env1, name) || contains_is(env2, name)
        return s
    end
    resolve_relative(name, from, to, s.typ, s)
end

resolve_globals(x, from, to, env1, env2) = x

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

function exprtype(x::ANY)
    if isa(x,Expr)
        return x.typ
    elseif isa(x,SymbolNode)
        return x.typ
    elseif isa(x,TopNode)
        return abstract_eval_global(_basemod(), x.name)
    elseif isa(x,Symbol)
        sv = inference_stack.sv
        if is_local(sv, x)
            return Any
        end
        return abstract_eval(x, (), sv)
    elseif isa(x,QuoteNode)
        return typeof(x.value)
    elseif isa(x,Type)
        return Type{x}
    elseif isa(x,LambdaStaticData)
        return Function
    elseif isa(x,GetfieldNode)
        return x.typ
    else
        return typeof(x)
    end
end

function without_linenums(a::Array{Any,1})
    l = {}
    for x in a
        if (isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)
        else
            push!(l, x)
        end
    end
    l
end

# detect some important side-effect-free calls
function effect_free(e)
    if isa(e,Symbol) || isa(e,SymbolNode) || isa(e,Number) || isa(e,String) ||
        isa(e,TopNode) || isa(e,QuoteNode)
        return true
    end
    if isa(e,Expr) && (e.head === :call || e.head === :call1)
        ea = e.args
        for a in ea
            if !effect_free(a)
                return false
            end
        end
        if isa(ea[1],TopNode)
            n = ea[1].name
            if (n === :getfield || n === :tuple || n === :tupleref ||
                n === :tuplelen || n === :fieldtype)
                return true
            end
        end
    end
    return false
end

# for now, only inline functions whose bodies are of the form "return <expr>"
# where <expr> doesn't contain any argument more than once.
# functions with closure environments or varargs are also excluded.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
function inlineable(f, e::Expr, sv, enclosing_ast)
    if !(isa(f,Function)||isa(f,CompositeKind)||isa(f,IntrinsicFunction))
        return NF
    end
    argexprs = e.args[2:]
    atypes = limit_tuple_type(tuple(map(exprtype, argexprs)...))

    if is(f, convert_default) && length(atypes)==3
        # builtin case of convert. convert(T,x::S) => x, when S<:T
        if isType(atypes[1]) && isleaftype(atypes[1]) &&
            subtype(atypes[2],atypes[1].parameters[1])
            # todo: if T expression has side effects??!
            return (e.args[3],())
        end
    end
    if (is(f,apply_type) || is(f,fieldtype)) &&
        isType(e.typ) && isleaftype(e.typ.parameters[1])
        return (e.typ.parameters[1],())
    end
    if length(atypes)==2 && is(f,unbox) && isa(atypes[2],BitsKind)
        return (e.args[3],())
    end
    if isa(f,IntrinsicFunction)
        return NF
    end

    meth = methods(f, atypes)
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
    spvals = { sp[i] for i in 2:2:length(sp) }
    for i=1:length(spvals)
        if isa(spvals[i],TypeVar)
            return NF
        end
    end
    (ast, ty) = typeinf(meth[3], meth[1], meth[2], meth[3])
    if is(ast,())
        return NF
    end
    needcopy = true
    if !isa(ast,Expr)
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), meth[3], ast)
        needcopy = false
    end
    ast = ast::Expr
    for vi in ast.args[2][2]
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
        # construct tuple-forming expression for argument tail
        vararg = mk_tuplecall(argexprs[na:end])
        argexprs = {argexprs[1:(na-1)]..., vararg}
    end
    expr = body[1].args[1]

    # avoid capture if the function has free variables with the same name
    # as our vars
    if occurs_more(expr, x->(isa(x,Symbol)&&!is_global(sv,x)&&!contains_is(args,x)), 0) > 0
        return NF
    end

    stmts = {}
    # see if each argument occurs only once in the body expression
    for i=1:length(args)
        a = args[i]
        occ = occurs_more(expr, x->is(x,a), 1)
        if occ != 1
            aei = argexprs[i]; aeitype = exprtype(aei)
            # ok for argument to occur more than once if the actual argument
            # is a symbol or constant
            if (!isa(aei,Symbol) && !isa(aei,Number) && !isa(aei,SymbolNode) && !isa(aei,String)) || (occ==0 && is(aeitype,None))
                # introduce variable for this argument
                if occ > 1
                    vnew = unique_name(enclosing_ast)
                    add_variable(enclosing_ast, vnew, aeitype)
                    push!(stmts, Expr(:(=), {vnew, aei}, Any))
                    argexprs[i] = aeitype===Any ? vnew : SymbolNode(vnew,aeitype)
                elseif !isType(aeitype) && !effect_free(aei)
                    push!(stmts, aei)
                end
            end
        end
    end

    # ok, substitute argument expressions for argument names in the body
    spnames = { sp[i].name for i=1:2:length(sp) }
    if needcopy; expr = astcopy(expr); end
    mfrom = meth[3].module; mto = (inference_stack::CallStack).mod
    if !is(mfrom, mto)
        expr = resolve_globals(expr, mfrom, mto, args, spnames)
    end
    return (sym_replace(expr, args, spnames, argexprs, spvals), stmts)
end

tn(sym::Symbol) =
    ccall(:jl_new_struct, Any, (Any,Any...), TopNode, sym, Any)

const top_tupleref = tn(:tupleref)
const top_tuple = tn(:tuple)

function mk_tupleref(texpr, i)
    e = :(($top_tupleref)($texpr, $i))
    e.typ = exprtype(texpr)[i]
    e
end

function mk_tuplecall(args)
    Expr(:call1, {top_tuple, args...}, tuple(map(exprtype, args)...))
end

const basenumtype = Union(Int32,Int64,Float32,Float64,Complex64,Complex128,Rational)

function inlining_pass(e::Expr, sv, ast)
    # don't inline first argument of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall.
    eargs = e.args
    if length(eargs)<1
        return (e,())
    end
    arg1 = eargs[1]
    if is_known_call(e, ccall, sv)
        i0 = 3
        isccall = true
    else
        i0 = 1
        isccall = false
    end
    stmts = {}
    if e.head === :body
        i = i0
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
    else
        for i=i0:length(eargs)
            ei = eargs[i]
            if isa(ei,Expr)
                res = inlining_pass(ei, sv, ast)
                eargs[i] = res[1]
                if isa(res[2],Array)
                    append!(stmts,res[2]::Array{Any,1})
                end
            end
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
            if length(e.args) == 3 && isa(e.args[3],Union(Int32,Int64))
                a1 = e.args[2]
                if isa(a1,basenumtype) || ((isa(a1,Symbol) || isa(a1,SymbolNode)) &&
                                           exprtype(a1) <: basenumtype)
                    if e.args[3]==2
                        e.args = {tn(:*), a1, a1}
                        f = *
                    elseif e.args[3]==3
                        e.args = {tn(:*), a1, a1, a1}
                        f = *
                    end
                end
            end
        end

        res = inlineable(f, e, sv, ast)
        if isa(res,Tuple)
            if isa(res[2],Array)
                append!(stmts,res[2])
            end
            return (res[1],stmts)
        elseif !is(res,NF)
            return (res,stmts)
        end

        if is(f,apply)
            na = length(e.args)
            newargs = cell(na-2)
            for i = 3:na
                aarg = e.args[i]
                t = exprtype(aarg)
                if isa(aarg,Expr) && is_known_call(aarg, tuple, sv)
                    # apply(f,tuple(x,y,...)) => f(x,y,...)
                    newargs[i-2] = aarg.args[2:]
                elseif isa(t,Tuple) && !isvatuple(t) && effect_free(aarg)
                    # apply(f,t::(x,y)) => f(t[1],t[2])
                    newargs[i-2] = { mk_tupleref(aarg,j) for j=1:length(t) }
                else
                    # not all args expandable
                    return (e,stmts)
                end
            end
            e.args = [{e.args[2]}, newargs...]

            # now try to inline the simplified call
            res = inlineable(_ieval(e.args[1]), e, sv, ast)
            if isa(res,Tuple)
                if isa(res[2],Array)
                    append!(stmts,res[2])
                end
                return (res[1],stmts)
            elseif !is(res,NF)
                return (res,stmts)
            end

            return (e,stmts)
        end
    end
    return (e,stmts)
end

function add_variable(ast, name, typ)
    vinf = {name,typ,2}
    locllist = ast.args[2][1]::Array{Any,1}
    vinflist = ast.args[2][2]::Array{Any,1}
    push!(locllist, name)
    push!(vinflist, vinf)
end

const some_names = {:_var0, :_var1, :_var2, :_var3, :_var4, :_var5, :_var6,
                    :_var7, :_var8, :_var9, :_var10, :_var11, :_var12,
                    :_var13, :_var14, :_var15, :_var16, :_var17, :_var18,
                    :_var19, :_var20, :_var21, :_var22, :_var23, :_var24}

function unique_name(ast)
    locllist = ast.args[2][1]::Array{Any,1}
    for g in some_names
        if !contains_is(locllist, g)
            return g
        end
    end
    g = gensym()
    while contains_is(locllist, g)
        g = gensym()
    end
    g
end

function is_known_call(e::Expr, func, sv)
    if !(is(e.head,:call) || is(e.head,:call1))
        return false
    end
    f = isconstantfunc(e.args[1], sv)
    return !is(f,false) && is(_ieval(f), func)
end

# compute set of vars assigned once
function find_sa_vars(ast)
    body = ast.args[3].args
    av = ObjectIdDict()
    av2 = ObjectIdDict()
    for i = 1:length(body)
        e = body[i]
        if isa(e,Expr) && is(e.head,:(=))
            lhs = e.args[1]
            if !has(av, lhs)
                av[lhs] = e.args[2]
            else
                av2[lhs] = true
            end
        end
    end
    filter!((var,_)->!has(av2,var), av)
    for vi in ast.args[2][2]
        if (vi[3]&1)!=0
            # remove captured vars
            delete!(av, vi[1], nothing)
        end
    end
    av
end

function occurs_outside_tupleref(e, sym, sv, tuplen)
    if is(e, sym) || (isa(e, SymbolNode) && is(e.name, sym))
        return true
    end
    if isa(e,Expr)
        if is_known_call(e, tupleref, sv) && isequal(e.args[2],sym)
            targ = e.args[2]
            if !(exprtype(targ)<:Tuple)
                return true
            end
            idx = e.args[3]
            if !isa(idx,Int) || !(1 <= idx <= tuplen)
                return true
            end
            return false
        end
        if is(e.head,:(=))
            return occurs_outside_tupleref(e.args[2], sym, sv, tuplen)
        else
            for a in e.args
                if occurs_outside_tupleref(a, sym, sv, tuplen)
                    return true
                end
            end
        end
    end
    return false
end

# eliminate allocation of unnecessary tuples
function tuple_elim_pass(ast::Expr)
    sv = inference_stack.sv
    bexpr = ast.args[3]::Expr
    body = (ast.args[3].args)::Array{Any,1}
    vs = find_sa_vars(ast)
    i = 1
    while i < length(body)
        e = body[i]
        if !(isa(e,Expr) && is(e.head,:(=)) && has(vs, e.args[1]))
            i += 1
            continue
        end
        var = e.args[1]
        rhs = e.args[2]
        if isa(rhs,Expr) && is_known_call(rhs, tuple, sv)
            tup = rhs.args
            nv = length(tup)-1
            if occurs_outside_tupleref(bexpr, var, sv, nv) || !is_local(sv, var)
                i += 1
                continue
            end

            delete!(body, i)  # remove tuple allocation
            # convert tuple allocation to a series of local var assignments
            vals = cell(nv)
            n_ins = 0
            for j=1:nv
                tupelt = tup[j+1]
                if isa(tupelt,Number) || isa(tupelt,String) || isa(tupelt,QuoteNode)
                    vals[j] = tupelt
                else
                    elty = exprtype(tupelt)
                    tmpv = unique_name(ast)
                    tmp = Expr(:(=), {tmpv,tupelt}, Any)
                    add_variable(ast, tmpv, elty)
                    insert!(body, i+n_ins, tmp)
                    vals[j] = SymbolNode(tmpv, elty)
                    n_ins += 1
                end
            end
            i += n_ins
            replace_tupleref(bexpr, var, vals, sv, i)
        else
            i += 1
        end
    end
end

function replace_tupleref(e, tupname, vals, sv, i0)
    if !isa(e,Expr)
        return
    end
    for i = i0:length(e.args)
        a = e.args[i]
        if isa(a,Expr) && is_known_call(a,tupleref, sv) &&
            isequal(a.args[2],tupname)
            e.args[i] = vals[a.args[3]]
        else
            replace_tupleref(a, tupname, vals, sv, 1)
        end
    end
end

function finfer(f, types)
    x = methods(f,types)[1]
    (tree, ty) = typeinf(x[3], x[1], x[2])
    if !isa(tree,Expr)
        return ccall(:jl_uncompress_ast, Any, (Any,Any), x[3], tree)
    end
    tree
end

#tfunc(f,t) = (methods(f,t)[1][3]).tfunc

ccall(:jl_enable_inference, Void, ())
