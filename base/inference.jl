# parameters limiting potentially-infinite types
const MAX_TYPEUNION_LEN = 3
const MAX_TYPE_DEPTH = 4
const MAX_TUPLETYPE_LEN  = 8
const MAX_TUPLE_DEPTH = 4

type NotFound
end

const NF = NotFound()

type StaticVarInfo
    sp::Tuple            # static parameters tuple
    cenv::ObjectIdDict   # types of closed vars
    vars::Array{Any,1}   # names of args and locals
    label_counter::Int   # index of the current highest label for this function
end

type EmptyCallStack
end

type CallStack
    ast
    mod::Module
    types::Tuple
    recurred::Bool
    cycleid::Int
    result
    prev::Union(EmptyCallStack,CallStack)
    sv::StaticVarInfo

    CallStack(ast, mod, types::ANY, prev) = new(ast, mod, types, false, 0, Bottom, prev)
end

inference_stack = EmptyCallStack()

# Julia compiler options struct (see jl_compileropts_t in src/julia.h)
immutable JLCompilerOpts
    julia_home::Ptr{Cchar}
    julia_bin::Ptr{Cchar}
    build_path::Ptr{Cchar}
    image_file::Ptr{Cchar}
    cpu_target::Ptr{Cchar}
    code_coverage::Int8
    malloc_log::Int8
    check_bounds::Int8
    dumpbitcode::Int8
    int_literals::Cint
    compile_enabled::Int8
    opt_level::Int8
    depwarn::Int8
    can_inline::Int8
end

compileropts() = unsafe_load(cglobal(:jl_compileropts, JLCompilerOpts))

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
is_closed(sv::StaticVarInfo, s::Symbol) = haskey(sv.cenv, s)
is_global(sv::StaticVarInfo, s::Symbol) =
    !is_local(sv,s) && !is_closed(sv,s) && !is_static_parameter(sv,s)

function _iisconst(s::Symbol)
    m = (inference_stack::CallStack).mod
    isdefined(m,s) && (ccall(:jl_is_const, Int32, (Any, Any), m, s) != 0)
end

_ieval(x::ANY) =
    ccall(:jl_interpret_toplevel_expr_in, Any, (Any, Any, Ptr{Void}, Csize_t),
          (inference_stack::CallStack).mod, x, C_NULL, 0)
_iisdefined(x::ANY) = isdefined((inference_stack::CallStack).mod, x)

_iisconst(s::SymbolNode) = _iisconst(s.name)
_iisconst(s::TopNode) = isconst(_basemod(), s.name)
_iisconst(x::Expr) = false
_iisconst(x::ANY) = true

function _basemod()
    m = (inference_stack::CallStack).mod
    if m === Core || m === Base
        return m
    end
    return Main.Base
end

cmp_tfunc = (x,y)->Bool

isType(t::ANY) = isa(t,DataType) && is((t::DataType).name,Type.name)

isvarargtype(t::ANY) = isa(t,DataType)&&is((t::DataType).name,Vararg.name)

const t_func = ObjectIdDict()
#t_func[tuple] = (0, Inf, (args...)->limit_tuple_depth(args))
t_func[throw] = (1, 1, x->Bottom)
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
t_func[fpiseq] = (2, 2, cmp_tfunc)
t_func[fpislt] = (2, 2, cmp_tfunc)
t_func[nan_dom_err] = (2, 2, (a, b)->a)
t_func[eval(Core.Intrinsics,:ccall)] =
    (3, Inf, (fptr, rt, at, a...)->(isType(rt) ? rt.parameters[1] : Any))
t_func[eval(Core.Intrinsics,:llvmcall)] =
    (3, Inf, (fptr, rt, at, a...)->(isType(rt) ? rt.parameters[1] :
                                    isa(rt,Tuple) ? map(x->x.parameters[1],rt) : Any))
t_func[eval(Core.Intrinsics,:cglobal)] =
    (1, 2, (fptr, t...)->(isempty(t) ? Ptr{Void} :
                          isType(t[1]) ? Ptr{t[1].parameters[1]} : Ptr))
t_func[eval(Core.Intrinsics,:select_value)] =
    # TODO: return Bottom if cnd is definitely not a Bool
    (3, 3, (cnd, x, y)->Union(x,y))
t_func[is] = (2, 2, cmp_tfunc)
t_func[issubtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[isdefined] = (1, Inf, (args...)->Bool)
t_func[Core.sizeof] = (1, 1, x->Int)
t_func[Union] = (0, Inf,
                 (args...)->(if all(isType,args)
                                 Type{Union(map(t->t.parameters[1],args)...)}
                             else
                                 Type
                             end))
t_func[_expr] = (1, Inf, (args...)->Expr)
t_func[method_exists] = (2, 2, cmp_tfunc)
t_func[applicable] = (1, Inf, (f, args...)->Bool)
t_func[tuplelen] = (1, 1, x->Int)
t_func[arraylen] = (1, 1, x->Int)
#t_func[arrayref] = (2,Inf,(a,i...)->(isa(a,DataType) && a<:Array ?
#                                     a.parameters[1] : Any))
#t_func[arrayset] = (3, Inf, (a,v,i...)->a)
#arraysize_tfunc(a, d) = Int
arraysize_tfunc = function (a, d...)
    if !is(d,())
        return Int
    end
    if isa(a,DataType) && a<:Array
        N = a.parameters[2]
        return isa(N,Int) ? NTuple{N,Int} : (Int...)
    else
        return (Int...)
    end
end
t_func[arraysize] = (1, 2, arraysize_tfunc)
t_func[pointerref] = (2,2,(a,i)->(isa(a,DataType) && a<:Ptr ? a.parameters[1] : Any))
t_func[pointerset] = (3, 3, (a,v,i)->a)

const typeof_tfunc = function (t)
    if isType(t)
        t = t.parameters[1]
        if isa(t,TypeVar)
            Type
        else
            Type{typeof(t)}
        end
    elseif isvarargtype(t)
        Vararg{typeof_tfunc(t.parameters[1])}
    elseif isa(t,DataType)
        if isleaftype(t)
            Type{t}
        else
            Type{TypeVar(:_,t)}
        end
    elseif isa(t,UnionType)
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
    wrapType = false
    if isType(t)
        t = t.parameters[1]
        wrapType = true
    end
    if isa(t,DataType) && is(t.name,NTuple.name)
        T = t.parameters[2]
        return wrapType ? Type{T} : T
    end
    if !isa(t,Tuple)
        return Any
    end
    if is(t,())
        return Bottom
    end
    n = length(t)
    last = tupleref(t,n)
    vararg = isvarargtype(last)
    if A !== () && isa(A[2],Integer)
        # index is a constant
        i = A[2]
        if i > n
            if vararg
                T = last.parameters[1]
            else
                return Bottom
            end
        elseif i == n && vararg
            T = last.parameters[1]
        elseif i <= 0
            return Bottom
        else
            T = tupleref(t,i)
        end
    else
        # index unknown, could be anything from the tuple
        if vararg
            types = tuple(t[1:(n-1)]..., last.parameters[1])
        else
            types = t
        end
        if !isa(types, Type)
            return Any
        end
        T = reduce(tmerge, Bottom, types)
        if wrapType
            return isleaftype(T) || isa(T,TypeVar) ? Type{T} : Type{TypeVar(:_,T)}
        else
            return T
        end
    end
    return wrapType ? (isa(T,Type) ? Type{T} : typeof(T)) : T
end
t_func[tupleref] = (2, 2, tupleref_tfunc)

function limit_type_depth(t::ANY, d::Int, cov::Bool, vars)
    if isa(t,TypeVar) || isa(t,TypeConstructor)
        return t
    end
    inexact = !cov && d > MAX_TYPE_DEPTH
    if isa(t,Tuple)
        t === () && return t
        if d > MAX_TYPE_DEPTH
            if isvatuple(t)
                R = Tuple
            else
                R = NTuple{length(t),Any}
            end
        else
            l0 = length(vars)
            R = map(x->limit_type_depth(x, d+1, true, vars), t)
            if !cov && (length(vars) > l0 || d == MAX_TYPE_DEPTH)
                inexact = true
            end
        end
    elseif isa(t,UnionType)
        t === Bottom && return t
        if d > MAX_TYPE_DEPTH
            R = Any
        else
            R = limit_type_depth(t.types, d, cov, vars)
            if isa(R,TypeVar)
                R = Union(R.ub...)
                inexact = true
            else
                R = Union(R...)
            end
        end
    elseif isa(t,DataType)
        P = t.parameters
        P === () && return t
        if d > MAX_TYPE_DEPTH
            R = t.name.primary
        else
            Q = map(x->limit_type_depth(x, d+1, false, vars), P)
            if !cov && any(p->contains_is(vars,p), Q)
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


const getfield_tfunc = function (A, s0, name)
    s = s0
    if isType(s)
        s = typeof(s.parameters[1])
        if s === TypeVar
            return Any
        end
    end
    if isa(s,UnionType)
        return reduce(tmerge, Bottom, map(t->getfield_tfunc(A, t, name), s.types))
    end
    if !isa(s,DataType) || s.abstract
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
        if isType(s0)
            sp = s0.parameters[1]
            if isa(sp,DataType) && !any(x->isa(x,TypeVar), sp.parameters)
                if fld === :parameters
                    return Type{sp.parameters}
                end
                if fld === :types
                    return Type{sp.types}
                end
                if fld === :super
                    return Type{sp.super}
                end
            end
        end
        for i=1:length(s.names)
            if is(s.names[i],fld)
                R = s.types[i]
                if s.parameters === ()
                    return R
                else
                    return limit_type_depth(R, 0, true,
                                            filter!(x->isa(x,TypeVar), Any[s.parameters...]))
                end
            end
        end
        return Bottom
    elseif isa(A[2],Int)
        if isa(A[1],Module) || s === Module
            return Bottom
        end
        i::Int = A[2]
        if i < 1 || i > length(s.names)
            return Bottom
        end
        return s.types[i]
    else
        return reduce(tmerge, Bottom, s.types)#Union(s.types...)
    end
end
t_func[getfield] = (2, 2, getfield_tfunc)
t_func[setfield!] = (3, 3, (o, f, v)->v)
const fieldtype_tfunc = function (A, s, name)
    if isType(s)
        s = s.parameters[1]
    else
        return Type
    end
    t = getfield_tfunc(A, s, name)
    if is(t,Bottom)
        return t
    end
    Type{isleaftype(t) || isa(t,TypeVar) ? t : TypeVar(:_, t)}
end
t_func[fieldtype] = (2, 2, fieldtype_tfunc)
t_func[Box] = (1, 1, (a,)->Box)

valid_tparam(x::ANY) = isa(x,Int) || isa(x,Symbol) || isa(x,Bool)

# TODO: handle e.g. apply_type(T, R::Union(Type{Int32},Type{Float64}))
const apply_type_tfunc = function (A, args...)
    if !isType(args[1])
        return Any
    end
    headtype = args[1].parameters[1]
    if isa(headtype,UnionType) || isa(headtype,Tuple) || isa(headtype,TypeVar)
        return args[1]
    end
    tparams = ()
    uncertain = false
    lA = length(A)
    for i=2:max(lA,length(args))
        ai = args[i]
        if isType(ai)
            uncertain |= (!isleaftype(ai))
            tparams = tuple(tparams..., ai.parameters[1])
        elseif isa(ai,Tuple) && all(isType,ai)
            tparams = tuple(tparams..., map(t->t.parameters[1], ai))
        elseif i<=lA && (isa(A[i],Int) || isa(A[i],Bool))
            tparams = tuple(tparams..., A[i])
        elseif i<=lA && isa(A[i],QuoteNode) && valid_tparam(A[i].value)
            tparams = tuple(tparams..., A[i].value)
        else
            if i<=lA && isa(A[i],Symbol) && isa(inference_stack,CallStack)
                sp = inference_stack.sv.sp
                s = A[i]
                found = false
                for j=1:2:length(sp)
                    if is(sp[j].name,s)
                        # static parameter
                        val = sp[j+1]
                        if valid_tparam(val)
                            tparams = tuple(tparams..., val)
                            found = true
                            break
                        end
                    end
                end
                if found
                    continue
                end
            end
            if i-1 > length(headtype.parameters)
                # too many parameters for type
                return Bottom
            end
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
    if type_too_complex(appl,0)
        return Type{TypeVar(:_,headtype)}
    end
    uncertain && !isa(appl,TypeVar) ? Type{TypeVar(:_,appl)} : Type{appl}
end
t_func[apply_type] = (1, Inf, apply_type_tfunc)

function tuple_tfunc(argtypes::ANY, limit)
    t = argtypes
    if limit
        t = limit_tuple_depth(t)
    end
    # tuple(Type{T...}) should give Type{(T...)}
    if t!==() && all(isType, t) && isvarargtype(t[length(t)].parameters[1])
        return Type{map(t->t.parameters[1], t)}
    end
    return t
end

function builtin_tfunction(f::ANY, args::ANY, argtypes::ANY)
    isva = isvatuple(argtypes)
    if is(f,tuple)
        return tuple_tfunc(argtypes, true)
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
        return (isa(a,DataType) && a<:Array ?
                a.parameters[1] : Any)
    elseif is(f,Expr)
        if length(argtypes) < 1 && !isva
            return Bottom
        end
        return Expr
    end
    tf = get(t_func::ObjectIdDict, f, false)
    if is(tf,false)
        # unknown/unhandled builtin
        return Any
    end
    tf = tf::(Real, Real, Function)
    if isva
        # only some t-funcs can handle varargs
        if is(f,apply_type) || is(f,typeof)
        else
            return Any
        end
    elseif !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
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

function isconstantfunc(f::ANY, sv::StaticVarInfo)
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

    if isa(f,QuoteNode) && isa(f.value, Function)
        return f.value
    end
    if isa(f,Function)
        return f
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
    if isa(t,UnionType)
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

limit_tuple_type = t -> limit_tuple_type_n(t, MAX_TUPLETYPE_LEN)

const limit_tuple_type_n = function (t::Tuple, lim::Int)
    n = length(t)
    if n > lim
        last = t[n]
        if isvarargtype(last)
            last = last.parameters[1]
        end
        tail = tuple(t[lim:(n-1)]..., last)
        tail = typeintersect(reduce(tmerge, Bottom, tail), Any)
        return tuple(t[1:(lim-1)]..., Vararg{tail})
    end
    return t
end

function func_for_method(m::Method, tt, env)
    if !m.isstaged
        return m.func.code
    end
    (ccall(:jl_instantiate_staged,Any,(Any,Any,Any),m,tt,env)).code
end

function abstract_call_gf(f, fargs, argtypes, e)
    if length(argtypes)>1 && (argtypes[1] <: Tuple) && argtypes[2]===Int
        # allow tuple indexing functions to take advantage of constant
        # index arguments.
        if f === Main.Base.getindex
            isa(e,Expr) && (e.head = :call1)
            return tupleref_tfunc(fargs, argtypes[1], argtypes[2])
        elseif f === Main.Base.next
            isa(e,Expr) && (e.head = :call1)
            return (tupleref_tfunc(fargs, argtypes[1], argtypes[2]), Int)
        elseif f === Main.Base.indexed_next
            isa(e,Expr) && (e.head = :call1)
            return (tupleref_tfunc(fargs, argtypes[1], argtypes[2]), Int)
        end
    end
    if (isdefined(Main.Base,:promote_type) && f === Main.Base.promote_type) ||
       (isdefined(Main.Base,:typejoin) && f === Main.Base.typejoin)
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
        if isdefined(Main.Base,:promote_type) && f === Main.Base.promote_type
            try
                RT = Type{f(c...)}
                isa(e,Expr) && (e.head = :call1)
                return RT
            catch
            end
        else
            isa(e,Expr) && (e.head = :call1)
            return Type{f(c...)}
        end
    end
    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    # here I picked 4.
    argtypes = limit_tuple_type(argtypes)
    applicable = _methods(f, argtypes, 4)
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
    if isa(e,Expr)
        if length(x)==1
            # method match is unique; mark it
            e.head = :call1
        else
            e.head = :call
        end
    end
    for (m::Tuple) in x
        local linfo
        try
            linfo = func_for_method(m[3],argtypes,m[2])
        catch
            rettype = Any
            break
        end
        sig = m[1]
        lsig = length(m[3].sig)
        # limit argument type tuple based on size of definition signature.
        # for example, given function f(T, Any...), limit to 3 arguments
        # instead of the default (MAX_TUPLETYPE_LEN)
        ls = length(sig)
        if ls > lsig+1
            fst = sig[lsig+1]
            allsame = true
            # allow specializing on longer arglists if all the trailing
            # arguments are the same, since there is no exponential
            # blowup in this case.
            for i = lsig+2:ls
                if sig[i] != fst
                    allsame = false
                    break
                end
            end
            if !allsame
                sig = limit_tuple_type_n(sig, lsig+1)
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

function invoke_tfunc(f, types, argtypes)
    argtypes = typeintersect(types,limit_tuple_type(argtypes))
    if is(argtypes,Bottom)
        return Bottom
    end
    applicable = _methods(f, types, -1)
    if isempty(applicable)
        return Any
    end
    for (m::Tuple) in applicable
        local linfo
        try
            linfo = func_for_method(m[3],types,m[2])
        catch
            return Any
        end
        if typeseq(m[1],types)
            tvars = m[2][1:2:end]
            (ti, env) = ccall(:jl_match_method, Any, (Any,Any,Any),
                              argtypes, m[1], tvars)::(Any,Any)
            (_tree,rt) = typeinf(linfo, ti, env, linfo)
            return rt
        end
    end
    return Any
end

function to_tuple_of_Types(t::ANY)
    if isType(t)
        p1 = t.parameters[1]
        if isa(p1,Tuple) && !isvatuple(p1)
            return map(t->Type{t}, p1)
        end
    end
    return t
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(af, aargtypes, vtypes, sv, e)
    if all(x->isa(x,Tuple), aargtypes) &&
        !any(isvatuple, aargtypes[1:(length(aargtypes)-1)])
        e.head = :call1
        # apply with known func with known tuple types
        # can be collapsed to a call to the applied func
        at = length(aargtypes) > 0 ?
        limit_tuple_type(tuple(append_any(aargtypes...)...)) : ()
        return abstract_call(af, (), at, vtypes, sv, ())
    end
    if is(af,tuple) && length(aargtypes)==1
        # tuple(xs...)
        aat = aargtypes[1]
        if aat <: AbstractArray
            # tuple(array...)
            # TODO: > 1 array of the same type
            tn = AbstractArray.name
            while isa(aat, DataType)
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
    if is(af,kwcall)
        return Any
    end
    # apply known function with unknown args => f(Any...)
    return abstract_call(af, (), Tuple, vtypes, sv, ())
end

function abstract_call(f, fargs, argtypes, vtypes, sv::StaticVarInfo, e)
    if is(f,_apply) && length(fargs)>1
        a2type = argtypes[2]
        af = isconstantfunc(fargs[2], sv)

        if !is(af,false)
            af = _ieval(af)
            if isa(af,Function)
                aargtypes = map(to_tuple_of_Types, argtypes[3:end])
                return abstract_apply(af, aargtypes, vtypes, sv, e)
            end
        end
        # TODO: this slows down inference a lot
        if !(a2type===Function) && isleaftype(a2type)
            # would definitely use call()
            call_func = _ieval(isconstantfunc(fargs[1], sv))
            if isa(call_func,Function)
                aargtypes = Any[ to_tuple_of_Types(argtypes[i]) for i=2:length(argtypes) ]
                aargtypes[1] = (aargtypes[1],)  # don't splat "function"
                return abstract_apply(call_func, tuple(aargtypes...), vtypes, sv, e)
            end
        end
        return Any
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
                return invoke_tfunc(af, sig, argtypes[3:end])
            end
        end
    end
    if !is(f,_apply) && isa(e,Expr) && (isa(f,Function) || isa(f,IntrinsicFunction))
        e.head = :call1
    end
    if is(f,getfield)
        val = isconstantref(e, sv)
        if !is(val,false)
            return abstract_eval_constant(_ieval(val))
        end
    end
    if is(f,kwcall)
        if length(argtypes) < 4
            return Bottom
        end
        if length(fargs) < 3
            return Any
        end
        kwcount = fargs[2]
        ff = isconstantfunc(fargs[3 + 2*kwcount], sv)
        if !(ff===false)
            ff = _ieval(ff)
            if isgeneric(ff) && isdefined(ff.env,:kwsorter)
                # use the fact that kwcall(...) calls ff.env.kwsorter
                posargt = argtypes[(5+2*kwcount):end]
                return abstract_call_gf(ff.env.kwsorter, (),
                                        tuple(Array{Any,1}, posargt...), e)
            end
        end
        # TODO: call() case
        return Any
    end
    if !isa(f,Function) && !isa(f,IntrinsicFunction) && _iisdefined(:call)
        call_func = _ieval(:call)
        if isa(call_func,Function)
            return abstract_call(call_func, e.args, tuple(Any[abstract_eval_constant(f),argtypes...]...), vtypes, sv, e)
        else
            return Any
        end
    end
    rt = builtin_tfunction(f, fargs, argtypes)
    #print("=> ", rt, "\n")
    return rt
end

function abstract_eval_arg(a::ANY, vtypes::ANY, sv::StaticVarInfo)
    t = abstract_eval(a, vtypes, sv)
    if isa(a,Symbol) || isa(a,SymbolNode)
        t = typeintersect(t,Any)  # remove Undef
    end
    if isa(t,TypeVar) && t.lb == Bottom && isleaftype(t.ub)
        t = t.ub
    end
    return t
end

function abstract_eval_call(e, vtypes, sv::StaticVarInfo)
    fargs = e.args[2:end]
    argtypes = tuple([abstract_eval_arg(a, vtypes, sv) for a in fargs]...)
    if any(x->is(x,Bottom), argtypes)
        return Bottom
    end
    called = e.args[1]
    func = isconstantfunc(called, sv)
    if is(func,false)
        if isa(called, LambdaStaticData)
            # called lambda expression (let)
            (_, result) = typeinf(called, argtypes, called.sparams, called,
                                  true)
            return result
        end
        ft = abstract_eval(called, vtypes, sv)
        if !(Function <: ft) && _iisdefined(:call)
            call_func = _ieval(:call)
            if isa(call_func,Function)
                return abstract_call(call_func, e.args, tuple(Any[ft,argtypes...]...), vtypes, sv, e)
            end
        end
        return Any
    end
    #print("call ", e.args[1], argtypes, " ")
    f = _ieval(func)
    return abstract_call(f, fargs, argtypes, vtypes, sv, e)
end

function abstract_eval(e::ANY, vtypes, sv::StaticVarInfo)
    if isa(e,QuoteNode)
        return typeof(e.value)
    elseif isa(e,TopNode)
        return abstract_eval_global(_basemod(), e.name)
    elseif isa(e,Symbol)
        return abstract_eval_symbol(e, vtypes, sv)
    elseif isa(e,SymbolNode)
        return abstract_eval_symbol(e.name, vtypes, sv)
    elseif isa(e,LambdaStaticData)
        return Function
    elseif isa(e,GetfieldNode)
        return abstract_eval_global(e.value::Module, e.name)
    end

    if !isa(e,Expr)
        return abstract_eval_constant(e)
    end
    e = e::Expr
    # handle:
    # call  null  new  &  static_typeof
    if is(e.head,:call) || is(e.head,:call1)
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
        t0 = abstract_eval(e.args[1], vtypes, sv)
        # intersect with Any to remove Undef
        t = typeintersect(t0, Any)
        if isa(t,DataType) && typeseq(t,t.name.primary)
            # remove unnecessary typevars
            t = t.name.primary
        end
        if is(t,Bottom) && Undef<:t0
            # the first time we see this statement the variable will probably
            # be Undef; return Bottom so this doesn't contribute to the type
            # we eventually pick.
        elseif is(t,Bottom)
            t = Type{Bottom}
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
    elseif is(e.head,:copyast)
        t = abstract_eval(e.args[1], vtypes, sv)
    else
        t = Any
    end
    if isa(t,TypeVar) && t.lb === Bottom
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
    #if isa(x,Tuple) && all(e->isa(e,Type), x)
    #    return Type{x}
    #end
    return typeof(x)
end

# Undef is the static type of a value location (e.g. variable) that is
# undefined. The corresponding run-time type is Bottom, since accessing an
# undefined location is an error. A non-lvalue expression cannot have
# type Undef, only Bottom.
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

function abstract_eval_symbol(s::Symbol, vtypes::ObjectIdDict, sv::StaticVarInfo)
    if haskey(sv.cenv,s)
        # consider closed vars to always have their propagated (declared) type
        return sv.cenv[s]
    end
    t = get(vtypes,s,NF)
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
        if s in sv.vars
            # local variable use not reached
            return Top
        end
        # global
        return abstract_eval_global(s)
    end
    return t
end

typealias VarTable ObjectIdDict

type StateUpdate
    var::Symbol
    vtype
    state::VarTable
end

function getindex(x::StateUpdate, s::Symbol)
    if is(x.var,s)
        return x.vtype
    end
    return get(x.state,s,NF)
end

function abstract_interpret(e::ANY, vtypes, sv::StaticVarInfo)
    !isa(e,Expr) && return vtypes
    # handle assignment
    if is(e.head,:(=))
        t = abstract_eval(e.args[2], vtypes, sv)
        lhs = e.args[1]
        if isa(lhs,SymbolNode)
            lhs = lhs.name
        end
        assert(isa(lhs,Symbol))
        return StateUpdate(lhs, t, vtypes)
    elseif is(e.head,:call) || is(e.head,:call1)
        abstract_eval(e, vtypes, sv)
    elseif is(e.head,:gotoifnot)
        abstract_eval(e.args[1], vtypes, sv)
    elseif is(e.head,:method)
        fname = e.args[1]
        if isa(fname,Symbol)
            return StateUpdate(fname, Function, vtypes)
        end
    end
    return vtypes
end

function type_too_complex(t::ANY, d)
    if d > MAX_TYPE_DEPTH
        return true
    end
    if isa(t,UnionType)
        p = t.types
    elseif isa(t,DataType)
        p = t.parameters
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
    if typea <: typeb
        return typeb
    end
    if typeb <: typea
        return typea
    end
    if isa(typea, Tuple) && isa(typeb, Tuple)
        if length(typea) == length(typeb) && !isvatuple(typea) && !isvatuple(typeb)
            return typejoin(typea, typeb)
        end
        return Tuple
    end
    u = Union(typea, typeb)
    if length(u.types) > MAX_TYPEUNION_LEN || type_too_complex(u, 0)
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return Undef<:u ? Top : Any
    end
    return u
end

tchanged(n::ANY, o::ANY) = is(o,NF) || (!is(n,NF) && !(n <: o))

stupdate(state::(), changes::VarTable, vars) = copy(changes)
stupdate(state::(), changes::StateUpdate, vars) = stupdate(ObjectIdDict(), changes, vars)

function stupdate(state::ObjectIdDict, changes::Union(StateUpdate,VarTable), vars)
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

function findlabel(labels, l)
    i = l+1 > length(labels) ? 0 : labels[l+1]
    if i == 0
        error("label ",l," not found")
    end
    return i
end

function label_counter(body)
    l = 0
    for b in body
        if isa(b,LabelNode) && (b::LabelNode).label > l
            l = (b::LabelNode).label
        end
    end
    l
end
genlabel(sv) = LabelNode(sv.label_counter += 1)

f_argnames(ast) =
    Any[(isa(x,Expr) ? x.args[1] : x) for x in ast.args[1]::Array{Any,1}]

is_rest_arg(arg::ANY) = (ccall(:jl_is_rest_arg,Int32,(Any,), arg) != 0)

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

CYCLE_ID = 1

#trace_inf = false
#enable_trace_inf() = (global trace_inf=true)

# def is the original unspecialized version of a method. we aggregate all
# saved type inference data there.
function typeinf(linfo::LambdaStaticData,atypes::Tuple,sparams::Tuple, def, cop)
    if linfo.module === Core
        atypes = Tuple
    end
    #dbg =
    #dotrace = true
    local ast::Expr, tfunc_idx
    curtype = Bottom
    redo = false
    # check cached t-functions
    tf = def.tfunc
    if !is(tf,())
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
                curtype = ccall(:jl_ast_rettype, Any, (Any,Any), def, code)
                return (code, curtype)
            end
        end
    end
    # TODO: typeinf currently gets stuck without this
    if linfo.name === :abstract_interpret || linfo.name === :tuple_elim_pass || linfo.name === :abstract_call_gf
        return (linfo.ast, Any)
    end

    ast0 = def.ast

    #if dbg
    #    print("typeinf ", linfo.name, " ", object_id(ast0), "\n")
    #end
    #if trace_inf
    #    print("typeinf ", linfo.name, " ", atypes, " ", linfo.file,":",linfo.line,"\n")
    #end
    # if isdefined(:STDOUT)
    #     write(STDOUT, "typeinf ")
    #     write(STDOUT, string(linfo.name))
    #     write(STDOUT, string(atypes))
    #     write(STDOUT, '\n')
    # end
    #print("typeinf ", ast0, " ", sparams, " ", atypes, "\n")

    global inference_stack, CYCLE_ID
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
    assert(is(ast.head,:lambda))
    locals = (ast.args[2][1])::Array{Any,1}
    vars = [args, locals]
    body = (ast.args[3].args)::Array{Any,1}
    n = length(body)

    maxlabel = 0
    for i=1:length(body)
        b = body[i]
        if isa(b,LabelNode)
            maxlabel = max(maxlabel, b.label+1)
        end
    end
    labels = zeros(Int, maxlabel)

    for i=1:length(body)
        b = body[i]
        if isa(b,LabelNode)
            labels[b.label+1] = i
        end
    end

    # our stack frame
    frame = CallStack(ast0, linfo.module, atypes, inference_stack)
    inference_stack = frame
    frame.result = curtype

    rec = false
    toprec = false

    s = Any[ () for i=1:n ]
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
                s[1][args[la]] = limit_tuple_depth(atypes[la:end])
            end
            la -= 1
        else
            if atypes === Tuple
                atypes = tuple(NTuple{la,Any}..., Tuple[1])
            end
        end
    end

    laty = length(atypes)
    if laty > 0
        lastatype = atypes[laty]
        if isvarargtype(lastatype)
            lastatype = lastatype.parameters[1]
            laty -= 1
        end
        if laty > la
            laty = la
        end
        for i=1:laty
            s[1][args[i]] = atypes[i]
        end
        for i=laty+1:la
            s[1][args[i]] = lastatype
        end
    else
        @assert la == 0
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
    sv = StaticVarInfo(sparams, cenv, vars, label_counter(body))
    frame.sv = sv

    recpts = IntSet()  # statements that depend recursively on our value
    W = IntSet()

    @label typeinf_top

    old_s1 = nothing

    # exception handlers
    cur_hand = ()
    handler_at = Any[ () for i=1:n ]

    push!(W,1)  # initial set of pc

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
            changes = abstract_interpret(stmt, s[pc], sv)
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
            if isa(stmt,GotoNode)
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
                        var = stmt.args[i]
                        if isa(var,SymbolNode)
                            var = var.name
                        end
                        # Store types that need to be fed back via type_goto
                        # in position s[1]. After finishing inference, if any
                        # of these types changed, start over with the fed-back
                        # types known from the beginning.
                        # See issue #3821 (using !typeseq instead of !subtype),
                        # and issue #7810.
                        vt = changes[var]
                        ot = s[1][var]
                        if !typeseq(vt,ot)
                            if old_s1 === nothing
                                old_s1 = copy(s[1])
                            end
                            s[1][var] = vt
                        end
                    end
                elseif is(hd,:return)
                    pc´ = n+1
                    rt = abstract_eval_arg(stmt.args[1], s[pc], sv)
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
                            push!(W,r)
                        end
                    end
                elseif is(hd,:enter)
                    l = findlabel(labels,stmt.args[1]::Int)
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

    if old_s1 !== nothing
        # if any type_gotos changed, clear state and restart.
        for (v_,t_) in s[1]
            if !typeseq(t_, old_s1[v_])
                for ll = 2:length(s)
                    s[ll] = ()
                end
                empty!(W)
                @goto typeinf_top
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
        @assert fulltree.args[3].head === :body
        if compileropts().can_inline == 1
            fulltree.args[3] = inlining_pass(fulltree.args[3], sv, fulltree)[1]
            # inlining can add variables
            sv.vars = append_any(f_argnames(fulltree), fulltree.args[2][1])
        end
        tuple_elim_pass(fulltree)
        tupleref_elim_pass(fulltree.args[3], sv)
        linfo.inferred = true
        fulltree = ccall(:jl_compress_ast, Any, (Any,Any), def, fulltree)
    end

    if !redo
        if is(def.tfunc,())
            def.tfunc = []
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
        tfarr[idx+1] = rec ? frame.result : fulltree
        tfarr[idx+2] = rec
    else
        def.tfunc[tfunc_idx] = rec ? frame.result : fulltree
        def.tfunc[tfunc_idx+1] = rec
    end

    inference_stack = (inference_stack::CallStack).prev
    return (fulltree, frame.result)
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

function eval_annotate(e::ANY, vtypes::ANY, sv::StaticVarInfo, decls, clo)
    if isa(e, Symbol)
        e = e::Symbol

        if !is_local(sv, e) && !is_closed(sv, e)
            # can get types of globals and static params from the environment
            return e
        end
        t = abstract_eval(e, vtypes, sv)
        record_var_type(e, t, decls)
        return (is(t,Any) || is(t,IntrinsicFunction)) ? e : SymbolNode(e, t)
    end

    if isa(e, SymbolNode)
        e = e::SymbolNode
        curtype = e.typ
        t = abstract_eval(e.name, vtypes, sv)
        if !(curtype <: t) || typeseq(curtype, t)
            record_var_type(e.name, t, decls)
            e.typ = t
        end
        return e
    end

    if isa(e, LambdaStaticData)
        push!(clo, e)
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
            #s.typ = abstract_eval(s.name, vtypes, sv)
            s = s.name
        else
            #e.args[1] = SymbolNode(s, abstract_eval(s, vtypes, sv))
        end
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, decls, clo)
        # TODO: if this def does not reach any uses, maybe don't do this
        rhstype = exprtype(e.args[2])
        if !is(rhstype,Bottom)
            record_var_type(s, rhstype, decls)
        end
        return e
    end
    i0 = is(head,:method) ? 2 : 1
    for i=i0:length(e.args)
        subex = e.args[i]
        if !(isa(subex,Number) || isa(subex,AbstractString))
            e.args[i] = eval_annotate(subex, vtypes, sv, decls, clo)
        end
    end
    if (head === :call || head === :call1) && isa(e.args[1],LambdaStaticData)
        called = e.args[1]
        fargs = e.args[2:end]
        argtypes = tuple([abstract_eval_arg(a, vtypes, sv) for a in fargs]...)
        # recur inside inner functions once we have all types
        tr,ty = typeinf(called, argtypes, called.sparams, called, false)
        called.ast = tr
    end
    e
end

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array{Any,1}, sv::ANY, rettype::ANY,
                       args)
    decls = ObjectIdDict()
    # initialize decls with argument types
    for arg in args
        decls[arg] = states[1][arg]
    end
    closures = []
    body = ast.args[3].args::Array{Any,1}
    for i=1:length(body)
        st_i = states[i]
        if st_i !== ()
            # st_i === ()  =>  unreached statement  (see issue #7836)
            body[i] = eval_annotate(body[i], st_i, sv, decls, closures)
        end
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
            # NOTE: this is disabled, as it leads to inlining too early.
            # See issue #4688. We should wait until inner functions are called
            # to optimize them; this will be done by the method cache or
            # builtins.c:jl_trampoline. However if jl_trampoline is changed then
            # this code will need to be restored.
            #na = length(a.args[1])
            #li.ast, _ = typeinf(li, ntuple(na+1, i->(i>na ? (Tuple)[1] : Any)),
            #                    li.sparams, li, false)
        end
    end

    ast
end

function sym_replace(e::ANY, from1, from2, to1, to2)
    if isa(e,Symbol)
        return _sym_repl(e::Symbol, from1, from2, to1, to2, e)
    end
    if isa(e,SymbolNode)
        e2 = _sym_repl(e.name, from1, from2, to1, to2, e)
        if isa(e2, SymbolNode) || !isa(e2, Symbol)
            return e2
        else
            return SymbolNode(e2, e.typ)
        end
    end
    if !isa(e,Expr)
        return e
    end
    e = e::Expr
    if e.head === :(=)
        # remove_redundant_temp_vars can only handle Symbols
        # on the LHS of assignments, so we make sure not to put
        # something else there
        @assert length(e.args) == 2
        e2 = _sym_repl(e.args[1]::Symbol, from1, from2, to1, to2, e.args[1]::Symbol)
        if isa(e2, SymbolNode)
            e2 = e2.name
        end
        e.args[1] = e2::Symbol
        e.args[2] = sym_replace(e.args[2], from1, from2, to1, to2)
    elseif e.head !== :line
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

# return an expr to evaluate "from.sym" in module "to"
function resolve_relative(sym, locals, args, from, to, typ, orig)
    if sym in locals || sym in args
        return GetfieldNode(from, sym, typ)
    end
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
            return TopNode(sym)
        end
    end
    return GetfieldNode(from, sym, typ)
end

# annotate symbols with their original module for inlining
function resolve_globals(e::ANY, locals, args, from, to, env1, env2)
    if isa(e,Symbol)
        s = e::Symbol
        if contains_is(env1, s) || contains_is(env2, s)
            return s
        end
        return resolve_relative(s, locals, args, from, to, Any, s)
    end
    if isa(e,SymbolNode)
        s = e::SymbolNode
        name = s.name
        if contains_is(env1, name) || contains_is(env2, name)
            return s
        end
        return resolve_relative(name, locals, args, from, to, s.typ, s)
    end
    if !isa(e,Expr)
        return e
    end
    e = e::Expr
    if e.head === :(=)
        # remove_redundant_temp_vars can only handle Symbols
        # on the LHS of assignments, so we make sure not to put
        # something else there
        e2 = resolve_globals(e.args[1]::Symbol, locals, args, from, to, env1, env2)
        if isa(e2, GetfieldNode)
            # abort when trying to inline a function which assigns to a global
            # variable in a different module, since `Mod.X=V` isn't allowed
            throw(e2)
#            e2 = e2::GetfieldNode
#            e = Expr(:call, top_setfield, e2.value, qn(e2.name),
#                resolve_globals(e.args[2], locals, args, from, to, env1, env2))
#            e.typ = e2.typ
        else
            e.args[1] = e2::Symbol
            e.args[2] = resolve_globals(e.args[2], locals, args, from, to, env1, env2)
        end
    elseif !is(e.head,:line)
        for i=1:length(e.args)
            subex = e.args[i]
            if !(isa(subex,Number) || isa(subex,AbstractString))
                e.args[i] = resolve_globals(subex, locals, args, from, to, env1, env2)
            end
        end
    end
    e
end

# count occurrences up to n+1
function occurs_more(e::ANY, pred, n)
    if isa(e,Expr)
        e = e::Expr
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
        return abstract_eval(x, emptydict, sv)
    elseif isa(x,QuoteNode)
        v = x.value
        if isa(v,Type)
            return Type{v}
        end
        return typeof(v)
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
    l = []
    for x in a
        if (isa(x,Expr) && is(x.head,:line)) || isa(x,LineNumberNode)
        else
            push!(l, x)
        end
    end
    l
end

# known affect-free calls (also effect-free)
const _pure_builtins = Any[tuple, tupleref, tuplelen, fieldtype, apply_type, is, isa, typeof, typeassert]

# known effect-free calls (might not be affect-free)
const _pure_builtins_volatile = Any[getfield, arrayref]

function is_pure_builtin(f)
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
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.jl_alloca ||  # this one is volatile, TODO: possibly also effect-free?
             f === Intrinsics.pointertoref) # this one is volatile
            return true
        end
    end
    return false
end

# detect some important side-effect-free calls (allow_volatile=true)
# and some affect-free calls (allow_volatile=false) -- affect_free means the call
# cannot be affected by previous calls, except assignment nodes
function effect_free(e::ANY, sv, allow_volatile::Bool)
    if isa(e,Symbol) || isa(e,SymbolNode) || isa(e,Number) || isa(e,AbstractString) ||
        isa(e,TopNode) || isa(e,QuoteNode) || isa(e,Type) || isa(e,Tuple)
        return true
    end
    if isconstantfunc(e, sv) !== false
        return true
    end
    if isa(e,Expr)
        e = e::Expr
        if e.head === :static_typeof
            return true
        end
        ea = e.args
        if e.head === :call || e.head === :call1
            if is_known_call_p(e, is_pure_builtin, sv)
                if !allow_volatile
                    if is_known_call(e, arrayref, sv)
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
                                if !isa(typ,Tuple)
                                    if !isa(typ,DataType) || typ.mutable
                                        return false
                                    end
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
                typ = exprtype(a)
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

# inline functions whose bodies "inline_worthy"
# where the function body doesn't contain any argument more than once.
# functions with closure environments or varargs are also excluded.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
function inlineable(f, e::Expr, atypes, sv, enclosing_ast)
    if !(isa(f,Function) || isa(f,IntrinsicFunction))
        return NF
    end
    argexprs = e.args[2:end]

    if is(f, typeassert) && length(atypes)==2
        # typeassert(x::S, T) => x, when S<:T
        if isType(atypes[2]) && isleaftype(atypes[2]) &&
            atypes[1] <: atypes[2].parameters[1]
            return (e.args[2],())
        end
    end
    if length(atypes)==2 && is(f,unbox) && isa(atypes[2],DataType) && !atypes[2].mutable && atypes[2].pointerfree
        # remove redundant unbox
        return (e.args[3],())
    end
    if isdefined(Main.Base,:isbits) && is(f,Main.Base.isbits) &&
        length(atypes)==1 && isType(atypes[1]) && effect_free(argexprs[1],sv,true) &&
        isleaftype(atypes[1].parameters[1])
        return (isbits(atypes[1].parameters[1]),())
    end
    # special-case inliners for known pure functions that compute types
    if isType(e.typ)
        if (is(f,apply_type) || is(f,fieldtype) ||
            (isdefined(Main.Base,:typejoin) && is(f,Main.Base.typejoin)) ||
            (isdefined(Main.Base,:promote_type) && is(f,Main.Base.promote_type))) &&
                isleaftype(e.typ.parameters[1])
            return (e.typ.parameters[1],())
        end
        if is(f,Union)
            union = e.typ.parameters[1]
            if isa(union,UnionType) && all(isleaftype, (union::UnionType).types)
                return (union,())
            end
        end
    end
    if is(f,tuple) && isa(e.typ,Tuple) && all(isType,e.typ) && isleaftype(e.typ) && effect_free(e,sv,true)
        return (map(t->t.parameters[1], e.typ), ())
    end
    if isa(f,IntrinsicFunction)
        return NF
    end

    meth = _methods(f, atypes, 1)
    if meth === false || length(meth) != 1
        return NF
    end
    meth = meth[1]::Tuple

    local linfo
    try
        linfo = func_for_method(meth[3],atypes,meth[2])
    catch
        return NF
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

    if !isa(linfo,LambdaStaticData) || meth[3].func.env !== ()
        return NF
    end

    sp = meth[2]::Tuple
    sp = tuple(sp..., linfo.sparams...)
    spvals = Any[ sp[i] for i in 2:2:length(sp) ]
    for i=1:length(spvals)
        if isa(spvals[i], TypeVar)
            return NF
        end
        if isa(spvals[i],Symbol)
            spvals[i] = QuoteNode(spvals[i])
        end
    end

    methargs = meth[1]::Tuple
    nm = length(methargs)
    if !(atypes <: methargs)
        incompletematch = true
        if !inline_incompletematch_allowed
            return NF
        end
    else
        incompletematch = false
    end

    (ast, ty) = typeinf(linfo, methargs, meth[2]::Tuple, linfo)
    if is(ast,())
        return NF
    end
    needcopy = true
    if !isa(ast,Expr)
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, ast)
        needcopy = false
    end
    ast = ast::Expr
    vinflist = ast.args[2][2]::Array{Any,1}
    for vi in vinflist
        if (vi[3]&1)!=0
            # captures variables (TODO)
            return NF
        end
    end

    body = Expr(:block)
    body.args = without_linenums(ast.args[3].args)::Array{Any,1}
    need_mod_annotate = true
    cost = 1.0
    if incompletematch
        cost *= 4
    end
    if is(f, next) || is(f, done)
        cost /= 4
    end
    if !inline_worthy(body, cost)
        if incompletematch
            # inline a typeassert-based call-site, rather than a
            # full generic lookup, using the inliner to handle
            # all the fiddly details
            numarg = length(argexprs)
            newnames = unique_names(ast,numarg)
            sp = ()
            spvals = []
            meth = (methargs, sp)
            locals = []
            newcall = Expr(:call, e.args[1])
            newcall.typ = ty
            for i = 1:numarg
                name = newnames[i]
                argtype = exprtype(argexprs[i])
                argtype = typeintersect(argtype,Any)  # remove Undef
                push!(locals, Any[name,argtype,0])
                push!(newcall.args, argtype===Any ? name : SymbolNode(name, argtype))
            end
            body.args = Any[Expr(:return, newcall)]
            ast = Expr(:lambda, newnames, Any[[], locals, []], body)
            need_mod_annotate = false
        else
            return NF
        end
    end

    spnames = Any[ sp[i].name for i=1:2:length(sp) ]
    enc_vinflist = enclosing_ast.args[2][2]::Array{Any,1}
    enc_locllist = enclosing_ast.args[2][1]::Array{Any,1}
    locllist = ast.args[2][1]::Array{Any,1}

    # check for vararg function
    args = f_argnames(ast)
    na = length(args)

    isva = false
    if na>0 && is_rest_arg(ast.args[1][na])
        vaname = args[na]
        len_argexprs = length(argexprs)
        valen = len_argexprs-na+1
        if valen>0 && !occurs_outside_tupleref(body, vaname, sv, valen)
            # argument tuple is not used as a whole, so convert function body
            # to one accepting the exact number of arguments we have.
            newnames = unique_names(ast,valen)
            if needcopy
                body = astcopy(body)
                needcopy = false
            end
            replace_tupleref!(ast, body, vaname, newnames, sv, 1)
            args = vcat(args[1:na-1], newnames)
            na = length(args)

            islocal = false # if the argument name is also used as a local variable,
                            # we need to keep it around as a variable name
            vnew = unique_name(enclosing_ast, ast)
            for vi in vinflist
                if vi[1] === vaname && vi[2] != 0
                    islocal = true
                    push!(enc_vinflist, Any[vnew, vi[2], vi[3]])
                end
            end
            if islocal
                push!(spnames, vaname)
                push!(spvals, vnew)
                push!(enc_locllist, vnew)
            end
        else
            # construct tuple-forming expression for argument tail
            vararg = mk_tuplecall(argexprs[na:end])
            argexprs = Any[argexprs[1:(na-1)]..., vararg]
            isva = true
        end
    elseif na != length(argexprs)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        @assert isvarargtype(atypes[na])
        return NF
    end

    @assert na == length(argexprs)

    if needcopy
        body = astcopy(body)
    end

    # avoid capturing free variables in enclosing function with the same name as in our function
    for localval in locllist
        localval = localval::Symbol
        vnew = unique_name(enclosing_ast, ast)
        push!(spnames, localval)
        push!(spvals, vnew)
        push!(enc_locllist, vnew)
        for vi in vinflist
            if vi[1] === localval
                push!(enc_vinflist, Any[vnew, vi[2], vi[3]])
            end
        end
    end

    # annotate variables in the body expression with their module
    if need_mod_annotate
        mfrom = linfo.module; mto = (inference_stack::CallStack).mod
        enc_capt = enclosing_ast.args[2][3]
        if !isempty(enc_capt)
            # add captured var names to list of locals
            enc_vars = vcat(enc_locllist, map(vi->vi[1], enc_capt))
        else
            enc_vars = enc_locllist
        end
        try
            body = resolve_globals(body, enc_vars, enclosing_ast.args[1], mfrom, mto, args, spnames)
        catch ex
            if isa(ex,GetfieldNode)
                return NF
            end
            rethrow(ex)
        end
    end

    # see if each argument occurs only once in the body expression
    stmts = []
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
        thrw = Expr(:call, :throw, Expr(:call, Main.Base.MethodError, (f, :inline), t))
        thrw.typ = Bottom
    end

    for i=na:-1:1 # stmts_free needs to be calculated in reverse-argument order
        a = args[i]
        aei = argexprs[i]
        aeitype = argtype = exprtype(aei)
        needtypeassert = false
        if incompletematch
            if isva
                if nm == 0
                    methitype = ()
                elseif i > nm
                    methitype = methargs[end]
                    if isvarargtype(methitype)
                        methitype = (methitype,)
                    else
                        methitype = ()
                    end
                else
                    methitype = tuple(methargs[i:end]...)
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
            if vi[1] === a && vi[3] != 0
                if !islocal
                    islocal = true
                end
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
            push!(spnames, a)
            push!(spvals, v1)
            vnew2 = unique_name(enclosing_ast, ast)
            v2 = (argtype===Any ? vnew2 : SymbolNode(vnew2,argtype))
            unshift!(body.args, Expr(:(=), a, v2))
            args[i] = a = vnew2
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
                if occ < 1
                    occ += occurs_more(b, x->is(x,a), 5)
                end
                if occ > 0 && affect_free && !effect_free(b, sv, true) #TODO: we could short-circuit this test better by memoizing effect_free(b) in the for loop over i
                    affect_free = false
                end
                if occ > 5
                    occ = 6
                    break
                end
            end
        end
        free = effect_free(aei,sv,true)
        if ((occ==0 && is(aeitype,Bottom)) || islocal || (occ > 1 && !inline_worthy(aei, occ*2)) ||
                (affect_free && !free) || (!affect_free && !effect_free(aei,sv,false)))
            if occ != 0 # islocal=true is implied by occ!=0
                vnew = unique_name(enclosing_ast, ast)
                add_variable(enclosing_ast, vnew, aeitype, !islocal)
                unshift!(stmts, Expr(:(=), vnew, aei))
                argexprs[i] = aeitype===Any ? vnew : SymbolNode(vnew,aeitype)
                stmts_free &= free
            elseif !free && !isType(aeitype)
                unshift!(stmts, aei)
                stmts_free = false
            end
        end
        if incompletematch
            unshift!(argexprs2, (argtype===Any ? a : SymbolNode(a,argtype)))
        end
    end
    if incompletematch && partmatch.args[1] != false
        unshift!(body.args, icall)
        unshift!(body.args, thrw)
        unshift!(body.args, partmatch)
        unshift!(argexprs2, top_tuple)
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
            body.args[i] = gn(newlabels[a.label+1])
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
        push!(body.args, Expr(:call,:error,"fatal error in type inference"))
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
                push!(stmts, gn(retstmt))
            end
        end
    end

    if multiret
        rettype = exprtype(ast.args[3])
        add_variable(enclosing_ast, retval, rettype, false)
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

inline_worthy(body, cost::Real) = true
function inline_worthy(body::Expr, cost::Real=1.0) # precondition: 0<cost
    if popmeta!(body, :inline)[1]
        return true
    end
    if popmeta!(body, :noinline)[1]
        return false
    end
    symlim = 1+5/cost
    if length(body.args) < symlim
        symlim *= 16
        if occurs_more(body, e->true, symlim) < symlim
            return true
        end
    end
    return false
end

gn(v) = ccall(:jl_new_struct, Any, (Any,Any...), GotoNode, v)
gn(v::LabelNode) = ccall(:jl_new_struct, Any, (Any,Any...), GotoNode, v.label)

const top_setfield = TopNode(:setfield)
const top_tupleref = TopNode(:tupleref)
const top_tuple = TopNode(:tuple)

function mk_tupleref(texpr, i, T)
    e = :(($top_tupleref)($texpr, $i))
    e.typ = T
    e
end

function mk_tuplecall(args)
    e = Expr(:call1, top_tuple, args...)
    e.typ = tuple_tfunc(tuple(Any[exprtype(x) for x in args]...), false)
    e
end

const basenumtype = Union(Int32,Int64,Float32,Float64,Complex64,Complex128,Rational)

function inlining_pass(e::Expr, sv, ast)
    if e.head == :method
        # avoid running the inlining pass on function definitions
        return (e,())
    end
    eargs = e.args
    if length(eargs)<1
        return (e,())
    end
    arg1 = eargs[1]
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
    else
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
                    restype = exprtype(res1)
                    vnew = unique_name(ast)
                    add_variable(ast, vnew, restype, true)
                    unshift!(stmts, Expr(:(=), vnew, res1))
                    argloc[i] = restype===Any ? vnew : SymbolNode(vnew,restype)
                else
                    argloc[i] = res1
                end
                if isa(res[2],Array)
                    res2 = res[2]::Array{Any,1}
                    if length(res2) > 0
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
        if isccall
            le = length(eargs)
            for i=5:2:le
                if i<le && (isa(eargs[i],Symbol) || isa(eargs[i],SymbolNode))
                    eargs[i+1] = 0
                end
            end
        end
        if is(e.head,:call1)
            e.head = :call
            f1 = f = isconstantfunc(arg1, sv)
            if !is(f,false)
                f = _ieval(f)
            end
            if f1===false || !(isa(f,Function) || isa(f,IntrinsicFunction))
                f = _ieval(:call)
                e.args = Any[is_global(sv,:call) ? (:call) : GetfieldNode((inference_stack::CallStack).mod, :call, Function), e.args...]
            end

            if is(f, ^) || is(f, .^)
                if length(e.args) == 3 && isa(e.args[3],Union(Int32,Int64))
                    a1 = e.args[2]
                    if isa(a1,basenumtype) || ((isa(a1,Symbol) || isa(a1,SymbolNode)) &&
                                               exprtype(a1) <: basenumtype)
                        if e.args[3]==2
                            e.args = Any[TopNode(:*), a1, a1]
                            f = *
                        elseif e.args[3]==3
                            e.args = Any[TopNode(:*), a1, a1, a1]
                            f = *
                        end
                    end
                end
            end

            for ninline = 1:100
                atypes = tuple(Any[exprtype(x) for x in e.args[2:end]]...)
                if length(atypes) > MAX_TUPLETYPE_LEN
                    atypes = limit_tuple_type(atypes)
                end
                res = inlineable(f, e, atypes, sv, ast)
                if isa(res,Tuple)
                    if isa(res[2],Array)
                        append!(stmts,res[2])
                    end
                    res = res[1]
                end

                if !is(res,NF)
                    # iteratively inline apply(f, tuple(...), tuple(...), ...) in order
                    # to simplify long vararg lists as in multi-arg +
                    if isa(res,Expr) && is_known_call(res, _apply, sv)
                        e = res::Expr
                        f = _apply
                    else
                        return (res,stmts)
                    end
                end

                if is(f,_apply)
                    na = length(e.args)
                    newargs = cell(na-3)
                    for i = 4:na
                        aarg = e.args[i]
                        t = to_tuple_of_Types(exprtype(aarg))
                        if isa(aarg,Expr) && is_known_call(aarg, tuple, sv)
                            # apply(f,tuple(x,y,...)) => f(x,y,...)
                            newargs[i-3] = aarg.args[2:end]
                        elseif isa(aarg, Tuple)
                            newargs[i-3] = Any[ QuoteNode(x) for x in aarg ]
                        elseif isa(t,Tuple) && !isvatuple(t) && effect_free(aarg,sv,true)
                            # apply(f,t::(x,y)) => f(t[1],t[2])
                            newargs[i-3] = Any[ mk_tupleref(aarg,j,t[j]) for j=1:length(t) ]
                        else
                            # not all args expandable
                            return (e,stmts)
                        end
                    end
                    e.args = [Any[e.args[3]], newargs...]

                    # now try to inline the simplified call

                    f = isconstantfunc(e.args[1], sv)
                    if f===false
                        return (e,stmts)
                    end
                    f = _ieval(f)
                else
                    return (e,stmts)
                end
            end
        end
    end
    return (e,stmts)
end

function add_variable(ast, name, typ, is_sa)
    vinf = Any[name, typ, 2+16*is_sa]
    locllist = ast.args[2][1]::Array{Any,1}
    vinflist = ast.args[2][2]::Array{Any,1}
    push!(locllist, name)
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
    locllist = ast.args[2][2]::Array{Any,1}
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
    locllist1 = ast1.args[2][2]::Array{Any,1}
    locllist2 = ast2.args[2][2]::Array{Any,1}
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
    locllist = ast.args[2][2]::Array{Any,1}
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
    if !(is(e.head,:call) || is(e.head,:call1))
        return false
    end
    f = isconstantfunc(e.args[1], sv)
    return !is(f,false) && is(_ieval(f), func)
end

function is_known_call_p(e::Expr, pred::Function, sv)
    if !(is(e.head,:call) || is(e.head,:call1))
        return false
    end
    f = isconstantfunc(e.args[1], sv)
    return !is(f,false) && pred(_ieval(f))
end

function is_var_assigned(ast, v)
    for vi in ast.args[2][2]
        if symequal(vi[1], v) && (vi[3]&2)!=0
            return true
        end
    end
    return false
end

function delete_var!(ast, v)
    filter!(vi->!symequal(vi[1],v), ast.args[2][2])
    filter!(x->!symequal(x,v), ast.args[2][1])
    filter!(x->!(isa(x,Expr) && (x.head === :(=) || x.head === :const) &&
                 symequal(x.args[1],v)),
            ast.args[3].args)
    ast
end

# remove all single-assigned vars v in "v = x" where x is an argument
# and not assigned.
# "sa" is the result of find_sa_vars
function remove_redundant_temp_vars(ast, sa)
    varinfo = ast.args[2][2]
    for (v,init) in sa
        if ((isa(init,Symbol) || isa(init,SymbolNode)) &&
            any(vi->symequal(vi[1],init), varinfo) &&
            !is_var_assigned(ast, init))

            if !occurs_undef(v, ast.args[3])
                # this transformation is not valid for vars used before def.
                # we need to preserve the point of assignment to know where to
                # throw errors (issue #4645).

                if (isa(init,SymbolNode) ? (init.typ<:local_typeof(v, varinfo)) : true)
                    # the transformation is not ideal if the assignment
                    # is present for the auto-unbox functionality
                    # (from inlining improved type inference information)
                    # and this transformation would worsen the type information
                    # everywhere later in the function

                    delete_var!(ast, v)
                    sym_replace(ast.args[3], [v], [], [init], [])
                end
            end
        end
    end
    ast
end
function local_typeof(v, varinfo)
    for (v2, typ, info) in varinfo
        if v === v2
            return typ
        end
    end
    @assert false "v not in varinfo"
end

occurs_undef(var, expr) =
    occurs_more(expr,
                e->(isa(e,SymbolNode) && symequal(var,e) && issubtype(Undef,e.typ)), 0)>0

# compute set of vars assigned once
function find_sa_vars(ast)
    body = ast.args[3].args
    av = ObjectIdDict()
    av2 = ObjectIdDict()
    vnames = ast.args[2][1]
    for i = 1:length(body)
        e = body[i]
        if isa(e,Expr) && is(e.head,:(=))
            lhs = e.args[1]
            if contains_is(vnames, lhs)  # exclude globals
                if !haskey(av, lhs)
                    av[lhs] = e.args[2]
                else
                    av2[lhs] = true
                end
            end
        end
    end
    filter!((var,_)->!haskey(av2,var), av)
    for vi in ast.args[2][2]
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
symequal(x::ANY       , y::ANY)        = is(x,y)

function occurs_outside_tupleref(e::ANY, sym::ANY, sv::StaticVarInfo, tuplen::Int)
    if is(e, sym) || (isa(e, SymbolNode) && is(e.name, sym))
        return true
    end
    if isa(e,Expr)
        e = e::Expr
        if is_known_call(e, tupleref, sv) && symequal(e.args[2],sym)
            targ = e.args[2]
            if !(exprtype(targ) <: Tuple)
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

# replace tupleref(tuple(exprs...), i) with exprs[i]
function tupleref_elim_pass(e::Expr, sv)
    for i = 1:length(e.args)
        ei = e.args[i]
        if isa(ei,Expr)
            tupleref_elim_pass(ei, sv)
            if is_known_call(ei, tupleref, sv) && length(ei.args)==3 &&
                isa(ei.args[3],Int)
                e1 = ei.args[2]
                j = ei.args[3]
                if isa(e1,Expr)
                    if is_known_call(e1, tuple, sv) && (1 <= j < length(e1.args))
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
                elseif isa(e1,Tuple) && (1 <= j <= length(e1))
                    e1j = e1[j]
                    if !(isa(e1j,Number) || isa(e1j,AbstractString) || isa(e1j,Tuple) ||
                         isa(e1j,Type))
                        e1j = QuoteNode(e1j)
                    end
                    e.args[i] = e1j
                elseif isa(e1,QuoteNode) && isa(e1.value,Tuple) && (1 <= j <= length(e1.value))
                    e.args[i] = QuoteNode(e1.value[j])
                end
            end
        end
    end
end

# eliminate allocation of unnecessary tuples
function tuple_elim_pass(ast::Expr)
    sv = inference_stack.sv
    bexpr = ast.args[3]::Expr
    body = (ast.args[3].args)::Array{Any,1}
    vs = find_sa_vars(ast)
    remove_redundant_temp_vars(ast, vs)
    i = 1
    while i < length(body)
        e = body[i]
        if !(isa(e,Expr) && is(e.head,:(=)) && haskey(vs, e.args[1]))
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

            deleteat!(body, i)  # remove tuple allocation
            # convert tuple allocation to a series of local var assignments
            vals = cell(nv)
            n_ins = 0
            for j=1:nv
                tupelt = tup[j+1]
                if isa(tupelt,Number) || isa(tupelt,AbstractString) || isa(tupelt,QuoteNode)
                    vals[j] = tupelt
                else
                    elty = exprtype(tupelt)
                    tmpv = unique_name(ast)
                    tmp = Expr(:(=), tmpv, tupelt)
                    add_variable(ast, tmpv, elty, true)
                    insert!(body, i+n_ins, tmp)
                    vals[j] = SymbolNode(tmpv, elty)
                    n_ins += 1
                end
            end
            i += n_ins
            replace_tupleref!(ast, bexpr, var, vals, sv, i)
        else
            i += 1
        end
    end
end

function replace_tupleref!(ast, e::ANY, tupname, vals, sv, i0)
    if !isa(e,Expr)
        return
    end
    for i = i0:length(e.args)
        a = e.args[i]
        if isa(a,Expr) && is_known_call(a, tupleref, sv) &&
            symequal(a.args[2],tupname)
            val = vals[a.args[3]]
            if isa(val,SymbolNode) && a.typ <: val.typ && !typeseq(a.typ,val.typ)
                # original expression might have better type info than
                # the tuple element expression that's replacing it.
                val.typ = a.typ
                for vi in ast.args[2][2]::Array{Any,1}
                    if vi[1] === val.name
                        vi[2] = a.typ
                        break
                    end
                end
            end
            e.args[i] = val
        else
            replace_tupleref!(ast, a, tupname, vals, sv, 1)
        end
    end
end

code_typed(f, types::ANY) = code_typed(call, tuple(isa(f,Type)?Type{f}:typeof(f), types...))
function code_typed(f::Function, types::ANY)
    asts = []
    for x in _methods(f,types,-1)
        linfo = func_for_method(x[3],types,x[2])
        (tree, ty) = typeinf(linfo, x[1], x[2])
        if !isa(tree,Expr)
            push!(asts, ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, tree))
        else
            push!(asts, tree)
        end
    end
    asts
end

return_types(f, types::ANY) = return_types(call, tuple(isa(f,Type)?Type{f}:typeof(f), types...))
function return_types(f::Function, types::ANY)
    rt = []
    for x in _methods(f,types,-1)
        linfo = func_for_method(x[3],types,x[2])
        (tree, ty) = typeinf(linfo, x[1], x[2])
        push!(rt, ty)
    end
    rt
end

#tfunc(f,t) = methods(f,t)[1].func.code.tfunc

ccall(:jl_enable_inference, Void, ())
