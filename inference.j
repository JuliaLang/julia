# infrastructure needed for type inference:
# * shared assoc list
# * IntSet
# * printing exprs
# * compute type intersection
# - more method table reflection
#   . cached t-functions
#   . abstract_invoke()
#     * consult t-func cache
#     * determine applicable methods
#     * abstract_invoke all of them, type-union the result, and cache it
# * hash table of symbols
# * eval
# - t-functions for builtins
# * deal with call stack and recursive types
# - isconstant()
# * approximate static parameters
# - use type bounds
# * reflection for constructors
# * be able to infer the results of promote()
# - avoid branches when condition can be statically evaluated

# parameters limiting potentially-infinite types
MAX_TYPEUNION_SIZE = 3
MAX_TUPLETYPE_LEN  = 10
MAX_TUPLE_DEPTH = 4

struct NotFound
end

NF = NotFound()

struct EmptyCallStack
end

struct CallStack
    ast
    types::Tuple
    n::Int32
    recurred::Bool
    result
    prev::Union(EmptyCallStack,CallStack)

    CallStack(ast, types, prev::EmptyCallStack) =
        new(ast, types, 0, false, None, prev)
    CallStack(ast, types, prev::CallStack) =
        new(ast, types, prev.n+1, false, None, prev)
end

# TODO thread local
inference_stack = EmptyCallStack()

tintersect(a,b) = ccall(:jl_type_intersection, Any, (Any,Any), a, b)
tmatch(a,b) = ccall(:jl_type_match, Any, (Any,Any), a, b)

getmethods(f,t) = ccall(:jl_matching_methods, Any, (Any,Any), f, t)::Tuple

typeseq(a,b) = subtype(a,b)&&subtype(b,a)

isbuiltin(f) = ccall(:jl_is_builtin, Int32, (Any,), f) != 0
isgeneric(f) = ccall(:jl_is_genericfunc, Int32, (Any,), f) != 0
isleaftype(t) = ccall(:jl_is_leaf_type, Int32, (Any,), t) != 0

# for now assume all global functions constant
# TODO
isconstant(s::Symbol) = isbound(s) && (e=eval(s);
                                       isa(e,Function) || isa(e,Type))
isconstant(s::Expr) = is(s.head,:quote)
isconstant(x) = true

cmp_tfunc = (x,y)->Bool

isType(t) = isa(t,TagKind) && is(t.name,Type.name)

isseqtype(t) = isa(t,TagKind) && is(t.name.name,:...)

t_func = idtable()
t_func[tuple] = (0, Inf, (args...)->limit_tuple_depth(args))
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
t_func[sgt_int] = (2, 2, cmp_tfunc)
t_func[ugt_int] = (2, 2, cmp_tfunc)
t_func[sge_int] = (2, 2, cmp_tfunc)
t_func[uge_int] = (2, 2, cmp_tfunc)
t_func[eq_float] = (2, 2, cmp_tfunc)
t_func[ne_float] = (2, 2, cmp_tfunc)
t_func[lt_float] = (2, 2, cmp_tfunc)
t_func[le_float] = (2, 2, cmp_tfunc)
t_func[gt_float] = (2, 2, cmp_tfunc)
t_func[ge_float] = (2, 2, cmp_tfunc)
t_func[ccall] =
    (3, Inf, (fptr, rt, at, a...)->(isType(rt) ? rt.parameters[1] : Any))
t_func[is] = (2, 2, cmp_tfunc)
t_func[subtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[method_exists] = (2, 2, cmp_tfunc)
t_func[applicable] = (1, Inf, (f, args...)->Bool)
#t_func[new_generic_function] = (1, 1, s->(Any-->Any))
t_func[tuplelen] = (1, 1, x->Int32)
t_func[arraylen] = (1, 1, x->Int32)
t_func[arrayref] = (2, 2, (a,i)->(isa(a,StructKind) && subtype(a,Array) ?
                                  a.parameters[1] : Any))
t_func[arrayset] = (3, 3, (a,i,v)->a)
t_func[Array] =
    (1, Inf,
function (T, dims...)
    nd = length(dims)
    if nd==1
        dt = dims[1]
        if isa(dt,Tuple)
            if length(dt) > 0 && isseqtype(dt[length(dt)])
                # Array(T, (d...))
                nd = Array.parameters[2]
            else
                # Array(T, (m, n))
                nd = length(dt)
            end
        elseif subtype(Tuple, dt)
            # Array(T, ??)
            nd = Array.parameters[2]
        end
    end
    et = isType(T) ? T.parameters[1] : T
    Array{et,nd}
end)

static_convert(to, from) = (subtype(from, to) ? from : to)
function static_convert(to::Tuple, from::Tuple)
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
            return Union()
        end
        # tuple conversion calls convert recursively
        if isseqtype(ce)
            R = abstract_call_gf(convert, (), (Type{pe}, ce.parameters[1]), ())
            isType(R) && (R = R.parameters[1])
            result[i] = ...{R}
        else
            R = abstract_call_gf(convert, (), (Type{pe}, ce), ())
            isType(R) && (R = R.parameters[1])
            result[i] = R
        end
    end
    a2t(result)
end
t_func[:convert] =
    (2, 2, (t,x)->(if isa(t,Tuple) && allp(isType,t)
                       t = Type{map(t->t.parameters[1],t)}
                   end;
                   isType(t) ? static_convert(t.parameters[1],x) :
                   Any))

t_func[typeof] =
    (1, 1, t->(isType(t)      ? Type{typeof(t.parameters[1])} :
               isa(t,TagKind) ? Type{t} :
               typeof(t)))
# involving constants: typeassert, tupleref, getfield, apply_type
# therefore they get their arguments unevaluated
t_func[typeassert] =
    (2, 2, (A, v, t)->(isType(t) ? tintersect(v,t.parameters[1]) :
                       isconstant(A[2]) ? tintersect(v,eval(A[2])) :
                       Any))

function tupleref_tfunc(A, t, i)
    if is(t,())
        return None
    end
    if isa(t,TagKind) && is(t.name,NTuple.name)
        return t.parameters[2]
    end
    if !isa(t,Tuple)
        return Any
    end
    n = length(t)
    last = t[n]
    vararg = isseqtype(last)
    if isa(A[2],Int)
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
            return t[i]
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

function getfield_tfunc(A, s, name)
    if !isa(s,StructKind)
        return Any
    end
    if isa(A[2],Expr) && is(A[2].head,:quote) && isa(A[2].args[1],Symbol)
        fld = A[2].args[1]
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

# TODO: handle e.g. apply_type(T, R::Union(Type{Int32},Type{Float64}))
function apply_type_tfunc(A, args...)
    if !isType(args[1])
        return Any
    end
    tparams = ()
    for i=2:length(A)
        if isType(args[i])
            tparams = append(tparams, (args[i].parameters[1],))
        elseif isa(A[i],Int32)
            tparams = append(tparams, (A[i],))
        else
            return args[1]
        end
    end
    # good, all arguments understood
    Type{apply_type(args[1].parameters[1], tparams...)}
end
t_func[apply_type] = (1, Inf, apply_type_tfunc)

# other: apply, setfield

function builtin_tfunction(f, args::Tuple, argtypes::Tuple)
    tf = get(t_func::IdTable, f, false)
    if is(tf,false)
        # struct constructor
        if isa(f, StructKind)
            return f
        end
        # unknown/unhandled builtin
        return Any
    end
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return None
    end
    if is(f,typeassert) || is(f,tupleref) || is(f,getfield) || is(f,apply_type)
        # TODO: case of apply(), where we do not have the args
        return tf[3](args, argtypes...)
    end
    return tf[3](argtypes...)
end

struct StaticVarInfo
    sp::Tuple
    cenv::IdTable   # types of closed vars
end

function abstract_eval(e::Expr, vtypes, sv::StaticVarInfo)
    t = abstract_eval_expr(e, vtypes, sv)
    e.type = t
    return t
end

function a2t(a::Vector)
    n = length(a)
    if n==2 return (a[1],a[2]) end
    if n==1 return (a[1],) end
    if n==3 return (a[1],a[2],a[3]) end
    if n==0 return () end
    t = (a[1],a[2],a[3],a[4])
    if n==4 return t end
    for i=5:n
        t = tuple(t..., a[i])
    end
    t
end

function isconstantfunc(f, vtypes, sv::StaticVarInfo)
    if isa(f,Expr) && is(f.head,:top)
        abstract_eval(f, vtypes, sv)
        assert(isa(f.args[1],Symbol))
        return (true, f.args[1])
    end
    return (isa(f,Symbol) && !has(vtypes,f), f)
end

isvatuple(t) = (n = length(t); n > 0 && isseqtype(t[n]))

limit_tuple_depth(t) = limit_tuple_depth(t,0)

function limit_tuple_depth(t,d)
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
        tail = reduce(tmerge, None, tail)
        return tuple(t[1:(MAX_TUPLETYPE_LEN-1)]..., ...{tail})
    end
    return t
end

function abstract_call_gf(f, fargs, argtypes, e)
    applicable = getmethods(f, argtypes)
    rettype = None
    x = applicable
    if isa(e,Expr)
        if !is(x,()) && is(x[5],())
            # method match is unique; mark it
            e.head = :call1
        else
            e.head = :call
        end
    end
    ctr = 0
    while !is(x,())
        ctr += 1
        # don't consider more than N methods. this trades off between
        # compiler performance and generated code performance.
        # typically, considering many methods means spending lots of time
        # obtaining poor type information.
        # It is important for N to be >= the number of methods in the error()
        # function, so we can still know that error() is always None.
        if ctr > 4
            rettype = Any
            break
        end
        #print(x,"\n")
        if isa(x[3],Symbol)
            # when there is a builtin method in this GF, we get
            # a symbol with the name instead of a LambdaStaticData
            rt = builtin_tfunction(x[3], fargs, x[1])
        elseif isa(x[3],Type)
            # constructor
            rt = x[3]
        else
            (_tree,rt) = typeinf(x[3], x[1], x[2], true)
        end
        rettype = tmerge(rettype, rt)
        if is(rettype,Any)
            break
        end
        x = x[5]
    end
    # if rettype is None we've found a method not found error
    #print("=> ", rettype, "\n")
    return rettype
end

function abstract_call(f, fargs, argtypes, vtypes, sv::StaticVarInfo, e)
    if isbuiltin(f)
        if is(f,apply) && length(fargs)>0
            (isfunc, af) = isconstantfunc(fargs[1], vtypes, sv)
            if isfunc && isbound(af)
                aargtypes = argtypes[2:]
                if allp(x->isa(x,Tuple), aargtypes) &&
                   !anyp(isvatuple, aargtypes[1:(length(aargtypes)-1)])
                    e.head = :call1
                    # apply with known func with known tuple types
                    # can be collapsed to a call to the applied func
                    at = length(aargtypes) > 0 ?
                         limit_tuple_type(append(aargtypes...)) : ()
                    return abstract_call(eval(af), (), at, vtypes, sv, ())
                end
            end
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

ft_tfunc(ft, argtypes) = ccall(:jl_func_type_tfunc, Any,
                               (Any, Any), ft, argtypes)

function abstract_eval_call(e, vtypes, sv::StaticVarInfo)
    fargs = a2t(e.args[2:])
    argtypes = map(x->abstract_eval(x,vtypes,sv), fargs)
    (isfunc, func) = isconstantfunc(e.args[1], vtypes, sv)
    if !isfunc
        # TODO: lambda expression (let)
        ft = abstract_eval(e.args[1], vtypes, sv)
        if isa(ft,FuncKind)
            # use inferred function type
            return ft_tfunc(ft, argtypes)
        elseif isType(ft) && isbuiltin(ft.parameters[1]) &&
            # struct constructor
            isa(ft.parameters[1],StructKind)
            return ft.parameters[1]
        end
        return Any
    end
    #print("call ", e.args[1], argtypes, " ")
    if !isbound(func)
        #print("=> ", Any, "\n")
        return Any
    end
    f = eval(func)
    return abstract_call(f, fargs, argtypes, vtypes, sv, e)
end

function abstract_eval_expr(e, vtypes, sv::StaticVarInfo)
    # handle:
    # call  lambda  quote  null  top  unbound static_typeof
    if is(e.head,:call) || is(e.head,:call1)
        return abstract_eval_call(e, vtypes, sv)
    elseif is(e.head,:top)
        return abstract_eval_global(e.args[1])
    #elseif is(e.head,:unbound)
    #    return Bool
    elseif is(e.head,:method)
        return Any-->Any
    elseif is(e.head,:null)
        return ()
    elseif is(e.head,:quote)
        return typeof(e.args[1])
    elseif is(e.head,:static_typeof)
        t = abstract_eval(e.args[1], vtypes, sv)
        # intersect with Any to remove Undef
        t = tintersect(t, Any)
        return Type{t}
    end
    Any
end

ast_rettype(ast) = ast.args[3].type

function abstract_eval_constant(x)
    if isa(x,TagKind) || isa(x,TypeVar)
        return Type{x}
    end
    if isa(x,LambdaStaticData)
        return Any-->Any
    end
    return typeof(x)
end

function abstract_eval_global(s::Symbol)
    if !isbound(s)
        return Undef
    end
    if isconstant(s)
        return abstract_eval_constant(eval(s))
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
    t = get(vtypes,s,NF)
    if is(t,NF)
        sp = sv.sp
        for i=1:2:length(sp)
            if is(sp[i].name,s)
                # static parameter
                return abstract_eval_constant(sp[i+1])
            end
        end
        # global
        return abstract_eval_global(s)
    end
    return t
end

abstract_eval(x, vtypes, sv::StaticVarInfo) = abstract_eval_constant(x)

typealias VarTable IdTable

struct StateUpdate
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

interpret(e, vtypes, sv::StaticVarInfo) = vtypes

function interpret(e::Expr, vtypes, sv::StaticVarInfo)
    # handle assignment
    if is(e.head,symbol("="))
        t = abstract_eval(e.args[2], vtypes, sv)
        lhs = e.args[1]
        assert(isa(lhs,Symbol))
        return StateUpdate(lhs, t, vtypes)
    elseif is(e.head,:call) || is(e.head,:call1)
        abstract_eval(e, vtypes, sv)
    elseif is(e.head,:gotoifnot)
        abstract_eval(e.args[1], vtypes, sv)
    end
    return vtypes
end

tchanged(n, o) = is(o,NF) || (!is(n,NF) && !subtype(n,o))

function changed(new::Union(StateUpdate,VarTable), old, vars)
    for i = 1:length(vars)
        v = vars[i]
        if tchanged(new[v], get(old,v,NF))
            return true
        end
    end
    return false
end

badunion(t) = ccall(:jl_union_too_complex, Int32, (Any,), t) != 0

typealias Top Union(Any,Undef)

function tmerge(typea, typeb)
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
    t = ccall(:jl_compute_type_union,Any,(Any,), (typea, typeb))
    #if length(t)==1
    #    return t[1]
    #else
    #end
    if length(t)==0
        return None
    end
    if badunion(t)
        for elt = t
            if subtype(Undef,elt)
                return Top
            end
        end
        return Any
    end
    u = Union(t...)
    if length(u.types) > MAX_TYPEUNION_SIZE
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return subtype(Undef,u) ? Top : Any
    end
    return u
end

function update(state, changes::Union(StateUpdate,VarTable), vars)
    for i = 1:length(vars)
        v = vars[i]
        newtype = changes[v]
        oldtype = get(state::IdTable,v,NF)
        if tchanged(newtype, oldtype)
            state[v] = tmerge(oldtype, newtype)
        end
    end
    state
end

function findlabel(body, l)
    for i=1:length(body)
        b = body[i]
        if isa(b,Expr) && is(b.head,:label) && b.args[1]==l
            return i
        end
    end
    error("label ",l," not found")
end

f_argnames(ast) =
    map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1]::Array{Any,1})

is_rest_arg(arg) = (isa(arg,Expr) && is(arg.head,symbol("::")) &&
                    ccall(:jl_is_rest_arg,Int32,(Any,), arg) != 0)

function typeinf_task(caller)
    result = ()
    while true
        (caller, args) = yieldto(caller, result)
        result = typeinf_ext_(args...)
    end
end

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

function typeinf_ext(linfo, atypes, sparams, cop, def)
    global inference_stack
    last = inference_stack
    inference_stack = EmptyCallStack()
    result = typeinf(linfo, atypes, sparams, cop, def)
    inference_stack = last
    return result
end

typeinf(linfo,atypes,sparams,copy) = typeinf(linfo,atypes,sparams,copy,linfo)

# def is the original unspecialized version of a method. we aggregate all
# saved type inference data there.
function typeinf(linfo::LambdaStaticData,atypes::Tuple,sparams::Tuple, cop, def)
    #dotrace = true#is(linfo,sizestr)
    # check cached t-functions
    tf = linfo.tfunc
    while !is(tf,())
        if typeseq(tf[1],atypes)
            return (tf[2], ast_rettype(tf[2]))
        end
        tf = tf[3]
    end

    ast0 = linfo.ast

    #print("typeinf ", ast0, " ", sparams, " ", atypes, "\n")

    global inference_stack
    # check for recursion
    f = inference_stack
    while !isa(f,EmptyCallStack)
        if is(f.ast,ast0) && typeseq(f.types, atypes)
            # return best guess so far
            f.recurred = true
            #print("*==> ", f.result,"\n")
            return ((),f.result)
        end
        f = f.prev
    end

    #print("typeinf ", linfo.name, atypes, "\n")

    ast = cop ? copy(ast0) : ast0

    assert(is(ast.head,:lambda))
    args = f_argnames(ast)
    locals = ast.args[2].args[1].args
    vars = append(args, locals)
    body = ast.args[3].args

    n = length(body)
    s = { idtable() | i=1:n }
    recpts = IntSet(n+1)  # statements that depend recursively on our value
    W = IntSet(n+1)
    # initial set of pc
    adjoin(W,1)
    # initial types
    for v=vars
        s[1][v] = Undef
    end
    la = length(args)
    if la > 0
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
    cenv = idtable()
    for vi = ast.args[2].args[3]
        cenv[vi[1]] = vi[2]
        s[1][vi[1]] = vi[2]
    end
    sv = StaticVarInfo(sparams, cenv)

    # our stack frame
    frame = CallStack(ast0, atypes, inference_stack)
    inference_stack = frame

    # exception handlers
    cur_hand = ()
    handler_at = { () | i=1:n }

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
            changes = interpret(stmt, s[pc], sv)
            if frame.recurred
                adjoin(recpts, pc)
                frame.recurred = false
            end
            if !is(cur_hand,())
                # propagate type info to exception handler
                l = cur_hand[1]::Int32
                if changed(changes, s[l], vars)
                    adjoin(W, l)
                    update(s[l], changes, vars)
                end
            end
            pc´ = pc+1
            if isa(stmt,Expr)
                hd = stmt.head
                if is(hd,:goto)
                    pc´ = findlabel(body,stmt.args[1])
                elseif is(hd,:gotoifnot)
                    l = findlabel(body,stmt.args[2])
                    handler_at[l] = cur_hand
                    if changed(changes, s[l], vars)
                        adjoin(W, l)
                        update(s[l], changes, vars)
                    end
                elseif is(hd,symbol("return"))
                    pc´ = n+1
                    rt = abstract_eval(stmt.args[1], s[pc], sv)
                    if tchanged(rt, frame.result)
                        frame.result = tmerge(frame.result, rt)
                        # revisit states that recursively depend on this
                        for r=recpts
                            adjoin(W,r)
                        end
                    end
                elseif is(hd,:enter)
                    l = findlabel(body,stmt.args[1]::Int32)
                    cur_hand = (l,cur_hand)
                    handler_at[l] = cur_hand
                elseif is(hd,:leave)
                    for i=1:((stmt.args[1])::Int32)
                        cur_hand = cur_hand[2]
                    end
                end
            end
            if pc´<=n && (handler_at[pc´] = cur_hand; true) &&
               changed(changes, s[pc´], vars)
                update(s[pc´], changes, vars)
                pc = pc´
            else
                break
            end
        end
    end
    inference_stack = inference_stack.prev
    fulltree = type_annotate(ast, s, sv, frame.result, vars)
    def.tfunc = (atypes, fulltree, def.tfunc)
    fulltree.args[3] = inlining_pass(fulltree.args[3], s[1])
    #print("\n",fulltree,"\n")
    #print("==> ", frame.result,"\n")
    return (fulltree, frame.result)
end

function record_var_type(e::Symbol, t, decls)
    otherTy = get(decls::IdTable, e, false)
    # keep track of whether a variable is always the same type
    if !is(otherTy,false)
        if !is(otherTy, t)
            decls[e] = Any
        end
    else
        decls[e] = t
    end
end

expr_type(e::Expr) = e.type
expr_type(s::Symbol) = Any
expr_type(x) = typeof(x)

function eval_annotate(e::Expr, vtypes, sv, decls)
    if is(e.head,:quote) || is(e.head,:top) || is(e.head,:goto) ||
        is(e.head,:label) || is(e.head,:static_typeof)
        return e
    elseif is(e.head,:gotoifnot) || is(e.head,symbol("return"))
        e.type = Any
    elseif is(e.head,symbol("="))
        e.type = Any
        s = e.args[1]
        # assignment LHS not subject to all-same-type variable checking,
        # but the type of the RHS counts as one of its types.
        e.args[1] = Expr(:symbol, {s}, abstract_eval(s, vtypes, sv))
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, decls)
        # TODO: if this def does not reach any uses, maybe don't do this
        record_var_type(s, expr_type(e.args[2]), decls)
        return e
    end
    for i=1:length(e.args)
        e.args[i] = eval_annotate(e.args[i], vtypes, sv, decls)
    end
    e
end

function eval_annotate(e::Symbol, vtypes, sv, decls)
    t = abstract_eval(e, vtypes, sv)
    record_var_type(e, t, decls)
    Expr(:symbol, {e}, t)
end

eval_annotate(s, vtypes, sv, decls) = s

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array, sv, rettype, vnames)
    decls = idtable()
    body = ast.args[3].args
    for i=1:length(body)
        body[i] = eval_annotate(body[i], states[i], sv, decls)
    end
    ast.args[3].type = rettype

    vinf = append(ast.args[2].args[2],ast.args[2].args[3])
    # add declarations for variables that are always the same type
    for vi = vinf
        if has(decls,vi[1])
            vi[2] = decls[vi[1]]
        end
    end
    ast
end

function sym_replace(e::Expr, from, to)
    if is(e.head,:quote) || is(e.head,:top) || is(e.head,:goto) ||
        is(e.head,:label)
        return e
    end
    if is(e.head,:symbol)
        s = e.args[1]
        for i=1:length(from)
            if is(from[i],s)
                return to[i]
            end
        end
        return e
    end
    for i=1:length(e.args)
        e.args[i] = sym_replace(e.args[i], from, to)
    end
    e
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
    if is(e.head,:quote) || is(e.head,:top) || is(e.head,:goto) ||
        is(e.head,:label)
        return 0
    end
    c = 0
    for a = e.args
        c += occurs_more(a, pred, n)
        if c>n
            return c
        end
    end
    c
end

occurs_more(e, pred, n) = pred(e) ? 1 : 0

function contains(arr, item)
    for i = 1:length(arr)
        if is(arr[i],item)
            return true
        end
    end
    return false
end

exprtype(e::Expr) = e.type
exprtype(s::Symbol) = Any
exprtype(t::Type) = Type{t}
exprtype(other) = typeof(other)

# for now, only inline functions whose bodies are of the form "return <expr>"
# where <expr> doesn't contain any argument more than once.
# functions with closure environments or varargs are also excluded.
# static parameters are ok if all the static parameter values are leaf types,
# meaning they are fully known.
function inlineable(e::Expr, vars)
    f = eval(e.args[1])
    argexprs = a2t(e.args[2:])
    atypes = map(exprtype, argexprs)
    meth = getmethods(f, atypes)
    if is(meth,())
        return NF
    end
    #if !(!is(meth,()) && is(meth[5],()))
    #    print(e,"\n")
    #end
    #assert(is(meth[5],()))
    if !is(meth[5],())
        return NF
    end
    # when 1 method matches the inferred types, there is still a chance
    # of a no-method error at run time, unless the inferred types are a
    # subset of the method signature.
    if !subtype(atypes, meth[1])
        return NF
    end
    if is(meth[3],:convert) && length(atypes)==2
        # builtin case of convert. convert(T,x::S) => x, when S<:T
        if isType(atypes[1]) && subtype(atypes[2],atypes[1].parameters[1])
            # todo: if T expression has side effects??!
            return e.args[3]
        end
    end
    if !isa(meth[3],LambdaStaticData) || !is(meth[4],())
        return NF
    end
    sp = meth[2]::Tuple
    spvals = sp[2:2:]
    for i=1:length(spvals)
        if !isleaftype(spvals[i]) && !isa(spvals[i],Int32)
            return NF
        end
    end
    for vi = meth[3].ast.args[2].args[2]
        if vi[3]
            # captures variables (TODO)
            return NF
        end
    end
    (ast, ty) = typeinf(meth[3], meth[1], meth[2], true)
    if is(ast,())
        return NF
    end
    body = ast.args[3].args
    # see if body is only "return <expr>"
    if length(body) > 1
        return NF
    end
    assert(isa(body[1],Expr))
    assert(is(body[1].head,symbol("return")))
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
        if occurs_more(expr, x->is(x,a), 1) > 1
            return NF
        end
    end
    # avoid capture if the function has free variables with the same name
    # as our vars
    if occurs_more(expr, x->(has(vars,x)&&!contains(args,x)), 0) > 0
        return NF
    end
    # ok, substitute argument expressions for argument names in the body
    spnames = { sp[i].name | i=1:2:length(sp) }
    return sym_replace(copy(expr), append(args,spnames),
                       append(argexprs,spvals))
end

function remove_call1(e)
    if isa(e,Expr)
        for i=1:length(e.args)
            e.args[i] = remove_call1(e.args[i])
        end
        if is(e.head,:call1)
            e.head = :call
        end
    end
    e
end

inlining_pass(x, vars) = x

function inlining_pass(e::Expr, vars)
    # don't inline first argument of ccall, as this needs to be evaluated
    # by the interpreter and inlining might put in something it can't handle,
    # like another ccall.
    if is(e.head,:call) && isa(e.args[1],Expr) &&
       is(e.args[1].head,:symbol) && is(e.args[1].args[1],:ccall)
        if length(e.args)>1
            e.args[2] = remove_call1(e.args[2])
        end
        i0 = 3
    else
        i0 = 1
    end
    for i=i0:length(e.args)
        e.args[i] = inlining_pass(e.args[i], vars)
    end
    if is(e.head,:call1)
        e.head = :call
        body = inlineable(e, vars)
        if !is(body,NF)
            #print("inlining ", e, " => ", body, "\n")
            return body
        end
        if is(eval(e.args[1]),apply)
            if length(e.args) == 3
                aarg = e.args[3]
                if isa(aarg,Expr) && is(aarg.head,:call) &&
                    isa(aarg.args[1],Expr) && is(aarg.args[1].head,:top) &&
                    is(eval(aarg.args[1]),tuple)
                    # apply(f,tuple(x,y,...)) => f(x,y,...)
                    e.args = append({e.args[2]}, aarg.args[2:])
                    # now try to inline the simplified call
                    body = inlineable(e, vars)
                    if !is(body,NF)
                        return body
                    end
                    return e
                end
            end
        end
    end
    e
end

function finfer(f, types)
    x = getmethods(f,types)
    typeinf(x[3], x[1], x[2], true)[1]
end

tfunc(f,t) = (getmethods(f,t)[3]).tfunc

# stuff for testing

# T=typevar(:T)
# S=typevar(:S)
# R=typevar(:R)
# a=typevar(:a)
# b=typevar(:b)
# c=typevar(:c)
# d=typevar(:d)

# m = getmethods(fact,(Int32,))
# ast = m[3]

# function foo(x)
#     return x.re + x.im
# end

# m = getmethods(foo,(Complex{Float64},))
# ast = m[3]

# function bar(x)
#     if (x > 0)
#         return bar(x-1)
#     end
#     return 0
# end

# function qux(x)
#     if mystery()
#         a = 10
#     else
#         a = 2+b
#     end
#     b = 1
#     z = a + b
#     Range(1, 2, 10)
# end

# m = getmethods(qux,(Int32,))
# ast = m[3]

# fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

# function both()
#     a = 2
#     while mystery()
#         b = a+a
#         g(a)
#         a = 2.0
#         c = a+a
#         f(a)
#         f(c)
#     end
#     c
# end

# function und()
#     local a
#     if mystery()
#         a = other_mystery()
#     end
#     c = a
# end
