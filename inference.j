# infrastructure needed for type inference:
# * shared assoc list
# * IntSet
# * printing exprs
# * compute type intersection
# - more method table reflection
#   . cached t-functions
#   . abstract_invoke()
#     . consult t-func cache
#     * determine applicable methods
#     * abstract_invoke all of them, type-union the result, and cache it
# * hash table of symbols
# * eval
# - t-functions for builtins
# * deal with call stack and recursive types
# - isconstant()
# * approximate static parameters
# - use type bounds
# - reflection for constructors
# * be able to infer the results of promote()

# parameters limiting potentially-infinite types
MAX_TYPEUNION_SIZE = 3
MAX_TUPLETYPE_LEN  = 10

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
end

CallStack(ast, types, prev::EmptyCallStack) =
    CallStack(ast, types, 0, false, Bottom, prev)
CallStack(ast, types, prev::CallStack) =
    CallStack(ast, types, prev.n+1, false, Bottom, prev)

# TODO thread local
inference_stack = EmptyCallStack()

tintersect(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_intersection"), Any,
                        (Any,Any), a, b)
tmatch(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_match"), Any,
                    (Any,Any), a, b)

getmethods(f,t) = ccall(dlsym(JuliaDLHandle,"jl_matching_methods"), Any,
                        (Any,Any), f, t)

typeseq(a,b) = subtype(a,b)&&subtype(b,a)

isbuiltin(f) = ccall(dlsym(JuliaDLHandle,"jl_is_builtin"), Int32, (Any,),
                     f) != 0
isgeneric(f) = ccall(dlsym(JuliaDLHandle,"jl_is_genericfunc"), Int32, (Any,),
                     f) != 0

# for now assume all globals constant
# TODO
isconstant(s::Symbol) = true
isconstant(s::Expr) = is(s.head,`quote)
isconstant(x) = true

mintype(a, b) = subtype(a,b) ? a : b

cmp_tfunc = (x,y)->Bool

isType(t) = isa(t,TagKind) && is(t.name,Type{}.name)

t_func = idtable()
t_func[tuple] = (0, Inf, (args...)->args)
t_func[error] = (1, 1, x->Bottom)
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
t_func[eq_int] = (2, 2, cmp_tfunc)
t_func[slt_int] = (2, 2, cmp_tfunc)
t_func[ult_int] = (2, 2, cmp_tfunc)
t_func[eq_float] = (2, 2, cmp_tfunc)
t_func[lt_float] = (2, 2, cmp_tfunc)
t_func[ne_float] = (2, 2, cmp_tfunc)
t_func[ccall] =
    (3, Inf, (fptr, rt, at, a...)->(isType(rt) ? rt.parameters[1] : Any))
t_func[is] = (2, 2, cmp_tfunc)
t_func[subtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[tuplelen] = (1, 1, x->Int32)
t_func[arraylen] = (1, 1, x->Int32)
t_func[arrayref] = (2, 2, (a,i)->(isa(a,StructKind) && subtype(a,Array) ?
                                  a.parameters[1] : Any))
t_func[arrayset] = (3, 3, (a,i,v)->a)
t_func[Array] =
    (1, Inf, (T,dims...)->(nd = length(dims);
                           et = isType(T) ? T.parameters[1] : Any;
                           Array{et,nd}))
t_func[identity] = (1, 1, identity)
t_func[`convert] =
    (2, 2, (t,x)->(isType(t) ? t.parameters[1] : Any))
t_func[typeof] =
    (1, 1, t->(isType(t)      ? Type{typeof(t.parameters[1])} :
               isa(t,TagKind) ? Type{t} :
               typeof(t)))
# involving constants: typeassert, tupleref, getfield
# therefore they get their arguments unevaluated
t_func[typeassert] =
    (2, 2, (A, v, t)->(isType(t) ? mintype(v,t.parameters[1]) :
                       isconstant(A[2]) ? mintype(v,eval(A[2])) :
                       Any))
isseqtype(t) = isa(t,TagKind) && is(t.name.name,`...)
function tupleref_tfunc(A, t, i)
    if is(t,())
        return Bottom
    end
    if isa(t,TagKind) && is(t.name,NTuple{}.name)
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
                return Bottom
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
        # TODO: possibly use reduce of tmerge instead
        return Union(types...)
    end
end
t_func[tupleref] = (2, 2, tupleref_tfunc)
function getfield_tfunc(A, s, name)
    if !isa(s,StructKind)
        return Any
    end
    if isa(A[2],Expr) && is(A[2].head,`quote) && isa(A[2].args[1],Symbol)
        fld = A[2].args[1]
        for i=1:length(s.names)
            if is(s.names[i],fld)
                return s.types[i]
            end
        end
        return Bottom
    else
        return Union(s.types...)
    end
end
t_func[getfield] = (2, 2, getfield_tfunc)

# other: apply, setfield, new_closure

# Scalar{T} => T
normalize_numeric_type(t) = t
function normalize_numeric_type(t::Type{Scalar})
    if isa(t,TagKind) && length(t.parameters)==1 && subtype(t.parameters[1],t)
        return t.parameters[1]
    end
    return t
end

function builtin_tfunction(f, args::Tuple, argtypes::Tuple)
    tf = get(t_func, f, false)
    if is(tf,false)
        # unknown/unhandled builtin
        return Any
    end
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    if is(f,typeassert) || is(f,tupleref) || is(f,getfield)
        # TODO: case of apply(), where we do not have the args
        return normalize_numeric_type(tf[3](args, argtypes...))
    end
    return normalize_numeric_type(tf[3](argtypes...))
end

function abstract_eval(e::Expr, vtypes, sp)
    t = abstract_eval_expr(e, vtypes, sp)
    e.type = t
    return t
end

function a2t(a::Vector)
    t = ()
    for i=length(a):-1:1
        t = tuple(a[i], t...)
    end
    t
end

function isconstantfunc(f, vtypes)
    if isa(f,Expr) && is(f.head,`top)
        assert(isa(f.args[1],Symbol))
        return (true, f.args[1])
    end
    return (isa(f,Symbol) && !has(vtypes,f), f)
end

isvatuple(t) = (n = length(t); n > 0 && isseqtype(t[n]))

function limit_tuple_type(t)
    n = length(t)
    if n > MAX_TUPLETYPE_LEN
        last = t[n]
        if isseqtype(last)
            last = last.parameters[1]
        end
        tail = Union(t[MAX_TUPLETYPE_LEN:(n-1)]..., last)
        return tuple(t[1:(MAX_TUPLETYPE_LEN-1)]..., ...{tail})
    end
    return t
end

function abstract_call(f, fargs, argtypes, vtypes, sp)
    if isbuiltin(f)
        if is(f,apply) && length(fargs)>0
            (isfunc, af) = isconstantfunc(fargs[1], vtypes)
            if isfunc && isbound(af)
                afargs = fargs[2:]
                aargtypes = argtypes[2:]
                if all(map(x->isa(x,Tuple),aargtypes)) &&
                    !any(map(isvatuple,aargtypes[1:(length(aargtypes)-1)]))
                    # apply with known func with known tuple types
                    # can be collapsed to a call to the applied func
                    at = length(aargtypes) > 0 ?
                         limit_tuple_type(append(aargtypes...)) : ()
                    return abstract_call(eval(af), (), at, vtypes, sp)
                end
            end
        end
        rt = builtin_tfunction(f, fargs, argtypes)
        print("=> ", rt, "\n")
        return rt
    elseif isgeneric(f)
        applicable = getmethods(f, argtypes)
        rettype = Bottom
        x = applicable
        while !is(x,())
            #print(x,"\n")
            if isa(x[3],Symbol)
                # when there is a builtin method in this GF, we get
                # a symbol with the name instead of a LambdaStaticData
                rt = builtin_tfunction(x[3], fargs, x[1])
            elseif isa(x[3],Type)
                # constructor
                rt = x[3]
            else
                (_tree,rt) = typeinf(x[3], x[2], x[1])
            end
            rettype = tmerge(rettype, rt)
            x = x[4]
        end
        # if rettype is Bottom we've found a method not found error
        print("=> ", rettype, "\n")
        return rettype
    else
        print("=> ", Any, "\n")
        return Any
    end
end

function abstract_eval_call(e, vtypes, sp)
    (isfunc, func) = isconstantfunc(e.args[1], vtypes)
    if !isfunc
        # TODO: lambda expression (let)
        return Any
    end
    fargs = a2t(e.args[2:])
    argtypes = map(x->abstract_eval(x,vtypes,sp), fargs)
    print("call ", e.args[1], argtypes, " ")
    if !isbound(func)
        print("=> ", Any, "\n")
        return Any
    end
    f = eval(func)
    return abstract_call(f, fargs, argtypes, vtypes, sp)
end

function abstract_eval_expr(e, vtypes, sp)
    # handle:
    # call  lambda  quote  null  top  unbound  box-unbound
    # closure-ref
    if is(e.head,`unbound) || is(e.head,symbol("box-unbound"))
        return Bool
    elseif is(e.head,`null)
        return ()
    elseif is(e.head,`quote)
        return typeof(e.args[1])
    elseif is(e.head,`top)
        return abstract_eval_global(e.args[1])
    elseif is(e.head,symbol("closure-ref"))
        return Any
    elseif is(e.head,`call)
        return abstract_eval_call(e, vtypes, sp)
    end
end

ast_rettype(ast) = ast.args[3].type

function abstract_eval_constant(x)
    if isa(x,TagKind) || isa(x,TypeVar)
        return Type{x}
    end
    return typeof(x)
end

function abstract_eval_global(s::Symbol)
    if isconstant(s)
        if !isbound(s)
            return Any
        end
        return abstract_eval_constant(eval(s))
    else
        return Any
    end
end

function abstract_eval(s::Symbol, vtypes, sp)
    t = get(vtypes,s,NF)
    if is(t,NF)
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

abstract_eval(x, vtypes, sp) = abstract_eval_constant(x)

struct StateUpdate
    changes::((Symbol,Any)...)
    state::IdTable
end

function ref(x::StateUpdate, s::Symbol)
    for (v,t) = x.changes
        if is(v,s)
            return t
        end
    end
    return get(x.state,s,NF)
end

function interpret(e::Expr, vtypes, sp)
    # handle assignment
    if is(e.head,symbol("="))
        t = abstract_eval(e.args[2], vtypes, sp)
        lhs = e.args[1]
        assert(isa(lhs,Symbol))
        return StateUpdate(((lhs, t),), vtypes)
    elseif is(e.head,`call)
        abstract_eval(e, vtypes, sp)
    elseif is(e.head,`gotoifnot)
        abstract_eval(e.args[1], vtypes, sp)
    end
    return StateUpdate((), vtypes)
end

tchanged(n, o) = is(o,NF) || (!is(n,NF) && !subtype(n,o))

function changed(new::StateUpdate, old, vars)
    for v = vars
        if tchanged(new[v], get(old,v,NF))
            return true
        end
    end
    return false
end

badunion(t) = ccall(dlsym(JuliaDLHandle,"jl_union_too_complex"),
                    Int32, (Any,), t)!=0

function tmerge(typea, typeb)
    if is(typea,NF)
        return typeb
    end
    if is(typeb,NF)
        return typea
    end
    t = ccall(dlsym(JuliaDLHandle,"jl_compute_type_union"),Any,(Any,),
              (typea, typeb))
    if length(t)==1
        return t[1]
    elseif length(t)==0
        return Bottom
    end
    if badunion(t)
        return Any
    end
    u = Union(t...)
    if isa(u,UnionKind) && length(u.types) > MAX_TYPEUNION_SIZE
        # don't let type unions get too big
        return Any
    end
    return u
end

function update(state, changes::StateUpdate, vars)
    for v = vars
        newtype = changes[v]
        oldtype = get(state,v,NF)
        if tchanged(newtype, oldtype)
            state[v] = tmerge(oldtype, newtype)
        end
    end
    state
end

type Undef

function findlabel(body, l)
    for i=1:length(body)
        b = body[i]
        if isa(b,Expr) && is(b.head,`label) && b.args[1]==l
            return i
        end
    end
    error("label not found")
end

function typeinf(linfo::LambdaStaticData, sparams::Tuple, atypes::Tuple)
    # check cached t-functions
    tf = linfo.tfunc
    while !is(tf,())
        if typeseq(tf[1],atypes)
            return (tf[2], ast_rettype(tf[2]))
        end
        tf = tf[3]
    end

    ast0 = linfo.ast

    global inference_stack
    # check for recursion
    f = inference_stack
    while !isa(f,EmptyCallStack)
        if is(f.ast,ast0) && typeseq(f.types, atypes)
            # return best guess so far
            f.recurred = true
            return ((),f.result)
        end
        f = f.prev
    end

    ast = copy(ast0)
    #print("typeinf ", ast, " ", sparams, " ", atypes, "\n")

    assert(is(ast.head,`lambda))
    args = map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1])
    locals = ast.args[2].args[1].args
    vars = append(args, locals)
    body = ast.args[3].args

    n = length(body)
    s = { idtable() | i=1:n }
    recpts = intset(n+1)  # statements that depend recursively on our value
    W = intset(n+1)
    # initial set of pc
    adjoin(W,1)
    # initial types
    for v=vars
        s[1][v] = Undef
    end
    la = length(args)
    if la > 0
        lastarg = ast.args[1][la]
        if isa(lastarg,Expr) && is(lastarg.head,symbol("::"))
            if ccall(dlsym(JuliaDLHandle,"jl_is_rest_arg"),Int32,(Any,),
                     lastarg)!=0
                s[1][args[la]] = atypes[la:]
                la -= 1
            end
        end
    end
    for i=1:la
        s[1][args[i]] = atypes[i]
    end

    # our stack frame
    frame = CallStack(ast0, atypes, inference_stack)
    inference_stack = frame

    while !isempty(W)
        pc = choose(W)
        while true
            #print(pc,": ",s[pc],"\n")
            remove(W, pc)
            stmt = body[pc]
            changes = interpret(stmt, s[pc], sparams)
            if frame.recurred
                adjoin(recpts, pc)
                frame.recurred = false
            end
            pc´ = pc+1
            if isa(stmt,Expr)
                if is(stmt.head,`goto)
                    pc´ = findlabel(body,stmt.args[1])
                elseif is(stmt.head,`gotoifnot)
                    l = findlabel(body,stmt.args[2])
                    if changed(changes, s[l], vars)
                        adjoin(W, l)
                        update(s[l], changes, vars)
                    end
                elseif is(stmt.head,symbol("return"))
                    pc´ = n+1
                    rt = abstract_eval(stmt.args[1], s[pc], sparams)
                    if tchanged(rt, frame.result)
                        frame.result = tmerge(frame.result, rt)
                        # revisit states that recursively depend on this
                        for r=recpts
                            adjoin(W,r)
                        end
                    end
                end
            end
            if pc´<=n && changed(changes, s[pc´], vars)
                update(s[pc´], changes, vars)
                pc = pc´
            else
                break
            end
        end
    end
    inference_stack = inference_stack.prev
    fulltree = type_annotate(ast, s, sparams, frame.result)
    linfo.tfunc = (atypes, fulltree, linfo.tfunc)
    return (fulltree, frame.result)
end

function eval_annotate(e::Expr, vtypes, sp)
    if is(e.head,`quote) || is(e.head,`top) || is(e.head,`goto) ||
        is(e.head,`label)
        return e
    elseif is(e.head,`gotoifnot) || is(e.head,symbol("return")) ||
        is(e.head,symbol("="))
        e.type = Any
    end
    for i=1:length(e.args)
        e.args[i] = eval_annotate(e.args[i],vtypes,sp)
    end
    e
end

function eval_annotate(e::Symbol, vtypes, sp)
    Expr(`symbol, {e}, abstract_eval(e, vtypes, sp))
end

eval_annotate(s, vtypes, sp) = s

# expand v to v::T as appropriate based on all inferred type info
function add_decls(vars::Array, states::Array)
    # TODO
    return vars
end

# annotate types of all symbols in AST
function type_annotate(ast::Expr, states::Array, sp, rettype)
    vinf = ast.args[2]
    vinf.args[1].args = add_decls(vinf.args[1].args, states)
    body = ast.args[3].args
    for i=1:length(body)
        body[i] = eval_annotate(body[i], states[i], sp)
    end
    ast.args[3].type = rettype
    ast
end

# stuff for testing

T=typevar(`T)
S=typevar(`S)
R=typevar(`R)
a=typevar(`a)
b=typevar(`b)
c=typevar(`c)
d=typevar(`d)

function finfer(f, types)
    x = getmethods(f,types)
    typeinf(x[3], x[2], x[1])[1]
end

m = getmethods(fact,(Int32,))
ast = m[3]
#typeinf(ast, (`T,Int32), (Int32,))

function foo(x)
    return x.re + x.im
end

m = getmethods(foo,(Complex{Float64},))
ast = m[3]

function bar(x)
    if (x > 0)
        return bar(x-1)
    end
    return 0
end

function qux(x)
    if mystery()
        a = 10
    else
        a = 2+b
    end
    b = 1
    z = a + b
    Range(1, 2, 10)
end

m = getmethods(qux,(Int32,))
ast = m[3]

fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)

function both()
    a = 2
    while mystery()
        b = a+a
        g(a)
        a = 2.0
        c = a+a
        f(a)
        f(c)
    end
    c
end
