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
        new(ast, types, 0, false, Bottom, prev)
    CallStack(ast, types, prev::CallStack) =
        new(ast, types, prev.n+1, false, Bottom, prev)
end

# TODO thread local
inference_stack = EmptyCallStack()

tintersect(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_intersection"), Any,
                        (Any,Any), a, b)
tmatch(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_match"), Any,
                    (Any,Any), a, b)

getmethods(f,t) = ccall(dlsym(JuliaDLHandle,"jl_matching_methods"), Any,
                        (Any,Any), f, t)::Tuple

typeseq(a,b) = subtype(a,b)&&subtype(b,a)

isbuiltin(f) = ccall(dlsym(JuliaDLHandle,"jl_is_builtin"), Int32, (Any,),
                     f) != 0
isgeneric(f) = ccall(dlsym(JuliaDLHandle,"jl_is_genericfunc"), Int32, (Any,),
                     f) != 0

isleaftype(t) = ccall(dlsym(JuliaDLHandle,"jl_is_leaf_type"), Int32, (Any,),
                      t) != 0

# for now assume all global functions constant
# TODO
isconstant(s::Symbol) = isbound(s) && (e=eval(s);
                                       isa(e,Function) || isa(e,Type))
isconstant(s::Expr) = is(s.head,`quote)
isconstant(x) = true

cmp_tfunc = (x,y)->Bool

isType(t) = isa(t,TagKind) && is(t.name,Type.name)

t_func = idtable()
t_func[tuple] = (0, Inf, (args...)->args)
t_func[instantiate_type] =
    (1, Inf, (args...)->(all(map(isType,args)) ?
                         Type{instantiate_type(args[1].parameters[1],
                                               map(t->t.parameters[1],
                                                   args[2:])...)} :
                         isType(args[1]) ?
                         args[1] :
                         Any))
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
t_func[new_generic_function] = (1, 1, s->(Bottom-->Any))
t_func[tuplelen] = (1, 1, x->Int32)
t_func[arraylen] = (1, 1, x->Int32)
t_func[arrayref] = (2, 2, (a,i)->(isa(a,StructKind) && subtype(a,Array) ?
                                  a.parameters[1] : Any))
t_func[arrayset] = (3, 3, (a,i,v)->a)
t_func[Array] =
    (1, Inf, (T,dims...)->(nd = length(dims);
                           et = isType(T) ? T.parameters[1] : Any;
                           Array{et,nd}))
t_func[`convert] =
    (2, 2, (t,x)->(if isa(t,Tuple) && all(map(isType,t))
                       t = Type{map(t->t.parameters[1],t)}
                   end;
                   isType(t) ? tintersect(t.parameters[1],x) :
                   Any))
t_func[typeof] =
    (1, 1, t->(isType(t)      ? Type{typeof(t.parameters[1])} :
               isa(t,TagKind) ? Type{t} :
               typeof(t)))
# involving constants: typeassert, tupleref, getfield
# therefore they get their arguments unevaluated
t_func[typeassert] =
    (2, 2, (A, v, t)->(isType(t) ? tintersect(v,t.parameters[1]) :
                       isconstant(A[2]) ? tintersect(v,eval(A[2])) :
                       Any))
isseqtype(t) = isa(t,TagKind) && is(t.name.name,`...)
function tupleref_tfunc(A, t, i)
    if is(t,())
        return Bottom
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
        return reduce(tmerge, Bottom, types)
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
        return reduce(tmerge, Bottom, s.types)#Union(s.types...)
    end
end
t_func[getfield] = (2, 2, getfield_tfunc)

# other: apply, setfield

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
    t = ()
    for i=length(a):-1:1
        t = tuple(a[i], t...)
    end
    t
end

function isconstantfunc(f, vtypes, sv::StaticVarInfo)
    if isa(f,Expr) && is(f.head,`top)
        abstract_eval(f, vtypes, sv)
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
        tail = tuple(t[MAX_TUPLETYPE_LEN:(n-1)]..., last)
        tail = reduce(tmerge, Bottom, tail)
        return tuple(t[1:(MAX_TUPLETYPE_LEN-1)]..., ...{tail})
    end
    return t
end

function abstract_call(f, fargs, argtypes, vtypes, sv::StaticVarInfo, e)
    if isbuiltin(f)
        if is(f,apply) && length(fargs)>0
            (isfunc, af) = isconstantfunc(fargs[1], vtypes, sv)
            if isfunc && isbound(af)
                aargtypes = argtypes[2:]
                if all(map(x->isa(x,Tuple),aargtypes)) &&
                    !any(map(isvatuple,aargtypes[1:(length(aargtypes)-1)]))
                    e.head = `call1
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
        applicable = getmethods(f, argtypes)
        rettype = Bottom
        x = applicable
        if isa(e,Expr)
            if !is(x,()) && is(x[5],())
                # method match is unique; mark it
                e.head = `call1
            else
                e.head = `call
            end
        end
        ctr = 0
        while !is(x,())
            ctr += 1
            # don't consider more than 3 methods. this trades off between
            # compiler performance and generated code performance.
            # typically, considering many methods means spending lots of time
            # obtaining poor type information.
            if ctr > 3
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
        # if rettype is Bottom we've found a method not found error
        #print("=> ", rettype, "\n")
        return rettype
    else
        #print("=> ", Any, "\n")
        return Any
    end
end

ft_tfunc(ft, argtypes) = ccall(dlsym(JuliaDLHandle,"jl_func_type_tfunc"), Any,
                               (Any, Any), ft, argtypes)

function abstract_eval_call(e, vtypes, sv::StaticVarInfo)
    fargs = a2t(e.args[2:])
    argtypes = map(x->abstract_eval(x,vtypes,sv), fargs)
    (isfunc, func) = isconstantfunc(e.args[1], vtypes, sv)
    if !isfunc
        # TODO: lambda expression (let)
        ft = abstract_eval(e.args[1], vtypes, sv)
        if isa(ft,FuncKind)
            return ft_tfunc(ft, argtypes)
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
    if is(e.head,`call) || is(e.head,`call1)
        return abstract_eval_call(e, vtypes, sv)
    elseif is(e.head,`top)
        return abstract_eval_global(e.args[1])
    elseif is(e.head,`unbound)
        return Bool
    elseif is(e.head,`null)
        return ()
    elseif is(e.head,`quote)
        return typeof(e.args[1])
    elseif is(e.head,`static_typeof)
        t = abstract_eval(e.args[1], vtypes, sv)
        # intersect with Any to remove Undef
        t = tintersect(t, Any)
        return Type{t}
    end
end

ast_rettype(ast) = ast.args[3].type

function abstract_eval_constant(x)
    if isa(x,TagKind) || isa(x,TypeVar)
        return Type{x}
    end
    if isa(x,LambdaStaticData)
        return Bottom-->Any
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
    changes::((Symbol,Any)...)
    state::VarTable
end

function ref(x::StateUpdate, s::Symbol)
    for (v,t) = x.changes
        if is(v,s)
            return t
        end
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
        return StateUpdate(((lhs, t),), vtypes)
    elseif is(e.head,`call) || is(e.head,`call1)
        abstract_eval(e, vtypes, sv)
    elseif is(e.head,`gotoifnot)
        abstract_eval(e.args[1], vtypes, sv)
    end
    return vtypes
end

tchanged(n, o) = is(o,NF) || (!is(n,NF) && !subtype(n,o))

function changed(new::Union(StateUpdate,VarTable), old, vars)
    for v = vars
        if tchanged(new[v], get(old,v,NF))
            return true
        end
    end
    return false
end

badunion(t) = ccall(dlsym(JuliaDLHandle,"jl_union_too_complex"),
                    Int32, (Any,), t)!=0

typealias Top Union(Any,Undef)

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
        return subtype(Undef,t) ? Top : Any
    end
    u = Union(t...)
    if isa(u,UnionKind) && length(u.types) > MAX_TYPEUNION_SIZE
        # don't let type unions get too big
        # TODO: something smarter, like a common supertype
        return subtype(Undef,u) ? Top : Any
    end
    return u
end

function update(state, changes::Union(StateUpdate,VarTable), vars)
    for v = vars
        newtype = changes[v]
        oldtype = get(state,v,NF)
        if tchanged(newtype, oldtype)
            state[v] = tmerge(oldtype, newtype)
        end
    end
    state
end

function findlabel(body, l)
    for i=1:length(body)
        b = body[i]
        if isa(b,Expr) && is(b.head,`label) && b.args[1]==l
            return i
        end
    end
    error("label not found")
end

f_argnames(ast) = map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1])

is_rest_arg(arg) = (isa(arg,Expr) && is(arg.head,symbol("::")) &&
                    ccall(dlsym(JuliaDLHandle,"jl_is_rest_arg"),Int32,(Any,),
                          arg)!=0)

function typeinf_task(caller)
    result = ()
    while true
        (caller, args) = yieldto(caller, result)
        result = typeinf_ext_(args...)
    end
end

#Inference_Task = Task(typeinf_task, 2097152)
#yieldto(Inference_Task, current_task())

function typeinf_ext(linfo, atypes, sparams, cop)
    return typeinf_ext_(linfo, atypes, sparams, cop)

    #C = current_task()
    #args = (linfo, atypes, sparams, cop)
    #if is(C, Inference_Task)
    #    return typeinf_ext_(args...)
    #end
    #return yieldto(Inference_Task, C, args)
end

function typeinf_ext_(linfo, atypes, sparams, cop)
    global inference_stack
    last = inference_stack
    inference_stack = EmptyCallStack()
    result = typeinf(linfo, atypes, sparams, cop)
    inference_stack = last
    return result
end

function typeinf(linfo::LambdaStaticData, atypes::Tuple, sparams::Tuple, cop)
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

    assert(is(ast.head,`lambda))
    args = f_argnames(ast)
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
        if is_rest_arg(lastarg)
            s[1][args[la]] = atypes[la:]
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

    while !isempty(W)
        pc = choose(W)
        while true
            #print(pc,": ",s[pc],"\n")
            remove(W, pc)
            stmt = body[pc]
            changes = interpret(stmt, s[pc], sv)
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
                    rt = abstract_eval(stmt.args[1], s[pc], sv)
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
    fulltree = type_annotate(ast, s, sv, frame.result, vars)
    linfo.tfunc = (atypes, fulltree, linfo.tfunc)
    fulltree.args[3] = inlining_pass(fulltree.args[3], s[1])
    #print("\n",fulltree,"\n")
    #print("==> ", frame.result,"\n")
    return (fulltree, frame.result)
end

function eval_annotate(e::Expr, vtypes, sv, decls)
    if is(e.head,`quote) || is(e.head,`top) || is(e.head,`goto) ||
        is(e.head,`label) || is(e.head,`static_typeof)
        return e
    elseif is(e.head,`gotoifnot) || is(e.head,symbol("return"))
        e.type = Any
    elseif is(e.head,symbol("="))
        e.type = Any
        s = e.args[1]
        # assignment LHS not subject to all-same-type variable checking
        e.args[1] = Expr(`symbol, {s}, abstract_eval(s, vtypes, sv))
        e.args[2] = eval_annotate(e.args[2], vtypes, sv, decls)
        return e
    end
    for i=1:length(e.args)
        e.args[i] = eval_annotate(e.args[i], vtypes, sv, decls)
    end
    e
end

function eval_annotate(e::Symbol, vtypes, sv, decls)
    t = abstract_eval(e, vtypes, sv)
    otherTy = get(decls, e, false)
    # keep track of whether a variable is always the same type
    if !is(otherTy,false)
        if !is(otherTy, t)
            decls[e] = Any
        end
    else
        decls[e] = t
    end
    Expr(`symbol, {e}, t)
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
    if is(e.head,`quote) || is(e.head,`top) || is(e.head,`goto) ||
        is(e.head,`label)
        return e
    end
    if is(e.head,`symbol)
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
    if is(e.head,`quote) || is(e.head,`top) || is(e.head,`goto) ||
        is(e.head,`label)
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
    if is(meth[3],`convert) && length(atypes)==2
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
        if !isleaftype(spvals[i])
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
    # todo: make sure side effects aren't skipped if argument doesn't occur?
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

inlining_pass(x, vars) = x

function inlining_pass(e::Expr, vars)
    for i=1:length(e.args)
        e.args[i] = inlining_pass(e.args[i], vars)
    end
    if is(e.head,`call1)
        e.head = `call
        body = inlineable(e, vars)
        if !is(body,NF)
            #print("inlining ", e, " => ", body, "\n")
            return body
        end
        if is(eval(e.args[1]),apply)
            if length(e.args) == 3
                aarg = e.args[3]
                if isa(aarg,Expr) && is(aarg.head,`call) &&
                    isa(aarg.args[1],Expr) && is(aarg.args[1].head,`top) &&
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

# T=typevar(`T)
# S=typevar(`S)
# R=typevar(`R)
# a=typevar(`a)
# b=typevar(`b)
# c=typevar(`c)
# d=typevar(`d)

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
