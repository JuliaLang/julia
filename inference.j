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
# - isconstant()
# - deal with call stack and recursive types
# - approximate static parameters
# - use type bounds

# mutable pair
struct Pair
    a
    b
end

# delegated assoc list
# equivalent to some previous list except for local modifications
struct AList
    prev::Nullable{AList}
    elts::List
end

struct NotFound
end

NF = NotFound()

# note: with Nullable (or other union types) it might not be possible to
# infer type parameters from field values. bug or feature??
alist(kt::Type, vt::Type) = AList((), nil)

alist(a::AList) = AList(a, nil)

assoc(item, a::EmptyList) = a
assoc(item, p::List) = is(head(p).a,item) ? head(p) : assoc(item,tail(p))

function set(a::AList, val, key)
    p = assoc(key, a.elts)
    if !isa(p,EmptyList)
        p.b = val
    else
        a.elts = Cons(Pair(key,val),a.elts)
    end
    a
end

function ref(a::AList, key)
    p = assoc(key, a.elts)
    if isa(p,EmptyList)
        if is(a.prev,())
            return NF
        end
        return a.prev[key]
    end
    p.b
end

function print(a::AList)
    allv = idtable()
    al = a
    while !is(al,())
        l = al.elts
        while isa(l,Cons)
            allv[head(l).a] = true
            l = tail(l)
        end
        al = al.prev
    end
    print("AList(")
    for (sym,_)=allv
        print(sym,"::",a[sym],", ")
    end
    print(")")
end

tintersect(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_intersection"), Any,
                        (Any,Any), a, b)
tmatch(a,b) = ccall(dlsym(JuliaDLHandle,"jl_type_match"), Any,
                    (Any,Any), a, b)

getmethods(f,t) = ccall(dlsym(JuliaDLHandle,"jl_matching_methods"), Any,
                        (Any,Any), f, t)

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
t_func[is] = (2, 2, cmp_tfunc)
t_func[subtype] = (2, 2, cmp_tfunc)
t_func[isa] = (2, 2, cmp_tfunc)
t_func[tuplelen] = (1, 1, x->Int32)
t_func[arraylen] = (1, 1, x->Int32)
t_func[arrayref] = (2, 2, (a,i)->(subtype(a,Array) ? a.parameters[1] : Any))
t_func[arrayset] = (3, 3, (a,i,v)->a)
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
function tupleref_tfunc(A, t, i)
    if is(t,())
        return Bottom
    end
    if !isa(t,Tuple)
        return Any
    end
    n = length(t)
    last = t[n]
    vararg = (isa(last,TagKind) && is(last.name,`...))
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
        return normalize_numeric_type(tf[3](args, argtypes...))
    end
    return normalize_numeric_type(tf[3](argtypes...))
end

function abstract_eval(e::Expr, vtypes::AList, sp)
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
        func = e.args[1]
        if isa(func,Expr) && is(func.head,`top)
            func = func.args[1]
            assert(isa(func,Symbol))
        else
            if !isa(func,Symbol)
                # TODO: lambda expression (let)
                return Any
            end
            if !is(vtypes[func],NF)
                # computed call with local var
                return Any
            end
        end
        f = eval(func)
        fargs = e.args[2:]
        argtypes = map(x->abstract_eval(x,vtypes,sp), fargs)
        print("call ", e.args[1], argtypes, " ")
        if isbuiltin(f)
            rt = builtin_tfunction(f, fargs, argtypes)
            print("=> ", rt, "\n")
            return rt
        elseif isgeneric(f)
            applicable = getmethods(f, argtypes)
            rettype = Bottom
            x = applicable
            while !is(x,())
                #print(x,"\n")
                # TODO: approximate static parameters by calling tmatch
                # on argtypes and the intersection
                if isa(x[3],Symbol)
                    # when there is a builtin method in this GF, we get
                    # a symbol with the name instead of a LambdaStaticData
                    rt = builtin_tfunction(x[3], fargs, x[1])
                else
                    rt = ast_rettype(typeinf(x[3].ast, x[2], x[1]))
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
end

ast_rettype(ast) = ast.args[3].type

function abstract_eval_constant(x)
    if isa(x,TagKind)
        return Type{x}
    end
    return typeof(x)
end

function abstract_eval_global(s::Symbol)
    if isconstant(s)
        return abstract_eval_constant(eval(s))
    else
        return Any
    end
end

function abstract_eval(s::Symbol, vtypes::AList, sp)
    t = vtypes[s]
    if is(t,NF)
        for i=1:2:length(sp)
            if is(sp[i],s)
                # static parameter
                return abstract_eval_constant(sp[i+1])
            end
        end
        # global
        return abstract_eval_global(s)
    end
    return t
end

abstract_eval(x, vtypes::AList, sp) = abstract_eval_constant(x)

function interpret(e::Expr, vtypes::AList, sp)
    # handle assignment
    if is(e.head,symbol("="))
        t = abstract_eval(e.args[2], vtypes, sp)
        a = alist(vtypes)
        a[e.args[1]] = t
        return a
    elseif is(e.head,`label)
        # no effect
    end
    return vtypes
end

tchanged(n, o) = is(o,NF) || (!is(n,NF) && !subtype(n,o))

function changed(new::AList, old::AList, vars)
    for v=vars
        if tchanged(new[v], old[v])
            return true
        end
    end
    return false
end

function tmerge(typea, typeb)
    return Union(typea, typeb)
end

function update(state::AList, new::AList, vars)
    if is(state.prev,())
        state.prev = new
    else
        for v=vars
            if tchanged(new[v], state[v])
                state[v] = tmerge(state[v], new[v])
            end
        end
    end
    state
end

function typeinf(ast::Expr, sparams::Tuple, atypes::Tuple)
    function findlabel(body, l)
        for i=1:length(body)
            b = body[i]
            if isa(b,Expr) && is(b.head,`label) && b.args[1]==l
                return i
            end
        end
        error("label not found")
    end

    #print("typeinf ", ast, " ", sparams, " ", atypes, "\n")

    assert(is(ast.head,`lambda))
    args = map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1])
    locals = ast.args[2].args[1].args
    vars = append(args, locals)
    body = ast.args[3].args

    n = length(body)
    s = map(stmt->alist(Symbol,Type), body)
    W = intset(n+1)
    # initial set of pc
    adjoin(W,1)
    # initial types
    for v=vars
        s[1][v] = Bottom
    end
    rettype = Bottom
    la = length(args)
    lastarg = ast.args[1][la]
    if isa(lastarg,Expr) && is(lastarg.head,symbol("::"))
        if ccall(dlsym(JuliaDLHandle,"jl_is_rest_arg"),Int32,(Any,),
                 lastarg)!=0
            s[1][args[la]] = atypes[la:]
            la -= 1
        end
    end
    for i=1:la
        s[1][args[i]] = atypes[i]
    end

    while !isempty(W)
        pc = choose(W)
        while true
            #print(pc,": ",s[pc],"\n")
            remove(W, pc)
            stmt = body[pc]
            newstate = interpret(stmt, s[pc], sparams)
            pc´ = pc+1
            if isa(stmt,Expr)
                if is(stmt.head,`goto)
                    pc´ = findlabel(body,stmt.args[1])
                elseif is(stmt.head,`gotoifnot)
                    l = findlabel(body,stmt.args[2])
                    if changed(newstate, s[l], vars)
                        adjoin(W, l)
                        update(s[l], newstate, vars)
                    end
                elseif is(stmt.head,symbol("return"))
                    pc´ = n+1
                    rt = abstract_eval(stmt.args[1], s[pc], sparams)
                    rettype = tmerge(rettype, rt)
                end
            end
            if pc´<=n && changed(newstate, s[pc´], vars)
                update(s[pc´], newstate, vars)
                pc = pc´
            else
                break
            end
        end
    end
    return type_annotate(ast, s, sparams, rettype)
end

function eval_annotate(e::Expr, vtypes::AList, sp)
    if is(e.head,`quote) || is(e.head,`top)
        return Expr(e.head, e.args, abstract_eval(e, vtypes, sp))
    elseif is(e.head,`goto) || is(e.head,`label)
        return e
    elseif is(e.head,`gotoifnot) || is(e.head,symbol("return")) ||
        is(e.head,symbol("="))
        return Expr(e.head, map(x->eval_annotate(x,vtypes,sp), e.args), Any)
    end
    Expr(e.head, map(x->eval_annotate(x,vtypes,sp), e.args),
         abstract_eval(e, vtypes, sp))
end

function eval_annotate(e::Symbol, vtypes::AList, sp)
    Expr(`symbol, (e,), abstract_eval(e, vtypes, sp))
end

eval_annotate(s, vtypes::AList, sp) = s

# expand v to v::T as appropriate based on all inferred type info
function add_decls(vars::(Symbol...), states::(AList...))
    # TODO
    return vars
end

# copy of AST with all type information inserted
function type_annotate(ast::Expr, states::(AList...), sp, rettype)
    vinf = ast.args[2]
    expr(`lambda,
         ast.args[1],
         expr(symbol("var-info"), expr(`locals, add_decls(vinf.args[1].args,
                                                          states)),
              vinf.args[2], vinf.args[3], vinf.args[4]),
         Expr(`body,
              map((stmt,s)->eval_annotate(stmt, s, sp),
                  ast.args[3].args, states),
              rettype))
end

T=typevar(`T)
S=typevar(`S)
R=typevar(`R)
a=typevar(`a)
b=typevar(`b)
c=typevar(`c)
d=typevar(`d)

m = getmethods(fact,(Int32,))
ast = m[3]
#typeinf(ast.ast, (`T,Int32), (Int32,))

function foo(x)
    return x.re + x.im
end

m = getmethods(foo,(Complex{Float64},))
ast = m[3]
