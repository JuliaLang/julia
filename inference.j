# infrastructure needed for type inference:
# * shared assoc list
# * IntSet
# * printing exprs
# - compute type intersection
# - more method table reflection
#   . cached t-functions
#   . abstract_invoke()
#     . consult t-func cache
#     . determine applicable methods
#     . abstract_invoke all of them, type-union the result, and cache it
# * hash table of symbols
# * eval
# - t-functions for builtins
# - isconstant()

# mutable pair
struct Pair{T,S}
    a::T
    b::S
end

# delegated assoc list
# equivalent to some previous list except for local modifications
struct AList{T,S}
    prev::Nullable{AList{T,S}}
    elts::List{Pair{T,S}}
end

struct NotFound
end

NF = NotFound()

# note: with Nullable (or other union types) it might not be possible to
# infer type parameters from field values. bug or feature??
alist(kt::Type, vt::Type) = AList{kt,vt}.new((), nil)

alist(a::AList) = AList(a, nil)

assoc(item, a::EmptyList) = nil
assoc(item, p::List{Pair}) = is(head(p).a,item) ? head(p) : assoc(item,tail(p))

function set{T,S}(a::AList{T,S}, val::S, key::T)
    p = assoc(key, a.elts)
    if !is(p,nil)
        p.b = val
    else
        p.b = Cons(Pair(key,val),p.b)
    end
    a
end

function ref{T,S}(a::AList{T,S}, key::T)
    p = assoc(key, a.elts)
    if is(p,nil)
        if is(a.prev,())
            return NF
        end
        return a.prev[key]
    end
    p.b
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
isconstant(s::Symbol) = true

t_func = idtable()
t_func[`tuple] = (0, Inf, (args...)->args)
t_func[`boxsi8] = (1, 1, x->Int8)
t_func[`boxui8] = (1, 1, x->Uint8)
t_func[`boxsi16] = (1, 1, x->Int16)
t_func[`boxui16] = (1, 1, x->Uint16)
t_func[`boxsi32] = (1, 1, x->Int32)
t_func[`boxui32] = (1, 1, x->Uint32)
t_func[`boxsi64] = (1, 1, x->Int64)
t_func[`boxui64] = (1, 1, x->Uint64)
t_func[`boxf32] = (1, 1, x->Float32)
t_func[`boxf64] = (1, 1, x->Float64)
t_func[`is] = (2, 2, (x,y)->Bool)
t_func[`subtype] = (2, 2, (x,y)->Bool)
t_func[`isa] = (2, 2, (x,y)->Bool)
t_func[`tuplelen] = (1, 1, x->Int32)
t_func[`arraylen] = (1, 1, x->Int32)
t_func[`arrayref] = (2, 2, (a,i)->(subtype(a,Array) ? a.parameters[1] : Any))
t_func[`arrayset] = (3, 3, (a,i,v)->a)
t_func[`identity] = (1, 1, identity)

function builtin_tfunction(f::Symbol, argtypes::Tuple)
    tf = get(t_func, f, false)
    if is(tf,false)
        # unknown/unhandled builtin
        return Any
    end
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](argtypes...)
end

EmptyAList = alist(Symbol, Type)

function abstract_eval(e::Expr, vtypes::AList, vars)
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
        return abstract_eval(e.args[1], EmptyAList, ())
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
        if !isa(f,Function)
            return Any
        end
        argtypes = map(x->abstract_eval(x,vtypes,vars), e.args[2:])
        if isbuiltin(f)
            return builtin_tfunction(func, argtypes)
        elseif isgeneric(f)
            applicable = getmethods(f, argtypes)
            rettype = Bottom
            x = applicable
            while !is(x,())
                # TODO: approximate static parameters by calling tmatch
                # on argtypes and the intersection
                rt = ast_rettype(typeinf(x[2].ast, x[2].sparams,
                                         tintersect(x[1],argtypes)))
                rettype = tmerge(rettype, rt)
                x = x[3]
            end
            # if rettype is Bottom we've found a method not found error
            return rettype
        end
    end
end

ast_rettype(ast) = ast.args[3].type

function abstract_eval(s::Symbol, vtypes::AList, vars)
    t = vtypes[s]
    if is(t,NF)
        # global
        if isconstant(s)
            T = typeof(eval(s))
            if isa(T,TagKind)
                return Type{T}
            end
            return T
        else
            return Any
        end
    end
    return t
end

abstract_eval(s, vtypes::AList, vars) = typeof(s)

function interpret(e::Expr, vtypes::AList, vars)
    # handle assignment
    if is(e.head,symbol("="))
        t = abstract_eval(e.args[2], vtypes, vars)
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

    assert(is(ast.head,`lambda))
    args = map(x->(isa(x,Expr) ? x.args[1] : x), ast.args[1])
    locals = ast.args[2].args[1].args
    vars = append(args, locals)
    body = ast.args[3].args

    n = length(body)
    s = map(stmt->alist(Symbol,Type), body)
    W = intset(n+1)
    # initial set of pc
    for i=1:n; adjoin(W, i); end
    # initial types
    for v=vars
        s[1][v] = Bottom
    end
    rettype = Bottom
    for i=1:length(args)
        s[1][args[i]] = atypes[i]
    end

    while !isempty(W)
        pc = choose(W)
        while true
            remove(W, pc)
            stmt = body[pc]
            newstate = interpret(stmt, s[pc], vars)
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
                    rettype = tmerge(rettype, stmt.args[1].type)
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
    return type_annotate(ast, s, vars, rettype)
end

function eval_annotate(e::Expr, vtypes::AList, vars)
    if is(e.head,`quote) || is(e.head,`top)
        return Expr(e.head, e.args, abstract_eval(e, vtypes, vars))
    elseif is(e.head,`goto) || is(e.head,`label)
        return e
    elseif is(e.head,`gotoifnot) || is(e.head,symbol("return"))
        return Expr(e.head, map(x->eval_annotate(x,vtypes,vars), e.args), Any)
    end
    Expr(e.head, map(x->eval_annotate(x,vtypes,vars), e.args),
         abstract_eval(e, vtypes, vars))
end

function eval_annotate(e::Symbol, vtypes::AList, vars)
    Expr(`symbol, (e,), abstract_eval(e, vtypes, vars))
end

eval_annotate(s, vtypes::AList, vars) = s

# expand v to v::T as appropriate based on all inferred type info
function add_decls(vars::(Symbol...), states::(AList...))
    # TODO
    return vars
end

# copy of AST with all type information inserted
function type_annotate(ast::Expr, states::(AList...), vars, rettype)
    vinf = ast.args[2]
    expr(`lambda,
         ast.args[1],
         expr(symbol("var-info"), expr(`locals, add_decls(vinf.args[1].args,
                                                          states)),
              vinf.args[2], vinf.args[3], vinf.args[4]),
         Expr(`body,
              map((stmt,s)->eval_annotate(stmt, s, vars),
                  ast.args[3].args, states),
              rettype))
end
