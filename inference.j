# infrastructure needed for type inference:
# * shared assoc list
# * IntSet
# * printing exprs
# - more method table reflection
# * hash table of symbols
# * eval
# - t-functions for builtins
# - constantp()

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

function interpret(expr, vtypes::AList, vars)
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

function update(state::AList, new::AList, vars)
    if is(state.prev,())
        state.prev = new
    else
        for v=vars
            if tchanged(new[v], state[v])
                state[v] = Union(state[v], new[v])
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
                else
                    if is(stmt.head,`gotoifnot)
                        l = findlabel(body,stmt.args[2])
                        if changed(newstate, s[l], vars)
                            adjoin(W, l)
                            update(s[l], newstate, vars)
                        end
                    end
                end
            end
            if changed(newstate, s[pc´], vars)
                update(s[pc´], newstate, vars)
                pc = pc´
            else
                break
            end
            if pc == n+1
                break
            end
        end
    end
end
