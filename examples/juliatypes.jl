# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: convert, show

abstract Ty

type TypeName
    name::Symbol
    abs::Bool
    super::Ty    # actually TagT
    # arity
    # representation
    # mutable
    TypeName(name, abs, super) = new(name, abs, super)
    TypeName(name, abs) = new(name, abs)
end

show(io::IO, x::TypeName) = print(io, x.name)

type TagT <: Ty
    name::TypeName
    params
    vararg::Bool
    TagT(n, p, v=false) = new(n, p, v)
end

function show(io::IO, t::TagT)
    print(io, t.name.name)
    isempty(t.params) && return
    print(io, '{')
    l = length(t.params)
    for i=1:l
        show(io, t.params[i])
        if i == l && t.vararg
            print(io, "...")
        elseif i < l
            print(io, ",")
        end
    end
    print(io, '}')
end

type BottomTy <: Ty
end

show(io::IO, ::BottomTy) = print(io, "BottomT")

type UnionT <: Ty
    a
    b
    UnionT() = BottomT
    UnionT(a) = a
    UnionT(a, b) = new(a, b)
    UnionT(a, ts...) = new(a, UnionT(ts...))
end

function show(io::IO, t::UnionT)
    print(io, "UnionT(")
    while true
        show(io, t.a)
        print(io, ",")
        if isa(t.b, UnionT)
            t = t.b
        else
            show(io, t.b)
            break
        end
    end
    print(io, ')')
end

type Var
    name::Symbol
    lb
    ub
    Var(n, lb=BottomT, ub=AnyT) = new(n, lb, ub)
end

function show_var_bounds(io::IO, v::Var)
    if v.lb !== BottomT
        if v.ub === AnyT
            print(io, v.name)
            print(io, ">:")
            show(io, v.lb)
            return
        end
        show(io, v.lb)
        print(io, "<:")
    end
    print(io, v.name)
    if v.ub !== AnyT
        print(io, "<:")
        show(io, v.ub)
    end
end

show(io::IO, v::Var) = print(io, v.name)

type UnionAllT <: Ty
    var::Var
    T
    UnionAllT(v::Var, t) = new(v, t)
    UnionAllT(v::Var, t::Union{Type,Tuple}) = new(v, convert(Ty, t))
end

function show(io::IO, x::UnionAllT)
    print(io, "(@UnionAll ")
    show_var_bounds(io, x.var)
    print(io, " ")
    show(io, x.T)
    print(io, ")")
end


# Any, Bottom, and Tuple

const AnyT = TagT(TypeName(:Any,true), ())
AnyT.name.super = AnyT

const BottomT = BottomTy()

const TupleName = TypeName(:Tuple,false,AnyT)
const TupleT = TagT(TupleName, (AnyT,), true)
tupletype(xs...) = inst(TupleName, xs...)
vatype(xs...) = (t = inst(TupleName, xs...); t.vararg = true; t)


# type application

inst(typename::TypeName, params...) = TagT(typename, params)

inst(t::TagT) = t

inst(t::UnionAllT, param) = subst(t.T, Dict{Any,Any}(t.var => param))
inst(t::UnionAllT, param, rest...) = inst(inst(t,param), rest...)

supertype(t::TagT) = t.name===TupleName ? AnyT : inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = t===AnyT ? t : TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = UnionT(subst(t.a,env), subst(t.b,env))
subst(t::Var,     env) = get(env, t, t)
subst(t::UnionAllT, env) = (assert(!haskey(env, t.var));
                            newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                            UnionAllT(newVar, subst(t.T, extend(env, t.var, newVar))))
subst(t, env) = t


# subtype

isequal_type(x, y) = issub(x, y) && issub(y, x)

type Bounds
    # record current lower and upper bounds of a Var
    # right: whether this Var is on the right-hand side of A <: B
    lb
    ub
    right::Bool
end

type UnionState
    depth::Int           # number of union decision points we're inside
    more::Bool           # new union found; need to grow stack
    stack::Vector{Bool}  # stack of decisions
    UnionState() = new(1,0,Bool[])
end

type Env
    vars::Dict{Var,Bounds}
    Lunions::UnionState
    Runions::UnionState
    outer::Bool
    Env() = new(Dict{Var,Bounds}(), UnionState(), UnionState(), true)
end

function issub_env(x, y)
    e = Env()
    ans = forall_exists_issub(x, y, e)
    ans, e.vars
end
issub(x, y) = issub_env(x, y)[1]
issub(x, y, env) = (x === y)
issub(x::Ty, y::Ty, env) = (x === y) || x === BottomT

function forall_exists_issub(x, y, env)
    # for all combinations of elements from Unions on the left, there must
    # exist a combination of elements from Unions on the right that makes
    # issub() true. Unions in invariant position are on both the left and
    # the right in this formula.
    sub = exists_issub(x, y, env)

    if sub && env.Lunions.more
        push!(env.Lunions.stack, false)
        sub = forall_exists_issub(x, y, env)
        if sub
            env.Lunions.stack[end] = true
            sub = forall_exists_issub(x, y, env)
        end
        pop!(env.Lunions.stack)
    end
    return sub
end

function exists_issub(x, y, env)
    env.Lunions.depth = env.Runions.depth = 1
    env.Lunions.more = env.Runions.more = false

    found = issub(x, y, env)

    if env.Lunions.more
        # return up to forall_exists_issub. the recursion must have this shape:
        # ∀₁         ∀₁
        #   ∃₁  =>     ∀₂
        #                ...
        #                ∃₁
        #                  ∃₂
        return true
    end
    if env.Runions.more
        push!(env.Runions.stack, false)
        found = exists_issub(x, y, env)
        if !found
            env.Runions.stack[end] = true
            found = exists_issub(x, y, env)
        end
        pop!(env.Runions.stack)
    end
    return found
end

function issub_union(t, u::UnionT, env, R, state::UnionState)
    env.outer = false
    if state.depth > length(state.stack)
        # indicate that stack needs to grow
        state.more = true
        return true
    end
    ui = state.stack[state.depth]; state.depth += 1
    choice = getfield(u, 1+ui)
    return R ? issub(t, choice, env) : issub(choice, t, env)
end

issub(a::UnionT, b::UnionT, env) = a === b || issub_union(a, b, env, true, env.Runions)
issub(a::UnionT, b::Ty, env) = b===AnyT || issub_union(b, a, env, false, env.Lunions)
issub(a::Ty, b::UnionT, env) =
    a===BottomT || a===b.a || a===b.b || issub_union(a, b, env, true, env.Runions)

# handle vars before unions
issub(a::UnionT, b::Var, env) = var_gt(b, a, env)
issub(a::Var, b::UnionT, env) = var_lt(a, b, env)

function issub(a::TagT, b::TagT, env)
    env.outer = false
    a === b && return true
    b === AnyT && return true
    a === AnyT && return false
    if a.name !== b.name
        return issub(supertype(a), b, env)
    end
    if a.name === TupleName
        va, vb = a.vararg, b.vararg
        la, lb = length(a.params), length(b.params)
        ai = bi = 1
        while ai <= la
            bi > lb && return false
            !issub(a.params[ai], b.params[bi], env) && return false
            ai += 1
            if bi < lb || !vb
                bi += 1
            end
        end
        return (la==lb && va==vb) || (vb && (la >= (va ? lb : lb-1)))
    end
    for i = 1:length(a.params)
        ai, bi = a.params[i], b.params[i]
        # use issub in both directions to test equality
        if !(ai===bi || (issub(ai, bi, env) && issub(bi, ai, env)))
            return false
        end
    end
    return true
end

function join(a,b)
    (a===BottomT || b===AnyT || a === b) && return b
    (b===BottomT || a===AnyT) && return a
    UnionT(a,b)
end

issub(a::Ty, b::Var, env) = var_gt(b, a, env)
issub(a::Var, b::Ty, env) = var_lt(a, b, env)
function issub(a::Var, b::Var, env)
    env.outer = false
    a === b && return true
    aa = env.vars[a]; bb = env.vars[b]
    if aa.right
        # this is a bit odd, but seems necessary to make this case work:
        # (@UnionAll x<:T<:x RefT{RefT{T}}) == RefT{@UnionAll x<:T<:x RefT{T}}
        bb.right && return issub(bb.ub, bb.lb, env)
        return var_lt(a, b, env)
    else
        if !bb.right   # check ∀a,b . a<:b
            # the bounds of left-side variables never change, and can only lead
            # to other left-side variables, so using || here is safe.
            return issub(aa.ub, b, env) || issub(a, bb.lb, env)
        end
        return var_gt(b, a, env)
    end
end

# issub, but taking apart unions before handling vars
issub_ufirst(a::UnionT, b::Var, env) = issub_union(b, a, env, false, env.Lunions)
issub_ufirst(a::Var, b::UnionT, env) = a===b.a || a===b.b || issub_union(a, b, env, true, env.Runions)
issub_ufirst(a, b, env) = issub(a, b, env)

function var_lt(b::Var, a::Union{Ty,Var}, env)
    env.outer = false
    bb = env.vars[b]
    #println("$b($(bb.lb),$(bb.ub)) <: $a")
    !bb.right && return issub_ufirst(bb.ub, a, env)  # check ∀b . b<:a
    !issub_ufirst(bb.lb, a, env) && return false
    # for contravariance we would need to compute a meet here, but
    # because of invariance bb.ub ⊓ a == a here always. however for this
    # to work we need to compute issub(left,right) before issub(right,left),
    # since otherwise the issub(a, bb.ub) check in var_gt becomes vacuous.
    bb.ub = a  # meet(bb.ub, a)
    return true
end

function var_gt(b::Var, a::Union{Ty,Var}, env)
    env.outer = false
    bb = env.vars[b]
    #println("$b($(bb.lb),$(bb.ub)) >: $a")
    !bb.right && return issub_ufirst(a, bb.lb, env)  # check ∀b . b>:a
    !issub_ufirst(a, bb.ub, env) && return false
    bb.lb = join(bb.lb, a)
    return true
end

function rename(t::UnionAllT)
    v = Var(t.var.name, t.var.lb, t.var.ub)
    UnionAllT(v, inst(t,v))
end

function issub_unionall(t::Ty, u::UnionAllT, env, R)
    outer = env.outer
    haskey(env.vars, u.var) && (u = rename(u))
    env.vars[u.var] = Bounds(u.var.lb, u.var.ub, R)
    ans = R ? issub(t, u.T, env) : issub(u.T, t, env)
    !outer && delete!(env.vars, u.var)
    return ans
end

issub(a::UnionAllT, b::UnionAllT, env) = a === b || issub_unionall(a, b, env, true)
issub(a::UnionT, b::UnionAllT, env) = issub_unionall(a, b, env, true)
issub(a::UnionAllT, b::UnionT, env) = issub_unionall(b, a, env, false)
issub(a::Ty, b::UnionAllT, env) = a === BottomT || issub_unionall(a, b, env, true)
issub(a::UnionAllT, b::Ty, env) = b === AnyT || issub_unionall(b, a, env, false)


# convenient syntax

macro UnionAll(var, expr)
    lb = :BottomT
    ub = :AnyT
    if isa(var,Expr) && var.head === :comparison
        if length(var.args) == 3
            v = var.args[1]
            if var.args[2] == :(<:)
                ub = esc(var.args[3])
            elseif var.args[2] == :(>:)
                lb = esc(var.args[3])
            else
                error("invalid bounds in UnionAll")
            end
        elseif length(var.args) == 5
            v = var.args[3]
            if var.args[2] == var.args[4] == :(<:)
                lb = esc(var.args[1])
                ub = esc(var.args[5])
            else
                error("invalid bounds in UnionAll")
            end
        else
            error("invalid bounds in UnionAll")
        end
    elseif isa(var,Expr) && var.head === :(<:)
        v = var.args[1]
        ub = esc(var.args[2])
    elseif isa(var,Expr) && var.head === :(>:)
        v = var.args[1]
        lb = esc(var.args[2])
    elseif !isa(var,Symbol)
        error("invalid variable in UnionAll")
    else
        v = var
    end
    quote
        let $(esc(v)) = Var($(Expr(:quote,v)), $lb, $ub)
            UnionAllT($(esc(v)), $(esc(expr)))
        end
    end
end


# translating from existing julia types

const tndict = ObjectIdDict()

xlate(t) = xlate(t, ObjectIdDict())

xlate(t, env) = t

function xlate(t::Union, env)
    if t === Union{}
        return BottomT
    end
    UnionT(map(x->xlate(x,env), t.types)...)
end

function xlate(t::Tuple, env)
    if length(t) == 0
        return inst(TupleName)
    end
    va = Base.isvarargtype(t[end])
    ts = map(x->(Base.isvarargtype(x) ? xlate(x.parameters[1],env) : xlate(x,env)), t)
    tnew = inst(TupleName, ts...)
    tnew.vararg = va
    tnew
end

function xlate(t::TypeVar, env)
    if haskey(env, t)
        return env[t]
    end
    v = Var(t.name, xlate(t.lb,env), xlate(t.ub,env))
    env[t] = v
    v
end

function xlate(t::DataType, env)
    if t === Any
        return AnyT
    elseif t <: Tuple
        return xlate((t.parameters...,), env)
    elseif !haskey(tndict,t.name)
        para = map(x->xlate(x,env), t.name.primary.parameters)  # adds tvars to env
        sup = xlate(t.name.primary.super, env)
        for i = length(para):-1:1
            sup = UnionAllT(para[i], sup)
        end
        tn = TypeName(t.name.name, t.abstract, sup)
        tndict[t.name] = tn
    else
        tn = tndict[t.name]
    end
    inst(tn, map(x->xlate(x,env), t.parameters)...)
end

convert(::Type{Ty}, t::Union{Type,Tuple}) = xlate(t)
convert(::Type{Ty}, t::TypeVar) = xlate(t)

issub(a::Union{Type,Tuple}, b::Union{Type,Tuple}) = issub(xlate(a), xlate(b))
issub(a::Ty  , b::Union{Type,Tuple})              = issub(a       , xlate(b))
issub(a::Union{Type,Tuple}, b::Ty  )              = issub(xlate(a), b)
issub_env(a::Union{Type,Tuple}, b::Union{Type,Tuple}) = issub_env(xlate(a), xlate(b))
issub_env(a::Ty  , b::Union{Type,Tuple})              = issub_env(a       , xlate(b))
issub_env(a::Union{Type,Tuple}, b::Ty  )              = issub_env(xlate(a), b)

tt(ts...) = Tuple{ts...}
vt(ts...) = Tuple{ts[1:end-1]..., Vararg{ts[end]}}

# tests

AbstractArrayT =
    let AbstractArrayName = TypeName(:AbstractArray, true, @UnionAll T @UnionAll N AnyT)
        @UnionAll T @UnionAll N inst(AbstractArrayName, T, N)
    end

ArrayT =
    let ArrayName = TypeName(:Array, false, @UnionAll T @UnionAll N inst(AbstractArrayT, T, N))
        @UnionAll T @UnionAll N inst(ArrayName, T, N)
    end

PairT = let PairName = TypeName(:Pair, false, @UnionAll A @UnionAll B AnyT)
    @UnionAll A @UnionAll B inst(PairName, A, B)
end

RefT = let RefName = TypeName(:Ref, false, @UnionAll T AnyT)
    @UnionAll T inst(RefName, T)
end

tndict[AbstractArray.name] = AbstractArrayT.T.T.name
tndict[Array.name] = ArrayT.T.T.name
tndict[Pair.name] = PairT.T.T.name

using Base.Test

issub_strict(x,y) = issub(x,y) && !issub(y,x)

# level 1: no varags, union, UnionAll
function test_1()
    @test issub_strict(Int, Integer)
    @test issub_strict(Array{Int,1}, AbstractArray{Int,1})

    @test isequal_type(Int, Int)
    @test isequal_type(Integer, Integer)
    @test isequal_type(Array{Int,1}, Array{Int,1})
    @test isequal_type(AbstractArray{Int,1}, AbstractArray{Int,1})

    @test issub_strict((Int,Int), (Integer,Integer))
    @test issub_strict((Array{Int,1},), (AbstractArray{Int,1},))

    @test isequal_type((Integer,Integer), (Integer,Integer))

    @test !issub((Int,Int), (Int,))
    @test !issub((Int,), (Integer,Integer))
end

# level 2: varargs
function test_2()
    @test issub_strict((Int,Int), vt(Int,))
    @test issub_strict((Int,Int), vt(Int,Int,))
    @test issub_strict((Int,Int), vt(Int,Integer,))
    @test issub_strict((Int,Int), vt(Int,Int,Integer,))
    @test issub_strict(vt(Int,Int), vt(Int,))
    @test issub_strict(vt(Int,Int,Int), vt(Int,))
    @test issub_strict(vt(Int,Int,Int), vt(Integer,Int,))
    @test issub_strict(vt(Int,), vt(Any,))
    @test issub_strict((), vt(Any,))

    @test isequal_type(vt(Int,), vt(Int,))
    @test isequal_type(vt(Integer,), vt(Integer,))

    @test !issub((), vt(Int, Int))
    @test !issub((Int,), vt(Int, Int, Int))

    @test !issub((Int, (Real, Integer)), vt(Int))
end

function test_no_diagonal()
    # from test_3
    @test issub((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test isequal_type((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))

    @test issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Int)),
                (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test issub(Ty((Int,AbstractString,Vector{Any})),
                @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))

    @test isequal_type(Ty(Array{Tuple{Integer,Integer},1}),
                       inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))

    @test issub(Ty((Float32,Array{Real,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test isequal_type((@UnionAll T tupletype(inst(RefT,T), T)),
                       (@UnionAll T @UnionAll S<:T tupletype(inst(RefT,T),S)))
    @test isequal_type((@UnionAll T tupletype(inst(RefT,T), T)),
                       (@UnionAll T @UnionAll S<:T @UnionAll R<:S tupletype(inst(RefT,T),R)))

    @test  issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(AbstractString) tupletype(S,R,Ty(Vector{Any})))),
                 (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))
end

# level 3: UnionAll
function test_3()
    @test issub_strict(Ty(Array{Int,1}), @UnionAll T inst(ArrayT, T, 1))
    @test issub_strict((@UnionAll T inst(PairT,T,T)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll S inst(PairT,Ty(Int),S)))

    @test !issub((@UnionAll T<:Ty(Real) T), (@UnionAll T<:Ty(Integer) T))

    # diagonal
    #@test issub_strict((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test issub((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test isequal_type((@UnionAll T tupletype(T,T)), (@UnionAll R tupletype(R,R)))
    # diagonal
    #@test !issub(tupletype(Ty(Real),Ty(Real)), @UnionAll T<:Ty(Real) tupletype(T,T))

    @test !issub((@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
                 (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)))

    AUA = inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1)
    UAA = (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1))

    @test !issub(AUA, UAA)
    @test !issub(UAA, AUA)
    @test !isequal_type(AUA, UAA)

    @test issub_strict((@UnionAll T Int), (@UnionAll T<:Ty(Integer) Integer))

    @test isequal_type((@UnionAll T @UnionAll S tupletype(T, tupletype(S))),
                       (@UnionAll T tupletype(T, @UnionAll S tupletype(S))))

    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int8)))
    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int)))

    @test isequal_type((@UnionAll T tupletype(T)), tupletype(AnyT))
    @test isequal_type((@UnionAll T<:Ty(Real) tupletype(T)), tupletype(Ty(Real)))

    #@test !issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Int)),
    #             (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))
    @test  issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Int)),
                 (@UnionAll T<:Ty(Integer) @UnionAll S<:T tupletype(inst(ArrayT,T,1),S)))

    @test !issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Real)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(Ty((Int,AbstractString,Vector{Integer})),
                 @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))
    @test !issub(Ty((AbstractString,Int,Vector{Integer})),
                 @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))
    @test !issub(Ty((Int,AbstractString,Vector{Tuple{Integer}})),
                 @UnionAll T tupletype(T,T,inst(ArrayT,tupletype(T),1)))

    #@test !issub(Ty((Int,AbstractString,Vector{Any})),
    #             @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))

    @test isequal_type(Ty(Array{Int,1}), inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1))
    @test isequal_type(Ty(Array{Tuple{Any},1}), inst(ArrayT, (@UnionAll T tupletype(T)), 1))

    @test isequal_type(Ty(Array{Tuple{Int,Int},1}),
                       inst(ArrayT, (@UnionAll T<:Ty(Int) tupletype(T,T)), 1))
    @test !issub(Ty(Array{Tuple{Int,Integer},1}),
                 inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))


    @test !issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T inst(PairT,T,T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Int),1), Ty(Integer)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(Ty(Integer), inst(ArrayT,Ty(Int),1)),
                 (@UnionAll T<:Ty(Integer) tupletype(T, inst(ArrayT,T,1))))

    @test !issub(Ty(Array{Array{Int,1},Integer}),
                 (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test issub(Ty(Array{Array{Int,1},Int}),
                (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test  issub(Ty((Integer,Int)), @UnionAll T<:Ty(Integer) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(Int) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(AbstractString) @UnionAll S<:T tupletype(T,S))

    @test issub(Ty((Float32,Array{Float32,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Float32,Array{Float64,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    #@test !issub(Ty((Float32,Array{Real,1})),
    #             @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Number,Array{Real,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T), @UnionAll T<:Ty(Real) T)
    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                (@UnionAll T<:Ty(Real) inst(ArrayT,T,1)))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T),
                (@UnionAll Ty(Integer)<:T<:Ty(Real) T))
    @test !issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                 (@UnionAll Ty(Integer)<:T<:Ty(Real) inst(ArrayT,T,1)))

    X = (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))
    Y = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(A,B))
    @test isequal_type(X,Y)
    Z = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(Ty(Real),B))
    @test issub_strict(X,Z)

    @test issub_strict((@UnionAll T @UnionAll S<:T inst(PairT,T,S)),
                       (@UnionAll T @UnionAll S    inst(PairT,T,S)))
    @test issub_strict((@UnionAll T @UnionAll S>:T inst(PairT,T,S)),
                       (@UnionAll T @UnionAll S    inst(PairT,T,S)))

    #@test issub_strict((@UnionAll T tupletype(inst(RefT,T), T)),
    #                   (@UnionAll T @UnionAll S<:T tupletype(inst(RefT,T),S)))
    #@test issub_strict((@UnionAll T tupletype(inst(RefT,T), T)),
    #                   (@UnionAll T @UnionAll S<:T @UnionAll R<:S tupletype(inst(RefT,T),R)))
    @test isequal_type((@UnionAll T tupletype(inst(RefT,T), T)),
                       (@UnionAll T @UnionAll T<:S<:T tupletype(inst(RefT,T),S)))
    @test issub_strict((@UnionAll T tupletype(inst(RefT,T), T)),
                       (@UnionAll T @UnionAll S>:T tupletype(inst(RefT,T),S)))
end

# level 4: Union
function test_4()
    @test isequal_type(UnionT(BottomT,BottomT), BottomT)

    @test issub_strict(Int, Union{Int,AbstractString})
    @test issub_strict(Union{Int,Int8}, Integer)

    @test isequal_type(Union{Int,Int8}, Union{Int,Int8})

    @test isequal_type(UnionT(Ty(Int),Ty(Integer)), Ty(Integer))

    @test isequal_type(tt(Union{Int,Int8},Int16), Union{tt(Int,Int16),tt(Int8,Int16)})

    @test issub_strict((Int,Int8,Int), vt(Union{Int,Int8},))
    @test issub_strict((Int,Int8,Int), vt(Union{Int,Int8,Int16},))

    # nested unions
    @test !issub(UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
                 UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int8),Ty(Int16)))))

    A = Ty(Int);   B = Ty(Int8)
    C = Ty(Int16); D = Ty(Int32)
    @test  issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,D))))
    @test !issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,A))))

    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,B,C,D))
    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,UnionT(B,C),D))
    @test isequal_type(UnionT(UnionT(UnionT(UnionT(A)),B,C), UnionT(D)),
                       UnionT(A,UnionT(B,C),D))

    @test issub_strict(UnionT(UnionT(A,C), UnionT(D)),  UnionT(A,B,C,D))

    @test !issub(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,C,D))

    # obviously these unions can be simplified, but when they aren't there's trouble
    X = UnionT(UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),
               UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C))
    Y = UnionT(UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),
               UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(A,B,C))
    @test issub_strict(X,Y)
end

# level 5: union and UnionAll
function test_5()
    u = Ty(Union{Int8,Int})

    @test issub(Ty((AbstractString,Array{Int,1})),
                (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                                    tupletype(T,inst(ArrayT,Ty(Int),1)))))

    @test issub(Ty((Union{Vector{Int},Vector{Int8}},)),
                @UnionAll T tupletype(inst(ArrayT,T,1),))

    @test !issub(Ty((Union{Vector{Int},Vector{Int8}},Vector{Int})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty((Union{Vector{Int},Vector{Int8}},Vector{Int8})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty(Vector{Int}), @UnionAll T>:u inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Integer}), @UnionAll T>:u inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Union{Int,Int8}}), @UnionAll T>:u inst(ArrayT,T,1))

    @test issub((@UnionAll Ty(Int)<:T<:u inst(ArrayT,T,1)),
                (@UnionAll Ty(Int)<:T<:u inst(ArrayT,T,1)))

    # with varargs
    @test !issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                 @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test !issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                 @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))

    @test issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))

    B = @UnionAll S<:u tupletype(S, tupletype(AnyT,AnyT,AnyT), inst(RefT,S))
    # these tests require renaming in issub_unionall
    @test  issub((@UnionAll T<:B tupletype(Ty(Int8), T, inst(RefT,Ty(Int8)))), B)
    @test !issub((@UnionAll T<:B tupletype(Ty(Int8), T, inst(RefT,T))),        B)

    # the `convert(Type{T},T)` pattern, where T is a Union
    # required changing priority of unions and vars
    @test issub(tupletype(inst(ArrayT,u,1),Ty(Int)),
                @UnionAll T tupletype(inst(ArrayT,T,1), T))
    @test issub(tupletype(inst(ArrayT,u,1),Ty(Int)),
                @UnionAll T @UnionAll S<:T tupletype(inst(ArrayT,T,1), S))
end

# tricky type variable lower bounds
function test_6()
    # diagonal
    #@test !issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(AbstractString) tupletype(S,R,Ty(Vector{Any})))),
    #             (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))
    @test issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(AbstractString) tupletype(S,R,Ty(Vector{Any})))),
                (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))

    @test !issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(AbstractString) tupletype(S,R,Ty(Vector{Integer})))),
                 (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))

    t = @UnionAll T tupletype(T,T,inst(RefT,T))
    @test isequal_type(t, rename(t))

    @test !issub((@UnionAll T tupletype(T,Ty(AbstractString),inst(RefT,T))),
                 (@UnionAll T tupletype(T,T,inst(RefT,T))))

    @test !issub((@UnionAll T tupletype(T,inst(RefT,T),Ty(AbstractString))),
                 (@UnionAll T tupletype(T,inst(RefT,T),T)))

    i = Ty(Int); ai = Ty(Integer)
    @test isequal_type((@UnionAll i<:T<:i   inst(RefT,T)), inst(RefT,i))
    @test isequal_type((@UnionAll ai<:T<:ai inst(RefT,T)), inst(RefT,ai))

    # Pair{T,S} <: Pair{T,T} can be true with certain bounds
    @test issub_strict((@UnionAll i<:T<:i @UnionAll i<:S<:i inst(PairT,T,S)),
                       @UnionAll T inst(PairT,T,T))

    @test issub_strict(tupletype(i, inst(RefT,i)),
                       (@UnionAll T @UnionAll S<:T tupletype(S,inst(RefT,T))))

    @test !issub(tupletype(Ty(Real), inst(RefT,i)),
                 (@UnionAll T @UnionAll S<:T tupletype(S,inst(RefT,T))))

    # S >: T
    @test issub_strict(tupletype(Ty(Real), inst(RefT,i)),
                       (@UnionAll T @UnionAll S>:T tupletype(S,inst(RefT,T))))

    @test !issub(tupletype(inst(RefT,i), inst(RefT,ai)),
                 (@UnionAll T @UnionAll S>:T tupletype(inst(RefT,S),inst(RefT,T))))

    @test issub_strict(tupletype(inst(RefT,Ty(Real)), inst(RefT,ai)),
                       (@UnionAll T @UnionAll S>:T tupletype(inst(RefT,S),inst(RefT,T))))


    @test issub_strict(tupletype(Ty(Real), inst(RefT,tupletype(i))),
                       (@UnionAll T @UnionAll S>:T tupletype(S,inst(RefT,tupletype(T)))))

    @test !issub(tupletype(inst(RefT,tupletype(i)), inst(RefT,tupletype(ai))),
                 (@UnionAll T @UnionAll S>:T tupletype(inst(RefT,tupletype(S)),inst(RefT,tupletype(T)))))

    @test issub_strict(tupletype(inst(RefT,tupletype(Ty(Real))), inst(RefT,tupletype(ai))),
                       (@UnionAll T @UnionAll S>:T tupletype(inst(RefT,tupletype(S)),inst(RefT,tupletype(T)))))

    # (@UnionAll x<:T<:x Q{T}) == Q{x}
    @test isequal_type(inst(RefT,inst(RefT,i)),
                       inst(RefT,@UnionAll i<:T<:i inst(RefT,T)))
    @test isequal_type((@UnionAll i<:T<:i inst(RefT,inst(RefT,T))),
                       inst(RefT,@UnionAll i<:T<:i inst(RefT,T)))
    @test !issub((@UnionAll i<:T<:i inst(RefT,inst(RefT,T))),
                 inst(RefT,@UnionAll T<:i inst(RefT,T)))

    u = Ty(Union{Int8,Int64})
    A = inst(RefT,BottomT)
    B = @UnionAll S<:u inst(RefT,S)
    @test issub(inst(RefT,B), @UnionAll A<:T<:B inst(RefT,T))

    C = @UnionAll S<:u S
    @test issub(inst(RefT,C), @UnionAll u<:T<:u inst(RefT,T))

    BB = @UnionAll S<:BottomT S
    @test issub(inst(RefT,B), @UnionAll BB<:U<:B inst(RefT,U))
end

# tests that don't pass yet
function test_failing()
    # TODO: S <: Array{T} cases
end

function test_all()
    test_1()
    test_2()
    test_no_diagonal()
    test_3()
    test_4()
    test_5()
    test_6()
    test_failing()
end

const menagerie =
    Any[BottomT, AnyT, Ty(Int), Ty(Int8), Ty(Integer), Ty(Real),
        Ty(Array{Int,1}), Ty(AbstractArray{Int,1}),
        Ty(vt(Int,Integer,)), Ty(vt(Integer,Int,)), Ty(()),
        Ty(Union{Int,Int8}),
        (@UnionAll T inst(ArrayT, T, 1)),
        (@UnionAll T inst(PairT,T,T)),
        (@UnionAll T @UnionAll S inst(PairT,T,S)),
        inst(PairT,Ty(Int),Ty(Int8)),
        (@UnionAll S inst(PairT,Ty(Int),S)),
        (@UnionAll T tupletype(T,T)),
        (@UnionAll T<:Ty(Integer) tupletype(T,T)),
        (@UnionAll T @UnionAll S tupletype(T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)),
        inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1),
        (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1)),
        inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1),
        (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S)),
        UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
        (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                            tupletype(T,inst(ArrayT,Ty(Int),1)))),
        ]

let new = Any[]
    # add variants of each type
    for T in menagerie
        push!(new, inst(RefT, T))
        push!(new, tupletype(T))
        push!(new, tupletype(T,T))
        push!(new, vatype(T))
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T inst(RefT,S))
    end
    append!(menagerie, new)
end

function test_properties()
    x→y = !x || y
    ¬T = @UnionAll X>:T inst(RefT,X)

    for T in menagerie
        # top and bottom identities
        @test issub(BottomT, T)
        @test issub(T, AnyT)
        @test issub(T, BottomT) → isequal_type(T, BottomT)
        @test issub(AnyT, T) → isequal_type(T, AnyT)

        # unionall identity
        @test isequal_type(T, @UnionAll S<:T S)
        @test isequal_type(inst(RefT,T), @UnionAll T<:U<:T inst(RefT,U))

        # equality under renaming
        if isa(T, UnionAllT)
            @test isequal_type(T, rename(T))
        end

        # inequality under wrapping
        @test !isequal_type(T, inst(RefT,T))

        for S in menagerie
            issubTS = issub(T, S)
            # transitivity
            if issubTS
                for R in menagerie
                    if issub(S, R)
                        if !issub(T, R)
                            @show T
                            @show S
                            @show R
                        end
                        @test issub(T, R)  # issub(S, R) → issub(T, R)
                        @test issub(inst(RefT,S), @UnionAll T<:U<:R inst(RefT,U))
                    end
                end
            end

            # union subsumption
            @test isequal_type(T, UnionT(T,S)) → issub(S, T)

            # invariance
            @test isequal_type(T, S) == isequal_type(inst(RefT,T), inst(RefT,S))

            # covariance
            @test issubTS == issub(tupletype(T), tupletype(S))
            @test issubTS == issub(vatype(T), vatype(S))
            @test issubTS == issub(tupletype(T), vatype(S))

            # contravariance
            @test issubTS == issub(¬S, ¬T)
        end
    end
end

# function non_terminating_F()
#     # undecidable F_<: instance
#     ¬T = @ForAll α<:T α
#     θ = @ForAll α ¬(@ForAll β<:α ¬β)
#     a₀ = Var(:a₀, BottomT, θ)
#     issub(a₀, (@ForAll a₁<:a₀ ¬a₁))
# end

# attempt to implement non-terminating example from
# "On the Decidability of Subtyping with Bounded Existential Types"
function non_terminating_2()
    C = let CName = TypeName(:C, false, @UnionAll T AnyT)
        @UnionAll T inst(CName, T)
    end
    D = let DName = TypeName(:D, false, @UnionAll T AnyT)
        @UnionAll T inst(DName, T)
    end
    ¬T = @UnionAll X>:T inst(D,X)
    U = AnyT
    X = Var(:X, BottomT, ¬U)
    e = Env()
    e.vars[X] = Bounds(BottomT, ¬U, false)
    issub(X, ¬inst(C,X), e)
end

#=
Replacing type S with a union over var T(ᶜ)<:S can affect the subtype
relation in all possible ways:

    (C{Real},C{Real}) < @U T<:Real (C{T},C{T})
    (Real,Real)       = @U T<:Real (T,T)

    (C{Real},C{Real}) ! @U Tᶜ<:Real (C{T},C{T})  (*)  # neither <= nor >=
    (Real,Real)       > @U Tᶜ<:Real (T,T)

    (C{Real},) < @U T<:Real (C{T},)
    (Real,)    = @U T<:Real (T,)
    (C{Real},) ! @U Tᶜ<:Real (C{T},)  (*)
    (Real,)    = @U Tᶜ<:Real (T,)

cases (*) are probably not needed in practice.


transform all covariant occurrences of non-concrete types to
vars: (Real,Real) => @U T<:Real @U S<:Real (T,S)
and then treat all vars as invariant.
this can maybe even be done on the fly without actually making the
UnionAll types up-front.
in fact this may only need to be done on the left.
which implies right away that the algorithm works as-is if the left
side is a concrete type.

possible explanation:

1. match all tvars with invariant rule
2. imagine left side is concrete
3. therefore all (right-) vars will have to equal concrete types
   (this completes the base case)
4. inductive hypothesis: tvars match concrete types
5. "build up" an abstract type on the left over the initial concrete type
6. handle all abstract types T in covariant position on the left by treating
   them as variables V<:T instead
7. by the inductive hypothesis tvars continue to match concrete types

it seems you need to plug in the isconcrete predicate in step 6.

=#
