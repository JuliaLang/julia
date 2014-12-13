###### Functors ######

# Note that functors are merely used as internal machinery to enhance code reuse.
# They are not exported.
# When function arguments can be inlined, the use of functors can be removed.

abstract Func{N}

immutable IdFun <: Func{1} end
call(::IdFun, x)  = x

immutable AbsFun <: Func{1} end
call(::AbsFun, x) = abs(x)

immutable Abs2Fun <: Func{1} end
call(::Abs2Fun, x) = abs2(x)

immutable ExpFun <: Func{1} end
call(::ExpFun, x) = exp(x)

immutable LogFun <: Func{1} end
call(::LogFun, x) = log(x)

immutable AddFun <: Func{2} end
call(::AddFun, x, y) = x + y

immutable MulFun <: Func{2} end
call(::MulFun, x, y) = x * y

immutable AndFun <: Func{2} end
call(::AndFun, x, y) = x & y

immutable OrFun <: Func{2} end
call(::OrFun, x, y) =  x | y

immutable MaxFun <: Func{2} end
call(::MaxFun, x, y) = scalarmax(x,y)

immutable MinFun <: Func{2} end
call(::MinFun, x, y) = scalarmin(x, y)

immutable EqX{T} <: Func{1}
    x::T
end
call(f::EqX, y) = f.x == y

immutable NotEqZero <: Func{1} end
call(::NotEqZero, x) = x != 0

# apply a functor on the second passed-in argument
immutable Apply2nd{F<:Func{1}} <: Func{1}
    f::F
end
call(f::Apply2nd, x, y) = f.f(y)

# loose aliases, as we can't check that a Callable is unary
# or a predicate, but serves at least as documentation
typealias UnaryCallable Union(Callable, Func{1})
typealias Predicate UnaryCallable


filter(a, v)  = filter( EqX(v), a)
filter!(a, v) = filter!(EqX(v), a)

filter(a)  = filter( NotEqZero(), a)
filter!(a) = filter!(NotEqZero(), a)
