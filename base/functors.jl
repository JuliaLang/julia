# This file is a part of Julia. License is MIT: http://julialang.org/license

###### Function Objects ("Functors") ######

# Note that function objects are merely used as internal machinery to
# enhance code reuse and improve performance of map/reduce.
# They are not exported.
# When function arguments can be inlined, the use of function objects
# can be removed.

abstract Func{N}

immutable IdFun <: Func{1} end
(::IdFun)(x) = x

immutable AbsFun <: Func{1} end
(::AbsFun)(x) = abs(x)

immutable Abs2Fun <: Func{1} end
(::Abs2Fun)(x) = abs2(x)

immutable ExpFun <: Func{1} end
(::ExpFun)(x) = exp(x)

immutable LogFun <: Func{1} end
(::LogFun)(x) = log(x)

immutable ConjFun <: Func{1} end
(::ConjFun)(x) = conj(x)

immutable AndFun <: Func{2} end
(::AndFun)(x, y) = x & y

immutable OrFun <: Func{2} end
(::OrFun)(x, y) = x | y

immutable XorFun <: Func{2} end
(::XorFun)(x, y) = x $ y

immutable AddFun <: Func{2} end
(::AddFun)(x, y) = x + y

immutable DotAddFun <: Func{2} end
(::DotAddFun)(x, y) = x .+ y

immutable SubFun <: Func{2} end
(::SubFun)(x, y) = x - y

immutable DotSubFun <: Func{2} end
(::DotSubFun)(x, y) = x .- y

immutable MulFun <: Func{2} end
(::MulFun)(x, y) = x * y

immutable DotMulFun <: Func{2} end
(::DotMulFun)(x, y) = x .* y

immutable RDivFun <: Func{2} end
(::RDivFun)(x, y) = x / y

immutable DotRDivFun <: Func{2} end
(::DotRDivFun)(x, y) = x ./ y

immutable LDivFun <: Func{2} end
(::LDivFun)(x, y) = x \ y

immutable IDivFun <: Func{2} end
(::IDivFun)(x, y) = div(x, y)

immutable DotIDivFun <: Func{2} end
(::DotIDivFun)(x, y) = x .÷ y

immutable ModFun <: Func{2} end
(::ModFun)(x, y) = mod(x, y)

immutable RemFun <: Func{2} end
(::RemFun)(x, y) = rem(x, y)

immutable DotRemFun <: Func{2} end
(::DotRemFun)(x, y) = x .% y

immutable PowFun <: Func{2} end
(::PowFun)(x, y) = x ^ y

immutable MaxFun <: Func{2} end
(::MaxFun)(x, y) = scalarmax(x,y)

immutable MinFun <: Func{2} end
(::MinFun)(x, y) = scalarmin(x, y)

immutable LessFun <: Func{2} end
(::LessFun)(x, y) = x < y

immutable MoreFun <: Func{2} end
(::MoreFun)(x, y) = x > y

immutable DotLSFun <: Func{2} end
(::DotLSFun)(x, y) = x .<< y

immutable DotRSFun <: Func{2} end
(::DotRSFun)(x, y) = x .>> y

# a fallback unspecialized function object that allows code using
# function objects to not care whether they were able to specialize on
# the function value or not
immutable UnspecializedFun{N} <: Func{N}
    f::Function
end
(f::UnspecializedFun{1})(x) = f.f(x)
(f::UnspecializedFun{2})(x, y) = f.f(x,y)

# Special purpose functors

immutable Predicate{F} <: Func{1}
    f::F
end
(pred::Predicate)(x) = pred.f(x)::Bool

immutable EqX{T} <: Func{1}
    x::T
end

(f::EqX)(y) = f.x == y

# More promote_op rules

promote_op{T<:Integer}(::PowFun, ::Type{Bool}, ::Type{T}) = Bool

#### Bitwise operators ####

# BitFunctors are functions that behave in the same bit-wise manner when applied
# to individual bits as well as integers, allowing them to be used in BitArrays

# Note that there are 16 possible pure two-argument logical functions,
# of which eight don't exist as a single function in Base (but six of those are trivial):
##############################################################################
##  p = TTFF                          ##  p = TTFF                          ##
##  q = TFTF    function    bit-op    ##  q = TFTF    function    bit-op    ##
##  --------------------------------  ##  --------------------------------- ##
##      TTTT    (true)      p | ~p    ##      FFFF    (false)    p & ~p     ##
##      TTTF    |, max      p | q     ##      FFFT    ???        ~(p | q)   ##
##      TTFT    >=, ^       p | ~q    ##      FFTF    <          ~p & q     ##
##      TTFF    (p)         p         ##      FFTT    (~p)       ~p         ##
##      TFTT    <=          ~p | q    ##      FTFF    >          p & ~q     ##
##      TFTF    (q)         q         ##      FTFT    (~q)       ~q         ##
##      TFFT    ==          ~(p $ q)  ##      FTTF    $, !=      p $ q      ##
##      TFFF    &, *, min   p & q     ##      FTTT    ???        ~(p & q)   ##
##############################################################################


immutable BitFunctorUnary{T,F} <: Func{1} end
(::BitFunctorUnary{true,  true})( p) = p | ~p # Must work for bits and ints
(::BitFunctorUnary{false, false})(p) = p & ~p # LLVM figures them out nicely
(::BitFunctorUnary{true,  false})(p) =  p
(::BitFunctorUnary{false, true})( p) = ~p

immutable BitFunctorBinary{TT,TF,FT,FF} <: Func{2} end
(::BitFunctorBinary{true,  true,  true,  true })(p, q) = p | ~p
(::BitFunctorBinary{true,  true,  true,  false})(p, q) = p | q
(::BitFunctorBinary{true,  true,  false, true })(p, q) = p | ~q
(::BitFunctorBinary{true,  true,  false, false})(p, q) = p
(::BitFunctorBinary{true,  false, true,  true })(p, q) = ~p | q
(::BitFunctorBinary{true,  false, true,  false})(p, q) = q
(::BitFunctorBinary{true,  false, false, true })(p, q) = ~(p $ q)
(::BitFunctorBinary{true,  false, false, false})(p, q) = p & q

(::BitFunctorBinary{false, false, false, false})(p, q) = p & ~p
(::BitFunctorBinary{false, false, false, true })(p, q) = ~(p | q)
(::BitFunctorBinary{false, false, true,  false})(p, q) = ~p & q
(::BitFunctorBinary{false, false, true,  true })(p, q) = ~p
(::BitFunctorBinary{false, true,  false, false})(p, q) = p & ~q
(::BitFunctorBinary{false, true,  false, true })(p, q) = ~q
(::BitFunctorBinary{false, true,  true,  false})(p, q) = p $ q
(::BitFunctorBinary{false, true,  true,  true })(p, q) = ~(p & q)

# Specializations by value

function specialized_unary(f::Function)
    is(f, identity) ? IdFun()   :
    is(f, abs)      ? AbsFun()  :
    is(f, abs2)     ? Abs2Fun() :
    is(f, exp)      ? ExpFun()  :
    is(f, log)      ? LogFun()  :
    UnspecializedFun{1}(f)
end
function specialized_binary(f::Function)
    is(f, +) ? AddFun() :
    is(f, -) ? SubFun() :
    is(f, *) ? MulFun() :
    is(f, /) ? RDivFun() :
    is(f, \) ? LDivFun() :
    is(f, ^) ? PowFun() :
    is(f, &) ? AndFun() :
    is(f, |) ? OrFun()  :
    is(f, %) ? RemFun() :
    is(f, rem) ? RemFun() :
    is(f, ÷) ? IDivFun() :
    is(f, div) ? IDivFun() :
    UnspecializedFun{2}(f)
end

function specialized_bitwise_unary(f::Function)
    is(f, identity)     ? BitFunctorUnary{true,  false}() :
    is(f, !) | is(f, ~) ? BitFunctorUnary{false, true }() :
    is(f, one)          ? BitFunctorUnary{true,  true }() :
    is(f, zero)         ? BitFunctorUnary{false, false}() :
    UnspecializedFun{1}(f)
end
function specialized_bitwise_binary(f::Function)
    is(f, &)  | is(f, *) | is(f, min) ? BitFunctorBinary{true,  false, false, false}() :
    is(f, |)  | is(f, max)            ? BitFunctorBinary{true,  true,  true,  false}() :
    is(f, $)  | is(f, !=)             ? BitFunctorBinary{false, true,  true,  false}() :
    is(f, >=) | is(f, ^)              ? BitFunctorBinary{true,  true,  false, true }() :
    is(f, <=)                         ? BitFunctorBinary{true,  false, true,  true }() :
    is(f, ==)                         ? BitFunctorBinary{true,  false, false, true }() :
    is(f, <)                          ? BitFunctorBinary{false, false, true,  false}() :
    is(f, >)                          ? BitFunctorBinary{false, true,  false, false}() :
    UnspecializedFun{2}(f)
end
