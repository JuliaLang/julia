###### Functors ######

# Note that functors are merely used as internal machinery to enhance code
# reuse and improve performance of map/reduce.
# They are not exported.
# When function arguments can be inlined, the use of functors can be removed.

abstract Func{N}

immutable IdFun <: Func{1} end
call(::IdFun, x) = x

immutable AbsFun <: Func{1} end
call(::AbsFun, x) = abs(x)

immutable Abs2Fun <: Func{1} end
call(::Abs2Fun, x) = abs2(x)

immutable ExpFun <: Func{1} end
call(::ExpFun, x) = exp(x)

immutable LogFun <: Func{1} end
call(::LogFun, x) = log(x)

immutable AndFun <: Func{2} end
call(::AndFun, x, y) = x & y

immutable OrFun <: Func{2} end
call(::OrFun, x, y) = x | y

immutable AddFun <: Func{2} end
call(::AddFun, x, y) = x + y

immutable MulFun <: Func{2} end
call(::MulFun, x, y) = x * y

immutable MaxFun <: Func{2} end
call(::MaxFun, x, y) = scalarmax(x,y)

immutable MinFun <: Func{2} end
call(::MinFun, x, y) = scalarmin(x, y)

immutable LessFun <: Func{2} end
call(::LessFun, x, y) = x < y

immutable MoreFun <: Func{2} end
call(::MoreFun, x, y) = x > y

# a fallback unspecialized functor that allows code using functors to not care
# whether they were able to specialize on the function value or not
immutable UnspecializedFun{N,T<:Callable} <: Func{N}
    f::T
end
call{N,T}(::Type{UnspecializedFun{N}}, f::T) = UnspecializedFun{N,T}(f)
call(f::UnspecializedFun{1}, x) = f.f(x)
call(f::UnspecializedFun{2}, x, y) = f.f(x,y)


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
call(::BitFunctorUnary{true,  true},  p) = p | ~p # Must work for bits and ints
call(::BitFunctorUnary{false, false}, p) = p & ~p # LLVM figures them out nicely
call(::BitFunctorUnary{true,  false}, p) =  p
call(::BitFunctorUnary{false, true},  p) = ~p

immutable BitFunctorBinary{TT,TF,FT,FF} <: Func{2} end
call(::BitFunctorBinary{true,  true,  true,  true }, p, q) = p | ~p
call(::BitFunctorBinary{true,  true,  true,  false}, p, q) = p | q
call(::BitFunctorBinary{true,  true,  false, true }, p, q) = p | ~q
call(::BitFunctorBinary{true,  true,  false, false}, p, q) = p
call(::BitFunctorBinary{true,  false, true,  true }, p, q) = ~p | q
call(::BitFunctorBinary{true,  false, true,  false}, p, q) = q
call(::BitFunctorBinary{true,  false, false, true }, p, q) = ~(p $ q)
call(::BitFunctorBinary{true,  false, false, false}, p, q) = p & q

call(::BitFunctorBinary{false, false, false, false}, p, q) = p & ~p
call(::BitFunctorBinary{false, false, false, true }, p, q) = ~(p | q)
call(::BitFunctorBinary{false, false, true,  false}, p, q) = ~p & q
call(::BitFunctorBinary{false, false, true,  true }, p, q) = ~p
call(::BitFunctorBinary{false, true,  false, false}, p, q) = p & ~q
call(::BitFunctorBinary{false, true,  false, true }, p, q) = ~q
call(::BitFunctorBinary{false, true,  true,  false}, p, q) = p $ q
call(::BitFunctorBinary{false, true,  true,  true }, p, q) = ~(p & q)

# Specializations by value

function specialized_unary(f::Callable)
    is(f, identity) ? IdFun()   :
    is(f, abs)      ? AbsFun()  :
    is(f, abs2)     ? Abs2Fun() :
    is(f, exp)      ? ExpFun()  :
    is(f, log)      ? LogFun()  :
    UnspecializedFun{1}(f)
end
function specialized_binary(f::Callable)
    is(f, +) ? AddFun() :
    is(f, *) ? MulFun() :
    is(f, &) ? AndFun() :
    is(f, |) ? OrFun()  :
    UnspecializedFun{2}(f)
end

function specialized_bitwise_unary(f::Callable)
    is(f, identity)     ? BitFunctorUnary{true,  false}() :
    is(f, !) | is(f, ~) ? BitFunctorUnary{false, true }() :
    is(f, one)          ? BitFunctorUnary{true,  true }() :
    is(f, zero)         ? BitFunctorUnary{false, false}() :
    UnspecializedFun{1}(f)
end
function specialized_bitwise_binary(f::Callable)
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
