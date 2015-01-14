###### Functors ######

# Note that functors are merely used as internal machinery to enhance code
# reuse and improve performance of map/reduce.
# They are not exported.
# When function arguments can be inlined, the use of functors can be removed.

abstract Func{N}

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

immutable MaxFun <: Func{2} end
call(::MaxFun, x, y) = scalarmax(x,y)

immutable MinFun <: Func{2} end
call(::MinFun, x, y) = scalarmin(x, y)

# a fallback unspecialized functor that allows code using functors to not care
# whether they were able to specialize on the function value or not
immutable UnspecializedFun{N,T<:Callable} <: Func{N}
    f::T
end
call{N,T}(::Type{UnspecializedFun{N}}, f::T) = UnspecializedFun{N,T}(f)
call(f::UnspecializedFun{1}, x) = f.f(x)
call(f::UnspecializedFun{2}, x, y) = f.f(x,y)


#### Bitwise operators ####

# A BitFunc is a function that behaves in the same bit-wise manner when applied
# to individual bits as well as integers, allowing them to be used in BitArrays
abstract BitFunc{N} <: Func{N}

# Note that there are 16 possible pure two-argument logical functions,
# of which six are trivial and two don't exist as a single function in Base:
##############################################################################
##  p = TTFF                          ##  p = TTFF                          ##
##  q = TFTF    function  name        ##  q = TFTF    function  name        ##
##  --------------------------------  ##  --------------------------------- ##
##      TTTT    (true)    -           ##      FFFF    (false)   -           ##
##      TTTF    |, max    or          ##      FFFT    ???       (nor)       ##
##      TTFT    >=, ^     ???         ##      FFTF    <         ???         ##
##      TTFF    (A)       -           ##      FFTT    (~A)      -           ##
##      TFTT    <=        implies     ##      FTFF    >         notimplies  ##
##      TFTF    (B)       -           ##      FTFT    (~B)      -           ##
##      TFFT    ==        xnor        ##      FTTF    $, !=     xor         ##
##      TFFF    &, *, min and         ##      FTTT    ???       (nand)      ##
##############################################################################

immutable IdFun <: BitFunc{1} end
call(::IdFun, x) = x

immutable NotFun <: BitFunc{1} end
call(::NotFun, x) = ~x

immutable AndFun <: BitFunc{2} end
call(::AndFun, x, y) = x & y

immutable OrFun <: BitFunc{2} end
call(::OrFun, x, y) =  x | y

immutable POrNotQFun <: BitFunc{2} end
call(::POrNotQFun, x, y) = x | ~y

immutable ImpliesFun <: BitFunc{2} end
call(::ImpliesFun, x, y) = ~x | y

immutable XNorFun <: BitFunc{2} end
call(::XNorFun, x, y) = ~(x $ y)

immutable NotPAndQFun <: BitFunc{2} end
call(::NotPAndQFun, x, y) = ~x & y

immutable NotImpliesFun <: BitFunc{2} end
call(::NotImpliesFun, x, y) = x & ~y

immutable XOrFun <: BitFunc{2} end
call(::XOrFun, x, y) = x $ y

# Specializations by value

function specialized_unary(f::Callable)
    is(f, identity) ? IdFun()   :
    is(f, ~)        ? NotFun()  :
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
    is(f, !) | is(f, ~) ? NotFun() :
    is(f, identity)     ? IdFun()  :
    UnspecializedFun{1}(f)
end
function specialized_bitwise_binary(f::Callable)
    is(f, &)  | is(f, *) | is(f, min) ? AndFun()        :
    is(f, |)  | is(f, max)            ? OrFun()         :
    is(f, $)  | is(f, !=)             ? XOrFun()        :
    is(f, >=) | is(f, ^)              ? POrNotQFun()    :
    is(f, <=)                         ? ImpliesFun()    :
    is(f, ==)                         ? XNorFun()       :
    is(f, <)                          ? NotPAndQFun()   :
    is(f, >)                          ? NotImpliesFun() :
    UnspecializedFun{2}(f)
end
