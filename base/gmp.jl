module GMP

export BigInt, BigFloat

import
    Base.*,
    Base.+,
    Base.-,
    Base./,
    Base.<,
    Base.<<,
    Base.>>,
    Base.<=,
    Base.==,
    Base.>,
    Base.>=,
    Base.^,
    Base.binomial,
    Base.cmp,
    Base.convert,
    Base.div,
    Base.factorial,
    Base.gcd,
    Base.gcdx,
    Base.isinf,
    Base.isnan,
    Base.promote_rule,
    Base.rem,
    Base.show,
    Base.showcompact,
    Base.sqrt,
    Base.string

include("bigint.jl")
include("bigfloat.jl")

end # module
