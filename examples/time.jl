# This file is a part of Julia. License is MIT: http://julialang.org/license

module Time
export TimeDelta

import Base.show, Base.+, Base.-, Base.convert, Base.promote_rule

immutable TimeDelta{p}
    v::Int64
end

const PREFIXES = [
    "yocto", "zepto", "atto", "femto", "pico", "nano", "micro", "milli",
    "", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta",
]
const ZERO_INDEX = 9
const MAX_INDEX = 17

function show{p}(io::IO, x::TimeDelta{p})
    k = max(1,min(MAX_INDEX,fld(p,3)+ZERO_INDEX))
    r = p-3(k-ZERO_INDEX)
    prefix = PREFIXES[k]
    if r == 0
        s = x.v == 1 ? "" : "s"
        print(io, "$(x.v) $(prefix)second$s")
    elseif r > 0
        print(io, "$(x.v*10^r) $(prefix)seconds")
    else
        print(io, "$(x.v/10^-r) $(prefix)seconds")
    end
end

convert{p,q}(::Type{TimeDelta{p}}, x::TimeDelta{q}) =
    TimeDelta{p}(p <= q ? x.v*10^(q-p) : div(x.v,10^(p-q)))

promote_rule{p,q}(::Type{TimeDelta{p}}, ::Type{TimeDelta{q}}) = TimeDelta{min(p,q)}

-{p}(x::TimeDelta{p}) = TimeDelta{p}(-x.v)
+{p}(x::TimeDelta{p}, y::TimeDelta{p}) = TimeDelta{p}(x.v+y.v)
-{p}(x::TimeDelta{p}, y::TimeDelta{p}) = TimeDelta{p}(x.v-y.v)

+(x::TimeDelta, y::TimeDelta) = +(promote(x,y)...)
-(x::TimeDelta, y::TimeDelta) = -(promote(x,y)...)

end # module
