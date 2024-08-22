# This file is a part of Julia. License is MIT: https://julialang.org/license

function angle(@nospecialize n::TypeDomainInteger)
    if n isa NegativeInteger
        π
    else
        zero(TypeDomainInteger)
    end
end

function exp(@nospecialize n::Union{
    typeof(NonnegativeInteger(0)),
    typeof(NonnegativeInteger(1)),
})
    if n isa PositiveIntegerUpperBound
        ℯ
    else
        NonnegativeInteger(1)
    end
end

const ZeroOrPi = Union{
    typeof(NonnegativeInteger(0)),
    typeof(π),
}

function sin(@nospecialize x::ZeroOrPi)
    NonnegativeInteger(0)
end
function tan(@nospecialize x::ZeroOrPi)
    NonnegativeInteger(0)
end
function cos(@nospecialize x::ZeroOrPi)
    o = one(NonnegativeInteger)
    if iszero(x)
        o
    else
        -o
    end
end
function Math.sec(@nospecialize x::ZeroOrPi)
    o = one(NonnegativeInteger)
    if iszero(x)
        o
    else
        -o
    end
end

function asin(x::typeof(NonnegativeInteger(0)))
    x
end
function atan(x::typeof(NonnegativeInteger(0)))
    x
end
function acos(@nospecialize x::Union{
    typeof(TypeDomainInteger(-1)),
    typeof(TypeDomainInteger( 1)),
})
    if x isa NegativeInteger
        π
    else
        zero(NonnegativeInteger)
    end
end
function Math.asec(@nospecialize x::Union{
    typeof(TypeDomainInteger(-1)),
    typeof(TypeDomainInteger( 1)),
})
    if x isa NegativeInteger
        π
    else
        zero(NonnegativeInteger)
    end
end
