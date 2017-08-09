# This file is a part of Julia. License is MIT: https://julialang.org/license

module FieldValues

using ...VersionWeights

export FieldValue, Field, validmax, secondmax

# FieldValue is a hierarchical numeric type with 6 levels.
# When summing two FieldValues, the levels are summed independently.
# When comparing them, lower levels take precedence.
# The levels are used as such:
#  l0 : for hard constraints (dependencies and requirements)
#  l1 : for favoring higher versions of the explicitly required
#       packages
#  l2 : for favoring higher versions of all other packages
#  l3 : for favoring uninstallation of non-needed packages
#  l4 : for favoring dependants over dependencies
#  l5 : for symmetry-breaking random noise
#
struct FieldValue
    l0::Int
    l1::VersionWeight
    l2::VersionWeight
    l3::Int
    l4::Int
    l5::Int128
end
FieldValue(l0::Integer, l1::VersionWeight, l2::VersionWeight, l3::Integer, l4::Integer) = FieldValue(l0, l1, l2, l3, l4, Int128(0))
FieldValue(l0::Integer, l1::VersionWeight, l2::VersionWeight, l3::Integer) = FieldValue(l0, l1, l2, l3, 0)
FieldValue(l0::Integer, l1::VersionWeight, l2::VersionWeight) = FieldValue(l0, l1, l2, 0)
FieldValue(l0::Integer, l1::VersionWeight) = FieldValue(l0, l1, zero(VersionWeight))
FieldValue(l0::Integer) = FieldValue(l0, zero(VersionWeight))
FieldValue() = FieldValue(0)

const Field = Vector{FieldValue}

Base.zero(::Type{FieldValue}) = FieldValue()

Base.typemin(::Type{FieldValue}) = (x=typemin(Int); y=typemin(VersionWeight); FieldValue(x, y, y, x, x, typemin(Int128)))

Base.:-(a::FieldValue, b::FieldValue) = FieldValue(a.l0-b.l0, a.l1-b.l1, a.l2-b.l2, a.l3-b.l3, a.l4-b.l4, a.l5-b.l5)
Base.:+(a::FieldValue, b::FieldValue) = FieldValue(a.l0+b.l0, a.l1+b.l1, a.l2+b.l2, a.l3+b.l3, a.l4+b.l4, a.l5+b.l5)

function Base.isless(a::FieldValue, b::FieldValue)
    a.l0 < b.l0 && return true
    a.l0 > b.l0 && return false
    c = cmp(a.l1, b.l1)
    c < 0 && return true
    c > 0 && return false
    c = cmp(a.l2, b.l2)
    c < 0 && return true
    c > 0 && return false
    a.l3 < b.l3 && return true
    a.l3 > b.l3 && return false
    a.l4 < b.l4 && return true
    a.l4 > b.l4 && return false
    a.l5 < b.l5 && return true
    return false
end

Base.:(==)(a::FieldValue, b::FieldValue) =
    a.l0 == b.l0 && a.l1 == b.l1 && a.l2 == b.l2 && a.l3 == b.l3 && a.l4 == b.l4 && a.l5 == b.l5

Base.abs(a::FieldValue) = FieldValue(abs(a.l0), abs(a.l1), abs(a.l2), abs(a.l3), abs(a.l4), abs(a.l5))

Base.copy(a::FieldValue) = FieldValue(a.l0, copy(a.l1), copy(a.l2), a.l3, a.l4, a.l5)

function Base.unsafe_copy!(dest::Field, doffs, src::Field, soffs, n)
    for i = 1:n
        dest[doffs+i-1] = copy(src[soffs+i-1])
    end
    return dest
end

# if the maximum field has l0 < 0, it means that
# some hard constraint is being violated
validmax(a::FieldValue) = a.l0 >= 0

# like usual indmax, but favors the highest indices
# in case of a tie
function Base.indmax(f::Field)
    m = typemin(FieldValue)
    mi = 0
    for j = length(f):-1:1
        if f[j] > m
            m = f[j]
            mi = j
        end
    end
    @assert mi != 0
    return mi
end

# secondmax returns the (normalized) value of the second maximum in a
# field. It's used to determine the most polarized field.
function secondmax(f::Field)
    m = typemin(FieldValue)
    m2 = typemin(FieldValue)
    for i = 1:length(f)
        a = f[i]
        if a > m
            m2 = m
            m = a
        elseif a > m2
            m2 = a
        end
    end
    return m2 - m
end

end
