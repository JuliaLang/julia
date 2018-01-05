# This file is a part of Julia. License is MIT: https://julialang.org/license

module FieldValues

using ...VersionWeights

export FieldValue, Field, validmax, secondmax

# FieldValue is a hierarchical numeric type with 5 levels.
# When summing two FieldValues, the levels are summed independently.
# When comparing them, lower levels take precedence.
# The levels are used as such:
#  l0 : for hard constraints (dependencies and requirements)
#  l1 : for favoring higher versions of the explicitly required
#       packages
#  l2 : for favoring higher versions of all other packages
#  l3 : for favoring uninstallation of non-needed packages
#
struct FieldValue
    l0::Int64
    l1::VersionWeight
    l2::VersionWeight
    l3::Int64
    FieldValue(l0::Integer = 0,
               l1::VersionWeight = zero(VersionWeight),
               l2::VersionWeight = zero(VersionWeight),
               l3::Integer = 0) = new(l0, l1, l2, l3)
end

# This isn't nice, but it's for debugging only anyway
function Base.show(io::IO, a::FieldValue)
    print(io, a.l0)
    a == FieldValue(a.l0) && return
    print(io, ".", a.l1)
    a == FieldValue(a.l0, a.l1) && return
    print(io, ".", a.l2)
    a == FieldValue(a.l0, a.l1, a.l2) && return
    print(io, ".", a.l3)
    return
end

const Field = Vector{FieldValue}

Base.zero(::Type{FieldValue}) = FieldValue()

Base.typemin(::Type{FieldValue}) = (x=typemin(Int64); y=typemin(VersionWeight); FieldValue(x, y, y, x))

Base.:-(a::FieldValue, b::FieldValue) = FieldValue(a.l0-b.l0, a.l1-b.l1, a.l2-b.l2, a.l3-b.l3)
Base.:+(a::FieldValue, b::FieldValue) = FieldValue(a.l0+b.l0, a.l1+b.l1, a.l2+b.l2, a.l3+b.l3)

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
    return false
end

Base.:(==)(a::FieldValue, b::FieldValue) =
    a.l0 == b.l0 && a.l1 == b.l1 && a.l2 == b.l2 && a.l3 == b.l3

Base.abs(a::FieldValue) = FieldValue(abs(a.l0), abs(a.l1), abs(a.l2), abs(a.l3))

Base.copy(a::FieldValue) = FieldValue(a.l0, copy(a.l1), copy(a.l2), a.l3)

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
function secondmax(f::Field, msk::BitVector = trues(length(f)))
    m = typemin(FieldValue)
    m2 = typemin(FieldValue)
    for i = 1:length(f)
        msk[i] || continue
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
