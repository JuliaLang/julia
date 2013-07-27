module VersionWeights

export VersionWeight

# The numeric type used to determine how the different
# versions of a package should be weighed
immutable VersionWeight
    major::Int
    minor::Int
    patch::Int
    uninstall::Int
end
VersionWeight(major::Int,minor::Int,patch::Int) = VersionWeight(major, minor, patch, 0)
VersionWeight(major::Int,minor::Int) = VersionWeight(major, minor, 0)
VersionWeight(major::Int) = VersionWeight(major, 0)
VersionWeight() = VersionWeight(0)

VersionWeight(vn::VersionNumber, uninstall=false) = VersionWeight(vn.major, vn.minor, vn.patch, int(uninstall))

Base.zero(::Type{VersionWeight}) = VersionWeight()

Base.typemin(::Type{VersionWeight}) = (x=typemin(Int); VersionWeight(x,x,x,x))
Base.typemax(::Type{VersionWeight}) = (x=typemax(Int); VersionWeight(x,x,x,x))

Base.(:-)(a::VersionWeight, b::VersionWeight) = VersionWeight(a.major-b.major, a.minor-b.minor, a.patch-b.patch, a.uninstall-b.uninstall)
Base.(:+)(a::VersionWeight, b::VersionWeight) = VersionWeight(a.major+b.major, a.minor+b.minor, a.patch+b.patch, a.uninstall+b.uninstall)

Base.(:-)(a::VersionWeight) = VersionWeight(-a.major, -a.minor, -a.patch, -a.uninstall)

function Base.isless(a::VersionWeight, b::VersionWeight)
    a.major < b.major && return true
    a.major > b.major && return false
    a.minor < b.minor && return true
    a.minor > b.minor && return false
    a.patch < b.patch && return true
    a.patch > b.patch && return false
    a.uninstall < b.uninstall && return true
    return false
end

Base.abs(a::VersionWeight) = VersionWeight(abs(a.major), abs(a.minor), abs(a.patch), abs(a.uninstall))

end
