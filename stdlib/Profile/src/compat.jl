# Compatibility shims for old users who aren't used to the `Time` and `Memory` sub-modules
function init(args...; kwargs...)
    @warn("Profile.init() is deprecated, use Profile.Time.init() or Profile.Memory.init() directly")
    Time.init(args...; kwargs...)
end
function print(args...; kwargs...)
    @warn("Profile.print() is deprecated, use Profile.Time.print() or Profile.Memory.print() directly")
    Time.print(args...; kwargs...)
end