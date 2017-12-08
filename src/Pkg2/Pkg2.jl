module Pkg2

struct PkgError <: Exception
    msg::AbstractString
    ex::Nullable{Exception}
end
PkgError(msg::AbstractString) = PkgError(msg, Nullable{Exception}())
function Base.showerror(io::IO, pkgerr::PkgError)
    print(io, pkgerr.msg)
    if !isnull(pkgerr.ex)
        pkgex = get(pkgerr.ex)
        if isa(pkgex, CompositeException)
            for cex in pkgex
                print(io, "\n=> ")
                showerror(io, cex)
            end
        else
            print(io, "\n")
            showerror(io, pkgex)
        end
    end
end

include("query.jl")
include("resolve.jl")

end # module
