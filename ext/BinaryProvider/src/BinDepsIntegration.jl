# This file contains the ingredients to create a PackageManager for BinDeps
import BinDeps: Binaries, can_use, package_available, bindir, libdir,
                generate_steps, LibraryDependency, provider, provides
import Base: show

mutable struct BP <: Binaries
    url::String
    hash::String
    prefix::Prefix
end

show(io::IO, p::BP) = write(io, "BinaryProvider for $(p.url)")

# We are cross-platform baby, and we never say no to a party
can_use(::Type{BP}) = true
package_available(p::BP) = true
libdir(p::BP, dep) = @static if Compat.Sys.iswindows()
    joinpath(p.prefix, "bin")
else
    joinpath(p.prefix, "lib")
end

# We provide our own overload of provides() for BP
macro BP_provides(url, hash, dep, opts...)
    return quote
        prefix = Prefix(joinpath(dirname(@__FILE__), "usr"))
        activate(prefix)
        return provides(BP, ($url, $hash, prefix), $(esc(dep)), $(opts...))
    end
end
provider(::Type{BP}, data; opts...) = BP(data...)

function generate_steps(dep::LibraryDependency, p::BP, opts)
    () -> begin
        install(p.url, p.hash; prefix=p.prefix, verbose=true)
    end
end
