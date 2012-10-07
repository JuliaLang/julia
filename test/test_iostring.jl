# Demonstration of QuickCheck, and simple round-trip tests for IOString
require("quickcheck.jl")
require("iostring.jl")

import QuickCheck.*

typs = {
        Uint8; Int8;
        Uint16; Int16;
        Uint32; Int32;
        Uint64; Int64;
        }

for typ in typs
    T = :($typ)
    @eval property() do (x::$T)
        io = IOString()
        write(io, x)
        seek(io, 0)
        x == read(io, $T)
    end

    for ndims in 1:3
        @eval property() do (x::Array{$T,ndims})
            io = IOString()
            write(io, x)
            seek(io, 0)
            x == read(io, $T, size(x))
        end
    end
end

if !isinteractive()
    exit()
end