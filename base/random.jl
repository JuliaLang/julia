# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.CoreRandom

CoreRandom defines a random number generator by wrapping the API provided by
the operating system.  It is an internal module used primary by `Base`.
Standard library `Random` re-exposes the implementations defined here for external
use.
"""
module CoreRandom

using Base: BitInteger

# Functions local to `CoreRandom` and separated from `Random`.
rand(::Type{T}) where {T} = rand(RandomDevice(), T)
rand!(A::Array) = rand!(RandomDevice(), A)

# Note that CoreRandom.RandomDevice != Random.RandomDevice
if Sys.iswindows()
    struct RandomDevice
        buffer::Vector{UInt128}

        RandomDevice() = new(Vector{UInt128}(undef, 1))
    end

    function rand(rd::RandomDevice, ::Type{T}) where {T<:Union{Bool, BitInteger}}
        rand!(rd, rd.buffer)
        @inbounds return rd.buffer[1] % T
    end
else # !windows
    struct RandomDevice
        unlimited::Bool

        RandomDevice(; unlimited::Bool=true) = new(unlimited)
    end

    rand(rd::RandomDevice, ::Type{T}) where {T<:BitInteger} = read(getfile(rd), T)
    rand(rd::RandomDevice, ::Type{Bool}) = read(getfile(rd), UInt8) % Bool

    function getfile(rd::RandomDevice)
        devrandom = rd.unlimited ? DEV_URANDOM : DEV_RANDOM
        # TODO: there is a data-race, this can leak up to nthreads() copies of the file descriptors,
        # so use a "thread-once" utility once available
        isassigned(devrandom) || (devrandom[] = open(rd.unlimited ? "/dev/urandom" : "/dev/random"))
        devrandom[]
    end

    const DEV_RANDOM  = Ref{IOStream}()
    const DEV_URANDOM = Ref{IOStream}()

end # os-test

# NOTE: this can't be put within the if-else block above
if Sys.iswindows()
    function rand!(rd::RandomDevice, A::Array)
        Base.windowserror("SystemFunction036 (RtlGenRandom)", 0 == ccall(
            (:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Cvoid}, UInt32),
            A, sizeof(A)))
        A
    end
else
    rand!(rd::RandomDevice, A::Array) = read!(getfile(rd), A)
end

end # CoreRandom
