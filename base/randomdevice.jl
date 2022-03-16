# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file contains the minimal support of RandomDevice for Base's own usage.
# The actual RandomDevice type that makes use of this infrastructure is defined
# in the Random stdlib.

module DevRandomState
    if !Sys.iswindows()
        mutable struct FileRef
            @atomic file::Union{IOStream, Nothing}
        end
        const DEV_RANDOM  = FileRef(nothing)
        const DEV_URANDOM = FileRef(nothing)
    end
    function __init__()
        if !Sys.iswindows()
            @atomic DEV_RANDOM.file = nothing
            @atomic DEV_URANDOM.file = nothing
        end
    end
end

if Sys.iswindows()
    function RtlGenRandom!(A::Union{Array, Ref})
        Base.windowserror("SystemFunction036 (RtlGenRandom)", 0 == ccall(
            (:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Cvoid}, UInt32),
              A, sizeof(A)))
    end

    # Manually implemented to work without the Random machinery
    function _rand_uint()
        r = Ref{Cuint}()
        RtlGenRandom!(r)
        return r[]
    end
else # !windows
    function _get_dev_random_fd(unlimited::Bool)
        ref = unlimited ? DevRandomState.DEV_URANDOM : DevRandomState.DEV_RANDOM
        fd = ref.file
        if fd === nothing
            fd = open(unlimited ? "/dev/urandom" : "/dev/random")
            old, ok = @atomicreplace ref.file nothing => fd
            if !ok
                close(fd)
                fd = old::IOStream
            end
        end
        return fd
    end

    # Manually implemented to work without the Random machinery
    function _rand_uint()
        return read(_get_dev_random_fd(true), Cuint)
    end
end # os-test

function _ad_hoc_entropy()
    println(stderr,
        "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
    seed = reinterpret(UInt64, time())
    seed = hash(seed, getpid() % UInt)
    try
        seed = hash(seed, parse(UInt64,
                                read(pipeline(`ifconfig`, `sha1sum`), String)[1:40],
                                base = 16) % UInt)
    catch
    end
    return seed
end

function _make_uint_seed()
    try
        _rand_uint()
    catch
        return _ad_hoc_entropy() % Cuint
    end
end
