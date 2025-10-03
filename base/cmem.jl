# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    memcpy(dst::Ptr, src::Ptr, n::Integer)::Ptr{Cvoid}

Call `memcpy` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memcpy` requires at least Julia 1.10.

"""
function memcpy(dst::Ptr, src::Ptr, n::Integer)
    @_terminates_globally_meta
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, n)
end

"""
    memmove(dst::Ptr, src::Ptr, n::Integer)::Ptr{Cvoid}

Call `memmove` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memmove` requires at least Julia 1.10.

"""
function memmove(dst::Ptr, src::Ptr, n::Integer)
    @_terminates_globally_meta
    ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, n)
end

"""
    memset(dst::Ptr, val, n::Integer)::Ptr{Cvoid}

Call `memset` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memset` requires at least Julia 1.10.

"""
function memset(p::Ptr, val, n::Integer)
    @_terminates_globally_meta
    ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), p, val, n)
end

"""
    memcmp(a::Ptr, b::Ptr, n::Integer)::Int

Call `memcmp` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memcmp` requires at least Julia 1.9.

"""
function memcmp(a::Ptr, b::Ptr, n::Integer)
    @_terminates_globally_meta
    ccall(:memcmp, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), a, b, n % Csize_t) % Int
end
