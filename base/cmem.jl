# This file is a part of Julia. License is MIT: https://julialang.org/license

# C NUL-terminated string pointers; these can be used in ccall
# instead of Ptr{Cchar} and Ptr{Cwchar_t}, respectively, to enforce
# a check for embedded NUL chars in the string (to avoid silent truncation).
if Int === Int64
    primitive type Cstring  64 end
    primitive type Cwstring 64 end
else
    primitive type Cstring  32 end
    primitive type Cwstring 32 end
end

"""
    free(addr::Ptr)

Call `free` from the C standard library. Only use this on memory obtained from [`malloc`](@ref), not
on pointers retrieved from other C libraries. [`Ptr`](@ref) objects obtained from C libraries should
be freed by the free functions defined in that library, to avoid assertion failures if
multiple `libc` libraries exist on the system.
"""
free(p::Ptr) = ccall(:free, Cvoid, (Ptr{Cvoid},), p)
free(p::Cstring) = free(convert(Ptr{UInt8}, p))
free(p::Cwstring) = free(convert(Ptr{Cwchar_t}, p))

"""
    malloc(size::Integer) -> Ptr{Cvoid}

Call `malloc` from the C standard library.
"""
malloc(size::Integer) = ccall(:malloc, Ptr{Cvoid}, (Csize_t,), size)

"""
    realloc(addr::Ptr, size::Integer) -> Ptr{Cvoid}

Call `realloc` from the C standard library.

See warning in the documentation for [`free`](@ref) regarding only using this on memory originally
obtained from [`malloc`](@ref).
"""
realloc(p::Ptr, size::Integer) = ccall(:realloc, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), p, size)

"""
    calloc(num::Integer, size::Integer) -> Ptr{Cvoid}

Call `calloc` from the C standard library.
"""
calloc(num::Integer, size::Integer) = ccall(:calloc, Ptr{Cvoid}, (Csize_t, Csize_t), num, size)

"""
    memcpy(dst::Ptr, src::Ptr, n::Integer) -> Ptr{Cvoid}

Call `memcpy` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memcpy` requires at least Julia 1.10.

"""
function memcpy(dst::Ptr, src::Ptr, n::Integer)
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, n)
end

"""
    memmove(dst::Ptr, src::Ptr, n::Integer) -> Ptr{Cvoid}

Call `memmove` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memmove` requires at least Julia 1.10.

"""
function memmove(dst::Ptr, src::Ptr, n::Integer)
    ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, n)
end

"""
    memset(dst::Ptr, val, n::Integer) -> Ptr{Cvoid}

Call `memset` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memset` requires at least Julia 1.10.

"""
function memset(p::Ptr, val, n::Integer)
    ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), p, val, n)
end

"""
    memcmp(a::Ptr, b::Ptr, n::Integer) -> Int

Call `memcmp` from the C standard library.

!!! compat "Julia 1.10"
    Support for `memcmp` requires at least Julia 1.9.

"""
function memcmp(a::Ptr, b::Ptr, n::Integer)
    ccall(:memcmp, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), a, b, n % Csize_t) % Int
end

