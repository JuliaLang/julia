# This file is a part of Julia. License is MIT: http://julialang.org/license

# essential type definitions for interacting with C code
# (platform- or OS-dependent types are defined in c.jl)

"""
    Cuchar

Equivalent to the native `unsigned char` c-type (`UInt8`).
"""
typealias Cuchar UInt8


"""
    Cshort

Equivalent to the native `signed short` c-type (`Int16`).
"""
typealias Cshort Int16


"""
    Cushort

Equivalent to the native `unsigned short` c-type (`UInt16`).
"""
typealias Cushort UInt16


"""
    Cint

Equivalent to the native `signed int` c-type (`Int32`).
"""
typealias Cint Int32


"""
    Cuint

Equivalent to the native `unsigned int` c-type (`UInt32`).
"""
typealias Cuint UInt32


"""
    Cptrdiff_t

Equivalent to the native `ptrdiff_t` c-type (`Int`).
"""
typealias Cptrdiff_t Int


"""
    Csize_t

Equivalent to the native `size_t` c-type (`UInt`).
"""
typealias Csize_t UInt


"""
    Cssize_t

Equivalent to the native `ssize_t` c-type.
"""
typealias Cssize_t Int


"""
    Cintmax_t

Equivalent to the native `intmax_t` c-type (`Int64`).
"""
typealias Cintmax_t Int64


"""
    Cuintmax_t

Equivalent to the native `uintmax_t` c-type (`UInt64`).
"""
typealias Cuintmax_t UInt64


"""
    Clonglong

Equivalent to the native `signed long long` c-type (`Int64`).
"""
typealias Clonglong Int64


"""
    Culonglong

Equivalent to the native `unsigned long long` c-type (`UInt64`).
"""
typealias Culonglong UInt64


"""
    Cfloat

Equivalent to the native `float` c-type (`Float32`).
"""
typealias Cfloat Float32


"""
    Cdouble

Equivalent to the native `double` c-type (`Float64`).
"""
typealias Cdouble Float64
