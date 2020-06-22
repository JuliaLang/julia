# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Interface to BLAS subroutines.
"""
module BLAS

import ..axpy!, ..axpby!
import Base: copyto!
using Base: require_one_based_indexing

export
# Level 1
    asum,
    axpy!,
    axpby!,
    blascopy!,
    dotc,
    dotu,
    rot!,
    scal!,
    scal,
    nrm2,
    iamax,
# Level 2
    gbmv!,
    gbmv,
    gemv!,
    gemv,
    hemv!,
    hemv,
    hpmv!,
    sbmv!,
    sbmv,
    spmv!,
    symv!,
    symv,
    trsv!,
    trsv,
    trmv!,
    trmv,
    ger!,
    syr!,
    her!,
# Level 3
    herk!,
    herk,
    her2k!,
    her2k,
    gemm!,
    gemm,
    symm!,
    symm,
    hemm!,
    hemm,
    syrk!,
    syrk,
    syr2k!,
    syr2k,
    trmm!,
    trmm,
    trsm!,
    trsm


const libblas = Base.libblas_name
const liblapack = Base.liblapack_name

import LinearAlgebra
import LinearAlgebra: BlasReal, BlasComplex, BlasFloat, BlasInt, DimensionMismatch, checksquare, stride1, chkstride1, axpy!

import Libdl

# utility routines
let lib = C_NULL
global function determine_vendor()
    if lib == C_NULL
        lib = something(Libdl.dlopen(libblas; throw_error=false), C_NULL)
    end
    vend = :unknown
    if lib != C_NULL
        if Libdl.dlsym(lib, :openblas_set_num_threads; throw_error=false) !== nothing
            vend = :openblas
        elseif Libdl.dlsym(lib, :openblas_set_num_threads64_; throw_error=false) !== nothing
            vend = :openblas64
        elseif Libdl.dlsym(lib, :MKL_Set_Num_Threads; throw_error=false) !== nothing
            vend = :mkl
        end
    end
    return vend
end
end

const _vendor = determine_vendor()
vendor() = _vendor

if vendor() === :openblas64
    macro blasfunc(x)
        return Expr(:quote, Symbol(x, "64_"))
    end
else
    macro blasfunc(x)
        return Expr(:quote, x)
    end
end

openblas_get_config() = strip(unsafe_string(ccall((@blasfunc(openblas_get_config), libblas), Ptr{UInt8}, () )))

function guess_vendor()
    # like determine_vendor, but guesses blas in some cases
    # where determine_vendor returns :unknown
    ret = vendor()
    if Sys.isapple() && (ret == :unknown )
        ret = :osxblas
    end
    ret
end


"""
    set_num_threads(n::Integer)
    set_num_threads(::Nothing)

Set the number of threads the BLAS library should use equal to `n::Integer`.
If `nothing` is passed to this function, julia tries to figure out the optimial number of threads.
The exact heuristic is an implementation detail.

On exotic variants of `BLAS` this function can fail.
"""
function set_num_threads(n::Integer, _blas=guess_vendor())::Nothing
    if _blas === :openblas || _blas == :openblas64
        return ccall((@blasfunc(openblas_set_num_threads), libblas), Cvoid, (Cint,), n)
    elseif _blas === :mkl
        # MKL may let us set the number of threads in several ways
        return ccall((:MKL_Set_Num_Threads, libblas), Cvoid, (Cint,), n)
    elseif _blas === :osxblas
        # OSX BLAS looks at an environment variable
        ENV["VECLIB_MAXIMUM_THREADS"] = n
    else
        @assert _blas === :unknown
        @warn "Failed to set number of BLAS threads." maxlog=1
    end
    return nothing
end

_tryparse_env_int(key) = tryparse(Int, get(ENV, key, ""))

function set_num_threads(::Nothing, _blas=guess_vendor())
    n = something(
        _tryparse_env_int("OPENBLAS_NUM_THREADS"),
        _tryparse_env_int("OMP_NUM_THREADS"),
        max(1, Sys.CPU_THREADS ÷ 2),
    )
    set_num_threads(n, _blas)
end

"""
    get_num_threads()

Get the number of threads the BLAS library is using.

On exotic variants of `BLAS` this function can fail, which is indicated by returning `nothing`.
"""
function get_num_threads(_blas=guess_vendor())::Union{Int, Nothing}
    if _blas === :openblas || _blas === :openblas64
        return Int(ccall((@blasfunc(openblas_get_num_threads), libblas), Cint, ()))
    elseif _blas === :mkl
        return Int(ccall((:mkl_get_max_threads, libblas), Cint, ()))
    elseif _blas === :osxblas
        key = "VECLIB_MAXIMUM_THREADS"
        nt = _tryparse_env_int(key)
        if nt === nothing
            @warn "Failed to read environment variable $key" maxlog=1
        else
            return nt
        end
    else
        @assert _blas === :unknown
    end
    @warn "Could not get number of BLAS threads. Returning `nothing` instead." maxlog=1
    return nothing
end

const _testmat = [1.0 0.0; 0.0 -1.0]
function check()
    blas = vendor()
    if blas === :openblas || blas === :openblas64
        openblas_config = openblas_get_config()
        openblas64 = occursin(r".*USE64BITINT.*", openblas_config)
        if Base.USE_BLAS64 != openblas64
            if !openblas64
                @error """
                    OpenBLAS was not built with 64bit integer support.
                    You're seeing this error because Julia was built with USE_BLAS64=1.
                    Please rebuild Julia with USE_BLAS64=0"""
            else
                @error """
                    Julia was not built with support for OpenBLAS with 64bit integer support.
                    You're seeing this error because Julia was built with USE_BLAS64=0.
                    Please rebuild Julia with USE_BLAS64=1"""
            end
            println("Quitting.")
            exit()
        end
    elseif blas === :mkl
        if Base.USE_BLAS64
            ENV["MKL_INTERFACE_LAYER"] = "ILP64"
        end
    end

    #
    # Check if BlasInt is the expected bitsize, by triggering an error
    #
    (_, info) = LinearAlgebra.LAPACK.potrf!('U', _testmat)
    if info != 2 # mangled info code
        if info == 2^33
            error("BLAS and LAPACK are compiled with 32-bit integer support, but Julia expects 64-bit integers. Please build Julia with USE_BLAS64=0.")
        elseif info == 0
            error("BLAS and LAPACK are compiled with 64-bit integer support but Julia expects 32-bit integers. Please build Julia with USE_BLAS64=1.")
        else
            error("The LAPACK library produced an undefined error code. Please verify the installation of BLAS and LAPACK.")
        end
    end

end


# Level 1
## copy

"""
    blascopy!(n, X, incx, Y, incy)

Copy `n` elements of array `X` with stride `incx` to array `Y` with stride `incy`. Returns `Y`.
"""
function blascopy! end

for (fname, elty) in ((:dcopy_,:Float64),
                      (:scopy_,:Float32),
                      (:zcopy_,:ComplexF64),
                      (:ccopy_,:ComplexF32))
    @eval begin
        # SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
        function blascopy!(n::Integer, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},AbstractArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 n, DX, incx, DY, incy)
            DY
        end
    end
end


## rot

"""
    rot!(n, X, incx, Y, incy, c, s)

Overwrite `X` with `c*X + s*Y` and `Y` with `-conj(s)*X + c*Y` for the first `n` elements of array `X` with stride `incx` and
first `n` elements of array `Y` with stride `incy`. Returns `X` and `Y`.

!!! compat "Julia 1.5"
    `rot!` requires at least Julia 1.5.
"""
function rot! end

for (fname, elty, cty, sty, lib) in ((:drot_, :Float64, :Float64, :Float64, libblas),
                                     (:srot_, :Float32, :Float32, :Float32, libblas),
                                     (:zdrot_, :ComplexF64, :Float64, :Float64, libblas),
                                     (:csrot_, :ComplexF32, :Float32, :Float32, libblas),
                                     (:zrot_, :ComplexF64, :Float64, :ComplexF64, liblapack),
                                     (:crot_, :ComplexF32, :Float32, :ComplexF32, liblapack))
    @eval begin
        # SUBROUTINE DROT(N,DX,INCX,DY,INCY,C,S)
        function rot!(n::Integer, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},AbstractArray{$elty}}, incy::Integer, C::$cty, S::$sty)
            ccall((@blasfunc($fname), $lib), Cvoid,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ref{$cty}, Ref{$sty}),
                 n, DX, incx, DY, incy, C, S)
            DX, DY
        end
    end
end

## scal

"""
    scal!(n, a, X, incx)

Overwrite `X` with `a*X` for the first `n` elements of array `X` with stride `incx`. Returns `X`.
"""
function scal! end

"""
    scal(n, a, X, incx)

Return `X` scaled by `a` for the first `n` elements of array `X` with stride `incx`.
"""
function scal end

for (fname, elty) in ((:dscal_,:Float64),
                      (:sscal_,:Float32),
                      (:zscal_,:ComplexF64),
                      (:cscal_,:ComplexF32))
    @eval begin
        # SUBROUTINE DSCAL(N,DA,DX,INCX)
        function scal!(n::Integer, DA::$elty, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), Cvoid,
                  (Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                  n, DA, DX, incx)
            DX
        end
    end
end
scal(n, DA, DX, incx) = scal!(n, DA, copy(DX), incx)

## dot

"""
    dot(n, X, incx, Y, incy)

Dot product of two vectors consisting of `n` elements of array `X` with stride `incx` and
`n` elements of array `Y` with stride `incy`.

# Examples
```jldoctest
julia> BLAS.dot(10, fill(1.0, 10), 1, fill(1.0, 20), 2)
10.0
```
"""
function dot end

"""
    dotc(n, X, incx, U, incy)

Dot function for two complex vectors, consisting of `n` elements of array `X`
with stride `incx` and `n` elements of array `U` with stride `incy`,
conjugating the first vector.

# Examples
```jldoctest
julia> BLAS.dotc(10, fill(1.0im, 10), 1, fill(1.0+im, 20), 2)
10.0 - 10.0im
```
"""
function dotc end

"""
    dotu(n, X, incx, Y, incy)

Dot function for two complex vectors consisting of `n` elements of array `X`
with stride `incx` and `n` elements of array `Y` with stride `incy`.

# Examples
```jldoctest
julia> BLAS.dotu(10, fill(1.0im, 10), 1, fill(1.0+im, 20), 2)
-10.0 + 10.0im
```
"""
function dotu end

for (fname, elty) in ((:ddot_,:Float64),
                      (:sdot_,:Float32))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dot(n::Integer, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},AbstractArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), $elty,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 n, DX, incx, DY, incy)
        end
    end
end
for (fname, elty) in ((:cblas_zdotc_sub,:ComplexF64),
                      (:cblas_cdotc_sub,:ComplexF32))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dotc(n::Integer, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},AbstractArray{$elty}}, incy::Integer)
            result = Ref{$elty}()
            ccall((@blasfunc($fname), libblas), Cvoid,
                (BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}),
                 n, DX, incx, DY, incy, result)
            result[]
        end
    end
end
for (fname, elty) in ((:cblas_zdotu_sub,:ComplexF64),
                      (:cblas_cdotu_sub,:ComplexF32))
    @eval begin
                #       DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,INCY,N
                # *     ..
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION DX(*),DY(*)
        function dotu(n::Integer, DX::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer, DY::Union{Ptr{$elty},AbstractArray{$elty}}, incy::Integer)
            result = Ref{$elty}()
            ccall((@blasfunc($fname), libblas), Cvoid,
                (BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}, BlasInt, Ptr{$elty}),
                 n, DX, incx, DY, incy, result)
            result[]
        end
    end
end

function dot(DX::Union{DenseArray{T},AbstractVector{T}}, DY::Union{DenseArray{T},AbstractVector{T}}) where T<:BlasReal
    require_one_based_indexing(DX, DY)
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    return dot(n, DX, stride(DX, 1), DY, stride(DY, 1))
end
function dotc(DX::Union{DenseArray{T},AbstractVector{T}}, DY::Union{DenseArray{T},AbstractVector{T}}) where T<:BlasComplex
    require_one_based_indexing(DX, DY)
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    return dotc(n, DX, stride(DX, 1), DY, stride(DY, 1))
end
function dotu(DX::Union{DenseArray{T},AbstractVector{T}}, DY::Union{DenseArray{T},AbstractVector{T}}) where T<:BlasComplex
    require_one_based_indexing(DX, DY)
    n = length(DX)
    if n != length(DY)
        throw(DimensionMismatch("dot product arguments have lengths $(length(DX)) and $(length(DY))"))
    end
    return dotu(n, DX, stride(DX, 1), DY, stride(DY, 1))
end

## nrm2

"""
    nrm2(n, X, incx)

2-norm of a vector consisting of `n` elements of array `X` with stride `incx`.

# Examples
```jldoctest
julia> BLAS.nrm2(4, fill(1.0, 8), 2)
2.0

julia> BLAS.nrm2(1, fill(1.0, 8), 2)
1.0
```
"""
function nrm2 end

for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:ComplexF64,:Float64),
                                (:scnrm2_,:ComplexF32,:Float32))
    @eval begin
        # SUBROUTINE DNRM2(N,X,INCX)
        function nrm2(n::Integer, X::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), $ret_type,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 n, X, incx)
        end
    end
end
nrm2(x::Union{AbstractVector,DenseArray}) = nrm2(length(x), x, stride1(x))

## asum

"""
    asum(n, X, incx)

Sum of the absolute values of the first `n` elements of array `X` with stride `incx`.

# Examples
```jldoctest
julia> BLAS.asum(5, fill(1.0im, 10), 2)
5.0

julia> BLAS.asum(2, fill(1.0im, 10), 5)
2.0
```
"""
function asum end

for (fname, elty, ret_type) in ((:dasum_,:Float64,:Float64),
                                (:sasum_,:Float32,:Float32),
                                (:dzasum_,:ComplexF64,:Float64),
                                (:scasum_,:ComplexF32,:Float32))
    @eval begin
        # SUBROUTINE ASUM(N, X, INCX)
        function asum(n::Integer, X::Union{Ptr{$elty},AbstractArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas), $ret_type,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 n, X, incx)
        end
    end
end
asum(x::Union{AbstractVector,DenseArray}) = asum(length(x), x, stride1(x))

## axpy

"""
    axpy!(a, X, Y)

Overwrite `Y` with `X*a + Y`, where `a` is a scalar. Return `Y`.

# Examples
```jldoctest
julia> x = [1; 2; 3];

julia> y = [4; 5; 6];

julia> BLAS.axpy!(2, x, y)
3-element Array{Int64,1}:
  6
  9
 12
```
"""
function axpy! end

for (fname, elty) in ((:daxpy_,:Float64),
                      (:saxpy_,:Float32),
                      (:zaxpy_,:ComplexF64),
                      (:caxpy_,:ComplexF32))
    @eval begin
                # SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
                # DY <- DA*DX + DY
                #*     .. Scalar Arguments ..
                #      DOUBLE PRECISION DA
                #      INTEGER INCX,INCY,N
                #*     .. Array Arguments ..
                #      DOUBLE PRECISION DX(*),DY(*)
        function axpy!(n::Integer, alpha::($elty), dx::Union{Ptr{$elty}, AbstractArray{$elty}}, incx::Integer, dy::Union{Ptr{$elty}, AbstractArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 n, alpha, dx, incx, dy, incy)
            dy
        end
    end
end
function axpy!(alpha::Number, x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}}) where T<:BlasFloat
    if length(x) != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    return axpy!(length(x), convert(T,alpha), x, stride(x, 1), y, stride(y, 1))
end

function axpy!(alpha::Number, x::Array{T}, rx::Union{UnitRange{Ti},AbstractRange{Ti}},
               y::Array{T}, ry::Union{UnitRange{Ti},AbstractRange{Ti}}) where {T<:BlasFloat,Ti<:Integer}
    if length(rx) != length(ry)
        throw(DimensionMismatch("ranges of differing lengths"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(ArgumentError("range out of bounds for x, of length $(length(x))"))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(ArgumentError("range out of bounds for y, of length $(length(y))"))
    end
    GC.@preserve x y axpy!(
        length(rx),
        convert(T, alpha),
        pointer(x) + (first(rx) - 1)*sizeof(T),
        step(rx),
        pointer(y) + (first(ry) - 1)*sizeof(T),
        step(ry))

    return y
end

"""
    axpby!(a, X, b, Y)

Overwrite `Y` with `X*a + Y*b`, where `a` and `b` are scalars. Return `Y`.

# Examples
```jldoctest
julia> x = [1., 2, 3];

julia> y = [4., 5, 6];

julia> BLAS.axpby!(2., x, 3., y)
3-element Array{Float64,1}:
 14.0
 19.0
 24.0
```
"""
function axpby! end

for (fname, elty) in ((:daxpby_,:Float64), (:saxpby_,:Float32),
                      (:zaxpby_,:ComplexF64), (:caxpby_,:ComplexF32))
    @eval begin
        # SUBROUTINE DAXPBY(N,DA,DX,INCX,DB,DY,INCY)
        # DY <- DA*DX + DB*DY
        #*     .. Scalar Arguments ..
        #      DOUBLE PRECISION DA,DB
        #      INTEGER INCX,INCY,N
        #*     .. Array Arguments ..
        #      DOUBLE PRECISION DX(*),DY(*)
        function axpby!(n::Integer, alpha::($elty), dx::Union{Ptr{$elty},
                        AbstractArray{$elty}}, incx::Integer, beta::($elty),
                        dy::Union{Ptr{$elty}, AbstractArray{$elty}}, incy::Integer)
            ccall((@blasfunc($fname), libblas), Cvoid, (Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                n, alpha, dx, incx, beta, dy, incy)
            dy
        end
    end
end

function axpby!(alpha::Number, x::Union{DenseArray{T},AbstractVector{T}}, beta::Number, y::Union{DenseArray{T},AbstractVector{T}}) where T<:BlasFloat
    require_one_based_indexing(x, y)
    if length(x) != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    return axpby!(length(x), convert(T, alpha), x, stride(x, 1), convert(T, beta), y, stride(y, 1))
end

## iamax
for (fname, elty) in ((:idamax_,:Float64),
                      (:isamax_,:Float32),
                      (:izamax_,:ComplexF64),
                      (:icamax_,:ComplexF32))
    @eval begin
        function iamax(n::Integer, dx::Union{Ptr{$elty}, AbstractArray{$elty}}, incx::Integer)
            ccall((@blasfunc($fname), libblas),BlasInt,
                (Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                n, dx, incx)
        end
    end
end
iamax(dx::Union{AbstractVector,DenseArray}) = iamax(length(dx), dx, stride1(dx))

"""
    iamax(n, dx, incx)
    iamax(dx)

Find the index of the element of `dx` with the maximum absolute value. `n` is the length of `dx`, and `incx` is the
stride. If `n` and `incx` are not provided, they assume default values of `n=length(dx)` and `incx=stride1(dx)`.
"""
iamax

# Level 2
## mv
### gemv
for (fname, elty) in ((:dgemv_,:Float64),
                      (:sgemv_,:Float32),
                      (:zgemv_,:ComplexF64),
                      (:cgemv_,:ComplexF32))
    @eval begin
             #SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             #*     .. Scalar Arguments ..
             #      DOUBLE PRECISION ALPHA,BETA
             #      INTEGER INCX,INCY,LDA,M,N
             #      CHARACTER TRANS
             #*     .. Array Arguments ..
             #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function gemv!(trans::AbstractChar, alpha::Union{($elty), Bool},
                       A::AbstractVecOrMat{$elty}, X::AbstractVector{$elty},
                       beta::Union{($elty), Bool}, Y::AbstractVector{$elty})
            require_one_based_indexing(A, X, Y)
            m,n = size(A,1),size(A,2)
            if trans == 'N' && (length(X) != n || length(Y) != m)
                throw(DimensionMismatch("A has dimensions $(size(A)), X has length $(length(X)) and Y has length $(length(Y))"))
            elseif trans == 'C' && (length(X) != m || length(Y) != n)
                throw(DimensionMismatch("the adjoint of A has dimensions $n, $m, X has length $(length(X)) and Y has length $(length(Y))"))
            elseif trans == 'T' && (length(X) != m || length(Y) != n)
                throw(DimensionMismatch("the transpose of A has dimensions $n, $m, X has length $(length(X)) and Y has length $(length(Y))"))
            end
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                 trans, size(A,1), size(A,2), alpha,
                 A, max(1,stride(A,2)), X, stride(X,1),
                 beta, Y, stride(Y,1))
            Y
        end
        function gemv(trans::AbstractChar, alpha::($elty), A::AbstractMatrix{$elty}, X::AbstractVector{$elty})
            gemv!(trans, alpha, A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
        function gemv(trans::AbstractChar, A::AbstractMatrix{$elty}, X::AbstractVector{$elty})
            gemv!(trans, one($elty), A, X, zero($elty), similar(X, $elty, size(A, (trans == 'N' ? 1 : 2))))
        end
    end
end

"""
    gemv!(tA, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y` or `alpha*A'x + beta*y`
according to [`tA`](@ref stdlib-blas-trans).
`alpha` and `beta` are scalars. Return the updated `y`.
"""
gemv!

"""
    gemv(tA, alpha, A, x)

Return `alpha*A*x` or `alpha*A'x` according to [`tA`](@ref stdlib-blas-trans).
`alpha` is a scalar.
"""
gemv(tA, alpha, A, x)

"""
    gemv(tA, A, x)

Return `A*x` or `A'x` according to [`tA`](@ref stdlib-blas-trans).
"""
gemv(tA, A, x)

### (GB) general banded matrix-vector multiplication

"""
    gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` or `alpha*A'*x + beta*y` according to [`trans`](@ref stdlib-blas-trans).
The matrix `A` is a general band matrix of dimension `m` by `size(A,2)` with `kl`
sub-diagonals and `ku` super-diagonals. `alpha` and `beta` are scalars. Return the updated `y`.
"""
function gbmv! end

"""
    gbmv(trans, m, kl, ku, alpha, A, x)

Return `alpha*A*x` or `alpha*A'*x` according to [`trans`](@ref stdlib-blas-trans).
The matrix `A` is a general band matrix of dimension `m` by `size(A,2)` with `kl` sub-diagonals and `ku`
super-diagonals, and `alpha` is a scalar.
"""
function gbmv end

for (fname, elty) in ((:dgbmv_,:Float64),
                      (:sgbmv_,:Float32),
                      (:zgbmv_,:ComplexF64),
                      (:cgbmv_,:ComplexF32))
    @eval begin
             # SUBROUTINE DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,KL,KU,LDA,M,N
             #       CHARACTER TRANS
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function gbmv!(trans::AbstractChar, m::Integer, kl::Integer, ku::Integer,
                       alpha::Union{($elty), Bool}, A::AbstractMatrix{$elty},
                       x::AbstractVector{$elty}, beta::Union{($elty), Bool},
                       y::AbstractVector{$elty})
            require_one_based_indexing(A, x, y)
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt}, Ref{BlasInt},
                 Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt},
                 Ptr{$elty}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}),
                 trans, m, size(A,2), kl,
                 ku, alpha, A, max(1,stride(A,2)),
                 x, stride(x,1), beta, y, stride(y,1))
            y
        end
        function gbmv(trans::AbstractChar, m::Integer, kl::Integer, ku::Integer, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            n = size(A,2)
            leny = trans == 'N' ? m : n
            gbmv!(trans, m, kl, ku, alpha, A, x, zero($elty), similar(x, $elty, leny))
        end
        function gbmv(trans::AbstractChar, m::Integer, kl::Integer, ku::Integer, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            gbmv(trans, m, kl, ku, one($elty), A, x)
        end
    end
end

### symv

"""
    symv!(ul, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y`. `A` is assumed to be symmetric.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
`alpha` and `beta` are scalars. Return the updated `y`.
"""
function symv! end

for (fname, elty, lib) in ((:dsymv_,:Float64,libblas),
                           (:ssymv_,:Float32,libblas),
                           (:zsymv_,:ComplexF64,liblapack),
                           (:csymv_,:ComplexF32,liblapack))
    # Note that the complex symv are not BLAS but auiliary functions in LAPACK
    @eval begin
             #      SUBROUTINE DSYMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             #     .. Scalar Arguments ..
             #      DOUBLE PRECISION ALPHA,BETA
             #      INTEGER INCX,INCY,LDA,N
             #      CHARACTER UPLO
             #     .. Array Arguments ..
             #      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function symv!(uplo::AbstractChar, alpha::Union{($elty), Bool},
                       A::AbstractMatrix{$elty}, x::AbstractVector{$elty},
                       beta::Union{($elty), Bool}, y::AbstractVector{$elty})
            require_one_based_indexing(A, x, y)
            m, n = size(A)
            if m != n
                throw(DimensionMismatch("matrix A is $m by $n but must be square"))
            end
            if n != length(x)
                throw(DimensionMismatch("A has size $(size(A)), and x has length $(length(x))"))
            end
            if m != length(y)
                throw(DimensionMismatch("A has size $(size(A)), and y has length $(length(y))"))
            end
            chkstride1(A)
            ccall((@blasfunc($fname), $lib), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}),
                 uplo, n, alpha, A,
                 max(1,stride(A,2)), x, stride(x,1), beta,
                 y, stride(y,1))
            y
        end
        function symv(uplo::AbstractChar, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
                symv!(uplo, alpha, A, x, zero($elty), similar(x))
        end
        function symv(uplo::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            symv(uplo, one($elty), A, x)
        end
    end
end

"""
    symv(ul, alpha, A, x)

Return `alpha*A*x`. `A` is assumed to be symmetric.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
`alpha` is a scalar.
"""
symv(ul, alpha, A, x)

"""
    symv(ul, A, x)

Return `A*x`. `A` is assumed to be symmetric.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
symv(ul, A, x)

### hemv
"""
    hemv!(ul, alpha, A, x, beta, y)

Update the vector `y` as `alpha*A*x + beta*y`. `A` is assumed to be Hermitian.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
`alpha` and `beta` are scalars. Return the updated `y`.
"""
function hemv! end

for (fname, elty) in ((:zhemv_,:ComplexF64),
                      (:chemv_,:ComplexF32))
    @eval begin
        function hemv!(uplo::AbstractChar, α::Union{$elty, Bool}, A::AbstractMatrix{$elty}, x::AbstractVector{$elty}, β::Union{$elty, Bool}, y::AbstractVector{$elty})
            require_one_based_indexing(A, x, y)
            m, n = size(A)
            if m != n
                throw(DimensionMismatch("matrix A is $m by $n but must be square"))
            end
            if n != length(x)
                throw(DimensionMismatch("A has size $(size(A)), and x has length $(length(x))"))
            end
            if m != length(y)
                throw(DimensionMismatch("A has size $(size(A)), and y has length $(length(y))"))
            end
            chkstride1(A)
            lda = max(1, stride(A, 2))
            incx = stride(x, 1)
            incy = stride(y, 1)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}),
                uplo, n, α, A,
                lda, x, incx, β,
                y, incy)
            y
        end
        function hemv(uplo::AbstractChar, α::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            hemv!(uplo, α, A, x, zero($elty), similar(x))
        end
        function hemv(uplo::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            hemv(uplo, one($elty), A, x)
        end
    end
end

"""
    hemv(ul, alpha, A, x)

Return `alpha*A*x`. `A` is assumed to be Hermitian.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
`alpha` is a scalar.
"""
hemv(ul, alpha, A, x)

"""
    hemv(ul, A, x)

Return `A*x`. `A` is assumed to be Hermitian.
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
hemv(ul, A, x)

### hpmv!, (HP) Hermitian packed matrix-vector operation defined as y := alpha*A*x + beta*y.
for (fname, elty) in ((:zhpmv_, :ComplexF64),
                      (:chpmv_, :ComplexF32))
    @eval begin
        # SUBROUTINE ZHPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        # Y <- ALPHA*AP*X + BETA*Y
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA,BETA
        #       INTEGER INCX,INCY,N
        #       CHARACTER UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(N,N),X(N),Y(N)
        function hpmv!(uplo::AbstractChar,
                       n::Integer,
                       α::$elty,
                       AP::Union{Ptr{$elty}, AbstractArray{$elty}},
                       x::Union{Ptr{$elty}, AbstractArray{$elty}},
                       incx::Integer,
                       β::$elty,
                       y::Union{Ptr{$elty}, AbstractArray{$elty}},
                       incy::Integer)

            ccall((@blasfunc($fname), libblas), Cvoid,
                  (Ref{UInt8},     # uplo,
                   Ref{BlasInt},   # n,
                   Ref{$elty},     # α,
                   Ptr{$elty},     # AP,
                   Ptr{$elty},     # x,
                   Ref{BlasInt},   # incx,
                   Ref{$elty},     # β,
                   Ptr{$elty},     # y, output
                   Ref{BlasInt}),  # incy
                  uplo,
                  n,
                  α,
                  AP,
                  x,
                  incx,
                  β,
                  y,
                  incy)
            return y
        end
    end
end

function hpmv!(uplo::AbstractChar,
               α::Number, AP::Union{DenseArray{T}, AbstractVector{T}}, x::Union{DenseArray{T}, AbstractVector{T}},
               β::Number, y::Union{DenseArray{T}, AbstractVector{T}}) where {T <: BlasComplex}
    require_one_based_indexing(AP, x, y)
    N = length(x)
    if N != length(y)
        throw(DimensionMismatch("x has length $(N), but y has length $(length(y))"))
    end
    if 2*length(AP) < N*(N + 1)
        throw(DimensionMismatch("Packed Hermitian matrix A has size smaller than length(x) =  $(N)."))
    end
    return hpmv!(uplo, N, convert(T, α), AP, x, stride(x, 1), convert(T, β), y, stride(y, 1))
end

"""
    hpmv!(uplo, α, AP, x, β, y)

Update vector `y` as `α*A*x + β*y`, where `A` is a Hermitian matrix provided
in packed format `AP`.

With `uplo = 'U'`, the array AP must contain the upper triangular part of the
Hermitian matrix packed sequentially, column by column, so that `AP[1]`
contains `A[1, 1]`, `AP[2]` and `AP[3]` contain `A[1, 2]` and `A[2, 2]`
respectively, and so on.

With `uplo = 'L'`, the array AP must contain the lower triangular part of the
Hermitian matrix packed sequentially, column by column, so that `AP[1]`
contains `A[1, 1]`, `AP[2]` and `AP[3]` contain `A[2, 1]` and `A[3, 1]`
respectively, and so on.

The scalar inputs `α` and `β` must be complex or real numbers.

The array inputs `x`, `y` and `AP` must all be of `ComplexF32` or `ComplexF64` type.

Return the updated `y`.
"""
hpmv!

### sbmv, (SB) symmetric banded matrix-vector multiplication
for (fname, elty) in ((:dsbmv_,:Float64),
                      (:ssbmv_,:Float32))
    @eval begin
             #       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,K,LDA,N
             #       CHARACTER UPLO
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function sbmv!(uplo::AbstractChar, k::Integer, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty}, beta::($elty), y::AbstractVector{$elty})
            require_one_based_indexing(A, x, y)
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, size(A,2), k, alpha,
                 A, max(1,stride(A,2)), x, stride(x,1),
                 beta, y, stride(y,1))
            y
        end
        function sbmv(uplo::AbstractChar, k::Integer, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            n = size(A,2)
            sbmv!(uplo, k, alpha, A, x, zero($elty), similar(x, $elty, n))
        end
        function sbmv(uplo::AbstractChar, k::Integer, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            sbmv(uplo, k, one($elty), A, x)
        end
    end
end

"""
    sbmv(uplo, k, alpha, A, x)

Return `alpha*A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
sbmv(uplo, k, alpha, A, x)

"""
    sbmv(uplo, k, A, x)

Return `A*x` where `A` is a symmetric band matrix of order `size(A,2)` with `k`
super-diagonals stored in the argument `A`.
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
sbmv(uplo, k, A, x)

"""
    sbmv!(uplo, k, alpha, A, x, beta, y)

Update vector `y` as `alpha*A*x + beta*y` where `A` is a symmetric band matrix of order
`size(A,2)` with `k` super-diagonals stored in the argument `A`. The storage layout for `A`
is described the reference BLAS module, level-2 BLAS at
<http://www.netlib.org/lapack/explore-html/>.
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `A` is used.

Return the updated `y`.
"""
sbmv!

### spmv!, (SP) symmetric packed matrix-vector operation defined as y := alpha*A*x + beta*y.
for (fname, elty) in ((:dspmv_, :Float64),
                      (:sspmv_, :Float32))
    @eval begin
        # SUBROUTINE DSPMV(UPLO,N,ALPHA,AP,X,INCX,BETA,Y,INCY)
        # Y <- ALPHA*AP*X + BETA*Y
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA,BETA
        #       INTEGER INCX,INCY,N
        #       CHARACTER UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(N,N),X(N),Y(N)
        function spmv!(uplo::AbstractChar,
                       n::Integer,
                       α::$elty,
                       AP::Union{Ptr{$elty}, AbstractArray{$elty}},
                       x::Union{Ptr{$elty}, AbstractArray{$elty}},
                       incx::Integer,
                       β::$elty,
                       y::Union{Ptr{$elty}, AbstractArray{$elty}},
                       incy::Integer)

            ccall((@blasfunc($fname), libblas), Cvoid,
                  (Ref{UInt8},     # uplo,
                   Ref{BlasInt},   # n,
                   Ref{$elty},     # α,
                   Ptr{$elty},     # AP,
                   Ptr{$elty},     # x,
                   Ref{BlasInt},   # incx,
                   Ref{$elty},     # β,
                   Ptr{$elty},     # y, out
                   Ref{BlasInt}),  # incy
                  uplo,
                  n,
                  α,
                  AP,
                  x,
                  incx,
                  β,
                  y,
                  incy)
            return y
        end
    end
end

function spmv!(uplo::AbstractChar,
               α::Real, AP::Union{DenseArray{T}, AbstractVector{T}}, x::Union{DenseArray{T}, AbstractVector{T}},
               β::Real, y::Union{DenseArray{T}, AbstractVector{T}}) where {T <: BlasReal}
    require_one_based_indexing(AP, x, y)
    N = length(x)
    if N != length(y)
        throw(DimensionMismatch("x has length $(N), but y has length $(length(y))"))
    end
    if 2*length(AP) < N*(N + 1)
        throw(DimensionMismatch("Packed symmetric matrix A has size smaller than length(x) = $(N)."))
    end
    return spmv!(uplo, N, convert(T, α), AP, x, stride(x, 1), convert(T, β), y, stride(y, 1))
end

"""
    spmv!(uplo, α, AP, x, β, y)

Update vector `y` as `α*A*x + β*y`, where `A` is a symmetric matrix provided
in packed format `AP`.

With `uplo = 'U'`, the array AP must contain the upper triangular part of the
symmetric matrix packed sequentially, column by column, so that `AP[1]`
contains `A[1, 1]`, `AP[2]` and `AP[3]` contain `A[1, 2]` and `A[2, 2]`
respectively, and so on.

With `uplo = 'L'`, the array AP must contain the lower triangular part of the
symmetric matrix packed sequentially, column by column, so that `AP[1]`
contains `A[1, 1]`, `AP[2]` and `AP[3]` contain `A[2, 1]` and `A[3, 1]`
respectively, and so on.

The scalar inputs `α` and `β` must be real.

The array inputs `x`, `y` and `AP` must all be of `Float32` or `Float64` type.

Return the updated `y`.
"""
spmv!

### hbmv, (HB) Hermitian banded matrix-vector multiplication
for (fname, elty) in ((:zhbmv_,:ComplexF64),
                      (:chbmv_,:ComplexF32))
    @eval begin
             #       SUBROUTINE ZHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER INCX,INCY,K,LDA,N
             #       CHARACTER UPLO
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
        function hbmv!(uplo::AbstractChar, k::Integer, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty}, beta::($elty), y::AbstractVector{$elty})
            require_one_based_indexing(A, x, y)
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, size(A,2), k, alpha,
                 A, max(1,stride(A,2)), x, stride(x,1),
                 beta, y, stride(y,1))
            y
        end
        function hbmv(uplo::AbstractChar, k::Integer, alpha::($elty), A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            n = size(A,2)
            hbmv!(uplo, k, alpha, A, x, zero($elty), similar(x, $elty, n))
        end
        function hbmv(uplo::AbstractChar, k::Integer, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            hbmv(uplo, k, one($elty), A, x)
        end
    end
end

### trmv, Triangular matrix-vector multiplication

"""
    trmv(ul, tA, dA, A, b)

Return `op(A)*b`, where `op` is determined by [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
"""
function trmv end

"""
    trmv!(ul, tA, dA, A, b)

Return `op(A)*b`, where `op` is determined by [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
The multiplication occurs in-place on `b`.
"""
function trmv! end

for (fname, elty) in ((:dtrmv_,:Float64),
                        (:strmv_,:Float32),
                        (:ztrmv_,:ComplexF64),
                        (:ctrmv_,:ComplexF32))
    @eval begin
                #       SUBROUTINE DTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
                # *     .. Scalar Arguments ..
                #       INTEGER INCX,LDA,N
                #       CHARACTER DIAG,TRANS,UPLO
                # *     .. Array Arguments ..
                #       DOUBLE PRECISION A(LDA,*),X(*)
        function trmv!(uplo::AbstractChar, trans::AbstractChar, diag::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            require_one_based_indexing(A, x)
            n = checksquare(A)
            if n != length(x)
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{UInt8}, Ref{BlasInt},
                 Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, trans, diag, n,
                 A, max(1,stride(A,2)), x, max(1,stride(x, 1)))
            x
        end
        function trmv(uplo::AbstractChar, trans::AbstractChar, diag::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            trmv!(uplo, trans, diag, A, copy(x))
        end
    end
end

### trsv, Triangular matrix-vector solve

"""
    trsv!(ul, tA, dA, A, b)

Overwrite `b` with the solution to `A*x = b` or one of the other two variants determined by
[`tA`](@ref stdlib-blas-trans) and [`ul`](@ref stdlib-blas-uplo).
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
Return the updated `b`.
"""
function trsv! end

"""
    trsv(ul, tA, dA, A, b)

Return the solution to `A*x = b` or one of the other two variants determined by
[`tA`](@ref stdlib-blas-trans) and [`ul`](@ref stdlib-blas-uplo).
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
"""
function trsv end

for (fname, elty) in ((:dtrsv_,:Float64),
                        (:strsv_,:Float32),
                        (:ztrsv_,:ComplexF64),
                        (:ctrsv_,:ComplexF32))
    @eval begin
                #       SUBROUTINE DTRSV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
                #       .. Scalar Arguments ..
                #       INTEGER INCX,LDA,N
                #       CHARACTER DIAG,TRANS,UPLO
                #       .. Array Arguments ..
                #       DOUBLE PRECISION A(LDA,*),X(*)
        function trsv!(uplo::AbstractChar, trans::AbstractChar, diag::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            require_one_based_indexing(A, x)
            n = checksquare(A)
            if n != length(x)
                throw(DimensionMismatch("size of A is $n != length(x) = $(length(x))"))
            end
            chkstride1(A)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{UInt8}, Ref{BlasInt},
                 Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, trans, diag, n,
                 A, max(1,stride(A,2)), x, stride(x, 1))
            x
        end
        function trsv(uplo::AbstractChar, trans::AbstractChar, diag::AbstractChar, A::AbstractMatrix{$elty}, x::AbstractVector{$elty})
            trsv!(uplo, trans, diag, A, copy(x))
        end
    end
end

### ger

"""
    ger!(alpha, x, y, A)

Rank-1 update of the matrix `A` with vectors `x` and `y` as `alpha*x*y' + A`.
"""
function ger! end

for (fname, elty) in ((:dger_,:Float64),
                      (:sger_,:Float32),
                      (:zgerc_,:ComplexF64),
                      (:cgerc_,:ComplexF32))
    @eval begin
        function ger!(α::$elty, x::AbstractVector{$elty}, y::AbstractVector{$elty}, A::AbstractMatrix{$elty})
            require_one_based_indexing(A, x, y)
            m, n = size(A)
            if m != length(x) || n != length(y)
                throw(DimensionMismatch("A has size ($m,$n), x has length $(length(x)), y has length $(length(y))"))
            end
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{BlasInt}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty},
                 Ref{BlasInt}),
                 m, n, α, x,
                 stride(x, 1), y, stride(y, 1), A,
                 max(1,stride(A,2)))
            A
        end
    end
end

### syr

"""
    syr!(uplo, alpha, x, A)

Rank-1 update of the symmetric matrix `A` with vector `x` as `alpha*x*transpose(x) + A`.
[`uplo`](@ref stdlib-blas-uplo) controls which triangle of `A` is updated. Returns `A`.
"""
function syr! end

for (fname, elty, lib) in ((:dsyr_,:Float64,libblas),
                           (:ssyr_,:Float32,libblas),
                           (:zsyr_,:ComplexF64,liblapack),
                           (:csyr_,:ComplexF32,liblapack))
    @eval begin
        function syr!(uplo::AbstractChar, α::$elty, x::AbstractVector{$elty}, A::AbstractMatrix{$elty})
            require_one_based_indexing(A, x)
            n = checksquare(A)
            if length(x) != n
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            ccall((@blasfunc($fname), $lib), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, n, α, x,
                 stride(x, 1), A, max(1,stride(A, 2)))
            A
        end
    end
end

### her

"""
    her!(uplo, alpha, x, A)

Methods for complex arrays only. Rank-1 update of the Hermitian matrix `A` with vector `x`
as `alpha*x*x' + A`.
[`uplo`](@ref stdlib-blas-uplo) controls which triangle of `A` is updated. Returns `A`.
"""
function her! end

for (fname, elty, relty) in ((:zher_,:ComplexF64, :Float64),
                             (:cher_,:ComplexF32, :Float32))
    @eval begin
        function her!(uplo::AbstractChar, α::$relty, x::AbstractVector{$elty}, A::AbstractMatrix{$elty})
            require_one_based_indexing(A, x)
            n = checksquare(A)
            if length(x) != n
                throw(DimensionMismatch("A has size ($n,$n), x has length $(length(x))"))
            end
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{BlasInt}, Ref{$relty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 uplo, n, α, x,
                 stride(x, 1), A, max(1,stride(A,2)))
            A
        end
    end
end

# Level 3
## (GE) general matrix-matrix multiplication

"""
    gemm!(tA, tB, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or the other three variants according to
[`tA`](@ref stdlib-blas-trans) and `tB`. Return the updated `C`.
"""
function gemm! end

for (gemm, elty) in
        ((:dgemm_,:Float64),
         (:sgemm_,:Float32),
         (:zgemm_,:ComplexF64),
         (:cgemm_,:ComplexF32))
    @eval begin
             # SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             # *     .. Scalar Arguments ..
             #       DOUBLE PRECISION ALPHA,BETA
             #       INTEGER K,LDA,LDB,LDC,M,N
             #       CHARACTER TRANSA,TRANSB
             # *     .. Array Arguments ..
             #       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function gemm!(transA::AbstractChar, transB::AbstractChar,
                       alpha::Union{($elty), Bool},
                       A::AbstractVecOrMat{$elty}, B::AbstractVecOrMat{$elty},
                       beta::Union{($elty), Bool},
                       C::AbstractVecOrMat{$elty})
#           if any([stride(A,1), stride(B,1), stride(C,1)] .!= 1)
#               error("gemm!: BLAS module requires contiguous matrix columns")
#           end  # should this be checked on every call?
            require_one_based_indexing(A, B, C)
            m = size(A, transA == 'N' ? 1 : 2)
            ka = size(A, transA == 'N' ? 2 : 1)
            kb = size(B, transB == 'N' ? 1 : 2)
            n = size(B, transB == 'N' ? 2 : 1)
            if ka != kb || m != size(C,1) || n != size(C,2)
                throw(DimensionMismatch("A has size ($m,$ka), B has size ($kb,$n), C has size $(size(C))"))
            end
            chkstride1(A)
            chkstride1(B)
            chkstride1(C)
            ccall((@blasfunc($gemm), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                 Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt},
                 Ptr{$elty}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}),
                 transA, transB, m, n,
                 ka, alpha, A, max(1,stride(A,2)),
                 B, max(1,stride(B,2)), beta, C,
                 max(1,stride(C,2)))
            C
        end
        function gemm(transA::AbstractChar, transB::AbstractChar, alpha::($elty), A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            gemm!(transA, transB, alpha, A, B, zero($elty), similar(B, $elty, (size(A, transA == 'N' ? 1 : 2), size(B, transB == 'N' ? 2 : 1))))
        end
        function gemm(transA::AbstractChar, transB::AbstractChar, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            gemm(transA, transB, one($elty), A, B)
        end
    end
end

"""
    gemm(tA, tB, alpha, A, B)

Return `alpha*A*B` or the other three variants according to [`tA`](@ref stdlib-blas-trans) and `tB`.
"""
gemm(tA, tB, alpha, A, B)

"""
    gemm(tA, tB, A, B)

Return `A*B` or the other three variants according to [`tA`](@ref stdlib-blas-trans) and `tB`.
"""
gemm(tA, tB, A, B)


## (SY) symmetric matrix-matrix and matrix-vector multiplication
for (mfname, elty) in ((:dsymm_,:Float64),
                       (:ssymm_,:Float32),
                       (:zsymm_,:ComplexF64),
                       (:csymm_,:ComplexF32))
    @eval begin
             #     SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #     .. Scalar Arguments ..
             #     DOUBLE PRECISION ALPHA,BETA
             #     INTEGER LDA,LDB,LDC,M,N
             #     CHARACTER SIDE,UPLO
             #     .. Array Arguments ..
             #     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function symm!(side::AbstractChar, uplo::AbstractChar, alpha::Union{($elty), Bool},
                       A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty},
                       beta::Union{($elty), Bool}, C::AbstractMatrix{$elty})
            require_one_based_indexing(A, B, C)
            m, n = size(C)
            j = checksquare(A)
            if j != (side == 'L' ? m : n)
                throw(DimensionMismatch("A has size $(size(A)), C has size ($m,$n)"))
            end
            if size(B,2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)) but needs to match second dimension of C, $n"))
            end
            chkstride1(A)
            chkstride1(B)
            chkstride1(C)
            ccall((@blasfunc($mfname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty},
                 Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                 side, uplo, m, n,
                 alpha, A, max(1,stride(A,2)), B,
                 max(1,stride(B,2)), beta, C, max(1,stride(C,2)))
            C
        end
        function symm(side::AbstractChar, uplo::AbstractChar, alpha::($elty), A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            symm!(side, uplo, alpha, A, B, zero($elty), similar(B))
        end
        function symm(side::AbstractChar, uplo::AbstractChar, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            symm(side, uplo, one($elty), A, B)
        end
    end
end

"""
    symm(side, ul, alpha, A, B)

Return `alpha*A*B` or `alpha*B*A` according to [`side`](@ref stdlib-blas-side).
`A` is assumed to be symmetric. Only
the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
symm(side, ul, alpha, A, B)

"""
    symm(side, ul, A, B)

Return `A*B` or `B*A` according to [`side`](@ref stdlib-blas-side).
`A` is assumed to be symmetric. Only the [`ul`](@ref stdlib-blas-uplo)
triangle of `A` is used.
"""
symm(side, ul, A, B)

"""
    symm!(side, ul, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or `alpha*B*A + beta*C` according to [`side`](@ref stdlib-blas-side).
`A` is assumed to be symmetric. Only the [`ul`](@ref stdlib-blas-uplo) triangle of
`A` is used. Return the updated `C`.
"""
symm!

## (HE) Hermitian matrix-matrix and matrix-vector multiplication
for (mfname, elty) in ((:zhemm_,:ComplexF64),
                       (:chemm_,:ComplexF32))
    @eval begin
             #     SUBROUTINE DHEMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #     .. Scalar Arguments ..
             #     DOUBLE PRECISION ALPHA,BETA
             #     INTEGER LDA,LDB,LDC,M,N
             #     CHARACTER SIDE,UPLO
             #     .. Array Arguments ..
             #     DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function hemm!(side::AbstractChar, uplo::AbstractChar, alpha::Union{($elty), Bool},
                       A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty},
                       beta::Union{($elty), Bool}, C::AbstractMatrix{$elty})
            require_one_based_indexing(A, B, C)
            m, n = size(C)
            j = checksquare(A)
            if j != (side == 'L' ? m : n)
                throw(DimensionMismatch("A has size $(size(A)), C has size ($m,$n)"))
            end
            if size(B,2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)) but needs to match second dimension of C, $n"))
            end
            chkstride1(A)
            chkstride1(B)
            chkstride1(C)
            ccall((@blasfunc($mfname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty},
                 Ref{BlasInt}, Ref{$elty}, Ptr{$elty}, Ref{BlasInt}),
                 side, uplo, m, n,
                 alpha, A, max(1,stride(A,2)), B,
                 max(1,stride(B,2)), beta, C, max(1,stride(C,2)))
            C
        end
        function hemm(side::AbstractChar, uplo::AbstractChar, alpha::($elty), A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            hemm!(side, uplo, alpha, A, B, zero($elty), similar(B))
        end
        function hemm(side::AbstractChar, uplo::AbstractChar, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            hemm(side, uplo, one($elty), A, B)
        end
    end
end

"""
    hemm(side, ul, alpha, A, B)

Return `alpha*A*B` or `alpha*B*A` according to [`side`](@ref stdlib-blas-side).
`A` is assumed to be Hermitian. Only the [`ul`](@ref stdlib-blas-uplo) triangle
of `A` is used.
"""
hemm(side, ul, alpha, A, B)

"""
    hemm(side, ul, A, B)

Return `A*B` or `B*A` according to [`side`](@ref stdlib-blas-side). `A` is assumed
to be Hermitian. Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
"""
hemm(side, ul, A, B)

"""
    hemm!(side, ul, alpha, A, B, beta, C)

Update `C` as `alpha*A*B + beta*C` or `alpha*B*A + beta*C` according to
[`side`](@ref stdlib-blas-side). `A` is assumed to be Hermitian. Only the
[`ul`](@ref stdlib-blas-uplo) triangle of `A` is used. Return the updated `C`.
"""
hemm!

## syrk

"""
    syrk!(uplo, trans, alpha, A, beta, C)

Rank-k update of the symmetric matrix `C` as `alpha*A*transpose(A) + beta*C` or
`alpha*transpose(A)*A + beta*C` according to [`trans`](@ref stdlib-blas-trans).
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `C` is used. Returns `C`.
"""
function syrk! end

"""
    syrk(uplo, trans, alpha, A)

Returns either the upper triangle or the lower triangle of `A`,
according to [`uplo`](@ref stdlib-blas-uplo),
of `alpha*A*transpose(A)` or `alpha*transpose(A)*A`,
according to [`trans`](@ref stdlib-blas-trans).
"""
function syrk end

for (fname, elty) in ((:dsyrk_,:Float64),
                      (:ssyrk_,:Float32),
                      (:zsyrk_,:ComplexF64),
                      (:csyrk_,:ComplexF32))
   @eval begin
       # SUBROUTINE DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       REAL ALPHA,BETA
       #       INTEGER K,LDA,LDC,N
       #       CHARACTER TRANS,UPLO
       # *     .. Array Arguments ..
       #       REAL A(LDA,*),C(LDC,*)
       function syrk!(uplo::AbstractChar, trans::AbstractChar,
                      alpha::Union{($elty), Bool}, A::AbstractVecOrMat{$elty},
                      beta::Union{($elty), Bool}, C::AbstractMatrix{$elty})
           require_one_based_indexing(A, C)
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
           k  = size(A, trans == 'N' ? 2 : 1)
           chkstride1(A)
           chkstride1(C)
           ccall((@blasfunc($fname), libblas), Cvoid,
                 (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                  Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ref{$elty},
                  Ptr{$elty}, Ref{BlasInt}),
                 uplo, trans, n, k,
                 alpha, A, max(1,stride(A,2)), beta,
                 C, max(1,stride(C,2)))
            C
        end
    end
end
function syrk(uplo::AbstractChar, trans::AbstractChar, alpha::Number, A::AbstractVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syrk!(uplo, trans, convert(T,alpha), A, zero(T), similar(A, T, (n, n)))
end
syrk(uplo::AbstractChar, trans::AbstractChar, A::AbstractVecOrMat) = syrk(uplo, trans, one(eltype(A)), A)

"""
    herk!(uplo, trans, alpha, A, beta, C)

Methods for complex arrays only. Rank-k update of the Hermitian matrix `C` as
`alpha*A*A' + beta*C` or `alpha*A'*A + beta*C` according to [`trans`](@ref stdlib-blas-trans).
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `C` is updated. Returns `C`.
"""
function herk! end

"""
    herk(uplo, trans, alpha, A)

Methods for complex arrays only. Returns the [`uplo`](@ref stdlib-blas-uplo)
triangle of `alpha*A*A'` or `alpha*A'*A`, according to [`trans`](@ref stdlib-blas-trans).
"""
function herk end

for (fname, elty, relty) in ((:zherk_, :ComplexF64, :Float64),
                             (:cherk_, :ComplexF32, :Float32))
   @eval begin
       # SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
       # *     .. Scalar Arguments ..
       #       REAL ALPHA,BETA
       #       INTEGER K,LDA,LDC,N
       #       CHARACTER TRANS,UPLO
       # *     ..
       # *     .. Array Arguments ..
       #       COMPLEX A(LDA,*),C(LDC,*)
       function herk!(uplo::AbstractChar, trans::AbstractChar,
                      α::Union{$relty, Bool}, A::AbstractVecOrMat{$elty},
                      β::Union{$relty, Bool}, C::AbstractMatrix{$elty})
           require_one_based_indexing(A, C)
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n
               throw(DimensionMismatch("the matrix to update has dimension $n but the implied dimension of the update is $(size(A, trans == 'N' ? 1 : 2))"))
           end
           chkstride1(A)
           chkstride1(C)
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall((@blasfunc($fname), libblas), Cvoid,
                 (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                  Ref{$relty}, Ptr{$elty}, Ref{BlasInt}, Ref{$relty},
                  Ptr{$elty}, Ref{BlasInt}),
                 uplo, trans, n, k,
                 α, A, max(1,stride(A,2)), β,
                 C, max(1,stride(C,2)))
           C
       end
       function herk(uplo::AbstractChar, trans::AbstractChar, α::$relty, A::AbstractVecOrMat{$elty})
           n = size(A, trans == 'N' ? 1 : 2)
           herk!(uplo, trans, α, A, zero($relty), similar(A, (n,n)))
       end
       herk(uplo::AbstractChar, trans::AbstractChar, A::AbstractVecOrMat{$elty}) = herk(uplo, trans, one($relty), A)
   end
end

## syr2k
for (fname, elty) in ((:dsyr2k_,:Float64),
                      (:ssyr2k_,:Float32),
                      (:zsyr2k_,:ComplexF64),
                      (:csyr2k_,:ComplexF32))
    @eval begin
             #       SUBROUTINE DSYR2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
             #
             #       .. Scalar Arguments ..
             #       REAL PRECISION ALPHA,BETA
             #       INTEGER K,LDA,LDB,LDC,N
             #       CHARACTER TRANS,UPLO
             #       ..
             #       .. Array Arguments ..
             #       REAL PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
        function syr2k!(uplo::AbstractChar, trans::AbstractChar,
                        alpha::($elty), A::AbstractVecOrMat{$elty}, B::AbstractVecOrMat{$elty},
                        beta::($elty), C::AbstractMatrix{$elty})
            require_one_based_indexing(A, B, C)
            n = checksquare(C)
            nn = size(A, trans == 'N' ? 1 : 2)
            if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
            k  = size(A, trans == 'N' ? 2 : 1)
            chkstride1(A)
            chkstride1(B)
            chkstride1(C)
            ccall((@blasfunc($fname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                 Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}, Ref{$elty},
                 Ptr{$elty}, Ref{BlasInt}),
                 uplo, trans, n, k,
                 alpha, A, max(1,stride(A,2)), B, max(1,stride(B,2)), beta,
                 C, max(1,stride(C,2)))
            C
        end
    end
end

"""
    syr2k!(uplo, trans, alpha, A, B, beta, C)

Rank-2k update of the symmetric matrix `C` as
`alpha*A*transpose(B) + alpha*B*transpose(A) + beta*C` or
`alpha*transpose(A)*B + alpha*transpose(B)*A + beta*C`
according to [`trans`](@ref stdlib-blas-trans).
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `C` is used. Returns `C`.
"""
function syr2k! end

"""
    syr2k(uplo, trans, alpha, A, B)

Returns the [`uplo`](@ref stdlib-blas-uplo) triangle of
`alpha*A*transpose(B) + alpha*B*transpose(A)` or
`alpha*transpose(A)*B + alpha*transpose(B)*A`,
according to [`trans`](@ref stdlib-blas-trans).
"""
function syr2k(uplo::AbstractChar, trans::AbstractChar, alpha::Number, A::AbstractVecOrMat, B::AbstractVecOrMat)
    T = eltype(A)
    n = size(A, trans == 'N' ? 1 : 2)
    syr2k!(uplo, trans, convert(T,alpha), A, B, zero(T), similar(A, T, (n, n)))
end
"""
    syr2k(uplo, trans, A, B)

Returns the [`uplo`](@ref stdlib-blas-uplo) triangle of `A*transpose(B) + B*transpose(A)`
or `transpose(A)*B + transpose(B)*A`, according to [`trans`](@ref stdlib-blas-trans).
"""
syr2k(uplo::AbstractChar, trans::AbstractChar, A::AbstractVecOrMat, B::AbstractVecOrMat) = syr2k(uplo, trans, one(eltype(A)), A, B)

for (fname, elty1, elty2) in ((:zher2k_,:ComplexF64,:Float64), (:cher2k_,:ComplexF32,:Float32))
   @eval begin
       # SUBROUTINE CHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
       #
       #       .. Scalar Arguments ..
       #       COMPLEX ALPHA
       #       REAL BETA
       #       INTEGER K,LDA,LDB,LDC,N
       #       CHARACTER TRANS,UPLO
       #       ..
       #       .. Array Arguments ..
       #       COMPLEX A(LDA,*),B(LDB,*),C(LDC,*)
       function her2k!(uplo::AbstractChar, trans::AbstractChar, alpha::($elty1),
                       A::AbstractVecOrMat{$elty1}, B::AbstractVecOrMat{$elty1},
                       beta::($elty2), C::AbstractMatrix{$elty1})
           require_one_based_indexing(A, B, C)
           n = checksquare(C)
           nn = size(A, trans == 'N' ? 1 : 2)
           if nn != n throw(DimensionMismatch("C has size ($n,$n), corresponding dimension of A is $nn")) end
           chkstride1(A)
           chkstride1(B)
           chkstride1(C)
           k  = size(A, trans == 'N' ? 2 : 1)
           ccall((@blasfunc($fname), libblas), Cvoid,
                 (Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                  Ref{$elty1}, Ptr{$elty1}, Ref{BlasInt}, Ptr{$elty1}, Ref{BlasInt},
                  Ref{$elty2},  Ptr{$elty1}, Ref{BlasInt}),
                 uplo, trans, n, k,
                 alpha, A, max(1,stride(A,2)), B, max(1,stride(B,2)),
                 beta, C, max(1,stride(C,2)))
           C
       end
       function her2k(uplo::AbstractChar, trans::AbstractChar, alpha::($elty1), A::AbstractVecOrMat{$elty1}, B::AbstractVecOrMat{$elty1})
           n = size(A, trans == 'N' ? 1 : 2)
           her2k!(uplo, trans, alpha, A, B, zero($elty2), similar(A, $elty1, (n,n)))
       end
       her2k(uplo::AbstractChar, trans::AbstractChar, A::AbstractVecOrMat{$elty1}, B::AbstractVecOrMat{$elty1}) = her2k(uplo, trans, one($elty1), A, B)
   end
end

"""
    her2k!(uplo, trans, alpha, A, B, beta, C)

Rank-2k update of the Hermitian matrix `C` as
`alpha*A*B' + alpha*B*A' + beta*C` or `alpha*A'*B + alpha*B'*A + beta*C`
according to [`trans`](@ref stdlib-blas-trans). The scalar `beta` has to be real.
Only the [`uplo`](@ref stdlib-blas-uplo) triangle of `C` is used. Returns `C`.
"""
function her2k! end

"""
    her2k(uplo, trans, alpha, A, B)

Returns the [`uplo`](@ref stdlib-blas-uplo) triangle of `alpha*A*B' + alpha*B*A'`
or `alpha*A'*B + alpha*B'*A`, according to [`trans`](@ref stdlib-blas-trans).
"""
her2k(uplo, trans, alpha, A, B)

"""
    her2k(uplo, trans, A, B)

Returns the [`uplo`](@ref stdlib-blas-uplo) triangle of `A*B' + B*A'`
or `A'*B + B'*A`, according to [`trans`](@ref stdlib-blas-trans).
"""
her2k(uplo, trans, A, B)

## (TR) Triangular matrix and vector multiplication and solution

"""
    trmm!(side, ul, tA, dA, alpha, A, B)

Update `B` as `alpha*A*B` or one of the other three variants determined by
[`side`](@ref stdlib-blas-side) and [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
Returns the updated `B`.
"""
function trmm! end

"""
    trmm(side, ul, tA, dA, alpha, A, B)

Returns `alpha*A*B` or one of the other three variants determined by
[`side`](@ref stdlib-blas-side) and [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
"""
function trmm end

"""
    trsm!(side, ul, tA, dA, alpha, A, B)

Overwrite `B` with the solution to `A*X = alpha*B` or one of the other three variants
determined by [`side`](@ref stdlib-blas-side) and [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
Returns the updated `B`.
"""
function trsm! end

"""
    trsm(side, ul, tA, dA, alpha, A, B)

Return the solution to `A*X = alpha*B` or one of the other three variants determined by
determined by [`side`](@ref stdlib-blas-side) and [`tA`](@ref stdlib-blas-trans).
Only the [`ul`](@ref stdlib-blas-uplo) triangle of `A` is used.
[`dA`](@ref stdlib-blas-diag) determines if the diagonal values are read or
are assumed to be all ones.
"""
function trsm end

for (mmname, smname, elty) in
        ((:dtrmm_,:dtrsm_,:Float64),
         (:strmm_,:strsm_,:Float32),
         (:ztrmm_,:ztrsm_,:ComplexF64),
         (:ctrmm_,:ctrsm_,:ComplexF32))
    @eval begin
        #       SUBROUTINE DTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA
        #       INTEGER LDA,LDB,M,N
        #       CHARACTER DIAG,SIDE,TRANSA,UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),B(LDB,*)
        function trmm!(side::AbstractChar, uplo::AbstractChar, transa::AbstractChar, diag::AbstractChar, alpha::Number,
                       A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            require_one_based_indexing(A, B)
            m, n = size(B)
            nA = checksquare(A)
            if nA != (side == 'L' ? m : n)
                throw(DimensionMismatch("size of A, $(size(A)), doesn't match $side size of B with dims, $(size(B))"))
            end
            chkstride1(A)
            chkstride1(B)
            ccall((@blasfunc($mmname), libblas), Cvoid,
                  (Ref{UInt8}, Ref{UInt8}, Ref{UInt8}, Ref{UInt8}, Ref{BlasInt}, Ref{BlasInt},
                   Ref{$elty}, Ptr{$elty}, Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                  side, uplo, transa, diag, m, n,
                  alpha, A, max(1,stride(A,2)), B, max(1,stride(B,2)))
            B
        end
        function trmm(side::AbstractChar, uplo::AbstractChar, transa::AbstractChar, diag::AbstractChar,
                      alpha::$elty, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            trmm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
        #       SUBROUTINE DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
        # *     .. Scalar Arguments ..
        #       DOUBLE PRECISION ALPHA
        #       INTEGER LDA,LDB,M,N
        #       CHARACTER DIAG,SIDE,TRANSA,UPLO
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION A(LDA,*),B(LDB,*)
        function trsm!(side::AbstractChar, uplo::AbstractChar, transa::AbstractChar, diag::AbstractChar,
                       alpha::$elty, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            require_one_based_indexing(A, B)
            m, n = size(B)
            k = checksquare(A)
            if k != (side == 'L' ? m : n)
                throw(DimensionMismatch("size of A is ($k,$k), size of B is ($m,$n), side is $side, and transa='$transa'"))
            end
            chkstride1(A)
            chkstride1(B)
            ccall((@blasfunc($smname), libblas), Cvoid,
                (Ref{UInt8}, Ref{UInt8}, Ref{UInt8}, Ref{UInt8},
                 Ref{BlasInt}, Ref{BlasInt}, Ref{$elty}, Ptr{$elty},
                 Ref{BlasInt}, Ptr{$elty}, Ref{BlasInt}),
                 side, uplo, transa, diag,
                 m, n, alpha, A,
                 max(1,stride(A,2)), B, max(1,stride(B,2)))
            B
        end
        function trsm(side::AbstractChar, uplo::AbstractChar, transa::AbstractChar, diag::AbstractChar, alpha::$elty, A::AbstractMatrix{$elty}, B::AbstractMatrix{$elty})
            trsm!(side, uplo, transa, diag, alpha, A, copy(B))
        end
    end
end

end # module

function copyto!(dest::Array{T}, rdest::Union{UnitRange{Ti},AbstractRange{Ti}},
                 src::Array{T}, rsrc::Union{UnitRange{Ti},AbstractRange{Ti}}) where {T<:BlasFloat,Ti<:Integer}
    if minimum(rdest) < 1 || maximum(rdest) > length(dest)
        throw(ArgumentError("range out of bounds for dest, of length $(length(dest))"))
    end
    if minimum(rsrc) < 1 || maximum(rsrc) > length(src)
        throw(ArgumentError("range out of bounds for src, of length $(length(src))"))
    end
    if length(rdest) != length(rsrc)
        throw(DimensionMismatch("ranges must be of the same length"))
    end
    GC.@preserve src dest BLAS.blascopy!(
        length(rsrc),
        pointer(src) + (first(rsrc) - 1) * sizeof(T),
        step(rsrc),
        pointer(dest) + (first(rdest) - 1) * sizeof(T),
        step(rdest))

    return dest
end
