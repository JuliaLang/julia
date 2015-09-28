# This file is a part of Julia. License is MIT: http://julialang.org/license

# matmul.jl: Everything to do with dense matrix multiplication

arithtype(T) = T
arithtype(::Type{Bool}) = Int

# multiply by diagonal matrix as vector
function scale!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector)
    m, n = size(A)
    p, q = size(C)
    if size(A) != size(C)
        throw(DimensionMismatch("size of A, $(size(A)), does not match size of C, $(size(C))"))
    end
    if n != length(b)
        throw(DimensionMismatch("second dimension of A, $n, does not match length of b, $(length(b))"))
    end
    @inbounds for j = 1:n
        bj = b[j]
        for i = 1:m
            C[i,j] = A[i,j]*bj
        end
    end
    C
end

function scale!(C::AbstractMatrix, b::AbstractVector, A::AbstractMatrix)
    m, n = size(A)
    p, q = size(C)
    if size(A) != size(C)
        throw(DimensionMismatch("size of A, $(size(A)), does not match size of C, $(size(C))"))
    end
    if m != length(b)
        throw(DimensionMismatch("first dimension of A, $m, does not match length of b, $(length(b))"))
    end
    @inbounds for j = 1:n, i = 1:m
        C[i,j] = A[i,j]*b[i]
    end
    C
end
scale(A::Matrix, b::Vector) = scale!(similar(A, promote_type(eltype(A),eltype(b))), A, b)
scale(b::Vector, A::Matrix) = scale!(similar(b, promote_type(eltype(A),eltype(b)), size(A)), b, A)

# Dot products

vecdot{T<:BlasReal}(x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}}) = BLAS.dot(x, y)
vecdot{T<:BlasComplex}(x::Union{DenseArray{T},StridedVector{T}}, y::Union{DenseArray{T},StridedVector{T}}) = BLAS.dotc(x, y)

function dot{T<:BlasReal, TI<:Integer}(x::Vector{T}, rx::Union{UnitRange{TI},Range{TI}}, y::Vector{T}, ry::Union{UnitRange{TI},Range{TI}})
    if length(rx) != length(ry)
        throw(DimensionMismatch("length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

function dot{T<:BlasComplex, TI<:Integer}(x::Vector{T}, rx::Union{UnitRange{TI},Range{TI}}, y::Vector{T}, ry::Union{UnitRange{TI},Range{TI}})
    if length(rx) != length(ry)
        throw(DimensionMismatch("length of rx, $(length(rx)), does not equal length of ry, $(length(ry))"))
    end
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    if minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    BLAS.dotc(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

# Matrix-Matrix
mul_shape(A::AbstractMatrix,B::AbstractMatrix) = (size(A,1), size(B,2))
# Matrix-Vector
mul_shape(A::AbstractMatrix, B::AbstractVector) = (size(A,1),)
mul_shape(A::AbstractVector, B::AbstractMatrix) = (length(A), size(B,2))
# Matrix-Covector
mul_shape(A::AbstractMatrix, B::Covector) = (size(A,1), length(B))
# mul_shape(A::Covector, B::AbstractMatrix) = (size(B,2),) # Returns a covector
# Vector-Covector
mul_shape(A::Vector, B::Covector) = (length(A), length(B))

ntc(::AbstractMatrix) = 'N'
ntc{T<:Complex}(::MatrixTranspose{true, T}) = 'C'
ntc{T<:Real}(::MatrixTranspose{true, T}) = 'T'
ntc(::MatrixTranspose{true}) = 'C'
ntc(::MatrixTranspose{false}) = 'T'

ntc(::AbstractVector) = 'N'
ntc{T<:Complex}(::Covector{true, T}) = 'C'
ntc{T<:Real}(::Covector{true, T}) = 'T'
ntc(::Covector{true}) = 'C'
ntc(::Covector{false}) = 'T'

typealias StridedMatOrTrans{T,C,A<:StridedVecOrMat} Union{StridedMatrix{T}, MatrixTranspose{C,T,A}}
typealias StridedCovector{T,C,A<:StridedVector} Covector{C,T,A}

## There is a huge combinatorial explosion here. There are 4 mostly-orthoganol
# dimensions:
#
# * Mutation:  (Mutating, non-mutating). This can be a simple fallback
# * Shape:     (Mat*Mat; Vec*Mat; Mat*Vec). Plus some Vec*Vec with combinations of:
# * Transpose: (A, Aᵀ, Aᴴ) × (B, Bᵀ, Bᴴ).
# * Implementation: (BLAS, Strided, generic)
#
# The key to staying sane is keeping the number of combinations on any one
# name relatively limited.

# Covector-vector dot products
*(x::Covector{true}, y::AbstractVector) = dot(untranspose(x), y)
*(x::Covector{false}, y::AbstractVector) = dot(conj(untranspose(x)), y) # TODO: don't conjugate twice!
*{T<:BlasReal,A<:StridedVector}(x::Covector{true, T, A}, y::StridedVector{T}) = BLAS.dot(untranspose(x), y)
*{T<:BlasReal,A<:StridedVector}(x::Covector{false, T, A}, y::StridedVector{T}) = BLAS.dot(untranspose(x), y)
*{T<:BlasComplex,A<:StridedVector}(x::Covector{true, T, A}, y::StridedVector{T}) = BLAS.dotc(untranspose(x), y)
*{T<:BlasComplex,A<:StridedVector}(x::Covector{false, T, A}, y::StridedVector{T}) = BLAS.dotu(untranspose(x), y)
# Vector-covector outer products
# TODO: implement the typical BLAS conversion of the second argument?
# function (*){T<:BlasFloat,S,C}(x::StridedVector{T}, y::StridedCovector{S,C})
#     TS = promote_type(arithtype(T),arithtype(S))
#     mul!(similar(untranspose(y), TS, mul_shape(x, y)), x, convert(Covector{C,TS}, y))
# end
function (*){T,S,C}(x::AbstractVector{T}, y::Covector{C,S})
    TS = promote_type(arithtype(T),arithtype(S))
    mul!(similar(x,TS,mul_shape(x, y)),x,y)
end
mul!{T<:BlasFloat}(C::StridedMatrix{T}, x::StridedVector{T}, y::StridedCovector{T}) = BLAS.ger!(one(T), x, untranspose(y), C)
# TODO: Add specializations for BLAS Complex*Float and/or Float*Complex?
function mul!(C::AbstractMatrix, x::AbstractVector, y::Covector)
    for j=1:size(C,2), i=1:size(C,1) # TODO: move to generic.jl or use something pre-existing
        C[i,j] = x[i]*y[j]
    end
    C
end

# Matrix-vector multiplication
function (*){T<:BlasFloat,S}(A::StridedMatOrTrans{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    mul!(similar(x, TS, mul_shape(A, x)), A, convert(AbstractVector{TS}, x))
end
function (*){T,S}(A::AbstractMatrix{T}, x::AbstractVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    mul!(similar(x,TS,mul_shape(A, x)),A,x)
end
mul!{T<:BlasFloat}(y::StridedVector{T}, A::StridedMatOrTrans{T}, x::StridedVector{T}) = gemv!(y, ntc(A), untranspose(A), x)
for elty in (Float32,Float64)
    @eval begin
        function mul!(y::StridedVector{Complex{$elty}}, A::StridedMatOrTrans{Complex{$elty}}, x::StridedVector{$elty})
            Afl = reinterpret($elty,untranspose(A),(2size(A,1),size(A,2)))
            yfl = reinterpret($elty,y)
            gemv!(yfl,ntc(A),Afl,x)
            return y
        end
    end
end
mul!(y::AbstractVector, A::AbstractMatrix, x::AbstractVector) = generic_matvecmul!(y, ntc(A), untranspose(A), x)

# # Vector-matrix multiplication only works with implicit trailing singleton dimensions...
# (*)(A::AbstractVector, B::AbstractMatrix) = reshape(A,length(A),1)*B # TODO: deprecate it?
# mul!(C::AbstractMatrix, A::AbstractVector, B::AbstractMatrix) = mul!(C,reshape(A,length(A),1),B)

# Covector-matrix multiplication returns a covector; v'*A*v is associative and a scalar
# v'A => transpose(A'v), so we punt to gemv with a transpose
*(x::Covector, A::AbstractMatrix) = (A'x')'
mul!(c::Covector{true}, x::Covector, A::AbstractMatrix) = (mul!(untranspose(c), A', x'); c)
mul!(c::Covector{false}, x::Covector, A::AbstractMatrix) = (conj!(mul!(untranspose(c), A', x')); c)

# Matrix-matrix multiplication
function (*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})
    TS = promote_type(arithtype(T), arithtype(S))
    mul!(similar(untranspose(B), TS, mul_shape(A, B)), A, B)
end
mul!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatOrTrans{T}, B::StridedMatOrTrans{T}) = gemm_wrapper!(C, ntc(A), ntc(B), untranspose(A), untranspose(B))
for elty in (Float32,Float64)
    @eval begin
        function mul!(C::StridedMatrix{Complex{$elty}}, A::StridedMatOrTrans{Complex{$elty}}, B::StridedMatOrTrans{$elty})
            Afl = reinterpret($elty, untranspose(A), (2size(A,1), size(A,2)))
            Cfl = reinterpret($elty, C, (2size(C,1), size(C,2)))
            gemm_wrapper!(Cfl, ntc(A), ntc(B), Afl, B)
            return C
        end
    end
end
mul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix) = generic_matmatmul!(C, ntc(A), ntc(B), untranspose(A), untranspose(B))

# Supporting functions for matrix multiplication

function copytri!(A::StridedMatrix, uplo::Char, conjugate::Bool=false)
    n = chksquare(A)
    @chkuplo
    if uplo == 'U'
        for i = 1:(n-1), j = (i+1):n
            A[j,i] = conjugate ? conj(A[i,j]) : A[i,j]
        end
    elseif uplo == 'L'
        for i = 1:(n-1), j = (i+1):n
            A[i,j] = conjugate ? conj(A[j,i]) : A[j,i]
        end
    end
    A
end

function gemv!{T<:BlasFloat}(y::StridedVector{T}, tA::Char, A::StridedVecOrMat{T}, x::StridedVector{T})
    mA, nA = lapack_size(tA, A)
    if nA != length(x)
        throw(DimensionMismatch("second dimension of A, $nA, does not match length of x, $(length(x))"))
    end
    if mA != length(y)
        throw(DimensionMismatch("first dimension of A, $mA, does not match length of y, $(length(y))"))
    end
    if mA == 0
        return y
    end
    if nA == 0
        return fill!(y,0)
    end
    stride(A, 1) == 1 && stride(A, 2) >= size(A, 1) && return BLAS.gemv!(tA, one(T), A, x, zero(T), y)
    return generic_matvecmul!(y, tA, A, x)
end

function syrk_wrapper!{T<:BlasFloat}(C::StridedMatrix{T}, tA::Char, A::StridedVecOrMat{T})
    nC = chksquare(C)
    if tA == 'T'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'T'
    end
    if nC != mA
        throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0
        return fill!(C,0)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C,tA,tAt,A,A)
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C,tA,tAt,A,A)
    end

    if stride(A, 1) == 1 && stride(A, 2) >= size(A, 1)
        return copytri!(BLAS.syrk!('U', tA, one(T), A, zero(T), C), 'U')
    end
    return generic_matmatmul!(C, tA, tAt, A, A)
end

function herk_wrapper!{T<:BlasReal}(C::Union{StridedMatrix{T}, StridedMatrix{Complex{T}}}, tA::Char, A::Union{StridedVecOrMat{T}, StridedVecOrMat{Complex{T}}})
    nC = chksquare(C)
    if tA == 'C'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'C'
    end
    if nC != mA
        throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    end
    if mA == 0 || nA == 0
        return fill!(C,0)
    end
    if mA == 2 && nA == 2
        return matmul2x2!(C,tA,tAt,A,A)
    end
    if mA == 3 && nA == 3
        return matmul3x3!(C,tA,tAt,A,A)
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Array(T, mA, mA)

    if stride(A, 1) == 1 && stride(A, 2) >= size(A, 1)
        return copytri!(BLAS.herk!('U', tA, one(T), A, zero(T), C), 'U', true)
    end
    return generic_matmatmul!(C,tA, tAt, A, A)
end

function gemm_wrapper{T<:BlasFloat}(tA::Char, tB::Char,
                                    A::StridedVecOrMat{T},
                                    B::StridedVecOrMat{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = similar(B, T, mA, nB)
    gemm_wrapper!(C, tA, tB, A, B)
end

function gemm_wrapper!{T<:BlasFloat}(C::StridedVecOrMat{T}, tA::Char, tB::Char,
                                     A::StridedVecOrMat{T},
                                     B::StridedVecOrMat{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB
        throw(DimensionMismatch("A has dimensions ($mA,$nA) but B has dimensions ($mB,$nB)"))
    end

    if C === A || B === C
        throw(ArgumentError("output matrix must not be aliased with input matrix"))
    end

    if mA == 0 || nA == 0 || nB == 0
        if size(C) != (mA, nB)
            throw(DimensionMismatch("C has dimensions $(size(C)), should have ($mA,$nB)"))
        end
        return fill!(C,0)
    end

    if mA == 2 && nA == 2 && nB == 2
        return matmul2x2!(C,tA,tB,A,B)
    end
    if mA == 3 && nA == 3 && nB == 3
        return matmul3x3!(C,tA,tB,A,B)
    end

    if stride(A, 1) == stride(B, 1) == 1 && stride(A, 2) >= size(A, 1) && stride(B, 2) >= size(B, 1)
        return BLAS.gemm!(tA, tB, one(T), A, B, zero(T), C)
    end
    generic_matmatmul!(C, tA, tB, A, B)
end

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::Char, M::AbstractVecOrMat) = (size(M, t=='N' ? 1:2), size(M, t=='N' ? 2:1))

function copy!{R,S}(B::AbstractVecOrMat{R}, ir_dest::UnitRange{Int}, jr_dest::UnitRange{Int}, tM::Char, M::AbstractVecOrMat{S}, ir_src::UnitRange{Int}, jr_src::UnitRange{Int})
    if tM == 'N'
        copy!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        Base.copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(B)
    end
end

function copy_transpose!{R,S}(B::AbstractMatrix{R}, ir_dest::UnitRange{Int}, jr_dest::UnitRange{Int}, tM::Char, M::AbstractVecOrMat{S}, ir_src::UnitRange{Int}, jr_src::UnitRange{Int})
    if tM == 'N'
        Base.copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copy!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        tM == 'C' && conj!(B)
    end
    B
end

# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the generic version is also called as fallback for
#       strides != 1 cases

function generic_matvecmul!{T,S,R}(C::AbstractVector{R}, tA, A::AbstractVecOrMat{T}, B::AbstractVector{S})
    mB = length(B)
    mA, nA = lapack_size(tA, A)
    if mB != nA
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA), vector B has length $mB"))
    end
    if mA != length(C)
        throw(DimensionMismatch("result C has length $(length(C)), needs length $mA"))
    end

    Astride = size(A, 1)

    if tA == 'T'  # fastest case
        for k = 1:mA
            aoffs = (k-1)*Astride
            if mB == 0
                s = zero(R)
            else
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
            end
            for i = 1:nA
                s += A[aoffs+i].'B[i]
            end
            C[k] = s
        end
    elseif tA == 'C'
        for k = 1:mA
            aoffs = (k-1)*Astride
            if mB == 0
                s = zero(R)
            else
                s = zero(A[aoffs + 1]*B[1] + A[aoffs + 1]*B[1])
            end
            for i = 1:nA
                s += A[aoffs + i]'B[i]
            end
            C[k] = s
        end
    else # tA == 'N'
        for i = 1:mA
            if mB == 0
                C[i] = zero(R)
            else
                C[i] = zero(A[i]*B[1] + A[i]*B[1])
            end
        end
        for k = 1:mB
            aoffs = (k-1)*Astride
            b = B[k]
            for i = 1:mA
                C[i] += A[aoffs + i] * b
            end
        end
    end
    C
end

function generic_matmatmul{T,S}(tA, tB, A::AbstractVecOrMat{T}, B::AbstractMatrix{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = similar(B, promote_type(arithtype(T),arithtype(S)), mA, nB)
    generic_matmatmul!(C, tA, tB, A, B)
end

const tilebufsize = 10800  # Approximately 32k/3
const Abuf = Array(UInt8, tilebufsize)
const Bbuf = Array(UInt8, tilebufsize)
const Cbuf = Array(UInt8, tilebufsize)

function generic_matmatmul!{T,S,R}(C::AbstractVecOrMat{R}, tA, tB, A::AbstractVecOrMat{T}, B::AbstractVecOrMat{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if mB != nA
        throw(DimensionMismatch("matrix A has dimensions ($mA, $nA), matrix B has dimensions ($mB, $nB)"))
    end
    if size(C,1) != mA || size(C,2) != nB
        throw(DimensionMismatch("result C has dimensions $(size(C)), needs ($mA, $nB)"))
    end

    if mA == nA == nB == 2
        return matmul2x2!(C, tA, tB, A, B)
    end
    if mA == nA == nB == 3
        return matmul3x3!(C, tA, tB, A, B)
    end

    @inbounds begin
    if isbits(R)
        tile_size = floor(Int,sqrt(tilebufsize/sizeof(R)))
        sz = (tile_size, tile_size)
        Atile = pointer_to_array(convert(Ptr{R}, pointer(Abuf)), sz)
        Btile = pointer_to_array(convert(Ptr{R}, pointer(Bbuf)), sz)

        z = zero(R)

        if mA < tile_size && nA < tile_size && nB < tile_size
            Base.copy_transpose!(Atile, 1:nA, 1:mA, tA, A, 1:mA, 1:nA)
            copy!(Btile, 1:mB, 1:nB, tB, B, 1:mB, 1:nB)
            for j = 1:nB
                boff = (j-1)*tile_size
                for i = 1:mA
                    aoff = (i-1)*tile_size
                    s = z
                    for k = 1:nA
                        s += Atile[aoff+k] * Btile[boff+k]
                    end
                    C[i,j] = s
                end
            end
        else
            Ctile = pointer_to_array(convert(Ptr{R}, pointer(Cbuf)), sz)
            for jb = 1:tile_size:nB
                jlim = min(jb+tile_size-1,nB)
                jlen = jlim-jb+1
                for ib = 1:tile_size:mA
                    ilim = min(ib+tile_size-1,mA)
                    ilen = ilim-ib+1
                    fill!(Ctile, z)
                    for kb = 1:tile_size:nA
                        klim = min(kb+tile_size-1,mB)
                        klen = klim-kb+1
                        Base.copy_transpose!(Atile, 1:klen, 1:ilen, tA, A, ib:ilim, kb:klim)
                        copy!(Btile, 1:klen, 1:jlen, tB, B, kb:klim, jb:jlim)
                        for j=1:jlen
                            bcoff = (j-1)*tile_size
                            for i = 1:ilen
                                aoff = (i-1)*tile_size
                                s = z
                                for k = 1:klen
                                    s += Atile[aoff+k] * Btile[bcoff+k]
                                end
                                Ctile[bcoff+i] += s
                            end
                        end
                    end
                    copy!(C, ib:ilim, jb:jlim, Ctile, 1:ilen, 1:jlen)
                end
            end
        end
    else
        # Multiplication for non-plain-data uses the naive algorithm

        if tA == 'N'
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[i, 1]*B[1, j] + A[i, 1]*B[1, j])
                    end
                    for k = 1:nA
                        Ctmp += A[i, k]*B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[i, 1]*B[j, 1] + A[i, 1]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[i, k]*B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[i, 1]*B[j, 1] + A[i, 1]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[i, k]*B[j, k]'
                    end
                    C[i,j] = Ctmp
                end
            end
        elseif tA == 'T'
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[1, j] + A[1, i]*B[1, j])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i].'B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[j, 1] + A[1, i]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i].'B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[j, 1] + A[1, i]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i].'B[j, k]'
                    end
                    C[i,j] = Ctmp
                end
            end
        else
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[1, j] + A[1, i]*B[1, j])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i]'B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[j, 1] + A[1, i]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i]'B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    if length(A) == 0 || length(B) == 0
                        Ctmp = zero(R)
                    else
                        Ctmp = zero(A[1, i]*B[j, 1] + A[1, i]*B[j, 1])
                    end
                    for k = 1:nA
                        Ctmp += A[k, i]'B[j, k]'
                    end
                    C[i,j] = Ctmp
                end
            end
        end
    end
    end # @inbounds
    C
end


# multiply 2x2 matrices
function matmul2x2{T,S}(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
    matmul2x2!(similar(B, promote_type(T,S), 2, 2), tA, tB, A, B)
end

function matmul2x2!{T,S,R}(C::AbstractMatrix{R}, tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
    @inbounds begin
    if tA == 'T'
        A11 = transpose(A[1,1]); A12 = transpose(A[2,1]); A21 = transpose(A[1,2]); A22 = transpose(A[2,2])
    elseif tA == 'C'
        A11 = ctranspose(A[1,1]); A12 = ctranspose(A[2,1]); A21 = ctranspose(A[1,2]); A22 = ctranspose(A[2,2])
    else
        A11 = A[1,1]; A12 = A[1,2]; A21 = A[2,1]; A22 = A[2,2]
    end
    if tB == 'T'
        B11 = transpose(B[1,1]); B12 = transpose(B[2,1]); B21 = transpose(B[1,2]); B22 = transpose(B[2,2])
    elseif tB == 'C'
        B11 = ctranspose(B[1,1]); B12 = ctranspose(B[2,1]); B21 = ctranspose(B[1,2]); B22 = ctranspose(B[2,2])
    else
        B11 = B[1,1]; B12 = B[1,2]; B21 = B[2,1]; B22 = B[2,2]
    end
    C[1,1] = A11*B11 + A12*B21
    C[1,2] = A11*B12 + A12*B22
    C[2,1] = A21*B11 + A22*B21
    C[2,2] = A21*B12 + A22*B22
    end # inbounds
    C
end

# Multiply 3x3 matrices
function matmul3x3{T,S}(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
    matmul3x3!(similar(B, promote_type(T,S), 3, 3), tA, tB, A, B)
end

function matmul3x3!{T,S,R}(C::AbstractMatrix{R}, tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
    @inbounds begin
    if tA == 'T'
        A11 = transpose(A[1,1]); A12 = transpose(A[2,1]); A13 = transpose(A[3,1]);
        A21 = transpose(A[1,2]); A22 = transpose(A[2,2]); A23 = transpose(A[3,2]);
        A31 = transpose(A[1,3]); A32 = transpose(A[2,3]); A33 = transpose(A[3,3]);
    elseif tA == 'C'
        A11 = ctranspose(A[1,1]); A12 = ctranspose(A[2,1]); A13 = ctranspose(A[3,1]);
        A21 = ctranspose(A[1,2]); A22 = ctranspose(A[2,2]); A23 = ctranspose(A[3,2]);
        A31 = ctranspose(A[1,3]); A32 = ctranspose(A[2,3]); A33 = ctranspose(A[3,3]);
    else
        A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3];
        A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3];
        A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3];
    end

    if tB == 'T'
        B11 = transpose(B[1,1]); B12 = transpose(B[2,1]); B13 = transpose(B[3,1]);
        B21 = transpose(B[1,2]); B22 = transpose(B[2,2]); B23 = transpose(B[3,2]);
        B31 = transpose(B[1,3]); B32 = transpose(B[2,3]); B33 = transpose(B[3,3]);
    elseif tB == 'C'
        B11 = ctranspose(B[1,1]); B12 = ctranspose(B[2,1]); B13 = ctranspose(B[3,1]);
        B21 = ctranspose(B[1,2]); B22 = ctranspose(B[2,2]); B23 = ctranspose(B[3,2]);
        B31 = ctranspose(B[1,3]); B32 = ctranspose(B[2,3]); B33 = ctranspose(B[3,3]);
    else
        B11 = B[1,1]; B12 = B[1,2]; B13 = B[1,3];
        B21 = B[2,1]; B22 = B[2,2]; B23 = B[2,3];
        B31 = B[3,1]; B32 = B[3,2]; B33 = B[3,3];
    end

    C[1,1] = A11*B11 + A12*B21 + A13*B31
    C[1,2] = A11*B12 + A12*B22 + A13*B32
    C[1,3] = A11*B13 + A12*B23 + A13*B33

    C[2,1] = A21*B11 + A22*B21 + A23*B31
    C[2,2] = A21*B12 + A22*B22 + A23*B32
    C[2,3] = A21*B13 + A22*B23 + A23*B33

    C[3,1] = A31*B11 + A32*B21 + A33*B31
    C[3,2] = A31*B12 + A32*B22 + A33*B32
    C[3,3] = A31*B13 + A32*B23 + A33*B33
    end # inbounds
    C
end
