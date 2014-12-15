# matmul.jl: Everything to do with dense matrix multiplication

arithtype(T) = T
arithtype(::Type{Bool}) = Int

# multiply by diagonal matrix as vector
function scale!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector)
    m, n = size(A)
    n == length(b) || throw(DimensionMismatch())
    for j = 1:n
        bj = b[j]
        for i = 1:m
            C[i,j] = A[i,j]*bj
        end
    end
    C
end

function scale!(C::AbstractMatrix, b::AbstractVector, A::AbstractMatrix)
    m, n = size(A)
    m == length(b) || throw(DimensionMismatch())
    for j = 1:n, i = 1:m
        C[i,j] = A[i,j]*b[i]
    end
    C
end
scale(A::Matrix, b::Vector) = scale!(similar(A, promote_type(eltype(A),eltype(b))), A, b)
scale(b::Vector, A::Matrix) = scale!(similar(b, promote_type(eltype(A),eltype(b)), size(A)), b, A)

# Dot products

dot{T<:BlasReal}(x::StridedVector{T}, y::StridedVector{T}) = BLAS.dot(x, y)
dot{T<:BlasComplex}(x::StridedVector{T}, y::StridedVector{T}) = BLAS.dotc(x, y)
function dot{T<:BlasReal, TI<:Integer}(x::Vector{T}, rx::Union(UnitRange{TI},Range{TI}), y::Vector{T}, ry::Union(UnitRange{TI},Range{TI}))
    length(rx)==length(ry) || throw(DimensionMismatch())
    if minimum(rx) < 1 || maximum(rx) > length(x) || minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError())
    end
    BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end
function dot{T<:BlasComplex, TI<:Integer}(x::Vector{T}, rx::Union(UnitRange{TI},Range{TI}), y::Vector{T}, ry::Union(UnitRange{TI},Range{TI}))
    length(rx)==length(ry) || throw(DimensionMismatch())
    if minimum(rx) < 1 || maximum(rx) > length(x) || minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError())
    end
    BLAS.dotc(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end
function dot(x::AbstractVector, y::AbstractVector)
    lx = length(x)
    lx==length(y) || throw(DimensionMismatch())
    if lx == 0
        return zero(eltype(x))*zero(eltype(y))
    end
    s = conj(x[1])*y[1]
    @inbounds for i = 2:lx
        s += conj(x[i])*y[i]
    end
    s
end
dot(x::Number, y::Number) = conj(x) * y
Ac_mul_B(x::AbstractVector, y::AbstractVector) = [dot(x, y)]
At_mul_B{T<:Real}(x::AbstractVector{T}, y::AbstractVector{T}) = [dot(x, y)]
At_mul_B{T<:BlasComplex}(x::StridedVector{T}, y::StridedVector{T}) = [BLAS.dotu(x, y)]

# Matrix-vector multiplication
function (*){T<:BlasFloat,S}(A::StridedMatrix{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    A_mul_B!(similar(x, TS, size(A,1)), A, convert(AbstractVector{TS}, x))
end
function (*){T,S}(A::AbstractMatrix{T}, x::AbstractVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    A_mul_B!(similar(x,TS,size(A,1)),A,x)
end
(*)(A::AbstractVector, B::AbstractMatrix) = reshape(A,length(A),1)*B

A_mul_B!{T<:BlasFloat}(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) = gemv!(y, 'N', A, x)
for elty in (Float32,Float64)
    @eval begin
        function A_mul_B!(y::StridedVector{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, x::StridedVector{$elty})
            Afl = reinterpret($elty,A,(2size(A,1),size(A,2)))
            yfl = reinterpret($elty,y)
            gemv!(yfl,'N',Afl,x)
            return y
        end
    end
end
A_mul_B!(y::StridedVector, A::StridedVecOrMat, x::StridedVector) = generic_matvecmul!(y, 'N', A, x)

function At_mul_B{T<:BlasFloat,S}(A::StridedMatrix{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    At_mul_B!(similar(x,TS,size(A,2)), A, convert(AbstractVector{TS}, x))
end
function At_mul_B{T,S}(A::StridedMatrix{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    At_mul_B!(similar(x,TS,size(A,2)), A, x)
end
At_mul_B!{T<:BlasFloat}(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) = gemv!(y, 'T', A, x)
At_mul_B!(y::StridedVector, A::StridedVecOrMat, x::StridedVector) = generic_matvecmul!(y, 'T', A, x)

function Ac_mul_B{T<:BlasFloat,S}(A::StridedMatrix{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    Ac_mul_B!(similar(x,TS,size(A,2)),A,convert(AbstractVector{TS},x))
end
function Ac_mul_B{T,S}(A::StridedMatrix{T}, x::StridedVector{S})
    TS = promote_type(arithtype(T),arithtype(S))
    Ac_mul_B!(similar(x,TS,size(A,2)), A, x)
end

Ac_mul_B!{T<:BlasReal}(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) = At_mul_B!(y, A, x)
Ac_mul_B!{T<:BlasComplex}(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) = gemv!(y, 'C', A, x)
Ac_mul_B!(y::StridedVector, A::StridedVecOrMat, x::StridedVector) = generic_matvecmul!(y, 'C', A, x)

# Matrix-matrix multiplication

function (*){T,S}(A::AbstractMatrix{T}, B::StridedMatrix{S})
    TS = promote_type(arithtype(T), arithtype(S))
    A_mul_B!(similar(B, TS, (size(A,1), size(B,2))), A, B)
end
A_mul_B!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = gemm_wrapper!(C, 'N', 'N', A, B)
for elty in (Float32,Float64)
    @eval begin
        function A_mul_B!(C::StridedMatrix{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, B::StridedVecOrMat{$elty})
            Afl = reinterpret($elty, A, (2size(A,1), size(A,2)))
            Cfl = reinterpret($elty, C, (2size(C,1), size(C,2)))
            gemm_wrapper!(Cfl, 'N', 'N', Afl, B)
            return C
        end
    end
end
A_mul_B!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'N', 'N', A, B)

function At_mul_B{T,S}(A::AbstractMatrix{T}, B::StridedMatrix{S})
    TS = promote_type(arithtype(T), arithtype(S))
    At_mul_B!(similar(B, TS, (size(A,2), size(B,2))), A, B)
end
At_mul_B!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = is(A,B) ? syrk_wrapper!(C, 'T', A) : gemm_wrapper!(C, 'T', 'N', A, B)
At_mul_B!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'T', 'N', A, B)

function A_mul_Bt{T,S}(A::AbstractMatrix{T}, B::StridedMatrix{S})
    TS = promote_type(arithtype(T), arithtype(S))
    A_mul_Bt!(similar(B, TS, (size(A,1), size(B,1))), A, B)
end
A_mul_Bt!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = is(A,B) ? syrk_wrapper!(C, 'N', A) : gemm_wrapper!(C, 'N', 'T', A, B)
for elty in (Float32,Float64)
    @eval begin
        function A_mul_Bt!(C::StridedMatrix{Complex{$elty}}, A::StridedVecOrMat{Complex{$elty}}, B::StridedVecOrMat{$elty})
            Afl = reinterpret($elty, A, (2size(A,1), size(A,2)))
            Cfl = reinterpret($elty, C, (2size(C,1), size(C,2)))
            gemm_wrapper!(Cfl, 'N', 'T', Afl, B)
            return C
        end
    end
end
A_mul_Bt!(C::StridedVecOrMat, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'N', 'T', A, B)

function At_mul_Bt{T,S}(A::AbstractMatrix{T}, B::StridedVecOrMat{S})
    TS = promote_type(arithtype(T), arithtype(S))
    At_mul_Bt!(similar(B, TS, (size(A,2), size(B,1))), A, B)
end
At_mul_Bt!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = gemm_wrapper!(C, 'T', 'T', A, B)
At_mul_Bt!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'T', 'T', A, B)

Ac_mul_B{T<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(A, B)
Ac_mul_B!{T<:BlasReal}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = At_mul_B!(C, A, B)
function Ac_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    TS = promote_type(arithtype(T), arithtype(S))
    Ac_mul_B!(similar(B, TS, (size(A,2), size(B,2))), A, B)
end
Ac_mul_B!{T<:BlasComplex}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = is(A,B) ? herk_wrapper!(C,'C',A) : gemm_wrapper!(C,'C', 'N', A, B)
Ac_mul_B!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'C', 'N', A, B)

A_mul_Bc{T<:BlasFloat,S<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{S}) = A_mul_Bt(A, B)
A_mul_Bc!{T<:BlasFloat,S<:BlasReal}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{S}) = A_mul_Bt!(C, A, B)
function A_mul_Bc{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    TS = promote_type(arithtype(T),arithtype(S))
    A_mul_Bc!(similar(B,TS,(size(A,1),size(B,1))),A,B)
end
A_mul_Bc!{T<:BlasComplex}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = is(A,B) ? herk_wrapper!(C, 'N', A) : gemm_wrapper!(C, 'N', 'C', A, B)
A_mul_Bc!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'N', 'C', A, B)

Ac_mul_Bc{T,S}(A::AbstractMatrix{T}, B::StridedMatrix{S}) = Ac_mul_Bc!(similar(B, promote_type(arithtype(T), arithtype(S)), (size(A,2), size(B,1))), A, B)
Ac_mul_Bc!{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = gemm_wrapper!(C, 'C', 'C', A, B)
Ac_mul_Bc!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'C', 'C', A, B)
Ac_mul_Bt{T,S}(A::AbstractMatrix{T}, B::StridedMatrix{S}) = Ac_mul_Bt(similar(B, promote_type(arithtype(A), arithtype(B)), (size(A,2), size(B,1))), A, B)
Ac_mul_Bt!(C::StridedMatrix, A::StridedVecOrMat, B::StridedVecOrMat) = generic_matmatmul!(C, 'C', 'T', A, B)

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
    else
        throw(ArgumentError("second argument must be 'U' or 'L'"))
    end
    A
end

function gemv!{T<:BlasFloat}(y::StridedVector{T}, tA::Char, A::StridedVecOrMat{T}, x::StridedVector{T})
    stride(A, 1)==1 || return generic_matvecmul!(y, tA, A, x)
    mA, nA = lapack_size(tA, A)
    nA==length(x) || throw(DimensionMismatch())
    mA==length(y) || throw(DimensionMismatch())
    mA == 0 && return y
    nA == 0 && return fill!(y,0)
    return BLAS.gemv!(tA, one(T), A, x, zero(T), y)
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
    nC == mA || throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    if mA == 0 || nA == 0; return fill!(C,0); end
    if mA == 2 && nA == 2; return matmul2x2!(C,tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3!(C,tA,tAt,A,A); end

    stride(A, 1) == 1 || (return generic_matmatmul!(C, tA, tAt, A, A))
    copytri!(BLAS.syrk!('U', tA, one(T), A, zero(T), C), 'U')
end

function herk_wrapper!{T<:BlasFloat}(C::StridedMatrix{T}, tA::Char, A::StridedVecOrMat{T})
    nC = chksquare(C)
    if tA == 'C'
        (nA, mA) = size(A,1), size(A,2)
        tAt = 'N'
    else
        (mA, nA) = size(A,1), size(A,2)
        tAt = 'C'
    end
    nC == mA || throw(DimensionMismatch("output matrix has size: $(nC), but should have size $(mA)"))
    if mA == 0 || nA == 0; return fill!(C,0); end
    if mA == 2 && nA == 2; return matmul2x2!(C,tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3!(C,tA,tAt,A,A); end

    stride(A, 1) == 1 || (return generic_matmatmul!(C,tA, tAt, A, A))

    # Result array does not need to be initialized as long as beta==0
    #    C = Array(T, mA, mA)

    copytri!(BLAS.herk!('U', tA, one(T), A, zero(T), C), 'U', true)
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

    nA==mB || throw(DimensionMismatch("*"))

    if mA == 0 || nA == 0 || nB == 0
        size(C) == (mA, nB) || throw(DimensionMismatch())
        return fill!(C,0)
    end
    if mA == 2 && nA == 2 && nB == 2; return matmul2x2!(C,tA,tB,A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3!(C,tA,tB,A,B); end

    stride(A, 1)==stride(B, 1)==1 || (return generic_matmatmul!(C, tA, tB, A, B))
    BLAS.gemm!(tA, tB, one(T), A, B, zero(T), C)
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
end

# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the generic version is also called as fallback for
#       strides != 1 cases

function generic_matvecmul!{T,S,R}(C::AbstractVector{R}, tA, A::AbstractVecOrMat{T}, B::AbstractVector{S})
    mB = length(B)
    mA, nA = lapack_size(tA, A)
    mB==nA || throw(DimensionMismatch("*"))
    mA==length(C) || throw(DimensionMismatch("*"))
    z = zero(R)

    Astride = size(A, 1)

    if tA == 'T'  # fastest case
        for k = 1:mA
            aoffs = (k-1)*Astride
            s = z
            for i = 1:nA
                s += A[aoffs+i].'B[i]
            end
            C[k] = s
        end
    elseif tA == 'C'
        for k = 1:mA
            aoffs = (k-1)*Astride
            s = z
            for i = 1:nA
                s += A[aoffs+i]'B[i]
            end
            C[k] = s
        end
    else # tA == 'N'
        fill!(C, z)
        for k = 1:mB
            aoffs = (k-1)*Astride
            b = B[k]
            for i = 1:mA
                C[i] += A[aoffs+i] * b
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
    mB==nA || throw(DimensionMismatch("*"))
    if size(C,1) != mA || size(C,2) != nB; throw(DimensionMismatch("*")); end

    if mA == nA == nB == 2; return matmul2x2!(C, tA, tB, A, B); end
    if mA == nA == nB == 3; return matmul3x3!(C, tA, tB, A, B); end

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
                    Ctmp = A[i, 1]*B[1, j]
                    for k = 2:nA
                        Ctmp += A[i, k]*B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    Ctmp = A[i, 1]*B[j, 1].'
                    for k = 2:nA
                        Ctmp += A[i, k]*B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    Ctmp = A[i, 1]*B[j, 1]'
                    for k = 2:nA
                        Ctmp += A[i, k]*B[j, k]'
                    end
                    C[i,j] = Ctmp
                end
            end
        elseif tA == 'T'
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i].'B[1, j]
                    for k = 2:nA
                        Ctmp += A[k, i].'B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i].'B[j, 1].'
                    for k = 2:nA
                        Ctmp += A[k, i].'B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i].'B[j, 1]'
                    for k = 2:nA
                        Ctmp += A[k, i].'B[j, k]'
                    end
                    C[i,j] = Ctmp
                end
            end
        else
            if tB == 'N'
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i]'B[1, j]
                    for k = 2:nA
                        Ctmp += A[k, i]'B[k, j]
                    end
                    C[i,j] = Ctmp
                end
            elseif tB == 'T'
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i]'B[j, 1].'
                    for k = 2:nA
                        Ctmp += A[k, i]'B[j, k].'
                    end
                    C[i,j] = Ctmp
                end
            else
                for i = 1:mA, j = 1:nB
                    Ctmp = A[1, i]'B[j, 1]'
                    for k = 2:nA
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
    C
end

# Multiply 3x3 matrices
function matmul3x3{T,S}(tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
    matmul3x3!(similar(B, promote_type(T,S), 3, 3), tA, tB, A, B)
end

function matmul3x3!{T,S,R}(C::AbstractMatrix{R}, tA, tB, A::AbstractMatrix{T}, B::AbstractMatrix{S})
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

    C
end
