# matmul.jl: Everything to do with dense matrix multiplication

# multiply by diagonal matrix as vector
function scale!(C::Matrix, A::Matrix, b::Vector)
    m, n = size(A)
    if n != length(b)
        throw(DimensionMismatch(""))
    end
    for j = 1:n
        bj = b[j]
        for i = 1:m
            C[i,j] = A[i,j]*bj
        end
    end
    return C
end

function scale!(C::Matrix, b::Vector, A::Matrix)
    m, n = size(A)
    if m != length(b)
        throw(DimensionMismatch(""))
    end
    for j=1:n
        for i=1:m
            C[i,j] = A[i,j]*b[i]
        end
    end
    return C
end

scale(A::Matrix, b::Vector) =
    scale!(Array(promote_type(eltype(A),eltype(b)),size(A)), A, b)

scale(b::Vector, A::Matrix) =
    scale!(Array(promote_type(eltype(A),eltype(b)),size(A)), b, A)

# Dot products

dot{T<:Union(Float32, Float64)}(x::Vector{T}, y::Vector{T}) = BLAS.dot(x, y)
function dot{T<:BLAS.BlasFloat, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}), y::Vector{T}, ry::Union(Range1{TI},Range{TI}))
    length(rx) != length(ry) ? throw(DimensionMismatch("ranges should be of same length")) : true
    if minimum(rx) < 1 || maximum(rx) > length(x) || minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError())
    end
    BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end
function dot(x::AbstractVector, y::AbstractVector)
    if length(x) != length(y)
        throw(DimensionMismatch(""))
    end
    s = zero(eltype(x))*zero(eltype(y))
    for i=1:length(x)
        s += conj(x[i])*y[i]
    end
    s
end
dot(x::Number, y::Number) = conj(x) * y
Ac_mul_B(x::Vector, y::Vector) = [dot(x, y)]
At_mul_B{T<:Real}(x::Vector{T}, y::Vector{T}) = [dot(x, y)]

dot(x::Number, y::Number) = conj(x) * y

# Matrix-vector multiplication
function A_mul_B!{T<:BlasFloat, S<:BlasFloat}(tA::Char, α::Number, A::StridedMatrix{T}, x::StridedVector{S}, β::Number, y::StridedVector)
    TS = promote_type(T,S)
    return A_mul_B!(tA, convert(TS, α), convert(Matrix{TS}, A), convert(Vector{TS}, x), convert(TS, β), y)
end
function A_mul_B!{T<:BlasFloat}(tA::Char, α::T, A::StridedMatrix{T}, x::StridedVector{T}, β::T, y::StridedVector{T})
    if stride(A, 1) != 1
        return generic_matvecmul!(tA, α, A, x, β, y)
    end

    if tA != 'N'
        (nA, mA) = size(A)
    else
        (mA, nA) = size(A)
    end

    if nA != length(x); throw(DimensionMismatch("Argument shapes do not match")); end
    if mA != length(y); throw(DimensionMismatch("Output size is incorrect")); end
    if mA == 0; return y; end
    if nA == 0; return y; end

    BLAS.gemv!(tA, α, A, x, β, y)
end
A_mul_B!(tA::Char, α::Number, A::StridedMatrix, x::StridedVector, β::Number, y::StridedVector) = generic_matvecmul!(tA, α, A, x, β, y)

A_mul_B!(α, A::StridedMatrix, x::StridedVector, β, y::StridedVector) = A_mul_B!('N', α, A, x, β, y)
Ac_mul_B!(α, A::StridedMatrix, x::StridedVector, β, y::StridedVector) = A_mul_B!('C', α, A, x, β, y)
At_mul_B!(α, A::StridedMatrix, x::StridedVector, β, y::StridedVector) = A_mul_B!('T', α, A, x, β, y)

function (*){T,S}(A::StridedVecOrMat{T}, x::StridedVector{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_B!('N', one(TS), A, x, zero(TS), zeros(TS, size(A,1)))
end
function Ac_mul_B{T,S}(A::StridedMatrix{T}, x::StridedVector{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_B!('C', one(TS), A, x, zero(TS), zeros(TS, size(A,2)))
end
function At_mul_B{T,S}(A::StridedMatrix{T}, x::StridedVector{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_B!('T', one(TS), A, x, zero(TS), zeros(TS, size(A,2)))
end
(*)(a::StridedVector, B::StridedMatrix) = reshape(a,length(a),1)*B

# Matrix-matrix multiplication

function syrk!{T<:BlasFloat}(tA::Char, α::Number, A::StridedMatrix{T}, β::Number, C::StridedMatrix)
    if tA == 'T'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'T'
    end

    if mA == 0 || nA == 0; return β*C; end
    if mA == 2 && nA == 2; return matmul2x2!(tA, tAt, α, A, A, β, C); end
    if mA == 3 && nA == 3; return matmul3x3!(tA, tAt, α, A, A, β, C); end

    if stride(A, 1) != 1
        return generic_matmatmul!(tA, tAt, α, A, A, β, C)
    end

    symmetrize!(BLAS.syrk!('U', tA, convert(T, α), A, convert(T, β), C))
end

function herk!{T<:BlasComplex}(tA::Char, α::Number, A::StridedMatrix{T}, β::Number, C::StridedMatrix)
    if tA == 'C'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'C'
    end

    if mA == 0 || nA == 0; return β*C; end
    if mA == 2 && nA == 2; return matmul2x2!(tA, tAt, α, A, A, β, C); end
    if mA == 3 && nA == 3; return matmul3x3!(tA, tAt, α, A, A, β, C); end

    if stride(A, 1) != 1
        return generic_matmatmul!(tA, tAt, α, A, A, β, C)
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Array(T, mA, mA)

    symmetrize_conj!(BLAS.herk!('U', tA, convert(T, α), A, convert(T, β), C))
end

function gemm!{T<:BlasFloat, S<:BlasFloat}(tA::Char, tB::Char, α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    TS = promote_type(T,S)
    return gemm!(tA, tB, convert(TS, α), convert(Matrix{TS}, A), convert(Matrix{TS}, B), convert(TS, β), C)
end
function gemm!{T<:BlasFloat}(tA::Char, tB::Char, α::T, A::StridedVecOrMat{T}, B::StridedMatrix{T}, β::T, C::StridedVecOrMat{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB; throw(DimensionMismatch("")); end

    if mA == 0 || nA == 0 || nB == 0; return β*C; end
    if mA == 2 && nA == 2 && nB == 2; return matmul2x2!(tA, tB, α, A, B, β, C); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3!(tA, tB, α, A, B, β, C); end

    if stride(A, 1) != 1 || stride(B, 1) != 1
        return generic_matmatmul!(tA, tB, α, A, B, β, C)
    end

    BLAS.gemm!(tA, tB, α, A, B, β, C)
end

A_mul_B!{T<:BlasFloat,S<:BlasFloat}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix) = gemm!('N', 'N', α, A, B, β, C)    
A_mul_B!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('N', 'N', α, A, B, β, C)
function Ac_mul_B!{T<:BlasComplex,S<:BlasComplex}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return herk!('C', α, A, β, C) end
    return gemm!('C', 'N', α, A, B, β, C)
end
function Ac_mul_B!{T<:BlasReal,S<:BlasReal}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return syrk!('T', α, A, β, C) end
    return gemm!('T', 'N', α, A, B, β, C)
end
Ac_mul_B!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('C', 'N', α, A, B, β, C)
function At_mul_B!{T<:BlasFloat,S<:BlasFloat}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return syrk!('T', α, A, β, C) end
    return gemm!('T', 'N', α, A, B, β, C)
end
At_mul_B!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('T', 'N', α, A, B, β, C)
function A_mul_Bc!{T<:BlasComplex,S<:BlasComplex}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return herk!('N', α, A, β, C) end
    gemm!('N', 'C', α, A, B, β, C)
end
function A_mul_Bc!{T<:BlasReal,S<:BlasReal}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return syrk!('N', α, A, β, C) end
    gemm!('N', 'C', α, A, B, β, C)
end
A_mul_Bc!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('N', 'C', α, A, B, β, C)
function A_mul_Bt!{T<:BlasFloat,S<:BlasFloat}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    if is(A,B) return syrk!('N', α, A, β, C) end
    gemm!('N', 'T', α, A, B, β, C)
end
A_mul_Bt!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('N', 'T', α, A, B, β, C)
function Ac_mul_Bc!{T<:BlasComplex,S<:BlasComplex}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    gemm!('C', 'C', α, A, B, β, C)
end
function Ac_mul_Bc!{T<:BlasReal,S<:BlasReal}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    gemm!('C', 'C', α, A, B, β, C)
end
Ac_mul_Bc!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('C', 'C', α, A, B, β, C)
function At_mul_Bt!{T<:BlasFloat,S<:BlasFloat}(α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix)
    gemm!('T', 'T', α, A, B, β, C)
end
At_mul_Bt!(α, A::StridedMatrix, B::StridedMatrix, β, C::StridedMatrix) = generic_matmatmul!('T', 'T', α, A, B, β, C)


function (*){T,S}(A::StridedVecOrMat{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_B!(one(TS), A, B, zero(TS), zeros(TS, size(A,1), size(B,2)))
end
function Ac_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return Ac_mul_B!(one(TS), A, B, zero(TS), zeros(TS, size(A,2), size(B,2)))
end
function At_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return At_mul_B!(one(TS), A, B, zero(TS), zeros(TS, size(A,2), size(B,2)))
end
function A_mul_Bc{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_Bc!(one(TS), A, B, zero(TS), zeros(TS, size(A,1), size(B,1)))
end
function A_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return A_mul_Bt!(one(TS), A, B, zero(TS), zeros(TS, size(A,1), size(B,1)))
end
function Ac_mul_Bc{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return Ac_mul_Bc!(one(TS), A, B, zero(TS), zeros(TS, size(A,2), size(B,1)))
end
function At_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S})
    oneT, oneS = one(T), one(S)
    TS = typeof(oneT*oneS + oneT*oneS)
    return At_mul_Bt!(one(TS), A, B, zero(TS), zeros(TS, size(A,2), size(B,1)))
end
# Supporting functions for matrix multiplication

function symmetrize!(A::StridedMatrix, UL::BlasChar)
    m, n = size(A)
    if m != n throw(DimensionMismatch("Symmetrize: Matrix must be square")) end
    if UL == 'U'
        for i = 1:(n-1)
            for j = (i+1):n
                A[j,i] = A[i,j]
            end
        end
    elseif UL == 'L'
        for i = 1:(n-1)
            for j = (i+1):n
                 A[i,j] = A[j,i]
            end
        end
    else
        error("second argument UL should be 'U' or 'L'")
    end
    return A
end

symmetrize!(A) = symmetrize!(A, 'U')

function symmetrize_conj!(A::StridedMatrix, UL::BlasChar)
    m, n = size(A)
    if m != n throw(DimensionMismatch("symmetrize: Matrix must be square")) end
    if UL == 'U'
        for i = 1:(n-1)
            for j = (i+1):n
                A[j,i] = conj(A[i,j])
            end
        end
    elseif UL == 'L'
        for i = 1:(n-1)
            for j = (i+1):n
                 A[i,j] = conj(A[j,i])
            end
        end
    else
        error("second argument UL should be 'U' or 'L'")
    end
    return A
end

symmetrize_conj!(A) = symmetrize_conj!(A, 'U')

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::Char, M::StridedVecOrMat) = (size(M, t=='N' ? 1:2), size(M, t=='N' ? 2:1))

function copy!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        copy!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        Base.copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        if tM == 'C'
            conj!(B)
        end
    end
end

function copy_transpose!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        Base.copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copy!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        if tM == 'C'
            conj!(B)
        end
    end
end

# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the generic version is also called as fallback for
#       strides != 1 cases in libalg_blas.jl

function generic_matvecmul!{T,S,R}(tA, α::Number, A::StridedMatrix{T}, x::StridedVector{S}, β::Number, y::StridedVector{R})
    mx = length(x)
    my = length(y)
    mA, nA = lapack_size(tA, A)
    if nA != mx; throw(DimensionMismatch("")); end
    if my != mA; throw(DimensionMismatch("output size does not match")); end
    z = zero(R)

    Astride = size(A, 1)

    if tA == 'T'  # fastest case
        for k = 1:mA
            aoffs = (k-1)*Astride
            s = z
            for i = 1:nA
                s += A[aoffs+i]*x[i]
            end
            y[k] = α*s + β*y[k]
        end
    elseif tA == 'C'
        for k = 1:mA
            aoffs = (k-1)*Astride
            s = z
            for i = 1:nA
                s += conj(A[aoffs+i])*x[i]
            end
            y[k] = α*s + β*y[k]
        end
    else # tA == 'N'
        for i = 1:my
            y[i] *= β
        end
        for k = 1:mx
            aoffs = (k-1)*Astride
            xk = x[k]
            for i = 1:mA
                y[i] += α*A[aoffs+i]*xk
            end
        end
    end
    return y
end

# NOTE: the generic version is also called as fallback for strides != 1 cases
#       in blas.jl

const tilebufsize = 10800  # Approximately 32k/3
const Abuf = Array(Uint8, tilebufsize)
const Bbuf = Array(Uint8, tilebufsize)
const Cbuf = Array(Uint8, tilebufsize)

function generic_matmatmul!{T,S,R}(tA::Char, tB::Char, α::Number, A::StridedVecOrMat{T}, B::StridedMatrix{S}, β::Number, C::StridedVecOrMat{R})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if nA != mB; throw(DimensionMismatch("Argument shapes do not match")); end
    if size(C,1) != mA || size(C,2) != nB; throw(DimensionMismatch("Output size is incorrect")); end

    if mA == nA == nB == 2; return matmul2x2!(tA, tB, α, A, B, β, C); end
    if mA == nA == nB == 3; return matmul3x3!(tA, tB, α, A, B, β, C); end

    @inbounds begin
    if isbits(R)
        tile_size = int(ifloor(sqrt(tilebufsize/sizeof(R))))
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
                    C[i,j] = α*s + β*C[i,j]
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
                    copy!(Ctile, 1:ilen, 1:jlen, C, ib:ilim, jb:jlim)
                    Ctile *= β
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
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[i, 1]*B[1, j]
                        for k = 2:nA
                            Ctmp += A[i, k]*B[k, j]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[i, 1]*B[j, 1]
                        for k = 2:nA
                            Ctmp += A[i, k]*B[j, k]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[i, 1]*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += A[i, k]*conj(B[j, k])
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            end
        elseif tA == 'T'
            if tB == 'N'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[1, i]*B[1, j]
                        for k = 2:nA
                            Ctmp += A[k, i]*B[k, j]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[1, i]*B[j, 1]
                        for k = 2:nA
                            Ctmp += A[k, i]*B[j, k]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[1, i]*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += A[k, i]*conj(B[j, k])
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            end
        else
            if tB == 'N'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = conj(A[1, i])*B[1, j]
                        for k = 2:nA
                            Ctmp += conj(A[k, i])*B[k, j]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = conj(A[1, i])*B[j, 1]
                        for k = 2:nA
                            Ctmp += conj(A[k, i])*B[j, k]
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = conj(A[1, i])*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += conj(A[k, i])*conj(B[j, k])
                        end
                        C[i,j] = α*Ctmp + β*C[i,j]
                    end
                end
            end
        end
    end
    end # @inbounds
    return C
end


# multiply 2x2 matrices
function matmul2x2!{T,S,R}(tA, tB, α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix{R})
    if tA == 'T'
        A11 = A[1,1]; A12 = A[2,1]; A21 = A[1,2]; A22 = A[2,2]
    elseif tA == 'C'
        A11 = conj(A[1,1]); A12 = conj(A[2,1]); A21 = conj(A[1,2]); A22 = conj(A[2,2])
    else
        A11 = A[1,1]; A12 = A[1,2]; A21 = A[2,1]; A22 = A[2,2]
    end
    if tB == 'T'
        B11 = B[1,1]; B12 = B[2,1]; B21 = B[1,2]; B22 = B[2,2]
    elseif tB == 'C'
        B11 = conj(B[1,1]); B12 = conj(B[2,1]); B21 = conj(B[1,2]); B22 = conj(B[2,2])
    else
        B11 = B[1,1]; B12 = B[1,2]; B21 = B[2,1]; B22 = B[2,2]
    end

    C[1,1] = α*(A11*B11 + A12*B21) + β*C[1,1]
    C[1,2] = α*(A11*B12 + A12*B22) + β*C[1,2]
    C[2,1] = α*(A21*B11 + A22*B21) + β*C[2,1]
    C[2,2] = α*(A21*B12 + A22*B22) + β*C[2,2]

    return C
end

# Multiply 3x3 matrices
function matmul3x3!{T,S,R}(tA, tB, α::Number, A::StridedMatrix{T}, B::StridedMatrix{S}, β::Number, C::StridedMatrix{R})
    if tA == 'T'
        A11 = A[1,1]; A12 = A[2,1]; A13 = A[3,1];
        A21 = A[1,2]; A22 = A[2,2]; A23 = A[3,2];
        A31 = A[1,3]; A32 = A[2,3]; A33 = A[3,3];
    elseif tA == 'C'
        A11 = conj(A[1,1]); A12 = conj(A[2,1]); A13 = conj(A[3,1]);
        A21 = conj(A[1,2]); A22 = conj(A[2,2]); A23 = conj(A[3,2]);
        A31 = conj(A[1,3]); A32 = conj(A[2,3]); A33 = conj(A[3,3]);
    else
        A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3];
        A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3];
        A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3];
    end

    if tB == 'T'
        B11 = B[1,1]; B12 = B[2,1]; B13 = B[3,1];
        B21 = B[1,2]; B22 = B[2,2]; B23 = B[3,2];
        B31 = B[1,3]; B32 = B[2,3]; B33 = B[3,3];
    elseif tB == 'C'
        B11 = conj(B[1,1]); B12 = conj(B[2,1]); B13 = conj(B[3,1]);
        B21 = conj(B[1,2]); B22 = conj(B[2,2]); B23 = conj(B[3,2]);
        B31 = conj(B[1,3]); B32 = conj(B[2,3]); B33 = conj(B[3,3]);
    else
        B11 = B[1,1]; B12 = B[1,2]; B13 = B[1,3];
        B21 = B[2,1]; B22 = B[2,2]; B23 = B[2,3];
        B31 = B[3,1]; B32 = B[3,2]; B33 = B[3,3];
    end

    C[1,1] = α*(A11*B11 + A12*B21 + A13*B31) + β*C[1,1]
    C[1,2] = α*(A11*B12 + A12*B22 + A13*B32) + β*C[1,2]
    C[1,3] = α*(A11*B13 + A12*B23 + A13*B33) + β*C[1,3]

    C[2,1] = α*(A21*B11 + A22*B21 + A23*B31) + β*C[2,1]
    C[2,2] = α*(A21*B12 + A22*B22 + A23*B32) + β*C[2,2]
    C[2,3] = α*(A21*B13 + A22*B23 + A23*B33) + β*C[2,3]

    C[3,1] = α*(A31*B11 + A32*B21 + A33*B31) + β*C[3,1]
    C[3,2] = α*(A31*B12 + A32*B22 + A33*B32) + β*C[3,2]
    C[3,3] = α*(A31*B13 + A32*B23 + A33*B33) + β*C[3,3]

    return C
end
