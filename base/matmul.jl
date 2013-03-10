# matmul.jl: Everything to do with dense matrix multiplication

# multiply by diagonal matrix as vector
function diagmm!(C::Matrix, A::Matrix, b::Vector)
    m, n = size(A)
    if n != length(b)
        error("argument dimensions do not match")
    end
    for j = 1:n
        bj = b[j]
        for i = 1:m
            C[i,j] = A[i,j]*bj
        end
    end
    return C
end

function diagmm!(C::Matrix, b::Vector, A::Matrix)
    m, n = size(A)
    if m != length(b)
        error("argument dimensions do not match")
    end
    for j=1:n
        for i=1:m
            C[i,j] = A[i,j]*b[i]
        end
    end
    return C
end

diagmm(A::Matrix, b::Vector) =
    diagmm!(Array(promote_type(eltype(A),eltype(b)),size(A)), A, b)

diagmm(b::Vector, A::Matrix) =
    diagmm!(Array(promote_type(eltype(A),eltype(b)),size(A)), b, A)

# Dot products

dot{T<:Union(Float32, Float64)}(x::Vector{T}, y::Vector{T}) = BLAS.dot(x, y)
function dot{T<:BLAS.BlasFloat, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}), y::Vector{T}, ry::Union(Range1{TI},Range{TI}))
    length(rx) != length(ry) ? error("Ranges should be of same length") : true
    if min(rx) < 1 || max(rx) > length(x) || min(ry) < 1 || max(ry) > length(y)
        throw(BoundsError())
    end
    BLAS.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end
function dot(x::Vector, y::Vector)
    s = zero(eltype(x))
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

function (*){T<:BlasFloat}(A::StridedMatrix{T}, X::StridedVector{T})
    Y = similar(A, size(A,1))
    gemv(Y, 'N', A, X)
end

A_mul_B{T<:BlasFloat}(y::StridedVector{T}, A::StridedMatrix{T}, x::StridedVector{T}) = gemv(y, 'N', A, x)
A_mul_B(y::StridedVector, A::StridedMatrix, x::StridedVector) = generic_matvecmul(y, 'N', A, x)

function At_mul_B{T<:BlasFloat}(A::StridedMatrix{T}, x::StridedVector{T})
    y = similar(A, size(A, 2))
    gemv(y, 'T', A, x)
end

At_mul_B{T<:BlasFloat}(y::StridedVector{T}, A::StridedMatrix{T}, x::StridedVector{T}) = gemv(y, 'T', A, x)
At_mul_B(y::StridedVector, A::StridedMatrix, x::StridedVector) = generic_matvecmul(y, 'T', A, x)

Ac_mul_B{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, x::StridedVector{T}) = At_mul_B(A, x)
function Ac_mul_B{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T}, x::StridedVector{T})
    y = similar(A, size(A, 2))
    gemv(y, 'C', A, x)
end
Ac_mul_B(y::StridedVector, A::StridedMatrix, x::StridedVector) = generic_matvecmul(y, 'C', A, x)


# Matrix-matrix multiplication

(*){T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper('N', 'N', A, B)
A_mul_B{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'N', 'N', A, B)
A_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'N', 'N', A, B)

function At_mul_B{T<:BlasFloat}(A::StridedMatrix{T},
                                 B::StridedMatrix{T})
    if is(A, B)
        syrk_wrapper('T', A)
    else
        gemm_wrapper('T', 'N', A, B)
    end
end

At_mul_B{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'T', 'N', A, B)
At_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('T', 'N', A, B)
At_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'T', 'N', A, B)

function A_mul_Bt{T<:BlasFloat}(A::StridedMatrix{T},
                                 B::StridedMatrix{T})
    if is(A, B)
        syrk_wrapper('N', A)
    else
        gemm_wrapper('N', 'T', A, B)
    end
end

A_mul_Bt{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'N', 'T', A, B)
A_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('N', 'T', A, B)
A_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'N', 'T', A, B)

At_mul_Bt{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper('T', 'T', A, B)
At_mul_Bt{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'T', 'T', A, B)
At_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('T', 'T', A, B)
At_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'T', 'T', A, B)

Ac_mul_B{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(A, B)
Ac_mul_B{T<:Union(Float64,Float32)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(C, A, B)

function Ac_mul_B{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                                  B::StridedMatrix{T})
    if is(A, B)
        herk_wrapper('C', A)
    else
        gemm_wrapper('C', 'N', A, B)
    end
end

Ac_mul_B{T<:Union(Complex128,Complex64)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper('C', 'N', A, B)
Ac_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('C', 'N', A, B)
Ac_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'C', 'N', A, B)

A_mul_Bc{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = A_mul_Bt(A, B)
A_mul_Bc{T<:Union(Float64,Float32)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = A_mul_Bt(C, A, B)
function A_mul_Bc{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                                  B::StridedMatrix{T})
    if is(A, B)
        herk_wrapper('N', A)
    else
        gemm_wrapper('N', 'C', A, B)
    end
end
A_mul_Bc{T<:Union(Complex128,Complex64)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'N', 'C', A, B)
A_mul_Bc{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('N', 'C', A, B)
A_mul_Bc{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'N', 'C', A, B)

Ac_mul_Bc{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper('C', 'C', A, B)
Ac_mul_Bc{T<:BlasFloat}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = gemm_wrapper(C, 'C', 'C', A, B)
Ac_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('C', 'C', A, B)
Ac_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul(C, 'C', 'C', A, B)

# Supporting functions for matrix multiplication

function symmetrize!(A::StridedMatrix, UL::BlasChar)
    m, n = size(A)
    if m != n error("symmetrize: Matrix must be square") end
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
        error("Second argument UL should be 'U' or 'L'")
    end
    return A
end

symmetrize!(A) = symmetrize!(A, 'U')

function symmetrize_conj!(A::StridedMatrix, UL::BlasChar)
    m, n = size(A)
    if m != n error("symmetrize: Matrix must be square") end
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
        error("Second argument UL should be 'U' or 'L'")
    end
    return A
end

symmetrize_conj!(A) = symmetrize_conj!(A, 'U')

function gemv{T<:BlasFloat}(y::StridedVector{T},
                             tA,
                             A::StridedMatrix{T},
                             x::StridedVector{T})
    if stride(A, 1) != 1
        return generic_matvecmul(y, tA, A, x)
    end

    if tA != 'N'
        (nA, mA) = size(A)
    else
        (mA, nA) = size(A)
    end

    if nA != length(x); error("*: argument shapes do not match"); end
    if mA != length(y); error("*: output size is incorrect"); end
    if mA == 0; return zeros(T, 0); end

    BLAS.gemv!(tA, one(T), A, x, zero(T), y)
end

function syrk_wrapper{T<:BlasFloat}(tA, A::StridedMatrix{T})
    if tA == 'T'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'T'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        return generic_matmatmul(tA, tAt, A, A)
    end

    symmetrize!(BLAS.syrk('U', tA, one(T), A))
end

function herk_wrapper{T<:BlasFloat}(tA, A::StridedMatrix{T})
    if tA == 'C'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'C'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        return generic_matmatmul(tA, tAt, A, A)
    end

    # Result array does not need to be initialized as long as beta==0
    #    C = Array(T, mA, mA)

    symmetrize_conj!(BLAS.herk('U', tA, one(T), A))
end

function gemm_wrapper{T<:BlasFloat}(tA, tB,
                             A::StridedMatrix{T},
                             B::StridedMatrix{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = Array(T, mA, nB)
    gemm_wrapper(C, tA, tB, A, B)
end

function gemm_wrapper{T<:BlasFloat}(C::StridedMatrix{T}, tA, tB,
                             A::StridedMatrix{T},
                             B::StridedMatrix{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB; error("*: argument shapes do not match"); end

    if mA == 0 || nA == 0 || nB == 0; return zeros(T, mA, nB); end
    if mA == 2 && nA == 2 && nB == 2; return matmul2x2(C,tA,tB,A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3(C,tA,tB,A,B); end

    if stride(A, 1) != 1 || stride(B, 1) != 1
        return generic_matmatmul(C, tA, tB, A, B)
    end

    BLAS.gemm!(tA, tB, one(T), A, B, zero(T), C)
end

# blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::Char, M::StridedVecOrMat) = (t == 'N') ? (size(M, 1), size(M, 2)) : (size(M,2), size(M, 1))

function copy!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        copy!(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copy_transpose!(B, ir_dest, jr_dest, M, jr_src, ir_src)
        if tM == 'C'
            conj!(B)
        end
    end
end

function copy_transpose!{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        copy_transpose!(B, ir_dest, jr_dest, M, ir_src, jr_src)
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
(*){T,S}(A::StridedMatrix{T}, B::StridedVector{S}) = generic_matvecmul('N', A, B)

function generic_matvecmul{T,S}(tA, A::StridedMatrix{T}, B::StridedVector{S})
    C = Array(promote_type(T,S), size(A, tA=='N' ? 1 : 2))
    generic_matvecmul(C, tA, A, B)
end

function generic_matvecmul{T,S,R}(C::StridedVector{R}, tA, A::StridedMatrix{T}, B::StridedVector{S})
    mB = length(B)
    mA, nA = lapack_size(tA, A)
    if nA != mB; error("*: argument shapes do not match"); end
    if length(C) != mA; error("*: output size does not match"); end
    z = zero(R)

    Astride = size(A, 1)

    if tA == 'T'  # fastest case
    for k = 1:mA
        aoffs = (k-1)*Astride
        s = z
        for i = 1:nA
            s += A[aoffs+i] * B[i]
        end
        C[k] = s
    end
elseif tA == 'C'
    for k = 1:mA
        aoffs = (k-1)*Astride
        s = z
        for i = 1:nA
            s += conj(A[aoffs+i]) * B[i]
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
return C
end

(*){T,S}(A::Vector{S}, B::Matrix{T}) = reshape(A,length(A),1)*B

# NOTE: the generic version is also called as fallback for strides != 1 cases
#       in libalg_blas.jl
(*){T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = generic_matmatmul('N', 'N', A, B)

function generic_matmatmul{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = Array(promote_type(T,S), mA, nB)
    generic_matmatmul(C, tA, tB, A, B)
end

const tilebufsize = 10800  # Approximately 32k/3
const Abuf = Array(Uint8, tilebufsize)
const Bbuf = Array(Uint8, tilebufsize)
const Cbuf = Array(Uint8, tilebufsize)

function generic_matmatmul{T,S,R}(C::StridedMatrix{R}, tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if nA != mB; error("*: argument shapes do not match"); end
    if size(C,1) != mA || size(C,2) != nB; error("*: output size is incorrect"); end

    if mA == nA == nB == 2; return matmul2x2(C, tA, tB, A, B); end
    if mA == nA == nB == 3; return matmul3x3(C, tA, tB, A, B); end

    if isbits(R)
        tile_size = int(ifloor(sqrt(tilebufsize/sizeof(R))))
        sz = (tile_size, tile_size)
        Atile = pointer_to_array(convert(Ptr{R}, pointer(Abuf)), sz)
        Btile = pointer_to_array(convert(Ptr{R}, pointer(Bbuf)), sz)

        z = zero(R)

        if mA < tile_size && nA < tile_size && nB < tile_size
            copy_transpose!(Atile, 1:nA, 1:mA, tA, A, 1:mA, 1:nA)
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
                        copy_transpose!(Atile, 1:klen, 1:ilen, tA, A, ib:ilim, kb:klim)
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
                        C[i,j] = Ctmp
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[i, 1]*B[j, 1]
                        for k = 2:nA
                            Ctmp += A[i, k]*B[j, k]
                        end
                        C[i,j] = Ctmp
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[i, 1]*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += A[i, k]*conj(B[j, k])
                        end
                        C[i,j] = Ctmp
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
                        C[i,j] = Ctmp
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[1, i]*B[j, 1]
                        for k = 2:nA
                            Ctmp += A[k, i]*B[j, k]
                        end
                        C[i,j] = Ctmp
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = A[1, i]*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += A[k, i]*conj(B[j, k])
                        end
                        C[i,j] = Ctmp
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
                        C[i,j] = Ctmp
                    end
                end
            elseif tB == 'T'
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = conj(A[1, i])*B[j, 1]
                        for k = 2:nA
                            Ctmp += conj(A[k, i])*B[j, k]
                        end
                        C[i,j] = Ctmp
                    end
                end
            else
                for i = 1:mA
                    for j = 1:nB
                        Ctmp = conj(A[1, i])*conj(B[j, 1])
                        for k = 2:nA
                            Ctmp += conj(A[k, i])*conj(B[j, k])
                        end
                        C[i,j] = Ctmp
                    end
                end
            end
        end
    end
    return C
end


# multiply 2x2 matrices
function matmul2x2{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    matmul2x2(Array(promote_type(T,S), 2, 2), tA, tB, A, B)
end

function matmul2x2{T,S,R}(C::StridedMatrix{R}, tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
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

    C[1,1] = A11*B11 + A12*B21
    C[1,2] = A11*B12 + A12*B22
    C[2,1] = A21*B11 + A22*B21
    C[2,2] = A21*B12 + A22*B22

    return C
end

# Multiply 3x3 matrices
function matmul3x3{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    matmul3x3(Array(promote_type(T,S), 3, 3), tA, tB, A, B)
end

function matmul3x3{T,S,R}(C::StridedMatrix{R}, tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
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

    C[1,1] = A11*B11 + A12*B21 + A13*B31
    C[1,2] = A11*B12 + A12*B22 + A13*B32
    C[1,3] = A11*B13 + A12*B23 + A13*B33

    C[2,1] = A21*B11 + A22*B21 + A23*B31
    C[2,2] = A21*B12 + A22*B22 + A23*B32
    C[2,3] = A21*B13 + A22*B23 + A23*B33

    C[3,1] = A31*B11 + A32*B21 + A33*B31
    C[3,2] = A31*B12 + A32*B22 + A33*B32
    C[3,3] = A31*B13 + A32*B23 + A33*B33

    return C
end
