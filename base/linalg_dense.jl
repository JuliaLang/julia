## linalg_dense.jl: Basic Linear Algebra functions for dense representations ##
#
# note that many functions have specific versions for FloatingPoint/Complex arguments
# which use BLAS instead

Ac_mul_B(x::Vector, y::Vector) = [dot(x, y)]
At_mul_B{T<:Real}(x::Vector{T}, y::Vector{T}) = [dot(x, y)]

function dot(x::Vector, y::Vector)
    s = zero(eltype(x))
    for i=1:length(x)
        s += conj(x[i])*y[i]
    end
    s
end

cross(a::Vector, b::Vector) =
    [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

# linalg_blas.jl defines matmul for floats; other integer and mixed precision
# cases are handled here

lapack_size(t::Char, M::StridedVecOrMat) = (t == 'N') ? (size(M, 1), size(M, 2)) : (size(M,2), size(M, 1))
function copy_to{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        copy_to(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copy_to_transpose(B, ir_dest, jr_dest, M, jr_src, ir_src)
        if tM == 'C'
            conj!(B)
        end
    end
end
function copy_to_transpose{R,S}(B::Matrix{R}, ir_dest::Range1{Int}, jr_dest::Range1{Int}, tM::Char, M::StridedMatrix{S}, ir_src::Range1{Int}, jr_src::Range1{Int})
    if tM == 'N'
        copy_to_transpose(B, ir_dest, jr_dest, M, ir_src, jr_src)
    else
        copy_to(B, ir_dest, jr_dest, M, jr_src, ir_src)
        if tM == 'C'
            conj!(B)
        end
    end
end


# TODO: It will be faster for large matrices to convert to float,
# call BLAS, and convert back to required type.

# NOTE: the _jl_generic version is also called as fallback for
#       strides != 1 cases in libalg_blas.jl
(*){T,S}(A::StridedMatrix{T}, B::StridedVector{S}) = _jl_generic_matvecmul('N', A, B)
function _jl_generic_matvecmul{T,S}(tA, A::StridedMatrix{T}, B::StridedVector{S})
    if tA == 'N'
        C = Array(promote_type(T,S), size(A, 1))
    else
        C = Array(promote_type(T,S), size(A, 2))
    end
    _jl_generic_matvecmul(C, tA, A, B)
end
function _jl_generic_matvecmul{T,S,R}(C::StridedVector{R}, tA, A::StridedMatrix{T}, B::StridedVector{S})
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

# NOTE: the _jl_generic version is also called as fallback for strides != 1 cases
#       in libalg_blas.jl
(*){T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('N', 'N', A, B)
function _jl_generic_matmatmul{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = Array(promote_type(T,S), mA, nB)
    _jl_generic_matmatmul(C, tA, tB, A, B)
end

const tilebufsize = 10800  # Approximately 32k/3
const Abuf = Array(Uint8, tilebufsize)
const Bbuf = Array(Uint8, tilebufsize)
const Cbuf = Array(Uint8, tilebufsize)

function _jl_generic_matmatmul{T,S,R}(C::StridedMatrix{R}, tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    if nA != mB; error("*: argument shapes do not match"); end
    if size(C,1) != mA || size(C,2) != nB; error("*: output size is incorrect"); end

    if mA == nA == nB == 2; return matmul2x2(C, tA, tB, A, B); end
    if mA == nA == nB == 3; return matmul3x3(C, tA, tB, A, B); end

    tile_size = int(ifloor(sqrt(tilebufsize/sizeof(R))))
    sz = (tile_size, tile_size)
    Atile = pointer_to_array(convert(Ptr{R}, pointer(Abuf)), sz)
    Btile = pointer_to_array(convert(Ptr{R}, pointer(Bbuf)), sz)

    z = zero(R)

    if mA < tile_size && nA < tile_size && nB < tile_size
        copy_to_transpose(Atile, 1:nA, 1:mA, tA, A, 1:mA, 1:nA)
        copy_to(Btile, 1:mB, 1:nB, tB, B, 1:mB, 1:nB)
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
                    copy_to_transpose(Atile, 1:klen, 1:ilen, tA, A, ib:ilim, kb:klim)
                    copy_to(Btile, 1:klen, 1:jlen, tB, B, kb:klim, jb:jlim)
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
                copy_to(C, ib:ilim, jb:jlim, Ctile, 1:ilen, 1:jlen)
            end
        end
    end
    return C
end


# multiply 2x2 matrices
function matmul2x2{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    R = promote_type(T,S)
    C = Array(R, 2, 2)
    matmul2x2(C, tA, tB, A, B)
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

function matmul3x3{T,S}(tA, tB, A::StridedMatrix{T}, B::StridedMatrix{S})
    R = promote_type(T,S)
    C = Array(R, 3, 3)
    matmul3x3(C, tA, tB, A, B)
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


triu{T}(M::Matrix{T}, k::Integer) = [ j-i >= k ? M[i,j] : zero(T) for
                                     i=1:size(M,1), j=1:size(M,2) ]
tril{T}(M::Matrix{T}, k::Integer) = [ j-i <= k ? M[i,j] : zero(T) for
                                     i=1:size(M,1), j=1:size(M,2) ]
function triu!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    for i = 1:m
        for j = 1:n
            if j-i < k
                M[i,j] = zero(T)
            end
        end
    end
end
function tril!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    for i = 1:m
        for j = 1:n
            if j-i > k
                M[i,j] = zero(T)
            end
        end
    end
end

diff(a::Vector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

function diff(a::Matrix, dim::Integer)
    if dim == 1
        [ a[i+1,j] - a[i,j] for i=1:size(a,1)-1, j=1:size(a,2) ]
    else
        [ a[i,j+1] - a[i,j] for i=1:size(a,1), j=1:size(a,2)-1 ]
    end
end

function gradient(F::Vector, h::Vector)
    n = length(F)
    g = similar(F)
    if n > 0
        g[1] = 0
    end
    if n > 1
        g[1] = (F[2] - F[1]) / (h[2] - h[1])
        g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
    end
    if n > 2
        h = h[3:n] - h[1:n-2]
        g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
    end
    return g
end

diag(A::Matrix) = [ A[i,i] for i=1:min(size(A,1),size(A,2)) ]

function diagm{T}(v::Union(Vector{T},Matrix{T}))
    if isa(v, Matrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = numel(v)
    a = zeros(T, n, n)
    for i=1:n
        a[i,i] = v[i]
    end

    return a
end

function trace{T}(A::Matrix{T})
    t = zero(T)
    for i=1:min(size(A))
        t += A[i,i]
    end
    return t
end

kron(a::Vector, b::Vector) = [ a[i]*b[j] for i=1:length(a), j=1:length(b) ]

function kron{T,S}(a::Matrix{T}, b::Matrix{S})
    R = Array(promote_type(T,S), size(a,1)*size(b,1), size(a,2)*size(b,2))

    m = 1
    for j = 1:size(a,2)
        for l = 1:size(b,2)
            for i = 1:size(a,1)
                aij = a[i,j]
                for k = 1:size(b,1)
                    R[m] = aij*b[k,l]
                    m += 1
                end
            end
        end
    end
    R
end

function randsym(n)
    a = randn(n,n)
    for j=1:n-1, i=j+1:n
        x = (a[i,j]+a[j,i])/2
        a[i,j] = x
        a[j,i] = x
    end
    a
end

function issym(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:(n-1), j = (i+1):n
        if A[i,j] != A[j,i]
            return false
        end
    end
    return true
end

# Randomized matrix symmetry test
# Theorem: Matrix is symmetric iff x'*A*y == y'*A*x, for randomly chosen x and y.
function issym_rnd(A::Matrix)
    m, n = size(A)
    if m != n; return false; end
    ntrials = 5

    for i=1:ntrials
        x = randn(n)
        y = randn(n)
        if (x'*A*y - y'*A*x)[1] > 1e-6; return false; end
    end

    return true
end

function ishermitian(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:n, j = i:n
        if A[i,j] != conj(A[j,i])
            return false
        end
    end
    return true
end

function istriu(A::Matrix)
    m, n = size(A)
    for j = 1:min(n,m-1), i = j+1:m
        if A[i,j] != 0
            return false
        end
    end
    return true
end

function istril(A::Matrix)
    m, n = size(A)
    for j = 2:n, i = 1:min(j-1,m)
        if A[i,j] != 0
            return false
        end
    end
    return true
end

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

^(A::Matrix, p::Integer) = p < 0 ? inv(A^-p) : power_by_squaring(A,p)

function ^(A::Matrix, p::Number)
    if integer_valued(p)
        ip = integer(real(p))
        if ip < 0
            return inv(power_by_squaring(A, -ip))
        else
            return power_by_squaring(A, ip)
        end
    end
    if size(A,1) != size(A,2)
        error("matrix must be square")
    end
    (v, X) = eig(A)
    if isreal(v) && any(v.<0)
        v = complex(v)
    end
    if ishermitian(A)
        Xinv = X'
    else
        Xinv = inv(X)
    end
    diagmm(X, v.^p)*Xinv
end

function findmax(a::Array)
    m = typemin(eltype(a))
    mi = 0
    for i=1:length(a)
        if a[i] > m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
end

function findmin(a::Array)
    m = typemax(eltype(a))
    mi = 0
    for i=1:length(a)
        if a[i] < m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
end

function rref{T}(A::Matrix{T})
    nr, nc = size(A)
    U = copy_to(similar(A,Float64), A)
    e = eps(norm(U,Inf))
    i = j = 1
    while i <= nr && j <= nc
        (m, mi) = findmax(abs(U[i:nr,j]))
        mi = mi+i - 1
        if m <= e
            U[i:nr,j] = 0
            j += 1
        else
            for k=j:nc
                U[i, k], U[mi, k] = U[mi, k], U[i, k]
            end
            d = U[i,j]
            for k = j:nc
                U[i,k] /= d
            end
            for k = 1:nr
                if k != i
                    d = U[k,j]
                    for l = j:nc
                        U[k,l] -= d*U[i,l]
                    end
                end
            end
            i += 1
            j += 1
        end
    end
    return U
end
