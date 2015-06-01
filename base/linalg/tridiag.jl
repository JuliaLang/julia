# This file is a part of Julia. License is MIT: http://julialang.org/license

#### Specialized matrix types ####

## (complex) symmetric tridiagonal matrices
immutable SymTridiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # subdiagonal
    function SymTridiagonal(dv::Vector{T}, ev::Vector{T})
        length(dv) - 1 <= length(ev) <= length(dv) || throw(DimensionMismatch("subdiagonal has wrong length. Has length $(length(ev)), but should be either $(length(dv) - 1) or $(length(dv))."))
        new(dv,ev)
    end
end

SymTridiagonal{T}(dv::Vector{T}, ev::Vector{T}) = SymTridiagonal{T}(dv, ev)

function SymTridiagonal{Td,Te}(dv::Vector{Td}, ev::Vector{Te})
    T = promote_type(Td,Te)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end

SymTridiagonal(A::AbstractMatrix) = diag(A,1)==diag(A,-1)?SymTridiagonal(diag(A), diag(A,1)):throw(DimensionMismatch("matrix is not symmetric; cannot convert to SymTridiagonal"))
full{T}(M::SymTridiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{SymTridiagonal{T}}, S::SymTridiagonal) = SymTridiagonal(convert(Vector{T}, S.dv), convert(Vector{T}, S.ev))
convert{T}(::Type{AbstractMatrix{T}}, S::SymTridiagonal) = SymTridiagonal(convert(Vector{T}, S.dv), convert(Vector{T}, S.ev))
function convert{T}(::Type{Matrix{T}}, M::SymTridiagonal{T})
    n = size(M, 1)
    Mf = zeros(T, n, n)
    @inbounds begin
        @simd for i = 1:n-1
            Mf[i,i] = M.dv[i]
            Mf[i+1,i] = M.ev[i]
            Mf[i,i+1] = M.ev[i]
        end
        Mf[n,n] = M.dv[n]
    end
    return Mf
end
convert{T}(::Type{Matrix}, M::SymTridiagonal{T}) = convert(Matrix{T}, M)

size(A::SymTridiagonal) = (length(A.dv), length(A.dv))
size(A::SymTridiagonal, d::Integer) = d<1 ? throw(ArgumentError("dimension must be ≥ 1, got $d")) : (d<=2 ? length(A.dv) : 1)

#Elementary operations
for func in (:conj, :copy, :round, :trunc, :floor, :ceil)
    @eval ($func)(M::SymTridiagonal) = SymTridiagonal(($func)(M.dv), ($func)(M.ev))
end
for func in (:round, :trunc, :floor, :ceil)
    @eval ($func){T<:Integer}(::Type{T},M::SymTridiagonal) = SymTridiagonal(($func)(T,M.dv), (T,$func)(M.ev))
end
transpose(M::SymTridiagonal) = M #Identity operation
ctranspose(M::SymTridiagonal) = conj(M)

function diag{T}(M::SymTridiagonal{T}, n::Integer=0)
    absn = abs(n)
    if absn == 0
        return M.dv
    elseif absn==1
        return M.ev
    elseif absn<size(M,1)
        return zeros(T,size(M,1)-absn)
    else
        throw(BoundsError("$n-th diagonal of a $(size(M)) matrix doesn't exist!"))
    end
end

+(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv+B.dv, A.ev+B.ev)
-(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv-B.dv, A.ev-B.ev)
*(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv*B, A.ev*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv/B, A.ev/B)
==(A::SymTridiagonal, B::SymTridiagonal) = (A.dv==B.dv) && (A.ev==B.ev)

function A_mul_B!(C::StridedVecOrMat, S::SymTridiagonal, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if !(m == size(S, 1) == size(C, 1))
        throw(DimensionMismatch("A has first dimension $(size(A,1)), B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if n != size(C, 2)
        throw(DimensionMismatch("Second dimension of B, $n, doesn't match second dimension of C, $(size(C,2))"))
    end

    α = S.dv
    β = S.ev
    @inbounds begin
        for j = 1:n
            x₀, x₊ = B[1, j], B[2, j]
            β₀ = β[1]
            C[1, j] = α[1]*x₀ + x₊*β₀
            for i = 2:m - 1
                x₋, x₀, x₊ = x₀, x₊, B[i + 1, j]
                β₋, β₀ = β₀, β[i]
                C[i, j] = β₋*x₋ + α[i]*x₀ + β₀*x₊
            end
            C[m, j] = β₀*x₀ + α[m]*x₊
        end
    end

    return C
end

factorize(S::SymTridiagonal) = ldltfact(S)

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}) = Eigen(LAPACK.stegr!('V', A.dv, A.ev)...)
eigfact{T}(A::SymTridiagonal{T}) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(SymTridiagonal{S}, A) : copy(A)))

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) = Eigen(LAPACK.stegr!('V', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)...)
eigfact{T}(A::SymTridiagonal{T}, irange::UnitRange) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), irange))

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) = Eigen(LAPACK.stegr!('V', A.dv, A.ev, convert(T, vl), convert(T, vu), 0, 0)...)
eigfact{T}(A::SymTridiagonal{T}, vl::Real, vu::Real) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), vl, vu))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}) = LAPACK.stev!('N', A.dv, A.ev)[1]
eigvals{T}(A::SymTridiagonal{T}) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A)))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) = LAPACK.stegr!('N', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)[1]
eigvals{T}(A::SymTridiagonal{T}, irange::UnitRange) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), irange))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) = LAPACK.stegr!('N', 'V', A.dv, A.ev, vl, vu, 0, 0)[1]
eigvals{T}(A::SymTridiagonal{T}, vl::Real, vu::Real) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), vl, vu))

#Computes largest and smallest eigenvalue
eigmax(A::SymTridiagonal) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::SymTridiagonal) = eigvals(A, 1:1)[1]

#Compute selected eigenvectors only corresponding to particular eigenvalues
eigvecs(A::SymTridiagonal) = eigfact(A)[:vectors]
eigvecs{T<:BlasFloat,Eigenvalue<:Real}(A::SymTridiagonal{T}, eigvals::Vector{Eigenvalue}) = LAPACK.stein!(A.dv, A.ev, eigvals)

###################
# Generic methods #
###################

#Needed for inv_usmani()
type ZeroOffsetVector
    data::Vector
end
getindex (a::ZeroOffsetVector, i) = a.data[i+1]
setindex!(a::ZeroOffsetVector, x, i) = a.data[i+1]=x

#Implements the inverse using the recurrence relation between principal minors
# a, b, c are assumed to be the subdiagonal, diagonal, and superdiagonal of
# a tridiagonal matrix.
#Reference:
#    R. Usmani, "Inversion of a tridiagonal Jacobi matrix",
#    Linear Algebra and its Applications 212-213 (1994), pp.413-414
#    doi:10.1016/0024-3795(94)90414-6
function inv_usmani{T}(a::Vector{T}, b::Vector{T}, c::Vector{T})
    n = length(b)
    θ = ZeroOffsetVector(zeros(T, n+1)) #principal minors of A
    θ[0] = 1
    n>=1 && (θ[1] = b[1])
    for i=2:n
        θ[i] = b[i]*θ[i-1]-a[i-1]*c[i-1]*θ[i-2]
    end
    φ = zeros(T, n+1)
    φ[n+1] = 1
    n>=1 && (φ[n] = b[n])
    for i=n-1:-1:1
        φ[i] = b[i]*φ[i+1]-a[i]*c[i]*φ[i+2]
    end
    α = Array(T, n, n)
    for i=1:n, j=1:n
        sign = (i+j)%2==0 ? (+) : (-)
        if i<j
            α[i,j]=(sign)(prod(c[i:j-1]))*θ[i-1]*φ[j+1]/θ[n]
        elseif i==j
            α[i,i]=                       θ[i-1]*φ[i+1]/θ[n]
        else #i>j
            α[i,j]=(sign)(prod(a[j:i-1]))*θ[j-1]*φ[i+1]/θ[n]
        end
    end
    α
end

#Implements the determinant using principal minors
#Inputs and reference are as above for inv_usmani()
function det_usmani{T}(a::Vector{T}, b::Vector{T}, c::Vector{T})
    n = length(b)
    θa = one(T)
    if n == 0
        return θa
    end
    θb = b[1]
    for i=2:n
        θb, θa = b[i]*θb-a[i-1]*c[i-1]*θa, θb
    end
    return θb
end

inv(A::SymTridiagonal) = inv_usmani(A.ev, A.dv, A.ev)
det(A::SymTridiagonal) = det_usmani(A.ev, A.dv, A.ev)

function getindex{T}(A::SymTridiagonal{T}, i::Integer, j::Integer)
    if !(1 <= i <= size(A,2) && 1 <= j <= size(A,2))
        throw(BoundsError("(i,j) = ($i,$j) not within matrix of size $(size(A))"))
    end
    if i == j
        return A.dv[i]
    elseif i == j + 1
        return A.ev[j]
    elseif i + 1 == j
        return A.ev[i]
    else
        return zero(T)
    end
end

## Tridiagonal matrices ##
immutable Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}    # sub-diagonal
    d::Vector{T}     # diagonal
    du::Vector{T}    # sup-diagonal
    du2::Vector{T}   # supsup-diagonal for pivoting
end
function Tridiagonal{T}(dl::Vector{T}, d::Vector{T}, du::Vector{T})
    n = length(d)
    if (length(dl) != n-1 || length(du) != n-1)
        throw(ArgumentError("Cannot make Tridiagonal from incompatible lengths of subdiagonal, diagonal and superdiagonal: ($(length(dl)), $(length(d)), $(length(du))"))
    end
    Tridiagonal(dl, d, du, zeros(T,n-2))
end
function Tridiagonal{Tl, Td, Tu}(dl::Vector{Tl}, d::Vector{Td}, du::Vector{Tu})
    Tridiagonal(map(v->convert(Vector{promote_type(Tl,Td,Tu)}, v), (dl, d, du))...)
end

size(M::Tridiagonal) = (length(M.d), length(M.d))
function size(M::Tridiagonal, d::Integer)
    if d < 1
        throw(ArgumentError("dimension d must be ≥ 1, got $d"))
    elseif d <= 2
        return length(M.d)
    else
        return 1
    end
end

full{T}(M::Tridiagonal{T}) = convert(Matrix{T}, M)
function convert{T}(::Type{Matrix{T}}, M::Tridiagonal{T})
    A = zeros(T, size(M))
    for i = 1:length(M.d)
        A[i,i] = M.d[i]
    end
    for i = 1:length(M.d)-1
        A[i+1,i] = M.dl[i]
        A[i,i+1] = M.du[i]
    end
    A
end
convert{T}(::Type{Matrix}, M::Tridiagonal{T}) = convert(Matrix{T}, M)
function similar(M::Tridiagonal, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        throw(DimensionMismatch("Tridiagonal matrices must be square"))
    end
    Tridiagonal{T}(similar(M.dl), similar(M.d), similar(M.du), similar(M.du2))
end

# Operations on Tridiagonal matrices
copy!(dest::Tridiagonal, src::Tridiagonal) = Tridiagonal(copy!(dest.dl, src.dl), copy!(dest.d, src.d), copy!(dest.du, src.du), copy!(dest.du2, src.du2))

#Elementary operations
for func in (:conj, :copy, :round, :trunc, :floor, :ceil)
    @eval function ($func)(M::Tridiagonal)
        Tridiagonal(($func)(M.dl), ($func)(M.d), ($func)(M.du), ($func)(M.du2))
    end
end
for func in (:round, :trunc, :floor, :ceil)
    @eval function ($func){T<:Integer}(::Type{T},M::Tridiagonal)
        Tridiagonal(($func)(T,M.dl), ($func)(T,M.d), ($func)(T,M.du), ($func)(T,M.du2))
    end
end

transpose(M::Tridiagonal) = Tridiagonal(M.du, M.d, M.dl)
ctranspose(M::Tridiagonal) = conj(transpose(M))

diag{T}(M::Tridiagonal{T}, n::Integer=0) = n==0 ? M.d : n==-1 ? M.dl : n==1 ? M.du : abs(n)<size(M,1) ? zeros(T,size(M,1)-abs(n)) : throw(BoundsError())
function getindex{T}(A::Tridiagonal{T}, i::Integer, j::Integer)
    if !(1 <= i <= size(A,2) && 1 <= j <= size(A,2))
        throw(BoundsError("(i,j) = ($i,$j) not within matrix of size $(size(A))"))
    end
    if i == j
        return A.d[i]
    elseif i == j + 1
        return A.dl[j]
    elseif i + 1 == j
        return A.du[i]
    else
        return zero(T)
    end
end

###################
# Generic methods #
###################

+(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl+B.dl, A.d+B.d, A.du+B.du)
-(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl-B.dl, A.d-B.d, A.du-B.du)
*(A::Tridiagonal, B::Number) = Tridiagonal(A.dl*B, A.d*B, A.du*B)
*(B::Number, A::Tridiagonal) = A*B
/(A::Tridiagonal, B::Number) = Tridiagonal(A.dl/B, A.d/B, A.du/B)

==(A::Tridiagonal, B::Tridiagonal) = (A.dl==B.dl) && (A.d==B.d) && (A.du==B.du)
==(A::Tridiagonal, B::SymTridiagonal) = (A.dl==A.du==B.ev) && (A.d==B.dv)
==(A::SymTridiagonal, B::Tridiagonal) = (B.dl==B.du==A.ev) && (B.d==A.dv)

inv(A::Tridiagonal) = inv_usmani(A.dl, A.d, A.du)
det(A::Tridiagonal) = det_usmani(A.dl, A.d, A.du)

# Elementary operations that mix Tridiagonal and SymTridiagonal matrices
convert(::Type{Tridiagonal}, A::SymTridiagonal) = Tridiagonal(A.ev, A.dv, A.ev)
+(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl+B.ev, A.d+B.dv, A.du+B.ev)
+(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev+B.dl, A.dv+B.d, A.ev+B.du)
-(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl-B.ev, A.d-B.dv, A.du-B.ev)
-(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev-B.dl, A.dv-B.d, A.ev-B.du)

convert{T}(::Type{Tridiagonal{T}},M::Tridiagonal) = Tridiagonal(convert(Vector{T}, M.dl), convert(Vector{T}, M.d), convert(Vector{T}, M.du), convert(Vector{T}, M.du2))
convert{T}(::Type{AbstractMatrix{T}},M::Tridiagonal) = convert(Tridiagonal{T}, M)
convert{T}(::Type{Tridiagonal{T}}, M::SymTridiagonal{T}) = Tridiagonal(M)
convert{T}(::Type{SymTridiagonal{T}}, M::Tridiagonal) = M.dl==M.du ? (SymTridiagonal(M.dl, M.d)) :
    throw(ArgumentError("Tridiagonal is not symmetric, cannot convert to SymTridiagonal"))
convert{T}(::Type{SymTridiagonal{T}},M::SymTridiagonal) = SymTridiagonal(convert(Vector{T}, M.dv), convert(Vector{T}, M.ev))

function A_mul_B!(C::AbstractVecOrMat, A::Tridiagonal, B::AbstractVecOrMat)
    nA = size(A,1)
    nB = size(B,2)
    if !(size(C,1) == size(B,1) == nA)
        throw(DimensionMismatch("A has first dimension $nA, B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if size(C,2) != nB
        throw(DimensionMismatch("A has second dimension $nA, B has $(size(B,2)), C has $(size(C,2)) but all must match"))
    end
    l = A.dl
    d = A.d
    u = A.du
    @inbounds begin
        for j = 1:nB
            b₀, b₊ = B[1, j], B[2, j]
            C[1, j] = d[1]*b₀ + u[1]*b₊
            for i = 2:nA - 1
                b₋, b₀, b₊ = b₀, b₊, B[i + 1, j]
                C[i, j] = l[i - 1]*b₋ + d[i]*b₀ + u[i]*b₊
            end
            C[nA, j] = l[nA - 1]*b₀ + d[nA]*b₊
        end
    end
    C
end
