#### Specialized matrix types ####

import Base.conj, Base.transpose, Base.ctranspose, Base.convert

## Hermitian tridiagonal matrices
type SymTridiagonal{T<:BlasFloat} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # subdiagonal
    function SymTridiagonal(dv::Vector{T}, ev::Vector{T})
        if length(ev) != length(dv) - 1 error("dimension mismatch") end
        new(dv,ev)
    end
end

SymTridiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}) = SymTridiagonal{T}(copy(dv), copy(ev))

function SymTridiagonal{T<:Real}(dv::Vector{T}, ev::Vector{T})
    SymTridiagonal{Float64}(float64(dv),float64(ev))
end

function SymTridiagonal{Td<:Number,Te<:Number}(dv::Vector{Td}, ev::Vector{Te})
    T = promote(Td,Te)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end

SymTridiagonal(M::AbstractMatrix) = diag(A,1)==diag(A,-1)?SymTridiagonal(diag(A), diag(A,1)):error("Matrix is not symmetric, cannot convert to SymTridiagonal")
full{T}(M::SymTridiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{Matrix{T}}, M::SymTridiagonal{T})=diagm(M.dv)+diagm(M.ev,-1)+conj(diagm(M.ev,1))

function show(io::IO, S::SymTridiagonal)
    println(io, summary(S), ":")
    print(io, "diag: ")
    print_matrix(io, (S.dv)')
    print(io, "\n sub: ")
    print_matrix(io, (S.ev)')
end

size(m::SymTridiagonal) = (length(m.dv), length(m.dv))
size(m::SymTridiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(m.dv) : 1)

#Elementary operations
copy(S::SymTridiagonal) = SymTridiagonal(copy(S.dv), copy(S.ev))
round(M::SymTridiagonal) = SymTridiagonal(round(M.dv), round(M.ev))
iround(M::SymTridiagonal) = SymTridiagonal(iround(M.dv), iround(M.ev))

conj(M::SymTridiagonal) = SymTridiagonal(conj(M.dv), conj(M.ev))
transpose(M::SymTridiagonal) = M #Identity operation
ctranspose(M::SymTridiagonal) = conj(M)

+(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv+B.dv, A.ev+B.ev)
-(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv-B.dv, A.ev-B.ev)
*(A::SymTridiagonal, B::SymTridiagonal) = full(A)*full(B)
*(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv*B, A.ev*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv/B, A.ev/B)
==(A::SymTridiagonal, B::SymTridiagonal) = (A.dv==B.dv) && (A.ev==B.ev)

## Solver
function \{T<:BlasFloat}(M::SymTridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        return LAPACK.gtsv!(copy(M.dv), copy(M.ev), copy(M.dv), copy(rhs))
    end
    solve(Tridiagonal(M), rhs)  # use the Julia "fallback"
end

#Wrap LAPACK DSTE{GR,BZ} to compute eigenvalues
eig(m::SymTridiagonal) = LAPACK.stegr!('V', copy(m.dv), copy(m.ev))
eigvals(m::SymTridiagonal, il::Int, iu::Int) = LAPACK.stegr!('N', 'I', copy(m.dv), copy(m.ev), 0.0, 0.0, il, iu)[1]
eigvals{T<:BlasFloat}(m::SymTridiagonal, vl::T, vu::T) = LAPACK.stegr!('N', 'V', copy(m.dv), copy(m.ev), vl, vu, 0, 0)[1]
eigvals(m::SymTridiagonal) = LAPACK.stev!('N', m.dv, m.ev)[1]

#Computes largest and smallest eigenvalue
eigmax(m::SymTridiagonal) = eigvals(m, size(m, 1), size(m, 1))[1]
eigmin(m::SymTridiagonal) = eigvals(m, 1, 1)[1]

#Compute selected eigenvectors only corresponding to particular eigenvalues
eigvecs(m::SymTridiagonal) = eig(m)[2]
eigvecs{Eigenvalue<:Real}(m::SymTridiagonal, eigvals::Vector{Eigenvalue}) = LAPACK.stein!(m.dv, m.ev, eigvals)

## Tridiagonal matrices ##
type Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}    # sub-diagonal
    d::Vector{T}     # diagonal
    du::Vector{T}    # sup-diagonal
    dutmp::Vector{T} # scratch space for vector RHS solver, sup-diagonal
    rhstmp::Vector{T}# scratch space, rhs

    function Tridiagonal(N::Integer)
        dutmp = Array(T, N-1)
        rhstmp = Array(T, N)
        new(dutmp, rhstmp, dutmp, dutmp, rhstmp)  # first three will be overwritten
    end
end

function Tridiagonal{T<:Number}(dl::Vector{T}, d::Vector{T}, du::Vector{T})
    N = length(d)
    if (length(dl) != N-1 || length(du) != N-1)
        error(string("Cannot make Tridiagonal from incompatible lengths of subdiagonal, diagonal and superdiagonal: (", length(dl), ", ", length(d), ", ", length(du),")"))
    end
    M = Tridiagonal{T}(N)
    M.dl = copy(dl)
    M.d = copy(d)
    M.du = copy(du)
    return M
end
function Tridiagonal{Tl<:Number, Td<:Number, Tu<:Number}(dl::Vector{Tl}, d::Vector{Td}, du::Vector{Tu})
    R = promote(Tl, Td, Tu)
    Tridiagonal(convert(Vector{R}, dl), convert(Vector{R}, d), convert(Vector{R}, du))
end

size(M::Tridiagonal) = (length(M.d), length(M.d))
size(M::Tridiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(M.d) : 1)

function show(io::IO, M::Tridiagonal)
    println(io, summary(M), ":")
    print(io, " sub: ")
    print_matrix(io, (M.dl)')
    print(io, "\ndiag: ")
    print_matrix(io, (M.d)')
    print(io, "\n sup: ")
    print_matrix(io, (M.du)')
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
    return A
end
function similar(M::Tridiagonal, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        error("Tridiagonal matrices must be square")
    end
    return Tridiagonal{T}(dims[1])
end

# Operations on Tridiagonal matrices
copy(A::Tridiagonal) = Tridiagonal(copy(A.dl), copy(A.d), copy(A.du))
round(M::Tridiagonal) = Tridiagonal(round(M.dl), round(M.d), round(M.du))
iround(M::Tridiagonal) = Tridiagonal(iround(M.dl), iround(M.d), iround(M.du))

conj(M::Tridiagonal) = Tridiagonal(conj(M.dl), conj(M.d), conj(M.du))
transpose(M::Tridiagonal) = Tridiagonal(M.du, M.d, M.dl)
ctranspose(M::Tridiagonal) = conj(transpose(M))

+(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl+B.dl, A.d+B.d, A.du+B.du)
-(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl-B.dl, A.d-B.d, A.du+B.du)
*(A::Tridiagonal, B::Tridiagonal) = full(A)*full(B)
*(A::Tridiagonal, B::Number) = Tridiagonal(A.dl*B, A.d*B, A.du*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::Tridiagonal, B::Number) = Tridiagonal(A.dl/B, A.d/B, A.du/B)

==(A::Tridiagonal, B::Tridiagonal) = (A.dl==B.dl) && (A.d==B.d) && (A.du==B.du)
==(A::Tridiagonal, B::SymTridiagonal) = (A.dl==A.du==B.ev) && (A.d==B.dv)
==(A::SymTridiagonal, B::SymTridiagonal) = B==A

# Elementary operations that mix Tridiagonal and SymTridiagonal matrices
convert(::Type{Tridiagonal}, A::SymTridiagonal) = Tridiagonal(A.ev, A.dv, A.ev)
+(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl+B.ev, A.d+B.dv, A.du+B.ev)
+(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev+B.dl, A.dv+B.d, A.ev+B.du)
-(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl-B.ev, A.d-B.dv, A.du-B.ev)
-(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev-B.dl, A.dv-B.d, A.ev-B.du)
#XXX Returns dense matrix but really should be banded
*(A::SymTridiagonal, B::Tridiagonal) = full(A)*full(B)
*(A::Tridiagonal, B::SymTridiagonal) = full(A)*full(B)

convert{T}(::Type{Tridiagonal{T}}, M::SymTridiagonal{T}) = Tridiagonal(M)
convert{T}(::Type{SymTridiagonal{T}}, M::Tridiagonal) = M.dl==M.du ? (SymTridiagonal(M.dl, M.d)) :
    error("Tridiagonal is not symmetric, cannot convert to SymTridiagonal")

## Solvers

#### Tridiagonal matrix routines ####
function \{T<:BlasFloat}(M::Tridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        return LAPACK.gtsv!(copy(M.dl), copy(M.d), copy(M.du), copy(rhs))
    end
    solve(M, rhs)  # use the Julia "fallback"
end

# This is definitely not going to work
#eig(M::Tridiagonal) = LAPACK.stev!('V', copy(M))

# Allocation-free variants
# Note that solve is non-aliasing, so you can use the same array for
# input and output
function solve(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, rhs::AbstractArray, rhsrng::Ranges{Int})
    d = M.d
    N = length(d)
    if length(xrng) != N || length(rhsrng) != N
        error("dimension mismatch")
    end
    dl = M.dl
    du = M.du
    dutmp = M.dutmp
    rhstmp = M.rhstmp
    xstart = first(xrng)
    xstride = step(xrng)
    rhsstart = first(rhsrng)
    rhsstride = step(rhsrng)
    # Forward sweep
    denom = d[1]
    dulast = du[1] / denom
    dutmp[1] = dulast
    rhslast = rhs[rhsstart] / denom
    rhstmp[1] = rhslast
    irhs = rhsstart+rhsstride
    for i in 2:N-1
        dltmp = dl[i-1]
        denom = d[i] - dltmp*dulast
        dulast = du[i] / denom
        dutmp[i] = dulast
        rhslast = (rhs[irhs] - dltmp*rhslast)/denom
        rhstmp[i] = rhslast
        irhs += rhsstride
    end
    dltmp = dl[N-1]
    denom = d[N] - dltmp*dulast
    xlast = (rhs[irhs] - dltmp*rhslast)/denom
    # Backward sweep
    ix = xstart + (N-2)*xstride
    x[ix+xstride] = xlast
    for i in N-1:-1:1
        xlast = rhstmp[i] - dutmp[i]*xlast
        x[ix] = xlast
        ix -= xstride
    end
    return x
end

solve(x::StridedVector, M::Tridiagonal, rhs::StridedVector) = solve(x, 1:length(x), M, rhs, 1:length(rhs))

function solve(M::Tridiagonal, rhs::StridedVector)
    x = similar(rhs)
    solve(x, M, rhs)
end

function solve(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    for j = 1:n
        r = Range1((j-1)*m+1,m)
        solve(X, r, M, B, r)
    end
    return X
end

function solve(M::Tridiagonal, B::StridedMatrix)
    X = similar(B)
    solve(X, M, B)
end

# User-friendly solver
\(M::Tridiagonal, rhs::Union(StridedVector,StridedMatrix)) = solve(M, rhs)

# Tridiagonal multiplication
function mult(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, v::AbstractArray, vrng::Ranges{Int})
    dl = M.dl
    d = M.d
    du = M.du
    N = length(d)
    xi = first(xrng)
    xstride = step(xrng)
    vi = first(vrng)
    vstride = step(vrng)
    x[xi] = d[1]*v[vi] + du[1]*v[vi+vstride]
    xi += xstride
    for i = 2:N-1
        x[xi] = dl[i-1]*v[vi] + d[i]*v[vi+vstride] + du[i]*v[vi+2*vstride]
        xi += xstride
        vi += vstride
    end
    x[xi] = dl[N-1]*v[vi] + d[N]*v[vi+vstride]
    return x
end

mult(x::StridedVector, M::Tridiagonal, v::StridedVector) = mult(x, 1:length(x), M, v, 1:length(v))

function mult(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    for j = 1:n
        r = Range1((j-1)*m+1,m)
        mult(X, r, M, B, r)
    end
    return X
end

mult(X::StridedMatrix, M1::Tridiagonal, M2::Tridiagonal) = mult(X, M1, full(M2))

function *(M::Tridiagonal, B::Union(StridedVector,StridedMatrix))
    X = similar(B)
    mult(X, M, B)
end

*(A::Tridiagonal, B::Tridiagonal) = A*full(B)

#### Factorizations for Tridiagonal ####
type LDLTTridiagonal{T<:BlasFloat,S<:BlasFloat} <: Factorization{T}
    D::Vector{S}
    E::Vector{T}
    function LDLTTridiagonal(D::Vector{S}, E::Vector{T})
        if typeof(real(E[1])) != eltype(D) error("Wrong eltype") end
        new(D, E)
    end
end

LDLTTridiagonal{S<:BlasFloat,T<:BlasFloat}(D::Vector{S}, E::Vector{T}) = LDLTTridiagonal{T,S}(D, E)

ldltd!{T<:BlasFloat}(A::SymTridiagonal{T}) = LDLTTridiagonal(LAPACK.pttrf!(real(A.dv),A.ev)...)
ldltd{T<:BlasFloat}(A::SymTridiagonal{T}) = ldltd!(copy(A))
factorize!(A::SymTridiagonal) = ldltd(A)

(\){T<:BlasReal}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) = LAPACK.pttrs!(C.D, C.E, copy(B))
(\){T<:BlasComplex}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) = LAPACK.pttrs!('L', C.D, C.E, copy(B))

type LUTridiagonal{T} <: Factorization{T}
    dl::Vector{T}
    d::Vector{T}
    du::Vector{T}
    du2::Vector{T}
    ipiv::Vector{BlasInt}
    # function LUTridiagonal(dl::Vector{T}, d::Vector{T}, du::Vector{T},
    #                        du2::Vector{T}, ipiv::Vector{BlasInt})
    #     n = length(d)
    #     if length(dl) != n - 1 || length(du) != n - 1 || length(ipiv) != n || length(du2) != n-2
    #         error("LUTridiagonal: dimension mismatch")
    #     end
    #     new(dl, d, du, du2, ipiv)
    # end
end
lufact!{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(A.dl,A.d,A.du)...)
lufact(A::Tridiagonal) = lufact!(copy(A))
factorize!(A::Tridiagonal) = lufact!(A)
#show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

function det{T}(lu::LUTridiagonal{T})
    n = length(lu.d)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

det(A::Tridiagonal) = det(lufact(A))

(\){T<:BlasFloat}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
    LAPACK.gttrs!('N', lu.dl, lu.d, lu.du, lu.du2, lu.ipiv, copy(B))
