#### Specialized matrix types ####

## Hermitian tridiagonal matrices
type SymTridiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # subdiagonal
    function SymTridiagonal(dv::Vector{T}, ev::Vector{T})
        length(ev)==length(dv)-1 || throw(DimensionMismatch(""))
        new(dv,ev)
    end
end

SymTridiagonal{T}(dv::Vector{T}, ev::Vector{T}) = SymTridiagonal{T}(copy(dv), copy(ev))

function SymTridiagonal{Td,Te}(dv::Vector{Td}, ev::Vector{Te})
    T = promote_type(Td,Te)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end

SymTridiagonal(A::AbstractMatrix) = diag(A,1)==diag(A,-1)?SymTridiagonal(diag(A), diag(A,1)):throw(DimensionMismatch("matrix is not symmetric; cannot convert to SymTridiagonal"))
full{T}(M::SymTridiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{Matrix{T}}, M::SymTridiagonal{T})=diagm(M.dv)+diagm(M.ev,-1)+conj(diagm(M.ev,1))

size(m::SymTridiagonal) = (length(m.dv), length(m.dv))
size(m::SymTridiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(m.dv) : 1)

#Elementary operations
for func in (:copy, :round, :iround, :conj)
    @eval begin
        ($func)(M::SymTridiagonal) = SymTridiagonal(($func)(M.dv), ($func)(M.ev))
    end
end

transpose(M::SymTridiagonal) = M #Identity operation
ctranspose(M::SymTridiagonal) = conj(M)

function diag{T}(M::SymTridiagonal{T}, n::Integer=0)
    absn = abs(n)
    absn==0 ? M.dv : absn==1 ? M.ev : absn<size(M,1) ? zeros(T,size(M,1)-absn) : throw(BoundsError())
end

+(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv+B.dv, A.ev+B.ev)
-(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv-B.dv, A.ev-B.ev)
*(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv*B, A.ev*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv/B, A.ev/B)
==(A::SymTridiagonal, B::SymTridiagonal) = (A.dv==B.dv) && (A.ev==B.ev)

## Solver
function \{T<:BlasFloat}(M::SymTridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        return LAPACK.gtsv!(copy(M.ev), copy(M.dv), copy(M.ev), copy(rhs))
    end
    solve(Tridiagonal(M), rhs)  # use the Julia "fallback"
end

#Wrap LAPACK DSTE{GR,BZ} to compute eigenvalues
eig{T<:BlasFloat}(m::SymTridiagonal{T}) = LAPACK.stegr!('V', copy(m.dv), copy(m.ev))
eigvals{T<:BlasFloat}(m::SymTridiagonal{T}, il::Int, iu::Int) = LAPACK.stegr!('N', 'I', copy(m.dv), copy(m.ev), 0.0, 0.0, il, iu)[1]
eigvals{T<:BlasFloat}(m::SymTridiagonal{T}, vl::Real, vu::Real) = LAPACK.stegr!('N', 'V', copy(m.dv), copy(m.ev), vl, vu, 0, 0)[1]
eigvals{T<:BlasFloat}(m::SymTridiagonal{T}) = LAPACK.stev!('N', copy(m.dv), copy(m.ev))[1]

#Computes largest and smallest eigenvalue
eigmax(m::SymTridiagonal) = eigvals(m, size(m, 1), size(m, 1))[1]
eigmin(m::SymTridiagonal) = eigvals(m, 1, 1)[1]

#Compute selected eigenvectors only corresponding to particular eigenvalues
eigvecs(m::SymTridiagonal) = eig(m)[2]
eigvecs{T<:BlasFloat,Eigenvalue<:Real}(m::SymTridiagonal{T}, eigvals::Vector{Eigenvalue}) = LAPACK.stein!(m.dv, m.ev, eigvals)

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
#Ref:
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
    n==0 && return θa
    θb = b[1]
    for i=2:n
        θb, θa = b[i]*θb-a[i-1]*c[i-1]*θa, θb
    end
    return θb
end

inv(A::SymTridiagonal) = inv_usmani(A.ev, A.dv, A.ev)
det(A::SymTridiagonal) = det_usmani(A.ev, A.dv, A.ev)

function getindex{T}(A::SymTridiagonal{T}, i::Integer, j::Integer)
    (1<=i<=size(A,2) && 1<=j<=size(A,2)) || throw(BoundsError())
    i==j ? A.dv[i] : i==j+1 ? A.ev[j] : i+1==j ? A.ev[i] : zero(T)
end

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
        new(dutmp, rhstmp, similar(dutmp), similar(dutmp), similar(rhstmp))
    end

    function Tridiagonal(dl::Vector{T}, d::Vector{T}, du::Vector{T})
        N = length(d)
        if (length(dl) != N-1 || length(du) != N-1)
            error(string("Cannot make Tridiagonal from incompatible lengths of subdiagonal, diagonal and superdiagonal: (", length(dl), ", ", length(d), ", ", length(du),")"))
        end
        new(copy(dl), copy(d), copy(du), Array(T,N-1), Array(T,N))
    end
end

Tridiagonal{T}(dl::Vector{T}, d::Vector{T}, du::Vector{T}) = Tridiagonal{T}(dl, d, du)

function Tridiagonal{Tl, Td, Tu}(dl::Vector{Tl}, d::Vector{Td}, du::Vector{Tu})
    Tridiagonal(map(v->copy(convert(Vector{promote_type(Tl,Td,Tu)}, v)), (dl, d, du))...)
end

size(M::Tridiagonal) = (length(M.d), length(M.d))
size(M::Tridiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(M.d) : 1)

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
function similar(M::Tridiagonal, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        throw(DimensionMismatch("Tridiagonal matrices must be square"))
    end
    Tridiagonal{T}(dims[1])
end

# Operations on Tridiagonal matrices
copy!(dest::Tridiagonal, src::Tridiagonal) = Tridiagonal(copy!(dest.dl, src.dl), copy!(dest.d, src.d), copy!(dest.du, src.du))

#Elementary operations
for func in (:copy, :round, :iround, :conj) 
    @eval begin
        ($func)(M::Tridiagonal) = Tridiagonal(map(($func), (M.dl, M.d, M.du))...)
    end
end

transpose(M::Tridiagonal) = Tridiagonal(M.du, M.d, M.dl)
ctranspose(M::Tridiagonal) = conj(transpose(M))

diag{T}(M::Tridiagonal{T}, n::Integer=0) = n==0 ? M.d : n==-1 ? M.dl : n==1 ? M.du : abs(n)<size(M,1) ? zeros(T,size(M,1)-abs(n)) : throw(BoundsError()) 

function getindex{T}(A::Tridiagonal{T}, i::Integer, j::Integer)
    (1<=i<=size(A,2) && 1<=j<=size(A,2)) || throw(BoundsError())
    i==j ? A.d[i] : i==j+1 ? A.dl[j] : i+1==j ? A.du[i] : zero(T)
end


###################
# Generic methods #
###################

+(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl+B.dl, A.d+B.d, A.du+B.du)
-(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl-B.dl, A.d-B.d, A.du+B.du)
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

convert{T}(::Type{Tridiagonal{T}},M::Tridiagonal) = Tridiagonal(convert(Vector{T}, M.dl), convert(Vector{T}, M.d), convert(Vector{T}, M.du))
convert{T}(::Type{Tridiagonal{T}}, M::SymTridiagonal{T}) = Tridiagonal(M)
convert{T}(::Type{SymTridiagonal{T}}, M::Tridiagonal) = M.dl==M.du ? (SymTridiagonal(M.dl, M.d)) :
    error("Tridiagonal is not symmetric, cannot convert to SymTridiagonal")
convert{T}(::Type{SymTridiagonal{T}},M::SymTridiagonal) = SymTridiagonal(convert(Vector{T}, M.dv), convert(Vector{T}, M.ev))

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
function solve!{T<:BlasFloat}(x::AbstractArray{T}, xrng::Ranges{Int}, M::Tridiagonal{T}, rhs::AbstractArray{T}, rhsrng::Ranges{Int})
    d = M.d
    N = length(d)
    if length(xrng) != N || length(rhsrng) != N
        throw(DimensionMismatch(""))
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
    nothing
end

function solve!(x::StridedVector, M::Tridiagonal, rhs::StridedVector)
    solve!(x, 1:length(x), M, rhs, 1:length(rhs))
    x
end
solve{TM<:BlasFloat,TB<:BlasFloat}(M::Tridiagonal{TM}, B::StridedVecOrMat{TB}) = solve!(zeros(typeof(one(TM)/one(TB)), size(B)), M, B)
solve(M::Tridiagonal, B::StridedVecOrMat) = solve(float(M), float(B))
function solve!(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    size(B, 1) == size(M, 1) || throw(DimensionMismatch(""))
    size(X) == size(B) || throw(DimensionMismatch(""))
    m, n = size(B)
    for j = 1:n
        r = Range1((j-1)*m+1,m)
        solve!(X, r, M, B, r)
    end
    X
end

# User-friendly solver
\(M::Tridiagonal, rhs::StridedVecOrMat) = solve(M, rhs)

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
    x
end

mult(x::StridedVector, M::Tridiagonal, v::StridedVector) = mult(x, 1:length(x), M, v, 1:length(v))

function mult(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    size(B, 1) == size(M, 1) || throw(DimensionMismatch(""))
    size(X) == size(B) || throw(DimensionMismatch(""))
    m, n = size(B)
    for j = 1:n
        r = Range1((j-1)*m+1,m)
        mult(X, r, M, B, r)
    end
    X
end

mult(X::StridedMatrix, M1::Tridiagonal, M2::Tridiagonal) = mult(X, M1, full(M2))

*(M::Tridiagonal, B::Union(StridedVector,StridedMatrix)) = mult(similar(B), M, B)
*(A::Tridiagonal, B::Tridiagonal) = A*full(B)

#### Factorizations for Tridiagonal ####
type LDLTTridiagonal{T<:BlasFloat,S<:BlasFloat} <: Factorization{T}
    D::Vector{S}
    E::Vector{T}
    function LDLTTridiagonal(D::Vector{S}, E::Vector{T})
        typeof(real(E[1])) == eltype(D) ? new(D, E) : error("element types do not match")
        new(D, E)
    end
end

LDLTTridiagonal{S<:BlasFloat,T<:BlasFloat}(D::Vector{S}, E::Vector{T}) = LDLTTridiagonal{T,S}(D, E)

ldltd!{T<:BlasFloat}(A::SymTridiagonal{T}) = LDLTTridiagonal(LAPACK.pttrf!(real(A.dv),A.ev)...)
ldltd{T<:BlasFloat}(A::SymTridiagonal{T}) = ldltd!(copy(A))
ldltd{T}(A::SymTridiagonal{T}) = (S = promote_type(typeof(sqrt(one(T))),Float32); S != T ? ldltd!(convert(SymTridiagonal{S},A)) : ldltd!(copy(A)))
factorize(A::SymTridiagonal) = ldltd(A)

A_ldiv_B!{T<:BlasReal}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) = LAPACK.pttrs!(C.D, C.E, B)
A_ldiv_B!{T<:BlasComplex}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) = LAPACK.pttrs!('L', C.D, C.E, B)
A_ldiv_B!(C::LDLTTridiagonal, B::StridedVecOrMat) = A_ldiv_B!(C, float(B))

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
    #         throw(DimensionMismatch("LUTridiagonal")
    #     end
    #     new(dl, d, du, du2, ipiv)
    # end
end
lufact!{T<:BlasFloat}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(A.dl,A.d,A.du)...)
lufact{T<:BlasFloat}(A::Tridiagonal{T}) = lufact!(copy(A))
lufact{T}(A::Tridiagonal{T}) = (S = promote_type(typeof(sqrt(one(T))),Float32); S != T ? lufact!(convert(Tridiagonal{S},A)) : lufact!(copy(A)))
factorize(A::Tridiagonal) = lufact(A)
#show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

function det{T}(lu::LUTridiagonal{T})
    n = length(lu.d)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

det{T<:BlasFloat}(A::Tridiagonal{T}) = det(lufact(A))

A_ldiv_B!{T<:BlasFloat}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
    LAPACK.gttrs!('N', lu.dl, lu.d, lu.du, lu.du2, lu.ipiv, B)
A_ldiv_B!(lu::LUTridiagonal, B::StridedVecOrMat) = A_ldiv_B!(lu, float(B))
