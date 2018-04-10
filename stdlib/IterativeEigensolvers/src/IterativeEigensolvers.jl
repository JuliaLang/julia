# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Arnoldi and Lanczos iteration for computing eigenvalues
"""
module IterativeEigensolvers

using LinearAlgebra: BlasFloat, BlasInt, Diagonal, I, SVD, UniformScaling,
                     checksquare, factorize,ishermitian, issymmetric, mul!,
                     rmul!, qr
import LinearAlgebra

export eigs, svds

include("arpack.jl")

using .ARPACK

## eigs
"""
    eigs(A; nev=6, ncv=max(20,2*nev+1), which=:LM, tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

Computes eigenvalues `d` of `A` using implicitly restarted Lanczos or Arnoldi iterations for real symmetric or
general nonsymmetric matrices respectively. See [the manual](@ref lib-itereigen) for more information.

`eigs` returns the `nev` requested eigenvalues in `d`, the corresponding Ritz vectors `v`
(only if `ritzvec=true`), the number of converged eigenvalues `nconv`, the number of
iterations `niter` and the number of matrix vector multiplications `nmult`, as well as the
final residual vector `resid`.

# Examples
```jldoctest
julia> using IterativeEigensolvers

julia> A = Diagonal(1:4);

julia> λ, ϕ = eigs(A, nev = 2);

julia> λ
2-element Array{Float64,1}:
 4.0
 3.0
```
"""
eigs(A; kwargs...) = eigs(A, I; kwargs...)
eigs(A::AbstractMatrix{<:BlasFloat}, ::UniformScaling; kwargs...) = _eigs(A, I; kwargs...)

eigs(A::AbstractMatrix{T}, B::AbstractMatrix{T}; kwargs...) where {T<:BlasFloat} = _eigs(A, B; kwargs...)
eigs(A::AbstractMatrix{BigFloat}, B::AbstractMatrix...; kwargs...) = throw(MethodError(eigs, Any[A,B,kwargs...]))
eigs(A::AbstractMatrix{BigFloat}, B::UniformScaling; kwargs...) = throw(MethodError(eigs, Any[A,B,kwargs...]))
function eigs(A::AbstractMatrix{T}, ::UniformScaling; kwargs...) where T
    Tnew = typeof(zero(T)/sqrt(one(T)))
    eigs(convert(AbstractMatrix{Tnew}, A), I; kwargs...)
end
function eigs(A::AbstractMatrix, B::AbstractMatrix; kwargs...)
    T = promote_type(eltype(A), eltype(B))
    Tnew = typeof(zero(T)/sqrt(one(T)))
    eigs(convert(AbstractMatrix{Tnew}, A), convert(AbstractMatrix{Tnew}, B); kwargs...)
end
"""
    eigs(A, B; nev=6, ncv=max(20,2*nev+1), which=:LM, tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0,))) -> (d,[v,],nconv,niter,nmult,resid)

Computes generalized eigenvalues `d` of `A` and `B` using implicitly restarted Lanczos or Arnoldi iterations for
real symmetric or general nonsymmetric matrices respectively. See [the manual](@ref lib-itereigen) for more information.
"""
eigs(A, B; kwargs...) = _eigs(A, B; kwargs...)
function _eigs(A, B;
               nev::Integer=6, ncv::Integer=max(20,2*nev+1), which=:LM,
               tol=0.0, maxiter::Integer=300, sigma=nothing, v0::Vector=zeros(eltype(A),(0,)),
               ritzvec::Bool=true)
    n = checksquare(A)

    T = eltype(A)
    iscmplx = T <: Complex
    isgeneral = B !== I
    sym = !iscmplx && issymmetric(A) && issymmetric(B)
    nevmax = sym ? n-1 : n-2
    if nevmax <= 0
        throw(ArgumentError("input matrix A is too small. Use eigfact instead."))
    end
    if nev > nevmax
        @warn "Adjusting nev from $nev to $nevmax"
        nev = nevmax
    end
    if nev <= 0
        throw(ArgumentError("requested number of eigenvalues (nev) must be ≥ 1, got $nev"))
    end
    ncvmin = nev + (sym ? 1 : 2)
    if ncv < ncvmin
        @warn "Adjusting ncv from $ncv to $ncvmin"
        ncv = ncvmin
    end
    ncv = BlasInt(min(ncv, n))
    bmat = isgeneral ? "G" : "I"
    isshift = sigma !== nothing

    if isa(which,AbstractString)
        @warn "Use symbols instead of strings for specifying which eigenvalues to compute"
        which=Symbol(which)
    end
    if (which != :LM && which != :SM && which != :LR && which != :SR &&
        which != :LI && which != :SI && which != :BE)
        throw(ArgumentError("which must be :LM, :SM, :LR, :SR, :LI, :SI, or :BE, got $(repr(which))"))
    end
    if which == :BE && !sym
        throw(ArgumentError("which=:BE only possible for real symmetric problem"))
    end
    isshift && which == :SM && @warn "Use of :SM in shift-and-invert mode is not recommended, use :LM to find eigenvalues closest to sigma"

    if which==:SM && !isshift # transform into shift-and-invert method with sigma = 0
        isshift=true
        sigma=zero(T)
        which=:LM
    end

    if sigma !== nothing && !iscmplx && isa(sigma,Complex)
        throw(ArgumentError("complex shifts for real problems are not yet supported"))
    end
    sigma = isshift ? convert(T,sigma) : zero(T)

    if !isempty(v0)
        if length(v0) != n
            throw(DimensionMismatch())
        end
        if eltype(v0) != T
            throw(ArgumentError("starting vector must have element type $T, got $(eltype(v0))"))
        end
    end

    whichstr = "LM"
    if which == :BE
        whichstr = "BE"
    end
    if which == :LR
        whichstr = (!sym ? "LR" : "LA")
    end
    if which == :SR
        whichstr = (!sym ? "SR" : "SA")
    end
    if which == :LI
        if !sym
            whichstr = "LI"
        else
            throw(ArgumentError("largest imaginary is meaningless for symmetric eigenvalue problems"))
        end
    end
    if which == :SI
        if !sym
            whichstr = "SI"
        else
            throw(ArgumentError("smallest imaginary is meaningless for symmetric eigenvalue problems"))
        end
    end

    # Refer to ex-*.doc files in ARPACK/DOCUMENTS for calling sequence
    matvecA!(y, x) = mul!(y, A, x)
    if !isgeneral           # Standard problem
        matvecB = x -> x
        if !isshift         #    Regular mode
            mode       = 1
            solveSI = x->x
        else                #    Shift-invert mode
            mode       = 3
            F = factorize(A - UniformScaling(sigma))
            solveSI = x -> F \ x
        end
    else                    # Generalized eigenproblem
        matvecB = x -> B * x
        if !isshift         #    Regular inverse mode
            mode       = 2
            F = factorize(B)
            solveSI = x -> F \ x
        else                #    Shift-invert mode
            mode       = 3
            F = factorize(A - sigma*B)
            solveSI = x -> F \ x
        end
    end

    # Compute the Ritz values and Ritz vectors
    (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork, TOL) =
       ARPACK.aupd_wrapper(T, matvecA!, matvecB, solveSI, n, sym, iscmplx, bmat, nev, ncv, whichstr, tol, maxiter, mode, v0)

    # Postprocessing to get eigenvalues and eigenvectors
    output = ARPACK.eupd_wrapper(T, n, sym, iscmplx, bmat, nev, whichstr, ritzvec, TOL,
                                 resid, ncv, v, ldv, sigma, iparam, ipntr, workd, workl, lworkl, rwork)

    # Issue 10495, 10701: Check that all eigenvalues are converged
    nev = length(output[1])
    nconv = output[ritzvec ? 3 : 2]
    nev ≤ nconv || @warn "Not all wanted Ritz pairs converged. Requested: $nev, converged: $nconv"

    return output
end


## svds
struct SVDAugmented{T,S} <: AbstractArray{T, 2}
    X::S
    SVDAugmented{T,S}(X::AbstractMatrix) where {T,S} = new(X)
end

function SVDAugmented(A::AbstractMatrix{T}) where T
    Tnew = typeof(zero(T)/sqrt(one(T)))
    Anew = convert(AbstractMatrix{Tnew}, A)
    SVDAugmented{Tnew,typeof(Anew)}(Anew)
end

function LinearAlgebra.mul!(y::StridedVector{T}, A::SVDAugmented{T}, x::StridedVector{T}) where T
    m, mn = size(A.X, 1), length(x)
    mul!( view(y, 1:m), A.X, view(x, m + 1:mn)) # left singular vector
    mul!(view(y, m + 1:mn), adjoint(A.X), view(x, 1:m)) # right singular vector
    return y
end
Base.size(A::SVDAugmented)  = ((+)(size(A.X)...), (+)(size(A.X)...))
LinearAlgebra.ishermitian(A::SVDAugmented) = true

struct AtA_or_AAt{T,S} <: AbstractArray{T, 2}
    A::S
    buffer::Vector{T}
end

function AtA_or_AAt(A::AbstractMatrix{T}) where T
    Tnew = typeof(zero(T)/sqrt(one(T)))
    Anew = convert(AbstractMatrix{Tnew}, A)
    AtA_or_AAt{Tnew,typeof(Anew)}(Anew, Vector{Tnew}(undef, max(size(A)...)))
end

function LinearAlgebra.mul!(y::StridedVector{T}, A::AtA_or_AAt{T}, x::StridedVector{T}) where T
    if size(A.A, 1) >= size(A.A, 2)
        mul!(A.buffer, A.A, x)
        return mul!(y, adjoint(A.A), A.buffer)
    else
        mul!(A.buffer, adjoint(A.A), x)
        return mul!(y, A.A, A.buffer)
    end
end
Base.size(A::AtA_or_AAt) = ntuple(i -> min(size(A.A)...), Val(2))
LinearAlgebra.ishermitian(s::AtA_or_AAt) = true


svds(A::AbstractMatrix{<:BlasFloat}; kwargs...) = _svds(A; kwargs...)
svds(A::AbstractMatrix{BigFloat}; kwargs...) = throw(MethodError(svds, Any[A, kwargs...]))
function svds(A::AbstractMatrix{T}; kwargs...) where T
    Tnew = typeof(zero(T)/sqrt(one(T)))
    svds(convert(AbstractMatrix{Tnew}, A); kwargs...)
end

"""
    svds(A; nsv=6, ritzvec=true, tol=0.0, maxiter=1000, ncv=2*nsv, v0=zeros((0,))) -> (SVD([left_sv,] s, [right_sv,]), nconv, niter, nmult, resid)

Computes the largest singular values `s` of `A` using implicitly restarted Lanczos
iterations derived from [`eigs`](@ref).

**Inputs**

* `A`: Linear operator whose singular values are desired. `A` may be represented as a
  subtype of `AbstractArray`, e.g., a sparse matrix, or any other type supporting the four
  methods `size(A)`, `eltype(A)`, `A * vector`, and `A' * vector`.
* `nsv`: Number of singular values. Default: 6.
* `ritzvec`: If `true`, return the left and right singular vectors `left_sv` and `right_sv`.
   If `false`, omit the singular vectors. Default: `true`.
* `tol`: tolerance, see [`eigs`](@ref).
* `maxiter`: Maximum number of iterations, see [`eigs`](@ref). Default: 1000.
* `ncv`: Maximum size of the Krylov subspace, see [`eigs`](@ref) (there called `nev`). Default: `2*nsv`.
* `v0`: Initial guess for the first Krylov vector. It may have length `min(size(A)...)`, or 0.

**Outputs**

* `svd`: An `SVD` object containing the left singular vectors, the requested values, and the
  right singular vectors. If `ritzvec = false`, the left and right singular vectors will be
  empty.
* `nconv`: Number of converged singular values.
* `niter`: Number of iterations.
* `nmult`: Number of matrix--vector products used.
* `resid`: Final residual vector.

# Examples
```jldoctest; filter = r"2-element Array{Float64,1}:\\n.*\\n.*"
julia> A = Diagonal(1:4);

julia> s = svds(A, nsv = 2)[1];

julia> s.S
2-element Array{Float64,1}:
 4.0
 2.9999999999999996
```

!!! note "Implementation"
    `svds(A)` is formally equivalent to calling [`eigs`](@ref) to perform implicitly restarted
    Lanczos tridiagonalization on the Hermitian matrix ``A^\\prime A`` or ``AA^\\prime`` such
    that the size is smallest.
"""
svds(A; kwargs...) = _svds(A; kwargs...)
function _svds(X; nsv::Int = 6, ritzvec::Bool = true, tol::Float64 = 0.0, maxiter::Int = 1000, ncv::Int = 2*nsv, v0::Vector=zeros(eltype(X),(0,)))
    if nsv < 1
        throw(ArgumentError("number of singular values (nsv) must be ≥ 1, got $nsv"))
    end
    if nsv > minimum(size(X))
        throw(ArgumentError("number of singular values (nsv) must be ≤ $(minimum(size(X))), got $nsv"))
    end
    m, n = size(X)
    otype = eltype(X)
    if length(v0) ∉ [0,n]
        throw(DimensionMismatch("length of v0, the guess for the starting right Krylov vector, must be 0, or $n, got $(length(v0))"))
    end
    ex    = eigs(AtA_or_AAt(X), I; which = :LM, ritzvec = ritzvec, nev = nsv, tol = tol, maxiter = maxiter, v0=v0)
    # ind   = [1:2:ncv;]
    # sval  = abs.(ex[1][ind])

    svals = sqrt.(real.(ex[1]))
    if ritzvec
        # calculating singular vectors
        # left_sv  = sqrt(2) * ex[2][ 1:size(X,1),     ind ] .* sign.(ex[1][ind]')
        if size(X, 1) >= size(X, 2)
            V = ex[2]
            U = qr(rmul!(X*V, Diagonal(inv.(svals))))[1]
        else
            U = ex[2]
            V = qr(rmul!(X'U, Diagonal(inv.(svals))))[1]
        end

        # right_sv = sqrt(2) * ex[2][ size(X,1)+1:end, ind ]
        return (SVD(U, svals, copy(V')), ex[3], ex[4], ex[5], ex[6])
    else
        #The sort is necessary to work around #10329
        return (SVD(zeros(eltype(svals), n, 0),
                    svals,
                    zeros(eltype(svals), 0, m)),
                    ex[2], ex[3], ex[4], ex[5])
    end
end

include("deprecated.jl")

end # module
