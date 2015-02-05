using .ARPACK

## eigs

eigs(A; args...) = eigs(A, I; args...)

function eigs(A, B;
              nev::Integer=6, ncv::Integer=max(20,2*nev+1), which=:LM,
              tol=0.0, maxiter::Integer=300, sigma=nothing, v0::Vector=zeros(eltype(A),(0,)),
              ritzvec::Bool=true)

    n = chksquare(A)

    T = eltype(A)
    iscmplx = T <: Complex
    isgeneral = B !== I
    sym = issym(A) && !iscmplx
    nevmax=sym ? n-1 : n-2
    if nev > nevmax
        nev = nevmax
        isinteractive() && warn("nev should be at most $nevmax")
    end
    nev > 0 || throw(ArgumentError("requested number of eigen values (nev) must be ≥ 1, got $nev"))
    ncvmin = nev + (sym ? 1 : 2)
    if ncv < ncvmin
        isinteractive() && warn("ncv should be at least $ncvmin")
        ncv = ncvmin
    end
    ncv = blas_int(min(ncv, n))
    isgeneral && !isposdef(B) && throw(PosDefException(0))
    bmat = isgeneral ? "G" : "I"
    isshift = sigma !== nothing

    if isa(which,AbstractString)
        isinteractive() && warn("Use symbols instead of strings for specifying which eigenvalues to compute")
        which=symbol(which)
    end
    if (which != :LM && which != :SM && which != :LR && which != :SR &&
        which != :LI && which != :SI && which != :BE)
       throw(ArgumentError("which must be :LM, :SM, :LR, :SR, :LI, :SI, or :BE, got $(repr(which))"))
    end
    which != :BE || sym || throw(ArgumentError("which=:BE only possible for real symmetric problem"))
    if isshift && which == :SM
        if isinteractive()
            warn("use of :SM in shift-and-invert mode is not recommended, use :LM to find eigenvalues closest to sigma")
        end
    end
    if which==:SM && !isshift # transform into shift-and-invert method with sigma = 0
        isshift=true
        sigma=zero(T)
        which=:LM
    end

    if sigma != nothing && !iscmplx && isa(sigma,Complex)
        throw(ArgumentError("complex shifts for real problems are not yet supported"))
    end
    sigma = isshift ? convert(T,sigma) : zero(T)

    if !isempty(v0)
        length(v0)==n || throw(DimensionMismatch())
        eltype(v0)==T || throw(ArgumentError("starting vector must have element type $T, got $(eltype(v0))"))
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
        whichstr = (!sym ? "LI" : throw(ArgumentError("largest imaginary is meaningless for symmetric eigenvalue problems")))
    end
    if which == :SI
        whichstr = (!sym ? "SI" : throw(ArgumentError("smallest imaginary is meaningless for symmetric eigenvalue problems")))
    end

    # Refer to ex-*.doc files in ARPACK/DOCUMENTS for calling sequence
    matvecA(x) = A * x
    if !isgeneral           # Standard problem
        matvecB(x) = x
        if !isshift         #    Regular mode
            mode       = 1
            solveSI(x) = x
        else                #    Shift-invert mode
            mode       = 3
            F = factorize(sigma==zero(T) ? A : A - UniformScaling(sigma))
            solveSI(x) = F \ x
        end
    else                    # Generalized eigen problem
        matvecB(x) = B * x
        if !isshift         #    Regular inverse mode
            mode       = 2
            F = factorize(B)
            solveSI(x) = F \ x
        else                #    Shift-invert mode
            mode       = 3
            F = factorize(sigma==zero(T) ? A : A-sigma*B)
            solveSI(x) = F \ x
        end
    end

    # Compute the Ritz values and Ritz vectors
    (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork, TOL) =
       ARPACK.aupd_wrapper(T, matvecA, matvecB, solveSI, n, sym, iscmplx, bmat, nev, ncv, whichstr, tol, maxiter, mode, v0)

    # Postprocessing to get eigenvalues and eigenvectors
    return ARPACK.eupd_wrapper(T, n, sym, iscmplx, bmat, nev, whichstr, ritzvec, TOL,
                                 resid, ncv, v, ldv, sigma, iparam, ipntr, workd, workl, lworkl, rwork)

end


## svds

type SVDOperator{T,S} <: AbstractArray{T, 2}
    X::S
    m::Int
    n::Int
    SVDOperator(X::S) = new(X, size(X,1), size(X,2))
end

## v = [ left_singular_vector; right_singular_vector ]
*{T,S}(s::SVDOperator{T,S}, v::Vector{T}) = [s.X * v[s.m+1:end]; s.X' * v[1:s.m]]
size(s::SVDOperator)  = s.m + s.n, s.m + s.n
issym(s::SVDOperator) = true

function svds{S}(X::S; nsv::Int = 6, ritzvec::Bool = true, tol::Float64 = 0.0, maxiter::Int = 1000)
    if nsv < 1
        throw(ArgumentError("number of singular values (nsv) must be ≥ 1, got $nsv"))
    end
    if nsv > minimum(size(X))
        throw(ArgumentError("number of singular values (nsv) must be ≤ $(minimum(size(X))), got $nsv"))
    end
    otype = eltype(X)
    ex    = eigs(SVDOperator{otype,S}(X), I; ritzvec = ritzvec, nev = 2*nsv, tol = tol, maxiter = maxiter)
    ind   = [1:2:nsv*2]
    sval  = abs(ex[1][ind])

    ritzvec || return (sval, ex[2], ex[3], ex[4], ex[5])

    # calculating singular vectors
    left_sv  = sqrt(2) * ex[2][ 1:size(X,1),     ind ] .* sign(ex[1][ind]')
    right_sv = sqrt(2) * ex[2][ size(X,1)+1:end, ind ]
    return (left_sv, sval, right_sv, ex[3], ex[4], ex[5], ex[6])
end
