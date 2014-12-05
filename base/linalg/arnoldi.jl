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
        warn("nev should be at most $nevmax")
    end
    nev > 0 || throw(ArgumentError("nev must be at least one"))
    ncvmin = nev + (sym ? 1 : 2)
    if ncv < ncvmin
        warn("ncv should be at least $ncvmin")
        ncv = ncvmin
    end
    ncv = blas_int(min(ncv, n))
    isgeneral && !isposdef(B) && throw(PosDefException(0))
    bmat = isgeneral ? "G" : "I"
    isshift = sigma !== nothing

    if isa(which,AbstractString)
        warn("Use symbols instead of strings for specifying which eigenvalues to compute")
        which=symbol(which)
    end
    which == :LM || which == :SM || which == :LR || which == :SR || which == :LI || which == :SI || which == :BE || error("invalid value for which")
    which != :BE || sym || error("which = :BE only possible for real symmetric problem")
    isshift && which == :SM && warn("use of :SM in shift-and-invert mode is not recommended, use :LM to find eigenvalues closest to sigma")

    if which==:SM && !isshift # transform into shift-and-invert method with sigma = 0
        isshift=true
        sigma=zero(T)
        which=:LM
    end

    if sigma != nothing && !iscmplx && isa(sigma,Complex)
        error("complex shifts for real problems are not yet supported")
    end
    sigma = isshift ? convert(T,sigma) : zero(T)

    if !isempty(v0)
        length(v0)==n || throw(DimensionMismatch())
        eltype(v0)==T || error("Starting vector must have eltype $T")
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
        whichstr = (!sym ? "LI" : error("largest imaginary is meaningless for symmetric eigenvalue problems"))
    end
    if which == :SI
        whichstr = (!sym ? "SI" : error("smallest imaginary is meaningless for symmetric eigenvalue problems"))
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
