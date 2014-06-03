using .ARPACK

## eigs

eigs(A; args...) = eigs(A, I; args...)

function eigs(A, B;
              nev::Integer=6, ncv::Integer=20, which::ASCIIString="LM",
              tol=0.0, maxiter::Integer=1000, sigma=nothing, v0::Vector=zeros((0,)),
              ritzvec::Bool=true)

    n = chksquare(A)

    T = eltype(A)
    iscmplx = T <: Complex
    isgeneral = B !== I
    sym = issym(A)
    (nev = min(nev, sym ? n - 1 : n - 2)) > 0 || throw(ArgumentError("nev must be at least one"))
    ncv = blas_int(min(max(2*nev + 2, ncv), n))
    isgeneral && !isposdef(B) && throw(PosDefException(0))
    bmat = isgeneral ? "G" : "I"
    isshift = sigma !== nothing
    sigma = isshift ? sigma : zero(T)

    if !isempty(v0)
        length(v0)==n || throw(DimensionMismatch(""))
        eltype(v0)==T || error("Starting vector must have eltype $T")
    else
        v0=zeros(T,(0,))
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
            solveSI(x) = factorize(sigma==0 ? A : A - UniformScaling(sigma)) \ x
        end
    else                    # Generalized eigen problem
        matvecB(x) = B * x
        if !isshift         #    Regular inverse mode
            mode       = 2
            solveSI(x) = factorize(B) \ x
        else                #    Shift-invert mode
            mode       = 3
            solveSI(x) = factorize(sigma==0 ? A : A-sigma*B) \ x
        end
    end

    # Compute the Ritz values and Ritz vectors
    (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork, TOL) = 
       ARPACK.aupd_wrapper(T, matvecA, matvecB, solveSI, n, sym, iscmplx, bmat, nev, ncv, which, tol, maxiter, mode, v0)
    
    # Postprocessing to get eigenvalues and eigenvectors
    if ritzvec
        (eval, evec, nconv, niter, nmult, resid) =
             ARPACK.eupd_wrapper(T, n, sym, iscmplx, bmat, nev, which, ritzvec, TOL,
                                 resid, ncv, v, ldv, sigma, iparam, ipntr, workd, workl, lworkl, rwork)
    else
        (eval, nconv, niter, nmult, resid) =
             ARPACK.eupd_wrapper(T, n, sym, iscmplx, bmat, nev, which, ritzvec, TOL,
                                 resid, ncv, v, ldv, sigma, iparam, ipntr, workd, workl, lworkl, rwork)
    end

end
