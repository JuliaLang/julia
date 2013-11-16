using .ARPACK

## eigs

function eigs(A;nev::Integer=6, ncv::Integer=20, which::ASCIIString="LM",
	      tol=0.0, maxiter::Integer=1000, sigma=0,v0::Vector=zeros((0,)),
	      ritzvec::Bool=true, complexOP::Bool=false)

    (m, n) = size(A)
    if m != n; error("Input must be square"); end
    if n <= 6 && nev > n-1; nev = n-1; end
    ncv = blas_int(min(max(2*nev+2, ncv), n))

    sym   = issym(A)
    T = eltype(A)
    cmplx = T<:Complex
    bmat  = "I"
	if !isempty(v0)
		if length(v0)!=n; error("Starting vector must have length $n"); end
		if eltype(v0)!=T; error("Starting vector must have eltype $T"); end
	else
		v0=zeros(T,(0,))
	end

    if sigma == 0
        mode = 1
        linop(x) = A * x
    else
        C = lufact(A - sigma*eye(T,n))
        if cmplx
            mode = 3
            linop(x) = C\x
        else
            if !complexOP
                mode = 3
                linop(x) = real(C\x)
            else
                mode = 4
                linop(x) = imag(C\x)
            end
        end     
    end
        
    # Compute the Ritz values and Ritz vectors
    (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork, TOL) = 
       ARPACK.aupd_wrapper(T, linop, n, sym, cmplx, bmat, nev, ncv, which, tol, maxiter, mode, v0)
    
    # Postprocessing to get eigenvalues and eigenvectors
    return ARPACK.eupd_wrapper(T, n, sym, cmplx, bmat, nev, which, ritzvec,
                               TOL, resid, ncv, v, ldv, sigma, iparam, ipntr, 
                               workd, workl, lworkl, rwork)

end
