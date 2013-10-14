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

## svds

# For a dense matrix A is ignored and At is actually A'*A
# sarupdate{T}(A::StridedMatrix{T}, At::StridedMatrix{T}, X::StridedVector{T}) = BLAS.symv('U', one(T), At, X)
# sarupdate{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, At::SparseMatrixCSC{Tv,Ti}, X::StridedVector{Tv}) = At*(A*X)

# function svds{T<:Union(Float64,Float32)}(A::AbstractMatrix{T};
#                                          nsv::Integer=6, ncv::Integer=20, which::ASCIIString="LA", 
#                                          tol=0.0, maxiter::Integer=1000,
#                                          ritzvec::Bool=true)

#     (m, n) = size(A)
#     ncv = blas_int(min(max(2*nsv+2, ncv), n))
# #    if m < n; error("m = $m, n = $n and only the m >= n case is implemented"); end
#     sym   = true
#     cmplx = false
#     bmat  = "I"
#     At = isa(A, StridedMatrix) ? BLAS.syrk('U','T',1.0,A) : A'
#     sigma = 0
#     mode = 1

#     # Compute the Ritz values and Ritz vectors
#     (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
#          ARPACK.aupd_wrapper(T, (x) -> sarupdate(A, At, x), n, sym, cmplx, bmat, 
#                              nsv, ncv, which, tol, maxiter, mode)

#     # Postprocessing to get eigenvalues and eigenvectors
#     (svals, svecs) = ARPACK.eupd_wrapper(T, n, sym, cmplx, bmat, nsv, which, ritzvec, 
#                                          tol, resid, ncv, v, ldv, sigma, iparam, ipntr, 
#                                          workd, workl, lworkl, rwork)
    
#     svals = sqrt(svals)
#     ritzvec ? (A*svecs*diagm(1./svals), svals, v[:,1:nsv].') : svals
# end

