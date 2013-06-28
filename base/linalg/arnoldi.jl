using .ARPACK

## eigs

function eigs{T<:BlasFloat}(A::AbstractMatrix{T};
                            nev::Integer=6, which::ASCIIString="LM", 
                            tol=0.0, maxiter::Integer=1000,
                            ritzvec::Bool=true)
    (m, n) = size(A)
    if m != n; error("Input must be square"); end
    if n <= 6 && nev > n-1; nev = n-1; end
    sym   = issym(A)
    cmplx = iseltype(A,Complex)
    bmat  = "I"

    # Compute the Ritz values and Ritz vectors
    (resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
       ARPACK.aupd_wrapper(T, (x) -> A * x, n, sym, cmplx, bmat, nev, which, tol, maxiter)
    
    # Postprocessing to get eigenvalues and eigenvectors
    return ARPACK.eupd_wrapper(T, n, sym, cmplx, bmat, nev, which, ritzvec,
                               tol, resid, ncv, v, ldv, iparam, ipntr, 
                               workd, workl, lworkl, rwork)

end

## svds

# For a dense matrix A is ignored and At is actually A'*A
sarupdate{T}(A::StridedMatrix{T}, At::StridedMatrix{T}, X::StridedVector{T}) = BLAS.symv('U', one(T), At, X)
sarupdate{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, At::SparseMatrixCSC{Tv,Ti}, X::StridedVector{Tv}) = At*(A*X)

function svds{T<:Union(Float64,Float32)}(A::AbstractMatrix{T};
                                         nsv::Integer=6, which::ASCIIString="LA", 
                                         tol=0.0, maxiter::Integer=1000,
                                         ritzvec::Bool=true)

    (m, n) = size(A)
    if m < n; error("m = $m, n = $n and only the m >= n case is implemented"); end
    sym   = true
    cmplx = false
    bmat  = "I"
    At = isa(A, StridedMatrix) ? BLAS.syrk('U','T',1.0,A) : A'

    # Compute the Ritz values and Ritz vectors
    (resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
         ARPACK.aupd_wrapper(T, (x) -> sarupdate(A, At, x), n, sym, cmplx, bmat, 
                             nsv, which, tol, maxiter)

    # Postprocessing to get eigenvalues and eigenvectors
    (svals, svecs) = ARPACK.eupd_wrapper(T, n, sym, cmplx, bmat, nsv, which, ritzvec, 
                                         tol, resid, ncv, v, ldv, iparam, ipntr, 
                                         workd, workl, lworkl, rwork)
    
    svals = sqrt(svals)
    ritzvec ? (A*svecs*diagm(1./svals), svals, v.') : svals
end

