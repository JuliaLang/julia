using .ARPACK

type ARPACKException <: Exception
    name::Symbol
    info::BlasInt
end

## eigs

function eigs{T<:BlasFloat}(A::AbstractMatrix{T};
                            nev::Integer=6, which::ASCIIString="LM", 
                            tol=0.0, maxiter::Integer=1000,
                            ritzvec::Bool=true)
    (m, n) = size(A)
    if m != n; error("Input must be square"); end
    if n <= 6 && nev > n-1; nev = n-1; end
    sym   = issym(A)
    cmplx = iscomplex(A)
    bmat  = "I"

    # Compute the Ritz values and Ritz vectors
    (select, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
          aupd_wrapper(T, (x) -> A * x, n, sym, cmplx, bmat, nev, which, tol, maxiter)

    # Postprocessing to get eigenvalues and eigenvectors
    return eupd_wrapper(T, n, sym, cmplx, bmat, nev, which, ritzvec,
                        select, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork)

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
    (select, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
         aupd_wrapper(T, (x) -> sarupdate(A, At, x), n, sym, cmplx, bmat, 
                      nsv, which, tol, maxiter)

    # Postprocessing to get eigenvalues and eigenvectors
    (svals, svecs) = eupd_wrapper(T, n, sym, cmplx, bmat, nsv, which, ritzvec, 
                                  select, tol, resid, ncv, v, ldv, iparam, ipntr, 
                                  workd, workl, lworkl, rwork)
    
    svals = sqrt(svals)
    ritzvec ? (A*svecs*diagm(1./svals), svals, v.') : svals
end

## aupd and eupd wrappers 

function aupd_wrapper(T, linop::Function, n::Integer,
                      sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, which::ASCIIString, 
                      tol, maxiter::Integer)

    ncv = min(max(nev*2, 20), n)

    bmat   = "I"
    lworkl = cmplx ? ncv * (3*ncv + 5) : ( lworkl = sym ? ncv * (ncv + 8) :  ncv * (3*ncv + 6) )
    TR     = cmplx ? T.types[1] : T
    tol = convert(TR, tol)

    v      = Array(T, n, ncv)
    workd  = Array(T, 3*n)
    workl  = Array(T, lworkl)
    rwork  = cmplx ? Array(TR, ncv) : Array(TR, 0)
    resid  = Array(T, n)
    select = Array(BlasInt, ncv)
    iparam = zeros(BlasInt, 11)
    ipntr  = zeros(BlasInt, 14)

    ido    = zeros(BlasInt, 1)
    info   = zeros(BlasInt, 1)

    iparam[1] = blas_int(1)       # ishifts
    iparam[3] = blas_int(maxiter) # maxiter
    iparam[7] = blas_int(1)       # mode 1

    zernm1 = 0:(n-1)

    while true
        if cmplx
            naupd(ido, bmat, n, which, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, rwork, info)            
        elseif sym
            saupd(ido, bmat, n, which, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, info)
        else
            naupd(ido, bmat, n, which, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, info)
        end
        if info[1] != 0; throw(ARPACKException(:aupd, info[1])); end
        if (ido[1] != -1 && ido[1] != 1); break; end
        workd[ipntr[2]+zernm1] = linop(getindex(workd, ipntr[1]+zernm1))
    end
    
    return (select, resid, ncv, v, n, iparam, ipntr, workd, workl, lworkl, rwork)
end

function eupd_wrapper(T, n::Integer, sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, which::ASCIIString, ritzvec::Bool,
                      select, tol, resid, ncv, v, ldv, iparam, ipntr,
                      workd, workl, lworkl, rwork)

    howmny = "A"
    info   = zeros(BlasInt, 1)
    
    if cmplx

        d = Array(T, nev+1)
        sigma = zeros(T, 1)
        workev = Array(T, 2ncv)
        neupd(ritzvec, howmny, select, d, v, ldv, workev, sigma,
              bmat, n, which, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[1] != 0; throw(ARPACKException(:eupd, info[1])); end
        return ritzvec ? (d, v[1:n, 1:nev]) : d

    elseif sym

        d = Array(T, nev)
        sigma = zeros(T, 1)
        seupd(ritzvec, howmny, select, d, v, ldv, sigma,
              bmat, n, which, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info) 
        if info[1] != 0; throw(ARPACKException(:eupd, info[1])); end
        return ritzvec ? (d, v[1:n, 1:nev]) : d

    else

        dr     = Array(T, nev+1)
        di     = Array(T, nev+1)
        sigmar = zeros(T, 1)
        sigmai = zeros(T, 1)
        workev = Array(T, 3*ncv)
        neupd(ritzvec, howmny, select, dr, di, v, ldv, sigmar, sigmai,
              workev, bmat, n, which, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[1] != 0; throw(ARPACKException(:eupd, info[1])); end
        evec = complex(zeros(T, n, nev+1), zeros(T, n, nev+1))
        j = 1
        while j <= nev
            if di[j] == 0.0
                evec[:,j] = v[:,j]
            else
                evec[:,j]   = v[:,j] + im*v[:,j+1]
                evec[:,j+1] = v[:,j] - im*v[:,j+1]
                j += 1
            end
            j += 1
        end
        d = complex(dr[1:nev],di[1:nev])
        return ritzvec ? (d, evec[1:n, 1:nev]) : d
    end
    
end
