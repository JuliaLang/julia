using ARPACK

## eigs

function eigs{T <: BlasFloat}(A::AbstractMatrix{T}, nev::Integer, evtype::ASCIIString, rvec::Bool)
    (m, n) = size(A)
    if m != n; error("Input must be square"); end
    sym   = issym(A)
    cmplx = iscomplex(A)
    bmat  = "I"

    # Compute the Ritz values and Ritz vectors
    (select, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
          aupd_wrapper(T, n, sym, cmplx, bmat, nev, evtype, (x) -> A * x)

    # Postprocessing to get eigenvalues and eigenvectors
    return eupd_wrapper(T, n, sym, cmplx, bmat, nev, evtype, rvec,
                        select, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork)

end

eigs(A::AbstractMatrix, nev::Integer, typ::ASCIIString) = eigs(A, nev, which, true)
eigs(A::AbstractMatrix, nev::Integer, rvec::Bool) = eigs(A, nev, "LM", rvec)
eigs(A::AbstractMatrix, rvec::Bool) = eigs(A, 6, "LM", rvec)
eigs(A::AbstractMatrix, nev::Integer) = eigs(A, nev, "LM", true)
eigs(A::AbstractMatrix) = eigs(A, 6, "LM", true)

## svds

# For a dense matrix A is ignored and At is actually A'*A
sarupdate{T}(A::StridedMatrix{T}, At::StridedMatrix{T}, X::StridedVector{T}) = BLAS.symv('U', one(T), At, X)
sarupdate{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, At::SparseMatrixCSC{Tv,Ti}, X::StridedVector{Tv}) = At*(A*X)

function svds{T <: Union(Float64,Float32)}(A::AbstractMatrix{T}, nev::Integer, 
                                           which::ASCIIString, rvec::Bool)

    (m, n) = size(A)
    if m < n error("m = $m, n = $n and only the m >= n case is implemented") end
    sym   = true
    cmplx = false
    bmat  = "I"
    At = isa(A, StridedMatrix) ? BLAS.syrk('U','T',1.0,A) : A'

    # Compute the Ritz values and Ritz vectors
    (select, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork) = 
         aupd_wrapper(T, n, sym, cmplx, bmat, nev, which, (x) -> sarupdate(A, At, x))

    # Postprocessing to get eigenvalues and eigenvectors
    (svals, svecs) = eupd_wrapper(T, n, sym, cmplx, bmat, nev, which, rvec, 
                                  select, tol, resid, ncv, v, ldv, iparam, ipntr, 
                                  workd, workl, lworkl, rwork)
    
    svals = sqrt(svals)
    rvec ? (A*svecs*diagm(1./svals), svals, v.') : svals
end

svds(A::AbstractMatrix, nev::Integer, which::ASCIIString) = svds(A, nev, which, true)
svds(A::AbstractMatrix, nev::Integer, rvec::Bool) = svds(A, nev, "LA", rvec)
svds(A::AbstractMatrix, rvec::Bool) = svds(A, 6, "LA", rvec)
svds(A::AbstractMatrix, nev::Integer) = svds(A, nev, "LA", true)
svds(A::AbstractMatrix) = svds(A, 6, "LA", true)

## aupd and eupd wrappers 

function aupd_wrapper(T, n::Integer, sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, evtype::ASCIIString, linop::Function)

    ncv = min(max(nev*2, 20), n)

    bmat   = "I"
    lworkl = cmplx ? ncv * (3*ncv + 5) : ( lworkl = sym ? ncv * (ncv + 8) :  ncv * (3*ncv + 6) )

    v      = Array(T, n, ncv)
    TR = typeof(real(v[1]))
    workd  = Array(T, 3*n)
    workl  = Array(T, lworkl)
    rwork  = cmplx ? Array(TR, ncv) : Array(TR, 0)
    resid  = Array(T, n)
    select = Array(BlasInt, ncv)
    iparam = zeros(BlasInt, 11)
    ipntr  = zeros(BlasInt, 14)

    tol    = zeros(TR, 1)
    ido    = zeros(BlasInt, 1)
    info   = zeros(BlasInt, 1)

    iparam[1] = blas_int(1)    # ishifts
    iparam[3] = blas_int(1000) # maxitr
    iparam[7] = blas_int(1)    # mode 1

    zernm1 = 0:(n-1)

    while true
        if cmplx
            naupd(ido, bmat, n, evtype, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, rwork, info)            
        elseif sym
            saupd(ido, bmat, n, evtype, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, info)
        else
            naupd(ido, bmat, n, evtype, nev, tol, resid, ncv, v, n, 
                  iparam, ipntr, workd, workl, lworkl, info)
        end
        if info[1] != 0; error("error code $(info[1]) from ARPACK aupd"); end
        if (ido[1] != -1 && ido[1] != 1); break; end
        workd[ipntr[2]+zernm1] = linop(getindex(workd, ipntr[1]+zernm1))
    end
    
    return (select, tol, resid, ncv, v, n, iparam, ipntr, workd, workl, lworkl, rwork)
end

function eupd_wrapper(T, n::Integer, sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, evtype::ASCIIString, rvec::Bool, 
                      select, tol, resid, ncv, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork)

    howmny = "A"
    info   = zeros(BlasInt, 1)
    
    if cmplx

        d = Array(T, nev+1)
        sigma = zeros(T, 1)
        workev = Array(T, 2ncv)
        neupd(rvec, howmny, select, d, v, ldv, workev, sigma,
              bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[1] != 0; error("error code $(info[1]) from ARPACK eupd"); end
        return rvec ? (d, v[1:n, 1:nev]) : d

    elseif sym

        d = Array(T, nev)
        sigma = zeros(T, 1)
        seupd(rvec, howmny, select, d, v, ldv, sigma,
              bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info) 
        if info[1] != 0; error("error code $(info[1]) from ARPACK eupd"); end
        return rvec ? (d, v[1:n, 1:nev]) : d

    else

        dr     = Array(T, nev+1)
        di     = Array(T, nev+1)
        sigmar = zeros(T, 1)
        sigmai = zeros(T, 1)
        workev = Array(T, 3*ncv)
        neupd(rvec, howmny, select, dr, di, v, ldv, sigmar, sigmai,
              workev, bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[1] != 0; error("error code $(info[1]) from ARPACK eupd"); end
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
        return rvec ? (d, evec[1:n, 1:nev]) : d
    end
    
end
