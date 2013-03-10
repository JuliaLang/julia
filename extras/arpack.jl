module ARPACK 

export eigs, svds

const libarpack = "libarpack"

import Base.BlasInt
import Base.blas_int

# For a dense matrix A is ignored and At is actually A'*A
sarupdate{T}(A::StridedMatrix{T}, At::StridedMatrix{T}, X::StridedVector{T}) = BLAS.symv('U', one(T), At, X)
sarupdate{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, At::SparseMatrixCSC{Tv,Ti}, X::StridedVector{Tv}) = At*(A*X)

for (T, saupd, seupd, naupd, neupd) in
    ((:Float64, :dsaupd_, :dseupd_, :dnaupd_, :dneupd_),
     (:Float32, :ssaupd_, :sseupd_, :snaupd_, :sneupd_))
    @eval begin
        function eigs(A::AbstractMatrix{$T}, nev::Integer, evtype::ASCIIString, rvec::Bool)
            (m, n) = size(A)
            if m  != n error("eigs: matrix A is $m by $n but must be square") end
            sym    = issym(A)
            if n <= nev nev = n - 1 end

            ncv = min(max(nev*2, 20), n)
#           if ncv-nev < 2 || ncv > n error("Compute fewer eigenvalues using eigs(A, k)") end

           bmat   = "I"
           lworkl = sym ? ncv * (ncv + 8) :  ncv * (3*ncv + 6)

           v      = Array($T, n, ncv)
           workd  = Array($T, 3*n)
           workl  = Array($T, lworkl)
           resid  = Array($T, n)
           select = Array(BlasInt, ncv)
           iparam = zeros(BlasInt, 11)
           ipntr  = zeros(BlasInt, 14)

           tol    = zeros($T, 1)
           ido    = zeros(BlasInt, 1)
           info   = zeros(BlasInt, 1)

           iparam[1] = blas_int(1)    # ishifts
           iparam[3] = blas_int(1000) # maxitr
           iparam[7] = blas_int(1)    # mode 1

           zernm1 = 0:(n-1)

           while true
               if sym
                   ccall(($(string(saupd)), libarpack), Void,
                         (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                          Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                          Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                         ido, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n, 
                         iparam, ipntr, workd, workl, &lworkl, info)
               else
                   ccall(($(string(naupd)), libarpack), Void,
                         (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                          Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                          Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                         ido, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n, 
                         iparam, ipntr, workd, workl, &lworkl, info)
               end
               if info[1] != 0 error("error code $(info[1]) from ARPACK aupd") end
               if (ido[1] != -1 && ido[1] != 1) break end
               workd[ipntr[2]+zernm1] = A*getindex(workd, ipntr[1]+zernm1)
           end

           howmny = "A"

           if sym
               d = Array($T, nev)
               sigma = zeros($T, 1)

               ccall(($(string(seupd)), libarpack), Void,
                     (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                      Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                      Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                      Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                     &rvec, howmny, select, d, v, &n, sigma,
                     bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n,
                     iparam, ipntr, workd, workl, &lworkl, info) 
               if info[1] != 0 error("error code $(info[1]) from ARPACK eupd") end
               return rvec ? (d, v[1:n, 1:nev]) : d
           end
           dr     = Array($T, nev+1)
           di     = Array($T, nev+1)
           sigmar = zeros($T, 1)
           sigmai = zeros($T, 1)
           workev = Array($T, 3*ncv)
            ccall(($(string(neupd)), libarpack), Void,
                 (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T},
                  Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt},
                  Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                  Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T},
                  Ptr{BlasInt}, Ptr{BlasInt}),
                 &rvec, howmny, select, dr, di, v, &n, sigmar, sigmai,
                 workev, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n,
                 iparam, ipntr, workd, workl, &lworkl, info)
           if info[1] != 0 error("error code $(info[1]) from ARPACK eupd") end
           evec = complex(zeros($T, n, nev+1), zeros($T, n, nev+1))
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
           complex(dr[1:nev],di[1:nev]), evec[1:n, 1:nev]
       end
   end
end

for (T, TR, naupd, neupd) in
    ((:Complex128, :Float64, :znaupd_, :zneupd_),
     (:Complex64, :Float32, :cnaupd_, :cneupd_))
   @eval begin
       function eigs(A::AbstractMatrix{$T}, nev::Integer, evtype::ASCIIString, rvec::Bool)
           (m, n) = size(A)
           if m  != n error("eigs: matrix A is $m by $n but must be square") end
           if n <= nev nev = n - 1 end

           ncv = min(max(nev*2, 20), n)
#           if ncv-nev < 2 || ncv > n error("Compute fewer eigenvalues using eigs(A, k)") end

           bmat   = "I"
           lworkl = ncv * (3*ncv + 5)

           v      = Array($T, n, ncv)
           workd  = Array($T, 3*n)
           workl  = Array($T, lworkl)
           rwork  = Array($TR, ncv)
           resid  = Array($T, n)
           select = Array(BlasInt, ncv)
           iparam = zeros(BlasInt, 11)
           ipntr  = zeros(BlasInt, 14)

           tol    = zeros($TR, 1)
           ido    = zeros(BlasInt, 1)
           info   = zeros(BlasInt, 1)

           iparam[1] = blas_int(1)    # ishifts
           iparam[3] = blas_int(1000) # maxitr
           iparam[7] = blas_int(1)    # mode 1

           zernm1 = 0:(n-1)

           while true
               ccall(($(string(naupd)), libarpack), Void,
                         (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                          Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                          Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                          Ptr{$TR}, Ptr{BlasInt}),
                         ido, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n, 
                         iparam, ipntr, workd, workl, &lworkl, rwork, info)
               if info[1] != 0 error("error code $(info[1]) from ARPACK aupd") end
               if (ido[1] != -1 && ido[1] != 1) break end
               workd[ipntr[2]+zernm1] = A*getindex(workd, ipntr[1]+zernm1)
           end

           howmny = "A"

           d = Array($T, nev+1)
           sigma = zeros($T, 1)
           workev = Array($T, 2ncv)
           ccall(($(string(neupd)), libarpack), Void,
                 (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                  Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                  Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                  Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$TR}, Ptr{BlasInt}),
                 &rvec, howmny, select, d, v, &n, workev, sigma,
                 bmat, &n, evtype, &nev, tol, resid, &ncv, v, &n,
                 iparam, ipntr, workd, workl, &lworkl, rwork, info) 
           if info[1] != 0 error("error code $(info[1]) from ARPACK eupd") end
           rvec ? (d, v[1:n, 1:nev]) : d
       end
   end
end

eigs(A::AbstractMatrix, nev::Integer, typ::ASCIIString) = eigs(A, nev, which, true)
eigs(A::AbstractMatrix, nev::Integer, rvec::Bool) = eigs(A, nev, "LM", rvec)
eigs(A::AbstractMatrix, rvec::Bool) = eigs(A, 6, "LM", rvec)
eigs(A::AbstractMatrix, nev::Integer) = eigs(A, nev, "LM", true)
eigs(A::AbstractMatrix) = eigs(A, 6, "LM", true)


# For a dense matrix A is ignored and At is actually A'*A
sarupdate{T}(A::StridedMatrix{T}, At::StridedMatrix{T}, X::StridedVector{T}) = BLAS.symv('U', one(T), At, X)
sarupdate{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, At::SparseMatrixCSC{Tv,Ti}, X::StridedVector{Tv}) = At*(A*X)

for (T, saupd, seupd) in ((:Float64, :dsaupd_, :dseupd_), (:Float32, :ssaupd_, :sseupd_))
   @eval begin
       function svds(A::AbstractMatrix{$T}, nev::Integer, which::ASCIIString, rvec::Bool)
           (m, n) = size(A)
           if m < n error("m = $m, n = $n and only the m >= n case is implemented") end
           if n <= nev nev = n - 1 end

           At = isa(A, StridedMatrix) ? BLAS.syrk('U','T',1.,A) : A'
    
           ncv    = min(max(nev*2, 20), n)
           lworkl = ncv*(ncv+8)

           v      = Array($T, n, ncv)
           workd  = Array($T, 3n)
           workl  = Array($T, lworkl)
           resid  = Array($T, n)
           select = Array(BlasInt, ncv)
           iparam = zeros(BlasInt, 11)
           iparam[1] = 1                # ishifts
           iparam[3] = 1000             # maxitr
           iparam[7] = 1                # mode 1
           ipntr  = zeros(BlasInt, 14)
    
           tol    = zeros($T, 1)
           sigma  = zeros($T, 1)
           ido    = zeros(BlasInt, 1)
           info   = Array(BlasInt, 1)
           bmat   = "I"
           zernm1 = 0:(n-1)

           while true
               ccall(($(string(saupd)), libarpack), Void,
                     (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                      Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                      Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                     ido, bmat, &n, which, &nev, tol, resid, &ncv, v, &n, 
                     iparam, ipntr, workd, workl, &lworkl, info)
               if (info[1] < 0) error("error code $(info[1]) from ARPACK saupd") end
               if (ido[1] != -1 && ido[1] != 1) break end
               workd[ipntr[2]+zernm1] = sarupdate(A, At, getindex(workd, ipntr[1]+zernm1))
           end

           d      = Array($T, nev)
           howmny = "A"

           ccall(($(string(seupd)), libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                 &rvec, howmny, select, d, v, &n, sigma,
                 bmat, &n, which, &nev, tol, resid, &ncv, v, &n,
                 iparam, ipntr, workd, workl, &lworkl, info)
           if info[1] != 0 error("error code $(info[1]) from ARPACK eupd") end
           d = sqrt(d)
           if !rvec return d end
           v = v[1:n, 1:nev]
           A*v*diagm(1./d), d, v.'
       end
   end
end

svds(A::AbstractMatrix, nev::Integer, which::ASCIIString) = svds(A, nev, which, true)
svds(A::AbstractMatrix, nev::Integer, rvec::Bool) = svds(A, nev, "LA", rvec)
svds(A::AbstractMatrix, rvec::Bool) = svds(A, 6, "LA", rvec)
svds(A::AbstractMatrix, nev::Integer) = svds(A, nev, "LA", true)
svds(A::AbstractMatrix) = svds(A, 6, "LA", true)

end #module ARPACK
