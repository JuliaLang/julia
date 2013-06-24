module ARPACK 

import ..LinAlg: BlasInt, blas_int, ARPACKException

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
        if info[1] != 0; throw(ARPACKException(info[1])); end
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
    TR     = cmplx ? T.types[1] : T
    tol = convert(TR, tol)
    info   = zeros(BlasInt, 1)
    
    if cmplx

        d = Array(T, nev+1)
        sigma = zeros(T, 1)
        workev = Array(T, 2ncv)
        neupd(ritzvec, howmny, select, d, v, ldv, sigma, workev,
              bmat, n, which, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[1] != 0; throw(ARPACKException(info[1])); end
        return ritzvec ? (d, v[1:n, 1:nev]) : d

    elseif sym

        d = Array(T, nev)
        sigma = zeros(T, 1)
        seupd(ritzvec, howmny, select, d, v, ldv, sigma,
              bmat, n, which, nev, tol, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info) 
        if info[1] != 0; throw(ARPACKException(info[1])); end
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
        if info[1] != 0; throw(ARPACKException(info[1])); end
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

for (T, saupd_name, seupd_name, naupd_name, neupd_name) in
    ((:Float64, :dsaupd_, :dseupd_, :dnaupd_, :dneupd_),
     (:Float32, :ssaupd_, :sseupd_, :snaupd_, :sneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, tol::$T, resid, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  ido, bmat, &n, evtype, &nev, &tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

        function neupd(rvec, howmny, select, dr, di, z, ldz, sigmar, sigmai,
                  workev::Array{$T}, bmat, n, evtype, nev, tol::$T, resid, ncv, v, ldv,
                  iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}),
                  &rvec, howmny, select, dr, di, z, &ldz, sigmar, sigmai,
                  workev, bmat, &n, evtype, &nev, &tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

        function saupd(ido, bmat, n, which, nev, tol::$T, resid, ncv, v::Array{$T}, ldv, 
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(saupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  ido, bmat, &n, which, &nev, &tol, resid, &ncv, v, &ldv, 
                  iparam, ipntr, workd, workl, &lworkl, info)

        end

        function seupd(rvec, howmny, select, d, z, ldz, sigma,
                       bmat, n, evtype, nev, tol::$T, resid, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info) 

            ccall(($(string(seupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, sigma,
                  bmat, &n, evtype, &nev, &tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

    end
end

for (T, TR, naupd_name, neupd_name) in
    ((:Complex128, :Float64, :znaupd_, :zneupd_),
     (:Complex64,  :Float32, :cnaupd_, :cneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, tol::$TR, resid::Array{$T}, ncv, v::Array{$T}, ldv, 
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{BlasInt}),
                  ido, bmat, &n, evtype, &nev, &tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info)

        end

        function neupd(rvec, howmny, select, d, z, ldz, sigma, workev::Array{$T},
                       bmat, n, evtype, nev, tol::$TR, resid, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$TR}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, sigma, workev,
                  bmat, &n, evtype, &nev, &tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info) 

        end

    end
end

end # module ARPACK
