module ARPACK 

import ..LinAlg: BlasInt, blas_int, ARPACKException

## aupd and eupd wrappers 

function aupd_wrapper(T, matvecA::Function, matvecB::Function, solveSI::Function, n::Integer,
                      sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, ncv::Integer, which::ASCIIString, 
                      tol::Real, maxiter::Integer, mode::Integer, v0::Vector)

    lworkl = cmplx ? ncv * (3*ncv + 5) : (sym ? ncv * (ncv + 8) :  ncv * (3*ncv + 6) )
    TR = cmplx ? T.types[1] : T
    TOL = Array(TR, 1)
    TOL[1] = tol

    v      = Array(T, n, ncv)
    workd  = Array(T, 3*n)
    workl  = Array(T, lworkl)
    rwork  = cmplx ? Array(TR, ncv) : Array(TR, 0)

    if isempty(v0)
    	resid  = Array(T, n)
    	info   = zeros(BlasInt, 1)
    else
	resid  = deepcopy(v0)
    	info   = ones(BlasInt, 1)
    end
    iparam = zeros(BlasInt, 11)
    ipntr  = zeros(BlasInt, (sym && !cmplx) ? 11 : 14)
    ido    = zeros(BlasInt, 1)

    iparam[1] = blas_int(1)       # ishifts
    iparam[3] = blas_int(maxiter) # maxiter
    iparam[7] = blas_int(mode)    # mode

    zernm1 = 0:(n-1)

    while true
        if cmplx
            naupd(ido, bmat, n, which, nev, TOL, resid, ncv, v, n,
                  iparam, ipntr, workd, workl, lworkl, rwork, info)            
        elseif sym
            saupd(ido, bmat, n, which, nev, TOL, resid, ncv, v, n,
                  iparam, ipntr, workd, workl, lworkl, info)
        else
            naupd(ido, bmat, n, which, nev, TOL, resid, ncv, v, n,
                  iparam, ipntr, workd, workl, lworkl, info)
        end

        # Check for warnings and errors
        # Refer to ex-*.doc files in ARPACK/DOCUMENTS for calling sequence
        if info[1] == 3; warn("try eigs/svds with a larger value for ncv"); end
        if info[1] == 1; warn("maximum number of iterations reached; check nconv for number of converged eigenvalues"); end
        if info[1] < 0; throw(ARPACKException(info[1])); end

        load_idx = ipntr[1]+zernm1
        store_idx = ipntr[2]+zernm1
        x = workd[load_idx]
        if ido[1] == -1
            if mode == 1
                workd[store_idx] = matvecA(x)
            elseif mode == 2
                if sym
                    temp = matvecA(x)
                    workd[load_idx] = temp    # overwrite as per Remark 5 in dsaupd.f
                    workd[store_idx] = solveSI(temp)
                else
                    workd[store_idx] = solveSI(matvecA(x))
                end
            elseif mode == 3
                if bmat == "I"
                    workd[store_idx] = solveSI(x)
                elseif bmat == "G"
                    workd[store_idx] = solveSI(matvecB(x))
                end
            end
        elseif ido[1] == 1
            if mode == 1
                workd[store_idx] = matvecA(x)
            elseif mode == 2
                if sym
                    temp = matvecA(x)
                    workd[load_idx] = temp
                    workd[store_idx] = solveSI(temp)
                else
                    workd[store_idx] = solveSI(matvecA(x))
                end
            elseif mode == 3
                workd[store_idx] = solveSI(workd[ipntr[3]+zernm1])
            end
        elseif ido[1] == 2
            workd[store_idx] = matvecB(x)
        elseif ido[1] == 99
            break
        else
            error("Internal ARPACK error")
        end
    end
    
    return (resid, v, n, iparam, ipntr, workd, workl, lworkl, rwork, TOL)
end

function eupd_wrapper(T, n::Integer, sym::Bool, cmplx::Bool, bmat::ASCIIString,
                      nev::Integer, which::ASCIIString, ritzvec::Bool,
                      TOL::Array, resid, ncv::Integer, v, ldv, sigma, iparam, ipntr,
                      workd, workl, lworkl, rwork)

    howmny = "A"
    select = Array(BlasInt, ncv)
    info   = zeros(BlasInt, 1)
    
    if cmplx

        d = Array(T, nev+1)
        sigma = ones(T, 1)*sigma
        workev = Array(T, 2ncv)
        neupd(ritzvec, howmny, select, d, v, ldv, sigma, workev,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[1] != 0; throw(ARPACKException(info[1])); end
        return ritzvec ? (d[1:nev], v[1:n, 1:nev],iparam[5],iparam[3],iparam[9],resid) : (d[1:nev],iparam[5],iparam[3],iparam[9],resid)

    elseif sym

        d = Array(T, nev)
        sigma = ones(T, 1)*sigma
        seupd(ritzvec, howmny, select, d, v, ldv, sigma,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info) 
        if info[1] != 0; throw(ARPACKException(info[1])); end
        return ritzvec ? (d, v[1:n, 1:nev],iparam[5],iparam[3],iparam[9],resid) : (d,iparam[5],iparam[3],iparam[9],resid)

    else

        dr     = Array(T, nev+1)
        di     = Array(T, nev+1)
        sigmar = ones(T, 1)*real(sigma)
        sigmai = ones(T, 1)*imag(sigma)
        workev = Array(T, 3*ncv)
        neupd(ritzvec, howmny, select, dr, di, v, ldv, sigmar, sigmai,
              workev, bmat, n, which, nev, TOL, resid, ncv, v, ldv,
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
        return ritzvec ? (d, evec[1:n, 1:nev],iparam[5],iparam[3],iparam[9],resid) : (d,iparam[5],iparam[3],iparam[9],resid)
    end
    
end

for (T, saupd_name, seupd_name, naupd_name, neupd_name) in
    ((:Float64, :dsaupd_, :dseupd_, :dnaupd_, :dneupd_),
     (:Float32, :ssaupd_, :sseupd_, :snaupd_, :sneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong),
                  ido, bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info, sizeof(bmat), sizeof(evtype))
        end

        function neupd(rvec, howmny, select, dr, di, z, ldz, sigmar, sigmai,
                  workev::Array{$T}, bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v, ldv,
                  iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong, Clong),
                  &rvec, howmny, select, dr, di, z, &ldz, sigmar, sigmai,
                  workev, bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info,
                  sizeof(howmny), sizeof(bmat), sizeof(evtype))
        end

        function saupd(ido, bmat, n, which, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v::Array{$T}, ldv, 
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(saupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong),
                  ido, bmat, &n, which, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info, sizeof(bmat), sizeof(which))

        end

        function seupd(rvec, howmny, select, d, z, ldz, sigma,
                       bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info) 

            ccall(($(string(seupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong, Clong),
                  &rvec, howmny, select, d, z, &ldz, sigma,
                  bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info, sizeof(howmny), sizeof(bmat), sizeof(evtype))
        end

    end
end

for (T, TR, naupd_name, neupd_name) in
    ((:Complex128, :Float64, :znaupd_, :zneupd_),
     (:Complex64,  :Float32, :cnaupd_, :cneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, TOL::Array{$TR}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{BlasInt}),
                  ido, bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info)

        end

        function neupd(rvec, howmny, select, d, z, ldz, sigma, workev::Array{$T},
                       bmat, n, evtype, nev, TOL::Array{$TR}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$TR}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, sigma, workev,
                  bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info) 

        end

    end
end

end # module ARPACK
