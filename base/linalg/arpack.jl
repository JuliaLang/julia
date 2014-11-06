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

        load_idx = ipntr[1]+zernm1
        store_idx = ipntr[2]+zernm1
        x = workd[load_idx]
        if mode == 1  # corresponds to dsdrv1, dndrv1 or zndrv1
            if ido[1] == 1
                workd[store_idx] = matvecA(x)
            elseif ido[1] == 99
                break
            else
                error("Internal ARPACK error")
            end
        elseif mode == 3 && bmat == "I" # corresponds to dsdrv2, dndrv2 or zndrv2
            if ido[1] == -1 || ido[1] == 1
                workd[store_idx] = solveSI(x)
            elseif ido[1] == 99
                break
            else
                error("Internal ARPACK error")
            end
        elseif mode == 2 # corresponds to dsdrv3, dndrv3 or zndrv3
            if ido[1] == -1 || ido[1] == 1
                tmp = matvecA(x)
                if sym
                    workd[load_idx] = tmp    # overwrite as per Remark 5 in dsaupd.f
                end
                workd[store_idx] = solveSI(tmp)
            elseif ido[1] == 2
                workd[store_idx] = matvecB(x)
            elseif ido[1] == 99
                break
            else
                error("Internal ARPACK error")
            end
        elseif mode == 3 && bmat == "G" # corresponds to dsdrv4, dndrv4 or zndrv4
            if ido[1] == -1
                workd[store_idx] = solveSI(matvecB(x))
            elseif  ido[1] == 1
                workd[store_idx] = solveSI(workd[ipntr[3]+zernm1])
            elseif ido[1] == 2
                workd[store_idx] = matvecB(x)
            elseif ido[1] == 99
                break
            else
                error("Internal ARPACK error")
            end
        else
            error("ARPACK mode not yet supported")
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

    dmap = x->abs(x)
    if iparam[7] == 3 # shift-and-invert
        dmap = x->abs(1./(x-sigma))
    elseif which == "LR" || which == "LA" || which == "BE"
        dmap = x->real(x)
    elseif which == "SR" || which == "SA"
        dmap = x->-real(x)
    elseif which == "LI"
        dmap = x->imag(x)
    elseif which == "SI"
        dmap = x->-imag(x)
    end

    if cmplx

        d = Array(T, nev+1)
        sigmar = ones(T, 1)*sigma
        workev = Array(T, 2ncv)
        neupd(ritzvec, howmny, select, d, v, ldv, sigmar, workev,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[1] != 0; throw(ARPACKException(info[1])); end

        p = sortperm(dmap(d[1:nev]), rev=true)
        return ritzvec ? (d[p], v[1:n, p],iparam[5],iparam[3],iparam[9],resid) : (d[p],iparam[5],iparam[3],iparam[9],resid)

    elseif sym

        d = Array(T, nev)
        sigmar = ones(T, 1)*sigma
        seupd(ritzvec, howmny, select, d, v, ldv, sigmar,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[1] != 0; throw(ARPACKException(info[1])); end

        p = sortperm(dmap(d), rev=true)
        return ritzvec ? (d[p], v[1:n, p],iparam[5],iparam[3],iparam[9],resid) : (d,iparam[5],iparam[3],iparam[9],resid)

    else

        dr     = Array(T, nev+1)
        di     = Array(T, nev+1)
        fill!(dr,NaN)
        fill!(di,NaN)
        sigmar = ones(T, 1)*real(sigma)
        sigmai = ones(T, 1)*imag(sigma)
        workev = Array(T, 3*ncv)
        neupd(ritzvec, howmny, select, dr, di, v, ldv, sigmar, sigmai,
              workev, bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[1] != 0; throw(ARPACKException(info[1])); end
        evec = complex(Array(T, n, nev+1), Array(T, n, nev+1))

        j = 1
        while j <= nev
            if di[j] == 0
                evec[:,j] = v[:,j]
            else # For complex conjugate pairs
                evec[:,j]   = v[:,j] + im*v[:,j+1]
                evec[:,j+1] = v[:,j] - im*v[:,j+1]
                j += 1
            end
            j += 1
        end
        if j == nev+1 && !isnan(di[j])
            if di[j] == 0
                evec[:,j] = v[:,j]
                j += 1
            else
                error("Arpack unexpected behavior")
            end
        end

        d = complex(dr,di)

        if j == nev+1
            p = sortperm(dmap(d[1:nev]), rev=true)
        else
            p = sortperm(dmap(d), rev=true)
            p = p[1:nev]
        end

        return ritzvec ? (d[p], evec[1:n, p],iparam[5],iparam[3],iparam[9],resid) : (d[p],iparam[5],iparam[3],iparam[9],resid)
    end

end

for (T, saupd_name, seupd_name, naupd_name, neupd_name) in
    ((:Float64, :dsaupd_, :dseupd_, :dnaupd_, :dneupd_),
     (:Float32, :ssaupd_, :sseupd_, :snaupd_, :sneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong),
                  ido, bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info, sizeof(bmat), sizeof(evtype))
        end

        function neupd(rvec, howmny, select, dr, di, z, ldz, sigmar, sigmai,
                  workev::Array{$T}, bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v, ldv,
                  iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{UInt8}, Ptr{BlasInt},
                   Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
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
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}, Clong, Clong),
                  ido, bmat, &n, which, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info, sizeof(bmat), sizeof(which))

        end

        function seupd(rvec, howmny, select, d, z, ldz, sigma,
                       bmat, n, evtype, nev, TOL::Array{$T}, resid::Array{$T}, ncv, v::Array{$T}, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(seupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{UInt8}, Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt},
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
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt},
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
                  (Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{UInt8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$TR}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, sigma, workev,
                  bmat, &n, evtype, &nev, TOL, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info)

        end

    end
end

end # module ARPACK
