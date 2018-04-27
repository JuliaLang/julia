# This file is a part of Julia. License is MIT: https://julialang.org/license

module ARPACK

import LinearAlgebra: BlasInt, ARPACKException

## aupd and eupd wrappers

function aupd_wrapper(T, matvecA!::Function, matvecB::Function, solveSI::Function, n::Integer,
                      sym::Bool, cmplx::Bool, bmat::String,
                      nev::Integer, ncv::Integer, which::String,
                      tol::Real, maxiter::Integer, mode::Integer, v0::Vector)
    lworkl = cmplx ? ncv * (3*ncv + 5) : (sym ? ncv * (ncv + 8) :  ncv * (3*ncv + 6) )
    TR = cmplx ? T.types[1] : T
    TOL = Ref{TR}(tol)

    v     = Matrix{T}(undef, n, ncv)
    workd = Vector{T}(undef, 3*n)
    workl = Vector{T}(undef, lworkl)
    rwork = cmplx ? Vector{TR}(undef, ncv) : Vector{TR}()

    if isempty(v0)
        resid = Vector{T}(undef, n)
        info  = Ref{BlasInt}(0)
    else
        resid = deepcopy(v0)
        info  = Ref{BlasInt}(1)
    end
    iparam = zeros(BlasInt, 11)
    ipntr  = zeros(BlasInt, (sym && !cmplx) ? 11 : 14)
    ido    = Ref{BlasInt}(0)

    iparam[1] = BlasInt(1)       # ishifts
    iparam[3] = BlasInt(maxiter) # maxiter
    iparam[7] = BlasInt(mode)    # mode

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
        if info[] != 0
            throw(ARPACKException(info[]))
        end

        x = view(workd, ipntr[1] .+ zernm1)
        y = view(workd, ipntr[2] .+ zernm1)
        if mode == 1  # corresponds to dsdrv1, dndrv1 or zndrv1
            if ido[] == 1
                matvecA!(y, x)
            elseif ido[] == 99
                break
            else
                throw(ARPACKException("unexpected behavior"))
            end
        elseif mode == 3 && bmat == "I" # corresponds to dsdrv2, dndrv2 or zndrv2
            if ido[] == -1 || ido[] == 1
                y[:] = solveSI(x)
            elseif ido[] == 99
                break
            else
                throw(ARPACKException("unexpected behavior"))
            end
        elseif mode == 2 # corresponds to dsdrv3, dndrv3 or zndrv3
            if ido[] == -1 || ido[] == 1
                matvecA!(y, x)
                if sym
                    x[:] = y    # overwrite as per Remark 5 in dsaupd.f
                end
                y[:] = solveSI(y)
            elseif ido[] == 2
                y[:] = matvecB(x)
            elseif ido[] == 99
                break
            else
                throw(ARPACKException("unexpected behavior"))
            end
        elseif mode == 3 && bmat == "G" # corresponds to dsdrv4, dndrv4 or zndrv4
            if ido[] == -1
                y[:] = solveSI(matvecB(x))
            elseif  ido[] == 1
                y[:] = solveSI(view(workd,ipntr[3] .+ zernm1))
            elseif ido[] == 2
                y[:] = matvecB(x)
            elseif ido[] == 99
                break
            else
                throw(ARPACKException("unexpected behavior"))
            end
        else
            throw(ArgumentError("ARPACK mode ($mode) not yet supported"))
        end
    end

    return (resid, v, n, iparam, ipntr, workd, workl, lworkl, rwork, TOL)
end

function eupd_wrapper(T, n::Integer, sym::Bool, cmplx::Bool, bmat::String,
                      nev::Integer, which::String, ritzvec::Bool,
                      TOL::Ref, resid, ncv::Integer, v, ldv, sigma, iparam, ipntr,
                      workd, workl, lworkl, rwork)
    howmny = "A"
    select = Vector{BlasInt}(undef, ncv)
    info   = Ref{BlasInt}(0)

    dmap = x -> abs.(x)
    if iparam[7] == 3 # shift-and-invert
        dmap = x -> abs.(1 ./ (x .- sigma))
    elseif which == "LR" || which == "LA" || which == "BE"
        dmap = real
    elseif which == "SR" || which == "SA"
        dmap = x -> -real(x)
    elseif which == "LI"
        dmap = imag
    elseif which == "SI"
        dmap = x -> -imag(x)
    end

    if cmplx
        d = Vector{T}(undef, nev+1)
        sigmar = Ref{T}(sigma)
        workev = Vector{T}(undef, 2ncv)
        neupd(ritzvec, howmny, select, d, v, ldv, sigmar, workev,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, rwork, info)
        if info[] != 0
            throw(ARPACKException(info[]))
        end

        p = sortperm(dmap(d[1:nev]), rev=true)
        return ritzvec ? (d[p], v[1:n, p],iparam[5],iparam[3],iparam[9],resid) : (d[p],iparam[5],iparam[3],iparam[9],resid)
    elseif sym
        d = Vector{T}(undef, nev)
        sigmar = Ref{T}(sigma)
        seupd(ritzvec, howmny, select, d, v, ldv, sigmar,
              bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[] != 0
            throw(ARPACKException(info[]))
        end

        p = sortperm(dmap(d), rev=true)
        return ritzvec ? (d[p], v[1:n, p],iparam[5],iparam[3],iparam[9],resid) : (d[p],iparam[5],iparam[3],iparam[9],resid)
    else
        dr = Vector{T}(undef, nev+1)
        di = Vector{T}(undef, nev+1)
        fill!(dr,NaN)
        fill!(di,NaN)
        sigmar = Ref{T}(real(sigma))
        sigmai = Ref{T}(imag(sigma))
        workev = Vector{T}(undef, 3*ncv)
        neupd(ritzvec, howmny, select, dr, di, v, ldv, sigmar, sigmai,
              workev, bmat, n, which, nev, TOL, resid, ncv, v, ldv,
              iparam, ipntr, workd, workl, lworkl, info)
        if info[] != 0
            throw(ARPACKException(info[]))
        end
        evec = complex.(Matrix{T}(undef, n, nev+1), Matrix{T}(undef, n, nev+1))

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
                throw(ARPACKException("unexpected behavior"))
            end
        end

        d = complex.(dr, di)

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
        function naupd(ido, bmat, n, evtype, nev, TOL::Ref{$T}, resid::Vector{$T}, ncv, v::Matrix{$T}, ldv,
                       iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl, info)
            ccall(($(string(naupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ref{BlasInt}, Clong, Clong),
                  ido, bmat, n, evtype, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, info, sizeof(bmat), sizeof(evtype))
        end

        function neupd(rvec, howmny, select, dr, di, z, ldz, sigmar, sigmai,
                  workev::Vector{$T}, bmat, n, evtype, nev, TOL::Ref{$T}, resid::Vector{$T}, ncv, v, ldv,
                  iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl, info)
            ccall(($(string(neupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ref{BlasInt},
                   Ref{$T}, Ref{$T}, Ptr{$T}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ref{BlasInt}, Clong, Clong, Clong),
                  rvec, howmny, select, dr, di, z, ldz,
                  sigmar, sigmai, workev, bmat, n, evtype, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, info, sizeof(howmny), sizeof(bmat), sizeof(evtype))
        end

        function saupd(ido, bmat, n, which, nev, TOL::Ref{$T}, resid::Vector{$T}, ncv, v::Matrix{$T}, ldv,
                       iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl, info)
            ccall(($(string(saupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ref{BlasInt}, Clong, Clong),
                  ido, bmat, n, which, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, info, sizeof(bmat), sizeof(which))
        end

        function seupd(rvec, howmny, select, d, z, ldz, sigma,
                       bmat, n, evtype, nev, TOL::Ref{$T}, resid::Vector{$T}, ncv, v::Matrix{$T}, ldv,
                       iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl, info)
            ccall(($(string(seupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt},
                   Ptr{$T}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ref{BlasInt}, Clong, Clong, Clong),
                  rvec, howmny, select, d, z, ldz,
                  sigma, bmat, n, evtype, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, info, sizeof(howmny), sizeof(bmat), sizeof(evtype))
        end
    end
end

for (T, TR, naupd_name, neupd_name) in
    ((:ComplexF64, :Float64, :znaupd_, :zneupd_),
     (:ComplexF32, :Float32, :cnaupd_, :cneupd_))
    @eval begin
        function naupd(ido, bmat, n, evtype, nev, TOL::Ref{$TR}, resid::Vector{$T}, ncv, v::Matrix{$T}, ldv,
                       iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl,
                       rwork::Vector{$TR}, info)
            ccall(($(string(naupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$TR}, Ref{BlasInt}),
                  ido, bmat, n, evtype, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, rwork, info)
        end

        function neupd(rvec, howmny, select, d, z, ldz, sigma, workev::Vector{$T},
                       bmat, n, evtype, nev, TOL::Ref{$TR}, resid::Vector{$T}, ncv, v::Matrix{$T}, ldv,
                       iparam, ipntr, workd::Vector{$T}, workl::Vector{$T}, lworkl,
                       rwork::Vector{$TR}, info)
            ccall(($(string(neupd_name)), :libarpack), Cvoid,
                  (Ref{BlasInt}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{UInt8}, Ref{BlasInt}, Ptr{UInt8}, Ref{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ref{BlasInt}, Ptr{$T}, Ref{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ref{BlasInt}, Ptr{$TR}, Ref{BlasInt}),
                  rvec, howmny, select, d, z, ldz,
                  sigma, workev, bmat, n, evtype, nev,
                  TOL, resid, ncv, v, ldv,
                  iparam, ipntr, workd, workl, lworkl, rwork, info)
        end
    end
end

end # module ARPACK
