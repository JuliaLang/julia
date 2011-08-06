libarpack = dlopen("libarpack")

macro jl_arpack_aupd_macro(T, Tc, saupd, real_naupd, complex_naupd)
    quote

        # call dsaupd
        #  ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #    IPNTR, WORKD, WORKL, LWORKL, INFO )
        function jl_arpack_saupd(ido, bmat, n, which, nev, 
                                 tol, resid, ncv, v::Array{$T}, ldv, 
                                 iparam, ipntr, workd, workl, lworkl, info)
            ccall(dlsym(libarpack, $saupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{Int32}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), info)
        end

        #  call dnaupd
        #     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #       IPNTR, WORKD, WORKL, LWORKL, INFO )
        function jl_arpack_naupd(ido, bmat, n, which, nev, 
                                 tol, resid, ncv, v::Array{$T}, ldv, 
                                 iparam, ipntr, workd, workl, lworkl, info)
            ccall(dlsym(libarpack, $real_naupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{Int32}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), info)
        end

        #  call znaupd
        #     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #       IPNTR, WORKD, WORKL, LWORKL, RWORK, INFO )
        function jl_arpack_naupd(ido, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$Tc}, ldv,
                                 iparam, ipntr, workd, workl, lworkl, rwork, info)
            ccall(dlsym(libarpack, $complex_naupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc}, Ptr{Int32},
                   Ptr{Int32}, Ptr{Int32}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), rwork, info)
        end

    end
end

@jl_arpack_aupd_macro Float64 Complex128 "dsaupd_" "dnaupd_" "znaupd_"
@jl_arpack_aupd_macro Float32 Complex64  "ssaupd_" "snaupd_" "cnaupd_"

macro jl_arpack_eupd_macro(T, Tc, seupd, real_neupd, complex_neupd)
    quote

        #  call dseupd  
        #     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, BMAT, N, WHICH, NEV, TOL,
        #       RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, LWORKL, INFO )
        function jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$T}, ldv, iparam,
                                 ipntr, workd, workl, lworkl, info)
            ccall(dlsym(libarpack, $seupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T},
                   Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  rvec, all, select, d, v, int32(ldv), sigma, 
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), info)
        end

        #  call dneupd  
        #     ( RVEC, HOWMNY, SELECT, DR, DI, Z, LDZ, SIGMAR, SIGMAI, WORKEV, BMAT, 
        #       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, 
        #       LWORKL, INFO )
        function jl_arpack_neupd(rvec, all, select, dr, di, v, ldv, sigmar, sigmai, workev,
                                 bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$T}, ldv, iparam,
                                 ipntr, workd, workl, lworkl, info)
            ccall(dlsym(libarpack, $real_neupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  rvec, all, select, dr, di, v, int32(ldv), sigmar, sigmai, workev,
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), info)
        end

        #  call zneupd 
        #     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, WORKEV, BMAT, 
        #       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, 
        #       WORKL, LWORKL, RWORK, INFO )
        function jl_arpack_neupd(rvec, all, select, d, v, ldv, sigma, workev, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$Tc}, ldv, iparam,
                                 ipntr, workd, workl, lworkl, rwork, info)
            ccall(dlsym(libarpack, $complex_neupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc},
                   Ptr{$Tc}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}),
                  rvec, all, select, d, v, int32(ldv), sigma, workev,
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), rwork, info)
        end

    end
end

@jl_arpack_eupd_macro Float64 Complex128 "dseupd_" "dneupd_" "zneupd_"
@jl_arpack_eupd_macro Float32 Complex64  "sseupd_" "sneupd_" "cneupd_"

eigs(A) = eigs(A, 6)
eigs(A, k) = eigs(A, k, "LM")

function eigs{T}(A::AbstractMatrix{T}, k::Int, evtype::ASCIIString)
    (m, n) = size(A)
    if m != n; error("Input should be square"); end

    isrealsymA = false
    if !iscomplex(A) && issymmetric(A); isrealsymA = true; end

    nev = k
    ncv = min(max(nev*2, 20), n)
    bmat = "I"
    which = evtype
    if iscomplex(A)
        lworkl = ncv * (3*ncv + 5)
    else
        if isrealsymA
            lworkl = ncv * (ncv + 8)
        else
            lworkl = ncv * (3*ncv + 6)
        end
    end

    v = Array(T, n, ncv)
    workd = Array(T, 3*n)
    workl = Array(T, lworkl)
    resid = Array(T, n)
    select = Array(Int32, ncv)
    iparam = zeros(Int32, 11)
    ipntr = zeros(Int32, 14)
    if iscomplex(A)
        rwork = Array(typeof(real(A[1])), ncv)
    end

    tol = zeros(typeof(real(A[1])), 1)
    ido = zeros(Int32, 1)
    info = zeros(Int32, 1)

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    while (true)

        if iscomplex(A)
            jl_arpack_naupd(ido, bmat, n, which, nev, tol, resid, 
                            ncv, v, n, 
                            iparam, ipntr, workd, workl, lworkl, rwork, info)
        else
            if isrealsymA
                jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                                ncv, v, n, 
                                iparam, ipntr, workd, workl, lworkl, info)
            else
                jl_arpack_naupd(ido, bmat, n, which, nev, tol, resid, 
                                ncv, v, n, 
                                iparam, ipntr, workd, workl, lworkl, info)
            end
        end

        if (info[1] < 0); print(info[1], ":"); error("Error in ARPACK aupd"); end
        if (info[1] == 1)
            print(info[1], ":")
            error("Maximum iterations reached in ARPACK aupd")
        end

        if (ido[1] == -1 || ido[1] == 1)
            # TODO: For the dense matrix case, just call BLAS directly here.
            workd[ipntr[2]:ipntr[2]+n-1] = A * workd[ipntr[1]:ipntr[1]+n-1]
        else
            break
        end

    end

    rvec = int32(1)
    all = "A"

    if iscomplex(A)
        d = Array(T, nev+1)
        sigma = zeros(T, 1)
        workev = Array(T, 2*ncv)

        jl_arpack_neupd(rvec, all, select, d, v, n, sigma, workev,
                        bmat, n, which, nev, tol, resid, ncv, v, n,
                        iparam, ipntr, workd, workl, lworkl, rwork, info)
    else
        if isrealsymA
            d = Array(T, nev)
            sigma = zeros(T, 1)

            jl_arpack_seupd(rvec, all, select, d, v, n, sigma,
                            bmat, n, which, nev, tol, resid, ncv, v, n,
                            iparam, ipntr, workd, workl, lworkl, info)
            
        else
            dr = Array(T, nev+1)
            di = Array(T, nev+1)
            sigmar = zeros(T, 1)
            sigmai = zeros(T, 1)
            workev = Array(T, 3*ncv)

            jl_arpack_neupd(rvec, all, select, dr, di, v, n, sigmar, sigmai, workev,
                            bmat, n, which, nev, tol, resid, ncv, v, n,
                            iparam, ipntr, workd, workl, lworkl, info)
        end

    end

    if (info[1] != 0); error("Error in ARPACK eupd"); end

    if iscomplex(A) || isrealsymA
        return (diagm(d), v[1:n, 1:nev])
    else
        evec = complex(zeros(T, n, nev+1), zeros(T, n, nev+1))
        for j=1:nev
            if di[j] == 0.0
                evec[:,j] = v[:,j]
            else
                evec[:,j]   = v[:,j] + im*v[:,j+1]
                evec[:,j+1] = v[:,j] - im*v[:,j+1]
                j += 1
            end
        end
        return (diagm(complex(dr[1:nev],di[1:nev])), evec[1:n, 1:nev])
    end

end


function svds{T}(A::AbstractMatrix{T}, k::Int)
    
    (m, n) = size(A)
    if m < n; error("Only the m >= n case is implemented"); end
    
    ldv = n
    nev = k
    ncv = min(max(nev*2, 20), n)
    bmat = "I"
    which = "LM"
    lworkl = ncv*(ncv+8)

    v = Array(T, n, ncv)
    workd = Array(T, 3*n)
    workl = Array(T, lworkl)
    d = Array(T, nev)
    resid = Array(T, n)
    select = Array(Int32, ncv)
    iparam = zeros(Int32, 11)
    ipntr = zeros(Int32, 14)

    tol = zeros(T, 1)
    sigma = zeros(T, 1)
    ido = zeros(Int32, 1)

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    At = A.'

    while (true)

        jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                        ncv, v, ldv, 
                        iparam, ipntr, workd, workl, lworkl, info)

        if (info[1] < 0); print(info[1], ":"); error("Error in ARPACK aupd"); end

        if (ido[1] == -1 || ido[1] == 1)
            workd[ipntr[2]:(ipntr[2]+n-1)] = At*(A*workd[ipntr[1]:(ipntr[1]+n-1)])
        else
            break
        end
        
    end

    rvec = int32(1)
    all = "A"

    jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, 
                    bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                    iparam, ipntr, workd, workl, lworkl, info)

    if (info[1] != 0); error("Error in ARPACK eupd"); end

    v = v[1:n, 1:nev]
    u = A*v*diagm(1./d)

    return (u, diagm(d), v.')

end
