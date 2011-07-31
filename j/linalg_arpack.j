libarpack = dlopen("libarpack")

macro jl_arpack_aupd_macro(T, Tc, saupd, real_naupd, complex_naupd)
    quote

        # call dsaupd
        #  ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #    IPNTR, WORKD, WORKL, LWORKL, INFO )
        function jl_arpack_saupd(ido, bmat, n, which, nev, 
                                 tol, resid, ncv, v::Array{$T}, ldv, 
                                 iparam, ipntr, workd, workl, lworkl)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $saupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{Int32}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), info)
            return info[1]
        end

        #  call dnaupd
        #     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #       IPNTR, WORKD, WORKL, LWORKL, INFO )
        function jl_arpack_naupd(ido, bmat, n, which, nev, 
                                 tol, resid, ncv, v::Array{$T}, ldv, 
                                 iparam, ipntr, workd, workl, lworkl)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $real_naupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{Int32}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), info)
            return info[1]
        end

        #  call znaupd
        #     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
        #       IPNTR, WORKD, WORKL, LWORKL, RWORK, INFO )
        function jl_arpack_naupd(ido, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$Tc}, ldv,
                                 iparam, ipntr, workd, workl, lworkl, rwork)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $complex_naupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc}, Ptr{Int32},
                   Ptr{Int32}, Ptr{Int32}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}),
                  ido, bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv), 
                  iparam, ipntr, workd, workl, int32(lworkl), rwork, info)
            return info[1]
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
                                 ipntr, workd, workl, lworkl)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $seupd),
                  Void,
                  (Ptr{Bool}, Ptr{Uint8}, Ptr{Bool}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T},
                   Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  rvec, all, select, d, v, int32(ldv), sigma, 
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), info)
            return info[1]
        end

        #  call dneupd  
        #     ( RVEC, HOWMNY, SELECT, DR, DI, Z, LDZ, SIGMAR, SIGMAI, WORKEV, BMAT, 
        #       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, 
        #       LWORKL, INFO )
        function jl_arpack_neupd(rvec, all, select, dr, di, v, ldv, sigmar, sigmai, 
                                 bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$T}, ldv, iparam,
                                 ipntr, workd, workl, lworkl)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $real_neupd),
                  Void,
                  (Ptr{Bool}, Ptr{Uint8}, Ptr{Bool}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  rvec, all, select, dr, di, v, int32(ldv), sigmar, sigmai,
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), info)
            return info[1]
        end

        #  call zneupd 
        #     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, WORKEV, BMAT, 
        #       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, 
        #       WORKL, LWORKL, RWORK, INFO )
        function jl_arpack_neupd(rvec, all, select, d, v, ldv, sigma, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$Tc}, ldv, iparam,
                                 ipntr, workd, workl, lworkl, rwork)
            info = zeros(Int32, 1)
            ccall(dlsym(libarpack, $complex_neupd),
                  Void,
                  (Ptr{Bool}, Ptr{Uint8}, Ptr{Bool}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc},
                   Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$Tc}, Ptr{Int32}, Ptr{$Tc}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}),
                  rvec, all, select, d, v, int32(ldv), sigma,
                  bmat, int32(n), which, int32(nev), tol, resid, int32(ncv), v, int32(ldv),
                  iparam, ipntr, workd, workl, int32(lworkl), rwork, info)
            return info[1]
        end

    end
end

@jl_arpack_eupd_macro Float64 Complex128 "dseupd_" "dneupd_" "zneupd_"
@jl_arpack_eupd_macro Float32 Complex64  "sseupd_" "sneupd_" "cneupd_"

function eigs{T}(A::AbstractMatrix{T}, k::Int)
    (m, n) = size(A)

    if m != n; error("Input should be square"); end
    if !issymmetric(A); error("Input should be symmetric"); end

    n = n
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
    select = Array(Bool, ncv)
    iparam = Array(Int32, 11)
    ipntr = Array(Int32, 11)

    tol = zeros(T, 1)
    sigma = zeros(T, 1)
    ido = zeros(Int32, 1)

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    while (true)

        info = jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                               ncv, v, ldv, 
                               iparam, ipntr, workd, workl, lworkl)

        if (info < 0); print(info); error("Error in ARPACK aupd"); end

        if (ido[1] == -1 || ido[1] == 1)
            workd[ipntr[2]:ipntr[2]+n-1] = A * workd[ipntr[1]:ipntr[1]+n-1]
        else
            break
        end

    end

    rvec = true
    all = "A"

    info = jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, 
                           bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                           iparam, ipntr, workd, workl, lworkl)

    if (info != 0); error("Error in ARPACK eupd"); end

    return (diagm(d), v[1:n, 1:nev])

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
    select = Array(Bool, ncv)
    iparam = Array(Int32, 11)
    ipntr = Array(Int32, 11)

    tol = zeros(T, 1)
    sigma = zeros(T, 1)
    ido = zeros(Int32, 1)

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    At = A.'

    while (true)

        info = jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                               ncv, v, ldv, 
                               iparam, ipntr, workd, workl, lworkl)

        if (info < 0); print(info); error("Error in ARPACK aupd"); end

        if (ido[1] == -1 || ido[1] == 1)
            workd[ipntr[2]:(ipntr[2]+n-1)] = At*(A*workd[ipntr[1]:(ipntr[1]+n-1)])
        else
            break
        end
        
    end

    rvec = true
    all = "A"

    info = jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, 
                           bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                           iparam, ipntr, workd, workl, lworkl)

    if (info != 0); error("Error in ARPACK eupd"); end

    v = v[1:n, 1:nev]
    u = A*v*diagm(1./d)

    return (u, diagm(d), v.')

end
