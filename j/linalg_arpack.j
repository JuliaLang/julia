libarpack = dlopen("libarpack")

# call dsaupd
#  ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
#    IPNTR, WORKD, WORKL, LWORKL, INFO )

macro jl_arpack_saupd_macro(saupd, T)
    quote
        function jl_arpack_saupd(ido, bmat, n, which, nev, 
                                 tol, resid, ncv, v::Array{$T}, ldv, 
                                 iparam, ipntr, workd, workl, lworkl, info)
            ccall(dlsym(libarpack, $saupd),
                  Void,
                  (Ptr{Int32}, Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, 
                   Ptr{Int32}, Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}),
                  ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                  iparam, ipntr, workd, workl, lworkl, info)
        end
    end
end

@jl_arpack_saupd_macro "ssaupd_" Float32
@jl_arpack_saupd_macro "dsaupd_" Float64

# call dseupd ( rvec, 'A', select, d, v, ldv, sigma,
#         bmat, n, which, nev, tol, resid, ncv, v, ldv,
#         iparam, ipntr, workd, workl, lworkl, ierr )

macro jl_arpack_seupd_macro(seupd, T)
    quote
        function jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, bmat, n, which, nev,
                                 tol, resid, ncv, v::Array{$T}, ldv, iparam,
                                 ipntr, workd, workl, lworkl, ierr)
            ccall(dlsym(libarpack, $seupd),
                  Void,
                  (Ptr{Bool}, Ptr{Uint8}, Ptr{Bool}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T},
                   Ptr{Uint8}, Ptr{Int32}, Ptr{Uint8}, Ptr{Int32},
                   Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{$T}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$T}, Ptr{$T}, Ptr{Int32}, Ptr{Int32}, ),
                  rvec, all, select, d, v, ldv, sigma, 
                  bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                  iparam, ipntr, workd, workl, lworkl, ierr)
        end
    end
end

@jl_arpack_seupd_macro "sseupd_" Float32
@jl_arpack_seupd_macro "dseupd_" Float64

#  call dnaupd
#     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
#       IPNTR, WORKD, WORKL, LWORKL, INFO )

#  call dneupd  
#     ( RVEC, HOWMNY, SELECT, DR, DI, Z, LDZ, SIGMAR, SIGMAI, WORKEV, BMAT, 
#       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, 
#       LWORKL, INFO )

jlArray(T, n::Int) = Array(T, int64(n))
jlArray(T, m::Int, n::Int) = Array(T, int64(m), int64(n))

function eigs{T}(A::AbstractMatrix{T}, k::Int)
    (m, n) = size(A)

    if m != n; error("Input should be square"); end
    if !issymmetric(A); error("Input should be symmetric"); end

    n = int32(n)
    ldv = int32(n)
    nev = int32(k)
    ncv = int32(min(max(nev*2, 20), n))
    bmat = "I"
    which = "LM"
    zero_T = convert(T, 0.0)
    lworkl = int32(ncv*(ncv+8))

    v = jlArray(T, n, ncv)
    workd = jlArray(T, 3*n)
    workl = jlArray(T, lworkl)
    d = jlArray(T, nev)
    resid = jlArray(T, n)
    select = jlArray(Bool, ncv)
    iparam = jlArray(Int32, 11)
    ipntr = jlArray(Int32, 11)

    tol = [zero_T]
    sigma = [zero_T]
    info = [int32(0)]
    ido = [int32(0)]

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    while (true)

        jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                        ncv, v, ldv, 
                        iparam, ipntr, workd, workl, lworkl, info)

        if (info[1] < 0); print(info[1]); error("Error in ARPACK aupd"); end

        if (ido[1] == -1 || ido[1] == 1)
            workd[ipntr[2]:ipntr[2]+n-1] = A * workd[ipntr[1]:ipntr[1]+n-1]
        else
            break
        end

    end

    rvec = true
    all = "A"
    ierr = [int32(0)]

    jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, 
                    bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                    iparam, ipntr, workd, workl, lworkl, ierr)

    if (ierr[1] != 0); error("Error in ARPACK eupd"); end

    return (diagm(d), v[1:n, 1:nev])

end

function svds{T}(A::AbstractMatrix{T}, k::Int)
    
    (m, n) = size(A)
    if m < n; error("Only the m>n case is implemented"); end
    
    n = int32(n)
    ldv = int32(n)
    nev = int32(k)
    ncv = int32(min(max(nev*2, 20), n))
    bmat = "I"
    which = "LM"
    zero_T = convert(T, 0.0)
    lworkl = int32(ncv*(ncv+8))

    v = jlArray(T, n, ncv)
    workd = jlArray(T, 3*n)
    workl = jlArray(T, lworkl)
    d = jlArray(T, nev)
    resid = jlArray(T, n)
    select = jlArray(Bool, ncv)
    iparam = jlArray(Int32, 11)
    ipntr = jlArray(Int32, 11)

    tol = [zero_T]
    sigma = [zero_T]
    info = [int32(0)]
    ido = [int32(0)]

    iparam[1] = int32(1)    # ishifts
    iparam[3] = int32(1000) # maxitr
    iparam[7] = int32(1)    # mode 1

    At = A.'

    while (true)

        jl_arpack_saupd(ido, bmat, n, which, nev, tol, resid, 
                        ncv, v, ldv, 
                        iparam, ipntr, workd, workl, lworkl, info)

        if (info[1] < 0); print(info[1]); error("Error in ARPACK aupd"); end

        if (ido[1] == -1 || ido[1] == 1)
            workd[ipntr[2]:(ipntr[2]+n-1)] = At*(A*workd[ipntr[1]:(ipntr[1]+n-1)])
        else
            break
        end
        
    end

    rvec = true
    all = "A"
    ierr = [int32(0)]

    jl_arpack_seupd(rvec, all, select, d, v, ldv, sigma, 
                    bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                    iparam, ipntr, workd, workl, lworkl, ierr)

    if (ierr[1] != 0); error("Error in ARPACK eupd"); end

    v = v[1:n, 1:nev]
    u = A*v*diagm(1./d)

    return (u, diagm(d), v.')

end
