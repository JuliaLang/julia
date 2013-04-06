module ARPACK 

export naupd, neupd, saupd, seupd

import ..LinAlg: BlasInt, blas_int

for (T, saupd_name, seupd_name, naupd_name, neupd_name) in
    ((:Float64, :dsaupd_, :dseupd_, :dnaupd_, :dneupd_),
     (:Float32, :ssaupd_, :sseupd_, :snaupd_, :sneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  ido, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

        function neupd(rvec, howmny, select, dr, di, z, ldz, sigmar, sigmai,
                  workev, bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
                  iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T},
                   Ptr{BlasInt}, Ptr{BlasInt}),
                  &rvec, howmny, select, dr, di, z, &ldz, sigmar, sigmai,
                  workev, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

        function saupd(ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, 
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info)
            
            ccall(($(string(saupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  ido, bmat, &n, which, &nev, tol, resid, &ncv, v, &ldv, 
                  iparam, ipntr, workd, workl, &lworkl, info)

        end

        function seupd(rvec, howmny, select, d, z, ldz, sigma,
                       bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, info) 

            ccall(($(string(seupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T},
                   Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, sigma,
                  bmat, &n, evtype, &nev, tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, info)
        end

    end
end

for (T, TR, naupd_name, neupd_name) in
    ((:Complex128, :Float64, :znaupd_, :zneupd_),
     (:Complex64, :Float32, :cnaupd_, :cneupd_))
    @eval begin

        function naupd(ido, bmat, n, evtype, nev, tol, resid, ncv, v, ldv, 
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info)
            
            ccall(($(string(naupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{BlasInt}),
                  ido, bmat, &n, evtype, &nev, tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info)

        end

        function neupd(rvec, howmny, select, d, z, ldz, workev, sigma,
                       bmat, n, evtype, nev, tol, resid, ncv, v, ldv,
                       iparam, ipntr, workd::Array{$T}, workl::Array{$T}, lworkl, 
                       rwork::Array{$TR}, info) 

            ccall(($(string(neupd_name)), :libarpack), Void,
                  (Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt},
                   Ptr{$T}, Ptr{$T}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{Uint8}, Ptr{BlasInt},
                   Ptr{$TR}, Ptr{$T}, Ptr{BlasInt}, Ptr{$T}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$T}, Ptr{$T}, Ptr{BlasInt}, Ptr{$TR}, Ptr{BlasInt}),
                  &rvec, howmny, select, d, z, &ldz, workev, sigma,
                  bmat, &n, evtype, &nev, tol, resid, &ncv, v, &ldv,
                  iparam, ipntr, workd, workl, &lworkl, rwork, info) 

        end

    end
end

end # module ARPACK
