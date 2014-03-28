function blas_nthreads(coefs, p...)
    np = coefs[1]
    for i = 1:length(p)
        np += coefs[i+1]*log(p[i])
    end
    min(CPU_CORES, max(1, iround(np)))
end
geevx!_nthreads(p) = blas_nthreads([0.999847584209725,0.0], p...)
gemm!_nthreads(p) = blas_nthreads([2.666666666666666,0.41321635739041884,0.0,0.0854930394600868], p...)
