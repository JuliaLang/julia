/*
  SuiteSparse_wrapper.c: Changes made to this file in the Julia repo
  in deps/SuiteSparse_wrapper.c should be also made in
  Yggdrasil/S/SuiteSparse and vice versa.
*/

#include <string.h>
#include <cholmod.h>

extern size_t jl_cholmod_sizeof_long(void) {
    return sizeof(SuiteSparse_long);
}

extern int jl_cholmod_version(int *ver) {
    if (ver != (int*) NULL) {
        ver[0] = CHOLMOD_MAIN_VERSION;
        ver[1] = CHOLMOD_SUB_VERSION;
        ver[2] = CHOLMOD_SUBSUB_VERSION;
    }
    return CHOLMOD_VERSION;
}

// Keep this synchronized with https://github.com/JuliaLang/julia/blob/master/stdlib/SuiteSparse/src/cholmod.jl
extern void jl_cholmod_method_offsets(size_t *v) {
    size_t offset = offsetof(cholmod_common, method[0]);
    v[ 0] = offsetof(cholmod_common, method[0].lnz) - offset;
    v[ 1] = offsetof(cholmod_common, method[0].fl) - offset;
    v[ 2] = offsetof(cholmod_common, method[0].prune_dense) - offset;
    v[ 3] = offsetof(cholmod_common, method[0].prune_dense2) - offset;
    v[ 4] = offsetof(cholmod_common, method[0].nd_oksep) - offset;
    v[ 5] = offsetof(cholmod_common, method[0].other_1) - offset;
    v[ 6] = offsetof(cholmod_common, method[0].nd_small) - offset;
    v[ 7] = offsetof(cholmod_common, method[0].other_2) - offset;
    v[ 8] = offsetof(cholmod_common, method[0].aggressive) - offset;
    v[ 9] = offsetof(cholmod_common, method[0].order_for_lu) - offset;
    v[10] = offsetof(cholmod_common, method[0].nd_compress) - offset;
    v[11] = offsetof(cholmod_common, method[0].nd_camd) - offset;
    v[12] = offsetof(cholmod_common, method[0].nd_components) - offset;
    v[13] = offsetof(cholmod_common, method[0].ordering) - offset;
    v[14] = offsetof(cholmod_common, method[0].other_3) - offset;
}

// Keep this synchronized with https://github.com/JuliaLang/julia/blob/master/stdlib/SuiteSparse/src/cholmod.jl
extern void jl_cholmod_common_offsets(size_t *v) {
    v[  0] = offsetof(cholmod_common, dbound);
    v[  1] = offsetof(cholmod_common, grow0);
    v[  2] = offsetof(cholmod_common, grow1);
    v[  3] = offsetof(cholmod_common, grow2);
    v[  4] = offsetof(cholmod_common, maxrank);
    v[  5] = offsetof(cholmod_common, supernodal_switch);
    v[  6] = offsetof(cholmod_common, supernodal);
    v[  7] = offsetof(cholmod_common, final_asis);
    v[  8] = offsetof(cholmod_common, final_super);
    v[  9] = offsetof(cholmod_common, final_ll);
    v[ 10] = offsetof(cholmod_common, final_pack);
    v[ 11] = offsetof(cholmod_common, final_monotonic);
    v[ 12] = offsetof(cholmod_common, final_resymbol);
    v[ 13] = offsetof(cholmod_common, zrelax);
    v[ 14] = offsetof(cholmod_common, nrelax);
    v[ 15] = offsetof(cholmod_common, prefer_zomplex);
    v[ 16] = offsetof(cholmod_common, prefer_upper);
    v[ 17] = offsetof(cholmod_common, quick_return_if_not_posdef);
    v[ 18] = offsetof(cholmod_common, prefer_binary);
    v[ 19] = offsetof(cholmod_common, print);
    v[ 20] = offsetof(cholmod_common, precise);
    v[ 21] = offsetof(cholmod_common, try_catch);
    v[ 22] = offsetof(cholmod_common, error_handler);
    v[ 23] = offsetof(cholmod_common, nmethods);
    v[ 24] = offsetof(cholmod_common, current);
    v[ 25] = offsetof(cholmod_common, selected);
    v[ 26] = offsetof(cholmod_common, method);
    v[ 27] = offsetof(cholmod_common, postorder);
    v[ 28] = offsetof(cholmod_common, default_nesdis);
    v[ 29] = offsetof(cholmod_common, metis_memory);
    v[ 30] = offsetof(cholmod_common, metis_dswitch);
    v[ 31] = offsetof(cholmod_common, metis_nswitch);
    v[ 32] = offsetof(cholmod_common, nrow);
    v[ 33] = offsetof(cholmod_common, mark);
    v[ 34] = offsetof(cholmod_common, iworksize);
    v[ 35] = offsetof(cholmod_common, xworksize);
    v[ 36] = offsetof(cholmod_common, Flag);
    v[ 37] = offsetof(cholmod_common, Head);
    v[ 38] = offsetof(cholmod_common, Xwork);
    v[ 39] = offsetof(cholmod_common, Iwork);
    v[ 40] = offsetof(cholmod_common, itype);
    v[ 41] = offsetof(cholmod_common, dtype);
    v[ 42] = offsetof(cholmod_common, no_workspace_reallocate);
    v[ 43] = offsetof(cholmod_common, status);
    v[ 44] = offsetof(cholmod_common, fl);
    v[ 45] = offsetof(cholmod_common, lnz);
    v[ 46] = offsetof(cholmod_common, anz);
    v[ 47] = offsetof(cholmod_common, modfl);
    v[ 48] = offsetof(cholmod_common, malloc_count);
    v[ 49] = offsetof(cholmod_common, memory_usage);
    v[ 50] = offsetof(cholmod_common, memory_inuse);
    v[ 51] = offsetof(cholmod_common, nrealloc_col);
    v[ 52] = offsetof(cholmod_common, nrealloc_factor);
    v[ 53] = offsetof(cholmod_common, ndbounds_hit);
    v[ 54] = offsetof(cholmod_common, rowfacfl);
    v[ 55] = offsetof(cholmod_common, aatfl);
    v[ 56] = offsetof(cholmod_common, called_nd);
    v[ 57] = offsetof(cholmod_common, blas_ok);
    v[ 58] = offsetof(cholmod_common, SPQR_grain);
    v[ 59] = offsetof(cholmod_common, SPQR_small);
    v[ 60] = offsetof(cholmod_common, SPQR_shrink);
    v[ 61] = offsetof(cholmod_common, SPQR_nthreads);
    v[ 62] = offsetof(cholmod_common, SPQR_flopcount);
    v[ 63] = offsetof(cholmod_common, SPQR_analyze_time);
    v[ 64] = offsetof(cholmod_common, SPQR_factorize_time);
    v[ 65] = offsetof(cholmod_common, SPQR_solve_time);
    v[ 66] = offsetof(cholmod_common, SPQR_flopcount_bound);
    v[ 67] = offsetof(cholmod_common, SPQR_tol_used);
    v[ 68] = offsetof(cholmod_common, SPQR_norm_E_fro);
    v[ 69] = offsetof(cholmod_common, SPQR_istat);
    v[ 70] = offsetof(cholmod_common, useGPU);
    v[ 71] = offsetof(cholmod_common, maxGpuMemBytes);
    v[ 72] = offsetof(cholmod_common, maxGpuMemFraction);
    v[ 73] = offsetof(cholmod_common, gpuMemorySize);
    v[ 74] = offsetof(cholmod_common, gpuKernelTime);
    v[ 75] = offsetof(cholmod_common, gpuFlops);
    v[ 76] = offsetof(cholmod_common, gpuNumKernelLaunches);
    v[ 77] = offsetof(cholmod_common, cublasHandle);
    v[ 78] = offsetof(cholmod_common, gpuStream);
    v[ 79] = offsetof(cholmod_common, cublasEventPotrf);
    v[ 80] = offsetof(cholmod_common, updateCKernelsComplete);
    v[ 81] = offsetof(cholmod_common, updateCBuffersFree);
    v[ 82] = offsetof(cholmod_common, dev_mempool);
    v[ 83] = offsetof(cholmod_common, dev_mempool_size);
    v[ 84] = offsetof(cholmod_common, host_pinned_mempool);
    v[ 85] = offsetof(cholmod_common, host_pinned_mempool_size);
    v[ 86] = offsetof(cholmod_common, devBuffSize);
    v[ 87] = offsetof(cholmod_common, ibuffer);
    v[ 88] = offsetof(cholmod_common, syrkStart);
    v[ 89] = offsetof(cholmod_common, cholmod_cpu_gemm_time);
    v[ 90] = offsetof(cholmod_common, cholmod_cpu_syrk_time);
    v[ 91] = offsetof(cholmod_common, cholmod_cpu_trsm_time);
    v[ 92] = offsetof(cholmod_common, cholmod_cpu_potrf_time);
    v[ 93] = offsetof(cholmod_common, cholmod_gpu_gemm_time);
    v[ 94] = offsetof(cholmod_common, cholmod_gpu_syrk_time);
    v[ 95] = offsetof(cholmod_common, cholmod_gpu_trsm_time);
    v[ 96] = offsetof(cholmod_common, cholmod_gpu_potrf_time);
    v[ 97] = offsetof(cholmod_common, cholmod_assemble_time);
    v[ 98] = offsetof(cholmod_common, cholmod_assemble_time2);
    v[ 99] = offsetof(cholmod_common, cholmod_cpu_gemm_calls);
    v[100] = offsetof(cholmod_common, cholmod_cpu_syrk_calls);
    v[101] = offsetof(cholmod_common, cholmod_cpu_trsm_calls);
    v[102] = offsetof(cholmod_common, cholmod_cpu_potrf_calls);
    v[103] = offsetof(cholmod_common, cholmod_gpu_gemm_calls);
    v[104] = offsetof(cholmod_common, cholmod_gpu_syrk_calls);
    v[105] = offsetof(cholmod_common, cholmod_gpu_trsm_calls);
    v[106] = offsetof(cholmod_common, cholmod_gpu_potrf_calls);
}

extern size_t jl_cholmod_common_size() {
    return sizeof(cholmod_common);
}
