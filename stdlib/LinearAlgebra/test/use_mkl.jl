using Pkg
using LinearAlgebra

if !("MKL_jll" ∈ keys(Pkg.installed()))
    Pkg.add("MKL_jll")
end

using MKL_jll

if MKL_jll.is_available()
   ccall((:MKL_Set_Interface_Layer, MKL_jll.libmkl_rt_path), Cvoid, (Cint,), Base.USE_BLAS64 ? 1 : 0)
   LinearAlgebra.set_blas_lapack_trampoline!(:mkl, MKL_jll.libmkl_rt_path, MKL_jll.libmkl_rt_path)
end
