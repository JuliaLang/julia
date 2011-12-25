_jl_libSuiteSparse = dlopen("libSuiteSparse")

const _jl_UMFPACK_A     =  int64(0)     # Ax=b    
const _jl_UMFPACK_At    =  int64(1)     # A'x=b   
const _jl_UMFPACK_Aat   =  int64(2)     # A.'x=b  
const _jl_UMFPACK_Pt_L  =  int64(3)     # P'Lx=b  
const _jl_UMFPACK_L     =  int64(4)     # Lx=b    
const _jl_UMFPACK_Lt_P  =  int64(5)     # L'Px=b  
const _jl_UMFPACK_Lat_P =  int64(6)     # L.'Px=b 
const _jl_UMFPACK_Lt    =  int64(7)     # L'x=b   
const _jl_UMFPACK_Lat   =  int64(8)     # L.'x=b  
const _jl_UMFPACK_U_Qt  =  int64(9)     # UQ'x=b  
const _jl_UMFPACK_U     =  int64(10)    # Ux=b    
const _jl_UMFPACK_Q_Ut  =  int64(11)    # QU'x=b  
const _jl_UMFPACK_Q_Uat =  int64(12)    # QU.'x=b 
const _jl_UMFPACK_Ut    =  int64(13)    # U'x=b   
const _jl_UMFPACK_Uat   =  int64(14)    # U.'x=b  

function _jl_umfpack_dl_symbolic(S::SparseMatrixCSC)
    Symbolic = pointer([0])
    ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_symbolic),
          Int,
          (Int, Int, Ptr{Int}, Ptr{Int}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
          S.m, S.n, S.colptr, S.rowval, S.nzval, Symbolic, C_NULL, C_NULL)
    return Symbolic
end

function _jl_umfpack_dl_numeric(S::SparseMatrixCSC, Symbolic::Ptr)
    Numeric = pointer([0])
    ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_numeric),
          Int,
          (Ptr{Int}, Ptr{Int}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
          S.colptr, S.rowval, S.nzval, Symbolic, Numeric, C_NULL, C_NULL)
    return Numeric
end

function _jl_umfpack_dl_solve(S::SparseMatrixCSC, b::Vector, Numeric::Ptr)
    x = similar(b)
    ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_solve),
          Int,
          (Int, Ptr{Int}, Ptr{Int}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
          _jl_UMFPACK_A, S.colptr, S.rowval, S.nzval, x, b, Numeric, C_NULL, C_NULL)
    return b
end

_jl_umfpack_dl_free_symbolic(Symbolic::Ptr) =
    ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_free_symbolic), Void, (Ptr{Void},), Symbolic)

_jl_umfpack_dl_free_numeric(Numeric::Ptr) = 
    ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_free_numeric), Void, (Ptr{Void},), Numeric)

function (\)(S::SparseMatrixCSC, b::Vector)
    symbolic = _jl_umfpack_dl_symbolic(S)
    numeric = _jl_umfpack_dl_numeric(S, symbolic)
    _jl_umfpack_dl_free_symbolic(symbolic)
    x = _jl_umfpack_dl_solve(S, b, numeric)
    _jl_umfpack_dl_free_numeric(numeric)

    return x
end
