_jl_libSuiteSparse = dlopen("libSuiteSparse")

## Type of solve
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

## Sizes of Control and Info arrays for returning information from solver
const _jl_UMFPACK_INFO = 90
const _jl_UMFPACK_CONTROL = 20

## Status codes
const _jl_UMFPACK_OK = 0
const _jl_UMFPACK_WARNING_singular_matrix = 1
const _jl_UMFPACK_WARNING_determinant_underflow = 2
const _jl_UMFPACK_WARNING_determinant_overflow = 3
const _jl_UMFPACK_ERROR_out_of_memory = -1
const _jl_UMFPACK_ERROR_invalid_Numeric_object = -3
const _jl_UMFPACK_ERROR_invalid_Symbolic_object = -4
const _jl_UMFPACK_ERROR_argument_missing = -5
const _jl_UMFPACK_ERROR_n_nonpositive = -6
const _jl_UMFPACK_ERROR_invalid_matrix = -8
const _jl_UMFPACK_ERROR_different_pattern = -11
const _jl_UMFPACK_ERROR_invalid_system = -13
const _jl_UMFPACK_ERROR_invalid_permutation = -15
const _jl_UMFPACK_ERROR_internal_error = -911
const _jl_UMFPACK_ERROR_file_IO = -17
const _jl_UMFPACK_ERROR_ordering_failed = -18

## UMFPACK works with 0 based indexing
function _jl_convert_to_0_based_indexing(S::SparseMatrixCSC)
    for i=1:(S.colptr[end]-1); S.rowval[i] -= 1; end
    for i=1:length(S.colptr); S.colptr[i] -= 1; end
    return S
end

function _jl_convert_to_1_based_indexing(S::SparseMatrixCSC)
    for i=1:length(S.colptr); S.colptr[i] += 1; end
    for i=1:(S.colptr[end]-1); S.rowval[i] += 1; end
    return S
end

## Wrappers around UMFPACK routines
function _jl_umfpack_symbolic{Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti})
    # Pointer to store the symbolic factorization returned by UMFPACK
    Symbolic = Array(Ptr{Void}, 1)
    status = ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_symbolic),
                   Ti,
                   (Ti, Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                   S.m, S.n, S.colptr, S.rowval, S.nzval, Symbolic, C_NULL, C_NULL)
    return Symbolic
end

function _jl_umfpack_numeric{Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
    # Pointer to store the numeric factorization returned by UMFPACK
    Numeric = Array(Ptr{Void}, 1)
    status = ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_numeric),
                   Ti,
                   (Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Void}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                   S.colptr, S.rowval, S.nzval, Symbolic[1], Numeric, C_NULL, C_NULL)
    return Numeric
end

function _jl_umfpack_solve{Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti}, b::Vector{Tv}, Numeric)
    x = similar(b)
    status = ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_solve),
                   Ti,
                   (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Tv}, Ptr{Tv}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                   _jl_UMFPACK_A, S.colptr, S.rowval, S.nzval, x, b, Numeric[1], C_NULL, C_NULL)
    return x
end

function _jl_umfpack_free_symbolic{Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
    status = ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_free_symbolic), Void, (Ptr{Void},), Symbolic)
end

function _jl_umfpack_free_numeric{Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti}, Numeric)
    status = ccall(dlsym(_jl_libSuiteSparse, :umfpack_dl_free_numeric), Void, (Ptr{Void},), Numeric)
end

## User-callable functions
function (\){Tv<:Float64,Ti<:Int64}(S::SparseMatrixCSC{Tv,Ti}, b::Vector{Tv})
    S = _jl_convert_to_0_based_indexing(S)

    symbolic = _jl_umfpack_symbolic(S)
    numeric = _jl_umfpack_numeric(S, symbolic)
    # _jl_umfpack_free_symbolic(S, symbolic)
    x = _jl_umfpack_solve(S, b, numeric)
    # _jl_umfpack_free_numeric(S, numeric)

    S = _jl_convert_to_1_based_indexing(S)
    return x
end
