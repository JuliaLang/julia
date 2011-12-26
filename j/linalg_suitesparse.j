_jl_libSuiteSparse = dlopen("libSuiteSparse")

## Type of solve
const _jl_UMFPACK_A     =  0     # Ax=b
const _jl_UMFPACK_At    =  1     # A'x=b
const _jl_UMFPACK_Aat   =  2     # A.'x=b
const _jl_UMFPACK_Pt_L  =  3     # P'Lx=b
const _jl_UMFPACK_L     =  4     # Lx=b
const _jl_UMFPACK_Lt_P  =  5     # L'Px=b
const _jl_UMFPACK_Lat_P =  6     # L.'Px=b
const _jl_UMFPACK_Lt    =  7     # L'x=b
const _jl_UMFPACK_Lat   =  8     # L.'x=b
const _jl_UMFPACK_U_Qt  =  9     # UQ'x=b
const _jl_UMFPACK_U     =  10    # Ux=b
const _jl_UMFPACK_Q_Ut  =  11    # QU'x=b
const _jl_UMFPACK_Q_Uat =  12    # QU.'x=b
const _jl_UMFPACK_Ut    =  13    # U'x=b
const _jl_UMFPACK_Uat   =  14    # U.'x=b

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

macro _jl_umfpack_macro(eltype, inttype, f_sym, f_num, f_sol, f_symfree, f_numfree)
    quote
        function _jl_umfpack_symbolic{Tv<:$eltype,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti})
            # Pointer to store the symbolic factorization returned by UMFPACK
            Symbolic = Array(Ptr{Void}, 1)
            status = ccall(dlsym(_jl_libSuiteSparse, $f_sym),
                           Ti,
                           (Ti, Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           S.m, S.n, S.colptr, S.rowval, S.nzval, Symbolic, C_NULL, C_NULL)
            return Symbolic
        end

        function _jl_umfpack_numeric{Tv<:$eltype,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
            # Pointer to store the numeric factorization returned by UMFPACK
            Numeric = Array(Ptr{Void}, 1)
            status = ccall(dlsym(_jl_libSuiteSparse, $f_num),
                           Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Void}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           S.colptr, S.rowval, S.nzval, Symbolic[1], Numeric, C_NULL, C_NULL)
            return Numeric
        end

        function _jl_umfpack_solve{Tv<:$eltype,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, b::Vector{Tv}, Numeric)
            x = similar(b)
            status = ccall(dlsym(_jl_libSuiteSparse, $f_sol),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Tv}, Ptr{Tv}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           convert(Ti, _jl_UMFPACK_A), S.colptr, S.rowval, S.nzval, x, b, Numeric[1], C_NULL, C_NULL)
            return x
        end

        _jl_umfpack_free_symbolic{Tv<:$eltype,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic) =
        ccall(dlsym(_jl_libSuiteSparse, $f_symfree), Void, (Ptr{Void},), Symbolic)
        
        _jl_umfpack_free_numeric{Tv<:$eltype,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Numeric) =
        ccall(dlsym(_jl_libSuiteSparse, $f_numfree), Void, (Ptr{Void},), Numeric)
        
    end # quote
end # macro

@_jl_umfpack_macro Float64 Int64 :umfpack_dl_symbolic :umfpack_dl_numeric :umfpack_dl_solve :umfpack_dl_free_symbolic :umfpack_dl_free_numeric
@_jl_umfpack_macro Float64 Int32 :umfpack_di_symbolic :umfpack_di_numeric :umfpack_di_solve :umfpack_di_free_symbolic :umfpack_di_free_numeric

## User-callable functions
function (\){Tv<:Float64,Ti<:Union(Int64,Int32)}(S::SparseMatrixCSC{Tv,Ti}, b::Vector{Tv})
    S = _jl_convert_to_0_based_indexing(S)

    symbolic = _jl_umfpack_symbolic(S)
    numeric = _jl_umfpack_numeric(S, symbolic)
    _jl_umfpack_free_symbolic(S, symbolic)
    x = _jl_umfpack_solve(S, b, numeric)
    _jl_umfpack_free_numeric(S, numeric)

    S = _jl_convert_to_1_based_indexing(S)
    return x
end
