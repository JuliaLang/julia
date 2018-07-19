# This file is a part of Julia. License is MIT: https://julialang.org/license

## UMFPACK

## Type of solve
const UMFPACK_A     =  0     # Ax=b
const UMFPACK_At    =  1     # adjoint(A)x=b
const UMFPACK_Aat   =  2     # transpose(A)x=b
const UMFPACK_Pt_L  =  3     # adjoint(P)Lx=b
const UMFPACK_L     =  4     # Lx=b
const UMFPACK_Lt_P  =  5     # adjoint(L)Px=b
const UMFPACK_Lat_P =  6     # transpose(L)Px=b
const UMFPACK_Lt    =  7     # adjoint(L)x=b
const UMFPACK_Lat   =  8     # transpose(L)x=b
const UMFPACK_U_Qt  =  9     # U*adjoint(Q)x=b
const UMFPACK_U     =  10    # Ux=b
const UMFPACK_Q_Ut  =  11    # Q*adjoint(U)x=b
const UMFPACK_Q_Uat =  12    # Q*transpose(U)x=b
const UMFPACK_Ut    =  13    # adjoint(U)x=b
const UMFPACK_Uat   =  14    # transpose(U)x=b

## Sizes of Control and Info arrays for returning information from solver
const UMFPACK_INFO = 90
const UMFPACK_CONTROL = 20
const UMFPACK_PRL = 1

## Status codes
const UMFPACK_OK = 0
const UMFPACK_WARNING_singular_matrix       = 1
const UMFPACK_WARNING_determinant_underflow = 2
const UMFPACK_WARNING_determinant_overflow  = 3
const UMFPACK_ERROR_out_of_memory           = -1
const UMFPACK_ERROR_invalid_Numeric_object  = -3
const UMFPACK_ERROR_invalid_Symbolic_object = -4
const UMFPACK_ERROR_argument_missing        = -5
const UMFPACK_ERROR_n_nonpositive           = -6
const UMFPACK_ERROR_invalid_matrix          = -8
const UMFPACK_ERROR_different_pattern       = -11
const UMFPACK_ERROR_invalid_system          = -13
const UMFPACK_ERROR_invalid_permutation     = -15
const UMFPACK_ERROR_internal_error          = -911
const UMFPACK_ERROR_file_IO                 = -17
const UMFPACK_ERROR_ordering_failed         = -18
