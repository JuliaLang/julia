## UMFPACK

## Type of solve
const UMFPACK_A     =  0     # Ax=b
const UMFPACK_At    =  1     # A'x=b
const UMFPACK_Aat   =  2     # A.'x=b
const UMFPACK_Pt_L  =  3     # P'Lx=b
const UMFPACK_L     =  4     # Lx=b
const UMFPACK_Lt_P  =  5     # L'Px=b
const UMFPACK_Lat_P =  6     # L.'Px=b
const UMFPACK_Lt    =  7     # L'x=b
const UMFPACK_Lat   =  8     # L.'x=b
const UMFPACK_U_Qt  =  9     # UQ'x=b
const UMFPACK_U     =  10    # Ux=b
const UMFPACK_Q_Ut  =  11    # QU'x=b
const UMFPACK_Q_Uat =  12    # QU.'x=b
const UMFPACK_Ut    =  13    # U'x=b
const UMFPACK_Uat   =  14    # U.'x=b

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

## SuiteSparseQR

## ordering options
const SPQR_ORDERING_FIXED   = int32(0)
const SPQR_ORDERING_NATURAL = int32(1)
const SPQR_ORDERING_COLAMD  = int32(2)
const SPQR_ORDERING_GIVEN   = int32(3) # only used for C/C++ interface
const SPQR_ORDERING_CHOLMOD = int32(4) # CHOLMOD best-effort (COLAMD, METIS,...)
const SPQR_ORDERING_AMD     = int32(5) # AMD(A'*A)
const SPQR_ORDERING_METIS   = int32(6) # metis(A'*A)
const SPQR_ORDERING_DEFAULT = int32(7) # SuiteSparseQR default ordering
const SPQR_ORDERING_BEST    = int32(8) # try COLAMD, AMD, and METIS; pick best
const SPQR_ORDERING_BESTAMD = int32(9) # try COLAMD and AMD; pick best

# Let [m n] = size of the matrix after pruning singletons.  The default
# ordering strategy is to use COLAMD if m <= 2*n.  Otherwise, AMD(A'A) is
# tried.  If there is a high fill-in with AMD then try METIS(A'A) and take
# the best of AMD and METIS.  METIS is not tried if it isn't installed.

## Operations in qmult
const SPQR_QTX = int32(0)        # Y = Q'*X
const SPQR_QX  = int32(1)        # Y = Q*X
const SPQR_XQT = int32(2)        # Y = X*Q'
const SPQR_XQ  = int32(3)        # Y = X*Q

## Types of systems to solve
const SPQR_RX_EQUALS_B    = int32(0)    # solve R*X=B      or X = R\B
const SPQR_RETX_EQUALS_B  = int32(1)    # solve R*E'*X=B   or X = E*(R\B)
const SPQR_RTX_EQUALS_B   = int32(2)    # solve R'*X=B     or X = R'\B
const SPQR_RTX_EQUALS_ETB = int32(3)    # solve R'*X=E'*B  or X = R'\(E'*B)

