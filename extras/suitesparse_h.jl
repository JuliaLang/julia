## CHOLMOD

const CHOLMOD_TRUE  = int32(1)
const CHOLMOD_FALSE = int32(0)

# Types of systems to solve
const CHOLMOD_A    = int32(0)         # solve Ax=b 
const CHOLMOD_LDLt = int32(1)         # solve LDL'x=b 
const CHOLMOD_LD   = int32(2)         # solve LDx=b 
const CHOLMOD_DLt  = int32(3)         # solve DL'x=b 
const CHOLMOD_L    = int32(4)         # solve Lx=b 
const CHOLMOD_Lt   = int32(5)         # solve L'x=b 
const CHOLMOD_D    = int32(6)         # solve Dx=b 
const CHOLMOD_P    = int32(7)         # permute x=Px 
const CHOLMOD_Pt   = int32(8)         # permute x=P'x 

# itype defines the types of integer used:
const CHOLMOD_INT  = int32(0)  # all integer arrays are int 
const CHOLMOD_LONG = int32(2)  # all integer arrays are UF_long 

# dtype defines what the numerical type is (double or float):
const CHOLMOD_DOUBLE = int32(0)       # all numerical values are double 
const CHOLMOD_SINGLE = int32(1)       # all numerical values are float 

# xtype defines the kind of numerical values used:
const CHOLMOD_PATTERN = int32(0)      # pattern only, no numerical values 
const CHOLMOD_REAL    = int32(1)      # a real matrix 
const CHOLMOD_COMPLEX = int32(2)      # a complex matrix (ANSI C99 compatible) 
const CHOLMOD_ZOMPLEX = int32(3)      # a complex matrix (MATLAB compatible) 

# Definitions for cholmod_common: 
const CHOLMOD_MAXMETHODS = int32(9)  # maximum number of different methods that 
                                     # cholmod_analyze can try. Must be >= 9. 

# Common->status values.  zero means success, negative means a fatal error, positive is a warning. 
const CHOLMOD_OK            = int32(0)    # success 
const CHOLMOD_NOT_INSTALLED = int32(-1)   # failure: method not installed 
const CHOLMOD_OUT_OF_MEMORY = int32(-2)   # failure: out of memory 
const CHOLMOD_TOO_LARGE     = int32(-3)   # failure: integer overflow occured 
const CHOLMOD_INVALID       = int32(-4)   # failure: invalid input 
const CHOLMOD_NOT_POSDEF    = int32(1)    # warning: matrix not pos. def. 
const CHOLMOD_DSMALL        = int32(2)    # warning: D for LDL'  or diag(L) or LL' has tiny absolute value 

# ordering method (also used for L->ordering) 
const CHOLMOD_NATURAL = int32(0)     # use natural ordering 
const CHOLMOD_GIVEN   = int32(1)     # use given permutation 
const CHOLMOD_AMD     = int32(2)     # use minimum degree (AMD) 
const CHOLMOD_METIS   = int32(3)     # use METIS' nested dissection 
const CHOLMOD_NESDIS  = int32(4)     # use CHOLMOD's version of nested dissection:
                                       # node bisector applied recursively, followed
                                       # by constrained minimum degree (CSYMAMD or CCOLAMD) 
const CHOLMOD_COLAMD  = int32(5)     # use AMD for A, COLAMD for A*A' 

# POSTORDERED is not a method, but a result of natural ordering followed by a
# weighted postorder.  It is used for L->ordering, not method [ ].ordering. 
const CHOLMOD_POSTORDERED  = int32(6)   # natural ordering, postordered. 

# supernodal strategy (for Common->supernodal) 
const CHOLMOD_SIMPLICIAL = int32(0)    # always do simplicial 
const CHOLMOD_AUTO       = int32(1)    # select simpl/super depending on matrix 
const CHOLMOD_SUPERNODAL = int32(2)    # always do supernodal 

# scaling modes, selected by the scale input parameter:
const CHOLMOD_SCALAR     = int32(0)    # A = s*A
const CHOLMOD_ROW        = int32(1)    # A = diag(s)*A
const CHOLMOD_COL        = int32(2)    # A = A*diag(s)
const CHOLMOD_SYM        = int32(3)    # A = diag(s)*A*diag(s)

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
