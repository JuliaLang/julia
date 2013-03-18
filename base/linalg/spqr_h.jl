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

