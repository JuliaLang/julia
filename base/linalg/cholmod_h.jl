## CHOLMOD
const CHOLMOD_TRUE  = int32(1)
const CHOLMOD_FALSE = int32(0)

## itype defines the types of integer used:
const CHOLMOD_INT  = int32(0)  # all integer arrays are int
const CHOLMOD_LONG = int32(2)  # all integer arrays are UF_long
ityp(::Type{Int32}) = CHOLMOD_INT
ityp(::Type{Int64}) = CHOLMOD_LONG

## dtype defines what the numerical type is (double or float):
const CHOLMOD_DOUBLE = int32(0)        # all numerical values are double
const CHOLMOD_SINGLE = int32(1)        # all numerical values are float
dtyp(::Type{Float32}) = CHOLMOD_SINGLE
dtyp(::Type{Float64}) = CHOLMOD_DOUBLE
dtyp(::Type{Complex64}) = CHOLMOD_SINGLE
dtyp(::Type{Complex128}) = CHOLMOD_DOUBLE

## xtype defines the kind of numerical values used:
const CHOLMOD_PATTERN = int32(0)       # pattern only, no numerical values
const CHOLMOD_REAL    = int32(1)       # a real matrix
const CHOLMOD_COMPLEX = int32(2)       # a complex matrix (ANSI C99 compatible)
const CHOLMOD_ZOMPLEX = int32(3)       # a complex matrix (MATLAB compatible)
xtyp(::Type{Float32}) = CHOLMOD_REAL
xtyp(::Type{Float64}) = CHOLMOD_REAL
xtyp(::Type{Complex64}) = CHOLMOD_COMPLEX
xtyp(::Type{Complex128}) = CHOLMOD_COMPLEX

## Scaling modes, selected by the scale input parameter:
const CHOLMOD_SCALAR = int32(0)        # A = s*A
const CHOLMOD_ROW    = int32(1)        # A = diag(s)*A
const CHOLMOD_COL    = int32(2)        # A = A*diag(s)
const CHOLMOD_SYM    = int32(3)        # A = diag(s)*A*diag(s)

## Types of systems to solve
const CHOLMOD_A    = int32(0)          # solve Ax=b
const CHOLMOD_LDLt = int32(1)          # solve LDL'x=b
const CHOLMOD_LD   = int32(2)          # solve LDx=b
const CHOLMOD_DLt  = int32(3)          # solve DL'x=b
const CHOLMOD_L    = int32(4)          # solve Lx=b
const CHOLMOD_Lt   = int32(5)          # solve L'x=b
const CHOLMOD_D    = int32(6)          # solve Dx=b
const CHOLMOD_P    = int32(7)          # permute x=Px
const CHOLMOD_Pt   = int32(8)          # permute x=P'x
