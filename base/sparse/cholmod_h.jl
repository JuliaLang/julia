# This file is a part of Julia. License is MIT: http://julialang.org/license

## CHOLMOD
const TRUE  = Int32(1)
const FALSE = Int32(0)

## itype defines the types of integer used:
const INT     = Int32(0)  # all integer arrays are int
const INTLONG = Int32(1)  # most are int, some are SuiteSparse_long
const LONG    = Int32(2)  # all integer arrays are SuiteSparse_long
ityp(::Type{Int32}) = INT
ityp(::Type{Int64}) = LONG

## dtype defines what the numerical type is (double or float):
const DOUBLE = Int32(0)        # all numerical values are double
const SINGLE = Int32(1)        # all numerical values are float
dtyp(::Type{Float32}) = SINGLE
dtyp(::Type{Float64}) = DOUBLE
dtyp(::Type{Complex64}) = SINGLE
dtyp(::Type{Complex128}) = DOUBLE

## xtype defines the kind of numerical values used:
const PATTERN = Int32(0)       # pattern only, no numerical values
const REAL    = Int32(1)       # a real matrix
const COMPLEX = Int32(2)       # a complex matrix (ANSI C99 compatible)
const ZOMPLEX = Int32(3)       # a complex matrix (MATLAB compatible)
xtyp(::Type{Float32})    = REAL
xtyp(::Type{Float64})    = REAL
xtyp(::Type{Complex64})  = COMPLEX
xtyp(::Type{Complex128}) = COMPLEX

## Scaling modes, selected by the scale input parameter:
const SCALAR = Int32(0)        # A = s*A
const ROW    = Int32(1)        # A = diag(s)*A
const COL    = Int32(2)        # A = A*diag(s)
const SYM    = Int32(3)        # A = diag(s)*A*diag(s)

## Types of systems to solve
const CHOLMOD_A    = Int32(0)          # solve Ax=b
const CHOLMOD_LDLt = Int32(1)          # solve LDL'x=b
const CHOLMOD_LD   = Int32(2)          # solve LDx=b
const CHOLMOD_DLt  = Int32(3)          # solve DL'x=b
const CHOLMOD_L    = Int32(4)          # solve Lx=b
const CHOLMOD_Lt   = Int32(5)          # solve L'x=b
const CHOLMOD_D    = Int32(6)          # solve Dx=b
const CHOLMOD_P    = Int32(7)          # permute x=Px
const CHOLMOD_Pt   = Int32(8)          # permute x=P'x

# Symmetry types
const EMPTY                 =-1
const MM_RECTANGULAR        = 1
const MM_UNSYMMETRIC        = 2
const MM_SYMMETRIC          = 3
const MM_HERMITIAN          = 4
const MM_SKEW_SYMMETRIC     = 5
const MM_SYMMETRIC_POSDIAG  = 6
const MM_HERMITIAN_POSDIAG  = 7

# check the size of SuiteSparse_long
if Int(ccall((:jl_cholmod_sizeof_long, :libsuitesparse_wrapper),Csize_t,())) == 4
    const SuiteSparse_long = Int32
    const IndexTypes = (:Int32, )
    typealias ITypes Union{Int32}
else
    const SuiteSparse_long = Int64
    const IndexTypes = (:Int32, :Int64)
    typealias ITypes Union{Int32, Int64}
end

typealias VTypes Union{Complex128, Float64}
typealias VRealTypes Union{Float64}

type CHOLMODException <: Exception
    msg::AbstractString
end

macro isok(A)
    :($(esc(A)) == TRUE || throw(CHOLMODException("")))
end
