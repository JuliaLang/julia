module CHOLMOD

import Base: (*), convert, copy, ctranspose, eltype, getindex, hcat, isvalid, show, size,
       sort!, transpose, vcat

import Base.LinAlg: (\), A_mul_Bc, A_mul_Bt, Ac_ldiv_B, Ac_mul_B, At_ldiv_B, At_mul_B,
                 A_ldiv_B!, cholfact, cholfact!, copy, det, diag,
                 full, ishermitian, isposdef!, issym, ldltfact, logdet, norm, scale, scale!

import Base.SparseMatrix: findnz, sparse

export
    Dense,
    Factor,
    Sparse,
    etree,
    sparse!

using Base.SparseMatrix: AbstractSparseMatrix, SparseMatrixCSC, increment, increment!, indtype, decrement, decrement!

include("cholmod_h.jl")

macro isok(A)
    :($A == TRUE || throw(CHOLMODException("")))
end

const version_array = Array(Cint, 3)
if dlsym(dlopen("libcholmod"), :cholmod_version) != C_NULL
    ccall((:cholmod_version, :libcholmod), Cint, (Ptr{Cint},), version_array)
else
    ccall((:jl_cholmod_version, :libsuitesparse_wrapper), Cint, (Ptr{Cint},), version_array)
end
const version = VersionNumber(version_array...)
const cholmod_com_sz = ccall((:jl_cholmod_common_size,:libsuitesparse_wrapper),Int,())

type CHOLMODException <: Exception
    msg::AbstractString
end

## macro to generate the name of the C function according to the integer type
macro cholmod_name(nm,typ) string("cholmod_", eval(typ) == Int64 ? "l_" : "", nm) end

for Ti in IndexTypes
    @eval begin
        function common(::Type{$Ti})
            a = fill(0xff, cholmod_com_sz)
            @isok ccall((@cholmod_name "start" $Ti
                , :libcholmod), Cint, (Ptr{UInt8},), a)
            set_print_level(a, 0) # no printing from CHOLMOD by default
            a
        end
    end
end

### A way of examining some of the fields in cholmod_com
### Probably better to make this a Dict{ASCIIString,Tuple} and
### save the offsets and the lengths and the types.  Then the names can be checked.
type Common
    dbound::Float64
    maxrank::Int
    supernodal_switch::Float64
    supernodal::Int32
    final_asis::Int32
    final_super::Int32
    final_ll::Int32
    final_pack::Int32
    final_monotonic::Int32
    final_resymbol::Int32
    prefer_zomplex::Int32               # should always be false
    prefer_upper::Int32
    print::Int32                        # print level. Default: 3
    precise::Int32                      # print 16 digits, otherwise 5
    nmethods::Int32                     # number of ordering methods
    selected::Int32
    postorder::Int32
    itype::Int32
    dtype::Int32
end

### These offsets should be reconfigured to be less error-prone in matches
const cholmod_com_offsets = Array(Int, length(Common.types))
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Int},), cholmod_com_offsets)
const common_final_ll = (1:4) + cholmod_com_offsets[7]
const common_print = (1:4) + cholmod_com_offsets[13]
const common_itype = (1:4) + cholmod_com_offsets[18]

### there must be an easier way but at least this works.
# function Common(aa::Array{UInt8,1})
#     typs = Common.types
#     sz = map(sizeof, typs)
#     args = map(i->reinterpret(typs[i], aa[cholmod_com_offsets[i] + (1:sz[i])])[1], 1:length(sz))
#     eval(Expr(:call, unshift!(args, :Common), Any))
# end

function set_print_level(cm::Array{UInt8}, lev::Integer)
    cm[common_print] = reinterpret(UInt8, [int32(lev)])
end

## cholmod_dense pointers passed to or returned from C functions are of Julia type
## Ptr{c_Dense}.  The Dense type contains a c_Dense object and other
## fields then ensure the memory pointed to is freed when it should be and not before.
type c_Dense{T<:VTypes}
    m::Int
    n::Int
    nzmax::Int
    lda::Int
    xpt::Ptr{T}
    zpt::Ptr{Void}
    xtype::Cint
    dtype::Cint
end

type Dense{T<:VTypes} <: AbstractMatrix{T}
    c::c_Dense
    mat::Matrix{T}
end

if version >= v"2.1.0" # CHOLMOD version 2.1.0 or later
    type c_Factor{Tv<:VTypes,Ti<:ITypes}
        n::Int
        minor::Int
        Perm::Ptr{Ti}
        ColCount::Ptr{Ti}
        IPerm::Ptr{Ti}        # this pointer was added in verison 2.1.0
        nzmax::Int
        p::Ptr{Ti}
        i::Ptr{Ti}
        x::Ptr{Tv}
        z::Ptr{Void}
        nz::Ptr{Ti}
        next::Ptr{Ti}
        prev::Ptr{Ti}
        nsuper::Int
        ssize::Int
        xsize::Int
        maxcsize::Int
        maxesize::Int
        super::Ptr{Ti}
        pi::Ptr{Ti}
        px::Ptr{Ti}
        s::Ptr{Ti}
        ordering::Cint
        is_ll::Cint
        is_super::Cint
        is_monotonic::Cint
        itype::Cint
        xtype::Cint
        dtype::Cint
    end

    type Factor{Tv<:VTypes,Ti<:ITypes}
        c::c_Factor{Tv,Ti}
        Perm::Vector{Ti}
        ColCount::Vector{Ti}
        IPerm::Vector{Ti}
        p::Vector{Ti}
        i::Vector{Ti}
        x::Vector{Tv}
        nz::Vector{Ti}
        next::Vector{Ti}
        prev::Vector{Ti}
        super::Vector{Ti}
        pi::Vector{Ti}
        px::Vector{Ti}
        s::Vector{Ti}
    end

    function Factor{Tv<:VTypes,Ti<:ITypes}(cp::Ptr{c_Factor{Tv,Ti}})
        cfp = unsafe_load(cp)
        Perm = pointer_to_array(cfp.Perm, (cfp.n,), true)
        ColCount = pointer_to_array(cfp.ColCount, (cfp.n,), true)
        IPerm = pointer_to_array(cfp.IPerm, (cfp.IPerm == C_NULL ? 0 : cfp.n + 1,), true)
        p = pointer_to_array(cfp.p, (cfp.p == C_NULL ? 0 : cfp.n + 1,), true)
        i = pointer_to_array(cfp.i, (cfp.i == C_NULL ? 0 : cfp.nzmax,), true)
        x = pointer_to_array(cfp.x, (cfp.x == C_NULL ? 0 : max(cfp.nzmax,cfp.xsize),), true)
        nz = pointer_to_array(cfp.nz, (cfp.nz == C_NULL ? 0 : cfp.n,), true)
        next = pointer_to_array(cfp.next, (cfp.next == C_NULL ? 0 : cfp.n + 2,), true)
        prev = pointer_to_array(cfp.prev, (cfp.prev == C_NULL ? 0 : cfp.n + 2,), true)
        super = pointer_to_array(cfp.super, (cfp.super == C_NULL ? 0 : cfp.nsuper + 1,), true)
        pi = pointer_to_array(cfp.pi, (cfp.pi == C_NULL ? 0 : cfp.nsuper + 1,), true)
        px = pointer_to_array(cfp.px, (cfp.px == C_NULL ? 0 : cfp.nsuper + 1,), true)
        s = pointer_to_array(cfp.s, (cfp.s == C_NULL ? 0 : cfp.ssize + 1,), true)
        cf = Factor{Tv,Ti}(cfp, Perm, ColCount, IPerm, p, i, x, nz, next, prev,
                                  super, pi, px, s)
        c_free(cp)
        cf
    end
else
    type c_Factor{Tv<:VTypes,Ti<:ITypes}
        n::Int
        minor::Int
        Perm::Ptr{Ti}
        ColCount::Ptr{Ti}
        nzmax::Int
        p::Ptr{Ti}
        i::Ptr{Ti}
        x::Ptr{Tv}
        z::Ptr{Void}
        nz::Ptr{Ti}
        next::Ptr{Ti}
        prev::Ptr{Ti}
        nsuper::Int
        ssize::Int
        xsize::Int
        maxcsize::Int
        maxesize::Int
        super::Ptr{Ti}
        pi::Ptr{Ti}
        px::Ptr{Ti}
        s::Ptr{Ti}
        ordering::Cint
        is_ll::Cint
        is_super::Cint
        is_monotonic::Cint
        itype::Cint
        xtype::Cint
        dtype::Cint
    end

    type Factor{Tv<:VTypes,Ti<:ITypes}
        c::c_Factor{Tv,Ti}
        Perm::Vector{Ti}
        ColCount::Vector{Ti}
        p::Vector{Ti}
        i::Vector{Ti}
        x::Vector{Tv}
        nz::Vector{Ti}
        next::Vector{Ti}
        prev::Vector{Ti}
        super::Vector{Ti}
        pi::Vector{Ti}
        px::Vector{Ti}
        s::Vector{Ti}
    end

    function Factor{Tv<:VTypes,Ti<:ITypes}(cp::Ptr{c_Factor{Tv,Ti}})
        cfp = unsafe_load(cp)
        Perm = pointer_to_array(cfp.Perm, (cfp.n,), true)
        ColCount = pointer_to_array(cfp.ColCount, (cfp.n,), true)
        p = pointer_to_array(cfp.p, (cfp.p == C_NULL ? 0 : cfp.n + 1,), true)
        i = pointer_to_array(cfp.i, (cfp.i == C_NULL ? 0 : cfp.nzmax,), true)
        x = pointer_to_array(cfp.x, (cfp.x == C_NULL ? 0 : max(cfp.nzmax,cfp.xsize),), true)
        nz = pointer_to_array(cfp.nz, (cfp.nz == C_NULL ? 0 : cfp.n,), true)
        next = pointer_to_array(cfp.next, (cfp.next == C_NULL ? 0 : cfp.n + 2,), true)
        prev = pointer_to_array(cfp.prev, (cfp.prev == C_NULL ? 0 : cfp.n + 2,), true)
        super = pointer_to_array(cfp.super, (cfp.super == C_NULL ? 0 : cfp.nsuper + 1,), true)
        pi = pointer_to_array(cfp.pi, (cfp.pi == C_NULL ? 0 : cfp.nsuper + 1,), true)
        px = pointer_to_array(cfp.px, (cfp.px == C_NULL ? 0 : cfp.nsuper + 1,), true)
        s = pointer_to_array(cfp.s, (cfp.s == C_NULL ? 0 : cfp.ssize + 1,), true)
        cf = Factor{Tv,Ti}(cfp, Perm, ColCount, p, i, x, nz, next, prev,
                                  super, pi, px, s)
        c_free(cp)
        cf
    end
end

type c_Sparse{Tv<:VTypes,Ti<:ITypes}
    m::Csize_t
    n::Csize_t
    nzmax::Csize_t
    ppt::Ptr{Ti}
    ipt::Ptr{Ti}
    nzpt::Ptr{Ti}
    xpt::Ptr{Tv}
    zpt::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

# Corresponds to the exact definition of cholmod_sparse_struct in the library. Useful when reading matrices of unknown type from files as in cholmod_read_sparse
type c_SparseVoid
    m::Csize_t
    n::Csize_t
    nzmax::Csize_t
    ppt::Ptr{Void}
    ipt::Ptr{Void}
    nzpt::Ptr{Void}
    xpt::Ptr{Void}
    zpt::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

type Sparse{Tv<:VTypes,Ti<:ITypes} <: AbstractSparseMatrix{Tv,Ti}
    c::c_Sparse{Tv,Ti}
    colptr0::Vector{Ti}
    rowval0::Vector{Ti}
    nzval::Vector{Tv}
end

eltype{T<:VTypes}(A::Dense{T}) = T
eltype{T<:VTypes}(A::Factor{T}) = T
eltype{T<:VTypes}(A::Sparse{T}) = T

## The Dense constructor does not copy the contents, which is generally what you
## want as most uses of Dense objects are read-only.
function Dense{T<:VTypes}(A::VecOrMat{T}) # uses the memory from Julia
    m, n = size(A,1),  size(A,2)
    Dense(c_Dense{T}(m, n, m*n, stride(A, 2), convert(Ptr{T}, A), C_NULL, xtyp(T), dtyp(T)),
        length(size(A)) == 2 ? A : reshape(A, (m, n)))
end
function Dense{T<:VTypes}(c::Ptr{c_Dense{T}})
    cp = unsafe_load(c)
    ## the true in the call to pointer_to_array means Julia will free the memory
    val = Dense(cp, pointer_to_array(cp.xpt, (cp.m,cp.n), true))
    c_free(c)
    val
end

function Sparse{Tv<:VTypes,Ti<:ITypes}(colpt::Vector{Ti},
                                                     rowval::Vector{Ti},
                                                     nzval::Vector{Tv},
                                                     m::Integer,
                                                     n::Integer,
                                                     stype::Signed)
    bb = colpt[1]

    if bool(bb)                         # one-based
        colpt0 = decrement(colpt)
        rowval0 = decrement(rowval)
    else                                # zero based
        colpt0 = colpt
        rowval0 = rowval
    end
    nz = colpt0[end]

    it = ityp(Ti)
    cs = Sparse(c_Sparse{Tv,Ti}(m,
                                n,
                                int(nz),
                                convert(Ptr{Ti}, colpt0),
                                convert(Ptr{Ti}, rowval0),
                                C_NULL,
                                convert(Ptr{Tv}, nzval),
                                C_NULL,
                                int32(stype),
                                ityp(Ti),
                                xtyp(Tv),
                                dtyp(Tv),
                                TRUE,
                                TRUE),
                colpt0,
                rowval0,
                nzval)

    @isok isvalid(cs)

    return cs
end
function Sparse{Tv<:VTypes,Ti<:ITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Signed)
    Sparse(A.colptr, A.rowval, A.nzval, size(A,1), size(A,2), stype)
end
function Sparse(A::SparseMatrixCSC)
    AC = Sparse(A, 0)
    i = symmetry(AC, 0)[1] # Check for Hermitianity
    if (isreal(A) && (i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG)) || # real case
       (i == MM_HERMITIAN || i == MM_HERMITIAN_POSDIAG) # complex case
        AC.c.stype = -1
    end
    AC
end
Sparse{Ti<:ITypes}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = Sparse(A.data, A.uplo == 'L' ? -1 : 1)
Sparse{Ti<:ITypes}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) = Sparse(A.data, A.uplo == 'L' ? -1 : 1)


# Constructs a Sparse object from a pointer to a cholmod_sparse_struct created by CHOLMOD. Julia takes ownership of the memory allocated by CHOLMOD and the pointer can therefore be freed without memory leek.
function Sparse{Tv<:VTypes,Ti<:ITypes}(cp::Ptr{c_Sparse{Tv,Ti}})
    csp = unsafe_load(cp)

    # Take control of the memory allocated by CHOLMOD
    colptr0 = pointer_to_array(csp.ppt, (csp.n + 1,), true)
    rowval0 = pointer_to_array(csp.ipt, (csp.nzmax,), true)
    nzval   = pointer_to_array(csp.xpt, (csp.nzmax,), true)

    cms = Sparse{Tv,Ti}(csp, colptr0, rowval0, nzval)

    # Free memory
    csp.packed == 1 && c_free(csp.nzpt) # Julia's Sparse is not using unpacked storage
    c_free(cp)

    cms
end
# Useful when reading in files, but not type stable
function Sparse(p::Ptr{c_SparseVoid})
    s = unsafe_load(p)

    # Check integer type
    if s.itype == INT
        Ti = Cint
    elseif s.itype == INTLONG
        throw(CHOLMODException("the value of itype was $s.itype. This combination of integer types shouldn't happen. Please submit a bug report."))
    elseif s.itype == LONG
        Ti = SuiteSparse_long
    else
        throw(CHOLMODException("illegal value of itype: $s.itype"))
    end

    # Check for double or single precision
    if s.dtype == DOUBLE
        Tv = Float64
    elseif s.dtype == SINGLE
        Tv = Float32
    else
        throw(CHOLMODException("illegal value of dtype: $s.dtype"))
    end

    # Check for real or complex
    if s.xtype == COMPLEX
        Tv = Complex{Tv}
    elseif s.xtype != REAL
        throw(CHOLMODException("illegal value of xtype: $s.xtype"))
    end

    # Take control of the memory allocated by CHOLMOD
    colptr0 = pointer_to_array(convert(Ptr{Ti}, s.ppt), (s.n + 1,), true)
    rowval0 = pointer_to_array(convert(Ptr{Ti}, s.ipt), (s.nzmax,), true)
    nzval   = pointer_to_array(convert(Ptr{Tv}, s.xpt), (s.nzmax,), true)

    r = Sparse{Tv,Ti}(c_Sparse{Tv,Ti}(s.m,
                                      s.n,
                                      s.nzmax,
                                      pointer(colptr0),
                                      pointer(rowval0),
                                      C_NULL,
                                      pointer(nzval),
                                      C_NULL,
                                      s.stype,
                                      s.itype,
                                      s.xtype,
                                      s.dtype,
                                      s.sorted,
                                      s.packed),
                            colptr0, rowval0, nzval)

    # Free memory
    s.packed == 1 && c_free(s.nzpt) # Julia's Sparse is not using unpacked storage
    c_free(p)

    r
end

Sparse{Tv<:VTypes}(D::Dense{Tv}) = Sparse(D,1) # default Ti is Int

# Dense wrappers
### cholmod_core_h ###
function zeros{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    Dense(ccall((:cholmod_zeros, :libcholmod), Ptr{c_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Int32)))
end
zeros(m::Integer, n::Integer) = zeros(m, n, Float64)

function ones{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    Dense(ccall((:cholmod_ones, :libcholmod), Ptr{c_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Int32)))
end
ones(m::Integer, n::Integer) = ones(m, n, Float64)

function eye{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    Dense(ccall((:cholmod_eye, :libcholmod), Ptr{c_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Int32)))
end
eye(m::Integer, n::Integer) = eye(m, n, Float64)
eye(n::Integer) = eye(n, n, Float64)

function copy_dense{Tv<:VTypes}(A::Dense{Tv})
    Dense(ccall((:cholmod_copy_dense, :libcholmod), Ptr{c_Dense{Tv}},
        (Ptr{c_Dense{Tv}}, Ptr{UInt8}),
         &A.c, common(Cint)))
end

### cholmod_matrixops.h ###
function norm_dense{Tv<:VTypes}(D::Dense{Tv}, p::Integer)
    ccall((:cholmod_norm_dense, :libcholmod), Cdouble,
        (Ptr{c_Dense{Tv}}, Cint, Ptr{UInt8}),
          &D.c, p, common(Int32))
end

### cholmod_check.h ###
function check_dense{T<:VTypes}(A::Dense{T})
    bool(ccall((:cholmod_check_dense, :libcholmod), Cint,
        (Ptr{c_Dense{T}}, Ptr{UInt8}),
         &A.c, common(Cint)))
end

# Non-Dense wrappers (which all depend on IType)
for Ti in IndexTypes
    @eval begin
        # This method is probably not necessary as memory allocated by CHOLMOD is usually overtaken by Julia when cosntructing Julia instances from pointers.
        function free_sparse{Tv<:VTypes}(ptr::Ptr{c_Sparse{Tv,$Ti}})
            cm = common($Ti)
            @isok ccall((@cholmod_name "free_sparse" $Ti
                ,:libcholmod), Cint,
                    (Ptr{Ptr{c_Sparse{Tv,$Ti}}}, Ptr{UInt8}),
                        &ptr, cm)
        end

        function (*){Tv<:VTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti})
            Sparse(ccall((@cholmod_name "ssmult" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{c_Sparse{Tv,$Ti}}, Cint, Cint, Cint, Ptr{UInt8}),
                          &A.c, &B.c, 0, true, true, common($Ti)))
        end

        function transpose{Tv<:VTypes}(A::Sparse{Tv,$Ti}, values::Integer)
            cm = common($Ti)
            Sparse(ccall((@cholmod_name "transpose" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        &A.c, values, cm))
        end

        function ssmult{Tv<:VTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, stype::Integer, values::Integer, sorted::Integer)
            cm = common($Ti)
            Sparse(ccall((@cholmod_name "ssmult" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{c_Sparse{Tv,$Ti}}, Cint, Cint, Cint, Ptr{UInt8}),
                        &A.c, &B.c, stype, values, sorted, cm))
        end

        function aat{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, fset::Vector{$Ti}, mode::Integer)
            cm = common($Ti)
            Sparse(ccall((@cholmod_name "aat" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{$Ti}, Csize_t, Cint, Ptr{UInt8}),
                        &A.c, fset, length(fset), mode, cm))
        end

        function copy{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, stype::Integer, mode::Integer)
            cm = common($Ti)
            Sparse(ccall((@cholmod_name "copy" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Cint, Ptr{UInt8}),
                        &A.c, 0, 1, cm))
        end

        function sparse_to_dense{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            Dense(ccall((@cholmod_name "sparse_to_dense" $Ti
                ,:libcholmod), Ptr{c_Dense{Tv}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, common($Ti)))
        end
        function dense_to_sparse{Tv<:VTypes}(D::Dense{Tv},i::$Ti)
            Sparse(ccall((@cholmod_name "dense_to_sparse" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Dense{Tv}}, Cint, Ptr{UInt8}),
                        &D.c, true, common($Ti)))
        end

        function factor_to_sparse{Tv<:VTypes}(L::Factor{Tv,$Ti})
            Sparse(ccall((@cholmod_name "factor_to_sparse" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &L.c, common($Ti)))
        end

        function copy_factor{Tv<:VTypes}(L::Factor{Tv,$Ti})
            Factor(ccall((@cholmod_name "copy_factor" $Ti
                ,:libcholmod), Ptr{c_Factor{Tv,$Ti}},
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &L.c, common($Ti)))
        end

        function change_factor{Tv<:VTypes}(to_xtype::Integer, to_ll::Bool, to_super::Bool, to_packed::Bool, to_monotonic::Bool, L::Factor{Tv,$Ti})
            @isok ccall((@cholmod_name "change_factor" $Ti
                ,:libcholmod), Cint,
                    (Cint, Cint, Cint, Cint, Cint, Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        to_xtype, to_ll, to_super, to_packed, to_monotonic, L, cm)
            L
        end

        function check_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            bool(ccall((@cholmod_name "check_sparse" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, common($Ti)))
        end

        function check_factor{Tv<:VTypes}(L::Factor{Tv,$Ti})
            bool(ccall((@cholmod_name "check_factor" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &L.c, common($Ti)))
        end

        function analyze{Tv<:VTypes}(A::Sparse{Tv,$Ti}, C::Vector{UInt8} = common($Ti))
            Factor(ccall((@cholmod_name "analyze" $Ti
                ,:libcholmod), Ptr{c_Factor{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, C))
        end

        function factorize{Tv<:VTypes}(A::Sparse{Tv,$Ti}, F::Factor{Tv,$Ti}, C::Vector{UInt8} = common($Ti))
            @isok ccall((@cholmod_name "factorize" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, &F.c, C)
            F
        end

        function factorize_p{Tv<:VTypes}(A::Sparse{Tv,$Ti}, β::Tv, fset::Vector{$Ti}, F::Factor{Tv,$Ti}, C::Vector{UInt8} = common($Ti))
            @isok ccall((@cholmod_name "factorize_p" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{Cdouble}, Ptr{$Ti}, Csize_t,
                        Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                            &A.c, &β, fset, length(fset),
                                &F.c, common($Ti))
            F
        end

        ### cholmod_core.h ###
        function pack_factor{Tv<:VTypes}(L::Factor{Tv,$Ti})
            @isok ccall((@cholmod_name "pack_factor" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &L.c, common($Ti))
            L
        end

        function nnz{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            ccall((@cholmod_name "nnz" $Ti
                ,:libcholmod), Int,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, common($Ti))
        end

        function speye{Tv<:VTypes}(m::Integer, n::Integer, ::Type{Tv})
            Sparse(ccall((@cholmod_name "speye" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                        m, n, xtyp(Tv), common($Ti)))
        end

        function spzeros{Tv<:VTypes}(m::Integer, n::Integer, nzmax::Integer, ::Type{Tv})
            Sparse(ccall((@cholmod_name "spzeros" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Csize_t, Csize_t, Csize_t, Cint, Ptr{UInt8}),
                        m, n, nzmax, xtyp(Tv), common($Ti)))
        end

        function copy_factor{Tv<:VTypes}(L::Factor{Tv,$Ti})
            Factor(ccall((@cholmod_name "copy_factor" $Ti
                ,:libcholmod), Ptr{c_Factor{Tv,$Ti}},
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        &L.c, common($Ti)))
        end
        function copy_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            Sparse(ccall((@cholmod_name "copy_sparse" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &A.c, common($Ti)))
        end
        function copy{Tv<:VTypes}(A::Sparse{Tv,$Ti}, stype::Integer, mode::Integer)
            Sparse(ccall((@cholmod_name "copy" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Cint, Ptr{UInt8}),
                        &A.c, stype, mode, common($Ti)))
        end

        ### cholmod_check.h ###
        function print_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti}, name::ASCIIString)
            cm = common($Ti)
            set_print_level(cm, 3)
            @isok ccall((@cholmod_name "print_sparse" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                         &A.c, name, cm)
            nothing
        end
        function print_factor{Tv<:VTypes}(L::Factor{Tv,$Ti}, name::ASCIIString)
            cm = common($Ti)
            set_print_level(cm, 3)
            @isok ccall((@cholmod_name "print_factor" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Factor{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                        &L.c, name, cm)
            nothing
        end

        ### cholmod_matrixops.h ###
        function norm_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti}, norm::Integer)
            norm == 0 || norm == 1 || throw(ArgumentError("norm argument must be either 0 or 1"))
            ccall((@cholmod_name "norm_sparse" $Ti
                , :libcholmod), Cdouble,
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        &A.c, norm, common($Ti))
        end

        function horzcat{Tv<:VTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, values::Bool)
            ccall((@cholmod_name "horzcat" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        &A.c, &B.c, values, common($Ti))
        end

        function scale{Tv<:VTypes}(S::Dense{Tv}, scale::Integer, A::Sparse{Tv,$Ti})
            @isok ccall((@cholmod_name "scale" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Dense{Tv}}, Cint, Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        &S.c, scale, &A.c, common($Ti))
            A
        end

        function sdmult{Tv<:VTypes}(A::Sparse{Tv,$Ti}, transpose::Bool, α::Tv, β::Tv, X::Dense{Tv}, Y::Dense{Tv})
            m, n = size(A)
            nc = transpose ? m : n
            nr = transpose ? n : m
            if nc != size(X,1)
                throw(DimensionMismatch("Incompatible dimensions, $nc and $(size(X,1))"))
            end
            @isok ccall((@cholmod_name "sdmult" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{Cdouble}, Ptr{Cdouble},
                        Ptr{c_Dense{Tv}}, Ptr{c_Dense{Tv}}, Ptr{UInt8}),
                            &A.c, transpose, &α, &β,
                                &X.c, &Y.c, common($Ti))
            Y
        end

        function vertcat{Tv<:VTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, values::Bool)
            ccall((@cholmod_name "vertcat" $Ti
                , :libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        &A.c, &B.c, values, common($Ti))
        end

        function symmetry{Tv<:VTypes}(A::Sparse{Tv,$Ti}, option::Integer)
            xmatched = Array($Ti, 1)
            pmatched = Array($Ti, 1)
            nzoffdiag = Array($Ti, 1)
            nzdiag = Array($Ti, 1)
            rv = ccall((@cholmod_name "symmetry" $Ti
                , :libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Cint, Ptr{$Ti}, Ptr{$Ti},
                        Ptr{$Ti}, Ptr{$Ti}, Ptr{UInt8}),
                            &A.c, option, xmatched, pmatched,
                                nzoffdiag, nzdiag, common($Ti))
            rv, xmatched[1], pmatched[1], nzoffdiag[1], nzdiag[1]
        end

        # cholmod_cholesky.h
        function etree{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            Parent = Array($Ti, size(A,2))
            @isok ccall((@cholmod_name "etree" $Ti
                ,:libcholmod), Cint,
                    (Ptr{c_Sparse{Tv,$Ti}}, Ptr{$Ti}, Ptr{UInt8}),
                        &A.c, Parent, common($Ti))
            return Parent
        end

        function solve{Tv<:VTypes}(sys::Integer, L::Factor{Tv,$Ti}, B::Dense{Tv})
            size(L,1) == size(B,1) || throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(L,1)) rows, but RHS has $(size(B,1)) rows."))
            Dense(ccall((@cholmod_name "solve" $Ti
                ,:libcholmod), Ptr{c_Dense{Tv}},
                    (Cint, Ptr{c_Factor{Tv,$Ti}}, Ptr{c_Dense{Tv}}, Ptr{UInt8}),
                        sys, &L.c, &B.c, common($Ti)))
        end

        function spsolve{Tv<:VTypes}(sys::Integer, L::Factor{Tv,$Ti}, B::Sparse{Tv,$Ti})
            size(L,1) == size(B,1) || throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(L,1)) rows, but RHS has $(size(B,1)) rows."))
            Sparse(ccall((@cholmod_name "spsolve" $Ti
                ,:libcholmod), Ptr{c_Sparse{Tv,$Ti}},
                    (Cint, Ptr{c_Factor{Tv,$Ti}}, Ptr{c_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        sys, &L.c, &B.c, common($Ti)))
        end
    end
end

# Autodetects the types
function read_sparse(file::CFILE)
    Sparse(ccall((:cholmod_read_sparse, :libcholmod), Ptr{c_SparseVoid},
        (Ptr{Void}, Ptr{UInt8}),
            file.ptr, common(Int32)))
end

#########################
# High level interfaces #
#########################

# Convertion
Dense(A::Sparse) = sparse_to_dense(A)

Sparse(A::Dense) = dense_to_sparse(A)
function Sparse(L::Factor)
    if bool(L.c.is_ll)
        return factor_to_sparse(L)
    end
    cm = common($Ti)
    Lcll = copy_factor(L)
    change_factor(L.c.xtype, true, L.c.is_super == 1, true, true, Lcll, L)
    return factor_to_sparse(Lcll)
end
function Sparse(filename::ASCIIString)
    f = open(filename)
    A = read_sparse(CFILE(f))
    close(f)
    A
end

show(io::IO, L::Factor) = print_factor(L, "")

isvalid(A::Dense) = check_dense(A)
isvalid(A::Sparse) = check_sparse(A)
isvalid(A::Factor) = check_factor(A)

copy(A::Dense) = copy_dense(A)
copy(A::Sparse) = copy_sparse(A)
copy(A::Factor) = copy_factor(A)

size(B::Dense) = size(B.mat)
size(B::Dense, d::Integer) = size(B.mat, d)
size(A::Sparse) = (int(A.c.m), int(A.c.n))
size(A::Sparse, d::Integer) = d > 0 ? (d == 1 ? int(A.c.m) : (d == 2 ? int(A.c.n) : 1)) : BoundsError()
size(L::Factor) = (n = int(L.c.n); (n,n))
size(L::Factor, d::Integer) = d < 1 ? throw(BoundsError("Dimension $d out of range")) : (d <= 2 ? int(L.c.n) : 1)

getindex(A::Dense, i::Integer) = A.mat[i]
getindex(A::Dense, i::Integer, j::Integer) = A.mat[i, j]
getindex(A::Sparse, i::Integer) = getindex(A, ind2sub(size(A),i)...)
function getindex{T}(A::Sparse{T}, i0::Integer, i1::Integer)
    !(1 <= i0 <= A.c.m && 1 <= i1 <= A.c.n) && throw(BoundsError())
    A.c.stype < 0 && i0 < i1 && return conj(A[i1,i0])
    A.c.stype > 0 && i0 > i1 && return conj(A[i1,i0])

    r1 = int(A.colptr0[i1] + 1)
    r2 = int(A.colptr0[i1 + 1])
    (r1 > r2) && return zero(T)
    r1 = int(searchsortedfirst(A.rowval0, i0 - 1, r1, r2, Base.Order.Forward))
    ((r1 > r2) || (A.rowval0[r1] + 1 != i0)) ? zero(T) : A.nzval[r1]
end

# Convert invalidates the input matrix
function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, A::Sparse{Tv,Ti})
    B = SparseMatrixCSC(A.c.m, A.c.n, increment!(A.colptr0), increment!(A.rowval0), A.nzval)
    if A.c.stype != 0
        return isreal(A) ? Symmetric(B, A.c.stype < 0 ? :L : :U) : Hermitian(B, A.c.stype < 0 ? :L : :U)
    end
    return B
end
sparse!{Tv,Ti}(A::Sparse{Tv,Ti}) = convert(SparseMatrixCSC{Tv,Ti}, A)
sparse(A::Sparse)   = sparse!(copy(A))
sparse(L::Factor)   = sparse!(Sparse(L))
sparse(D::Dense)    = sparse!(Sparse(D))
function full(A::Sparse)
    B = Dense(A).mat
    if A.c.stype != 0
        return isreal(A) ? Symmetric(B, A.c.stype < 0 ? :L : :U) : Hermitian(B, A.c.stype < 0 ? :L : :U)
    end
    return B
end
full(A::Dense) = A.mat

## Multiplication

(*)(A::Sparse, B::Dense) = sdmult(A, false, 1., 0., B, zeros(size(A, 1), size(B, 2)))
(*)(A::Sparse, B::VecOrMat) = (*)(A, Dense(B))

function A_mul_Bc{Tv<:VRealTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, B::Sparse{Tv,Ti})
    cm = common($Ti)

    if !is(A,B)
        aa1 = transpose(B, 1)
        ## result of ssmult will have stype==0, contain numerical values and be sorted
        return ssmult(A, aa1, 0, true, true)
    end

    ## The A*A' case is handled by cholmod_aat. This routine requires
    ## A->stype == 0 (storage of upper and lower parts). If neccesary
    ## the matrix A is first converted to stype == 0
    if A.c.stype != 0
        aa1 = copy(A, 0, 1)
        return aat(aa1, Ti[], 1)
    else
        return aat(A, Ti[], 1)
    end
end

function Ac_mul_B{Tv<:VRealTypes}(A::Sparse{Tv}, B::Sparse{Tv})
    aa1 = transpose(A, 1)
    if is(A,B)
        return A_mul_Bc(aa1, aa1)
    end
    ## result of ssmult will have stype==0, contain numerical values and be sorted
    return ssmult(aa1, B, 0, true, true, cm)
end

A_mul_Bt{Tv<:VRealTypes}(A::Sparse{Tv}, B::Sparse{Tv}) = A_mul_Bc(A, B) # in the unlikely event of writing A*B.' instead of A*B'

At_mul_B{Tv<:VRealTypes}(A::Sparse{Tv}, B::Sparse{Tv}) = Ac_mul_B(A,B) # in the unlikely event of writing A.'*B instead of A'*B

Ac_mul_B(A::Sparse, B::Dense) = sdmult(A, true, 1., 0., B, zeros(size(A, 2), size(B, 2)))
Ac_mul_B(A::Sparse, B::VecOrMat) =  Ac_mul_B(A, Dense(B))


## Factorization methods

analyze(A::SparseMatrixCSC) = analyze(Sparse(A))

function cholfact(A::Sparse)
    cm = common(indtype(A))
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    F = analyze(A, cm)
    factorize(A, F, cm)
    F.c.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(F.c.minor))
    return F
end

function ldltfact(A::Sparse)
    cm = common(indtype(A))
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [zero(Cint)]) # Hack! makes it a ldlt
    F = analyze(A, cm)
    factorize(A, F, cm)
    return F
end

function cholfact{Tv<:VTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, β::Tv)
    cm = common(Ti)
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    F = analyze(A, cm)
    factorize_p(A, β, Ti[], F, cm)
    F.c.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(F.c.minor))
    return F
end

function ldltfact{Tv<:VTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, β::Tv)
    cm = common(Ti)
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [zero(Cint)]) # Hack! makes it a ldlt
    F = analyze(A, cm)
    factorize_p(A, β, Ti[], F, cm)
    F.c.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(F.c.minor))
    return F
end

cholfact!(L::Factor, A::Sparse) = factorize(A, L)
cholfact!{Tv<:VTypes,Ti<:ITypes}(L::Factor{Tv,Ti}, A::Sparse{Tv,Ti}, β::Tv) = factorize_p(A, β, Ti[], L)

cholfact{T<:VTypes}(A::Sparse{T}, β::Number) = cholfact(A, convert(T, β))
cholfact(A::SparseMatrixCSC) = cholfact(Sparse(A))
cholfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = cholfact(Sparse(A))
cholfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) = cholfact(Sparse(A))
cholfact!{T<:VTypes}(L::Factor{T}, A::Sparse{T}, β::Number) = cholfact!(L, A, convert(T, β))
cholfact!{T<:VTypes}(L::Factor{T}, A::SparseMatrixCSC{T}, β::Number) = cholfact!(L, Sparse(A), convert(T, β))
cholfact!{T<:VTypes}(L::Factor{T}, A::SparseMatrixCSC{T}) = cholfact!(L, Sparse(A))

ldltfact(A::SparseMatrixCSC) = ldltfact(Sparse(A))
ldltfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = ldltfact(Sparse(A))
ldltfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) = ldltfact(Sparse(A))

## Solvers

(\)(L::Factor, B::Dense) = solve(CHOLMOD_A, L, B)
(\)(L::Factor, b::Vector) = reshape(solve(CHOLMOD_A, L, Dense(b)).mat, length(b))
(\)(L::Factor, B::Matrix) = solve(CHOLMOD_A, L, Dense(B)).mat
(\)(L::Factor, B::Sparse) = spsolve(CHOLMOD_A, L, B)
# When right hand side is sparse, we have to ensure that the rhs is not marked as symmetric.
(\)(L::Factor, B::SparseMatrixCSC) = sparse!(spsolve(CHOLMOD_A, L, Sparse(B, 0)))

Ac_ldiv_B(L::Factor, B::Dense) = solve(CHOLMOD_A, L, B)
Ac_ldiv_B(L::Factor, B::VecOrMat) = solve(CHOLMOD_A, L, Dense(B)).mat
Ac_ldiv_B(L::Factor, B::Sparse) = spsolve(CHOLMOD_A, L, B)
Ac_ldiv_B(L::Factor, B::SparseMatrixCSC) = sparse!(CHOLMOD_A, spsolve(L, Sparse(B)))

speye(m::Integer, n::Integer) = speye(m, n, Float64) # default element type is Float64
speye(n::Integer) = speye(n, n, Float64)             # default shape is square

spzeros(m::Integer, n::Integer, nzmax::Integer) = spzeros(m, n, nzmax, 1.)

function norm(A::Sparse, p = 1)
    if p == 1
        return norm_sparse(A, 1)
    elseif p == Inf
        return norm_sparse(A, 0)
    else
        throw(ArgumentError("only 1 and inf norms are supported"))
    end
end

## Julia methods (non-CHOLMOD)
function findnz{Tv,Ti}(A::Sparse{Tv,Ti})
    jj = similar(A.rowval0)             # expand A.colptr0 to a vector of indices
    for j in 1:A.c.n, k in (A.colptr0[j]+1):A.colptr0[j+1]
        jj[k] = j
    end

    ind = similar(A.rowval0)
    ipos = 1
    count = 0
    for k in 1:length(A.nzval)
        if A.nzval[k] != 0
            ind[ipos] = k
            ipos += 1
            count += 1
        else
            println("warning: sparse matrix contains explicitly stored zeros")
        end
    end
    ind = ind[1:count]                  # ind is the indices of nonzeros in A.nzval
    (increment!(A.rowval0[ind]), jj[ind], A.nzval[ind])
end

findnz(L::Factor) = findnz(Sparse(L))

function diag{Tv}(A::Sparse{Tv})
    minmn = minimum(size(A))
    res = zeros(Tv,minmn)
    cp0 = A.colptr0
    rv0 = A.rowval0
    anz = A.nzval
    for j in 1:minmn, k in (cp0[j]+1):cp0[j+1]
        if rv0[k] == j-1
            res[j] += anz[k]
        end
    end
    res
end

function diag{Tv}(L::Factor{Tv})
    res = zeros(Tv,L.c.n)
    xv  = L.x
    if bool(L.c.is_super)
        nec = decrement!(diff(L.super))  # number of excess columns per supernode
        dstride = increment!(diff(L.pi)) # stride of diagonal elements (nrow + 1)
        px = L.px
        pos = 1
        for i in 1:length(nec)
            base = px[i] + 1
            res[pos] = xv[base]
            pos += 1
            for j in 1:nec[i]
                res[pos] = xv[base + j*dstride[i]]
                pos += 1
            end
        end
    else
        c0 = L.p
        r0 = L.i
        xv = L.x
        for j in 1:L.c.n
            jj = c0[j]+1
            assert(r0[jj] == j-1)
            res[j] = xv[jj]
        end
    end
    res
end

function logdet{Tv,Ti}(L::Factor{Tv,Ti},sub)
    res = zero(Tv)
    for d in diag(L)[sub] res += log(abs(d)) end
    bool(L.c.is_ll) ? 2res : res
end
logdet(L::Factor) = logdet(L, 1:L.c.n)
det(L::Factor) = exp(logdet(L))

isposdef!{Tv<:VTypes,Ti}(A::SparseMatrixCSC{Tv,Ti}) = ishermitian(A) && cholfact(A).c.minor == size(A,1)

function issym(A::Sparse)
    if isreal(A)
        if A.c.stype != 0
            return true
        else
            i = symmetry(A, ifelse(version >= v"4.4.3", 0, 1))[1] # 0 is faster, but had a bug before 4.4.3
            return i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG
        end
    else
        if A.c.stype != 0
            return false
        else
            i = symmetry(A, ifelse(version >= v"4.4.3", 0, 1))[1] # 0 is faster, but had a bug before 4.4.3
            return i == MM_HERMITIAN || i == MM_HERMITIAN_POSDIAG
        end
    end
end

function ishermitian(A::Sparse)
    if isreal(A)
        if A.c.stype != 0
            return true
        else
            i = symmetry(A, ifelse(version >= v"4.4.3", 0, 1))[1] # 0 is faster, but had a bug before 4.4.3
            return i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG
        end
    else
        if A.c.stype != 0
            return true
        else
            i = symmetry(A, ifelse(version >= v"4.4.3", 0, 1))[1] # 0 is faster, but had a bug before 4.4.3
            return i == MM_HERMITIAN || i == MM_HERMITIAN_POSDIAG
        end
    end
end

end #module
