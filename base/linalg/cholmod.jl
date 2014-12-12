module CHOLMOD

export                                  # types
 CholmodDense,
 CholmodFactor,
 CholmodSparse,
 CholmodTriplet,

 CholmodSparse!,                        # destructive constructors
 CholmodDense!,

 etree

using Base.LinAlg.UMFPACK               # for decrement, increment, etc.

import Base: (*), convert, copy, ctranspose, eltype, findnz, getindex, hcat,
             isvalid, nnz, show, size, sort!, transpose, vcat

import ..LinAlg: (\), A_mul_Bc, A_mul_Bt, Ac_ldiv_B, Ac_mul_B, At_ldiv_B, At_mul_B,
                 A_ldiv_B!, cholfact, cholfact!, copy, det, diag,
                 full, isposdef!, logdet, norm, scale, scale!, sparse

include("cholmod_h.jl")

macro isok(A)
    :($A==CHOLMOD_TRUE || throw(CholmodException()))
end

const chm_ver    = Array(Cint, 3)
if dlsym(dlopen("libcholmod"), :cholmod_version) != C_NULL
    ccall((:cholmod_version, :libcholmod), Cint, (Ptr{Cint},), chm_ver)
else
    ccall((:jl_cholmod_version, :libsuitesparse_wrapper), Cint, (Ptr{Cint},), chm_ver)
end
const chm_com_sz = ccall((:jl_cholmod_common_size,:libsuitesparse_wrapper),Int,())
const chm_com    = fill(0xff, chm_com_sz)
const chm_l_com  = fill(0xff, chm_com_sz)
## chm_com and chm_l_com must be initialized at runtime because they contain pointers
## to functions in libc.so, whose addresses can change
function cmn(::Type{Int32})
    if isnan(reinterpret(Float64,chm_com[1:8])[1])
        @isok ccall((:cholmod_start, :libcholmod), Cint, (Ptr{UInt8},), chm_com)
    end
    chm_com
end
function cmn(::Type{Int64})
    if isnan(reinterpret(Float64,chm_l_com[1:8])[1])
        @isok ccall((:cholmod_l_start, :libcholmod), Cint, (Ptr{UInt8},), chm_l_com)
    end
    chm_l_com
end

# check the size of SuiteSparse_long
if int(ccall((:jl_cholmod_sizeof_long,:libsuitesparse_wrapper),Csize_t,())) == 4
    const CholmodIndexTypes = (:Int32, )
    typealias CHMITypes Union(Int32)
else
    const CholmodIndexTypes = (:Int32, :Int64)
    typealias CHMITypes Union(Int32, Int64)
end


typealias CHMVTypes Union(Complex128, Float64)
typealias CHMVRealTypes Union(Float64)

type CholmodException <: Exception end

## macro to generate the name of the C function according to the integer type
macro chm_nm(nm,typ) string("cholmod_", eval(typ) == Int64 ? "l_" : "", nm) end

### A way of examining some of the fields in chm_com
### Probably better to make this a Dict{ASCIIString,Tuple} and
### save the offsets and the lengths and the types.  Then the names can be checked.
type ChmCommon
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
const chm_com_offsets = Array(Int, length(ChmCommon.types))
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Int},), chm_com_offsets)
const chm_final_ll_inds = (1:4) + chm_com_offsets[7]
const chm_prt_inds = (1:4) + chm_com_offsets[13]
const chm_ityp_inds = (1:4) + chm_com_offsets[18]

### there must be an easier way but at least this works.
function ChmCommon(aa::Array{UInt8,1})
    typs = ChmCommon.types
    sz = map(sizeof, typs)
    args = map(i->reinterpret(typs[i], aa[chm_com_offsets[i] + (1:sz[i])])[1], 1:length(sz))
    eval(Expr(:call, unshift!(args, :ChmCommon), Any))
end

function set_chm_prt_lev(cm::Array{UInt8}, lev::Integer) # can probably be removed
    cm[(1:4) + chm_com_offsets[13]] = reinterpret(UInt8, [int32(lev)])
end

## cholmod_dense pointers passed to or returned from C functions are of Julia type
## Ptr{c_CholmodDense}.  The CholmodDense type contains a c_CholmodDense object and other
## fields then ensure the memory pointed to is freed when it should be and not before.
type c_CholmodDense{T<:CHMVTypes}
    m::Int
    n::Int
    nzmax::Int
    lda::Int
    xpt::Ptr{T}
    zpt::Ptr{Void}
    xtype::Cint
    dtype::Cint
end

type CholmodDense{T<:CHMVTypes}
    c::c_CholmodDense
    mat::Matrix{T}
end

if (1000chm_ver[1]+chm_ver[2]) >= 2001 # CHOLMOD version 2.1.0 or later
    type c_CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
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

    type CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
        c::c_CholmodFactor{Tv,Ti}
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

    function CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodFactor{Tv,Ti}})
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
        cf = CholmodFactor{Tv,Ti}(cfp, Perm, ColCount, IPerm, p, i, x, nz, next, prev,
                                  super, pi, px, s)
        c_free(cp)
        cf
    end
else
    type c_CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
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

    type CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
        c::c_CholmodFactor{Tv,Ti}
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

    function CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodFactor{Tv,Ti}})
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
        cf = CholmodFactor{Tv,Ti}(cfp, Perm, ColCount, p, i, x, nz, next, prev,
                                  super, pi, px, s)
        c_free(cp)
        cf
    end
end

type c_CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    m::Csize_t
    n::Csize_t
    nzmax::Csize_t
    ppt::Ptr{Ti}
    ipt::Ptr{Ti}
    nzpt::Ptr{Void}
    xpt::Ptr{Tv}
    zpt::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

type CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    c::c_CholmodSparse{Tv,Ti}
    colptr0::Vector{Ti}
    rowval0::Vector{Ti}
    nzval::Vector{Tv}
end

type c_CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}
    m::Int
    n::Int
    nzmax::Int
    nnz::Int
    i::Ptr{Ti}
    j::Ptr{Ti}
    x::Ptr{Tv}
    z::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
end

type CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}
    c::c_CholmodTriplet{Tv,Ti}
    i::Vector{Ti}
    j::Vector{Ti}
    x::Vector{Tv}
end

eltype{T<:CHMVTypes}(A::CholmodDense{T}) = T
eltype{T<:CHMVTypes}(A::CholmodFactor{T}) = T
eltype{T<:CHMVTypes}(A::CholmodSparse{T}) = T
eltype{T<:CHMVTypes}(A::CholmodTriplet{T}) = T

## The CholmodDense! constructor does not copy the contents, which is generally what you
## want as most uses of CholmodDense objects are read-only.
function CholmodDense!{T<:CHMVTypes}(aa::VecOrMat{T}) # uses the memory from Julia
    m = size(aa,1); n = size(aa,2)
    CholmodDense(c_CholmodDense{T}(m, n, m*n, stride(aa,2), convert(Ptr{T}, aa),
                                   C_NULL, xtyp(T), dtyp(T)),
                 length(size(aa)) == 2 ? aa : reshape(aa, (m,n)))
end

## The CholmodDense constructor copies the contents
function CholmodDense{T<:CHMVTypes}(aa::VecOrMat{T})
    m = size(aa,1); n = size(aa,2)
    acp = length(size(aa)) == 2 ? copy(aa) : reshape(copy(aa), (m,n))
    CholmodDense(c_CholmodDense{T}(m, n, m*n, stride(aa,2), convert(Ptr{T}, acp),
                                   C_NULL, xtyp(T), dtyp(T)), acp)
end

function CholmodDense{T<:CHMVTypes}(c::Ptr{c_CholmodDense{T}})
    cp = unsafe_load(c)
    if cp.lda != cp.m || cp.nzmax != cp.m * cp.n
        throw(DimensionMismatch("overallocated cholmod_dense returned object of size $(cp.m) by $(cp.n) with leading dim $(cp.lda) and nzmax $(cp.nzmax)"))
    end
    ## the true in the call to pointer_to_array means Julia will free the memory
    val = CholmodDense(cp, pointer_to_array(cp.xpt, (cp.m,cp.n), true))
    c_free(c)
    val
end
CholmodDense!{T<:CHMVTypes}(c::Ptr{c_CholmodDense{T}}) = CholmodDense(c) # no distinction

function isvalid{T<:CHMVTypes}(cd::CholmodDense{T})
    bool(ccall((:cholmod_check_dense, :libcholmod), Cint,
               (Ptr{c_CholmodDense{T}}, Ptr{UInt8}), &cd.c, cmn(Int32)))
end

function chm_eye{T<:CHMVTypes}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_eye, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{UInt8}),
                       m, n,xtyp(T),cmn(Int32)))
end
chm_eye(m::Integer, n::Integer) = chm_eye(m, n, 1.)
chm_eye(n::Integer) = chm_eye(n, n, 1.)

function chm_ones{T<:CHMVTypes}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_ones, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{UInt8}),
                       m, n, xtyp(T), cmn(Int32)))
end
chm_ones(m::Integer, n::Integer) = chm_ones(m, n, 1.)

function chm_zeros{T<:CHMVTypes}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_zeros, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{UInt8}),
                       m, n, xtyp(T), cmn(Int32)))
end
chm_zeros(m::Integer, n::Integer) = chm_zeros(m, n, 1.)

function chm_print{T<:CHMVTypes}(cd::CholmodDense{T}, lev::Integer, nm::ASCIIString)
    @isok isvalid(cd)

    cm = cmn(Int32)
    orig = cm[chm_prt_inds]
    cm[chm_prt_inds] = reinterpret(UInt8, [int32(lev)])
    @isok ccall((:cholmod_print_dense, :libcholmod), Cint,
                   (Ptr{c_CholmodDense{T}}, Ptr{UInt8}, Ptr{UInt8}),
                   &cd.c, nm, cm)
    cm[chm_prt_inds] = orig
end
chm_print(cd::CholmodDense, lev::Integer) = chm_print(cd, lev, "")
chm_print(cd::CholmodDense) = chm_print(cd, int32(4), "")
show(io::IO,cd::CholmodDense) = chm_print(cd, int32(4), "")

function copy{Tv<:CHMVTypes}(B::CholmodDense{Tv})
    CholmodDense(ccall((:cholmod_copy_dense,:libcholmod), Ptr{c_CholmodDense{Tv}},
                       (Ptr{c_CholmodDense{Tv}},Ptr{UInt8}), &B.c, cmn(Int32)))
end

function norm{Tv<:CHMVTypes}(D::CholmodDense{Tv},p::Real=1)
    ccall((:cholmod_norm_dense, :libcholmod), Float64,
          (Ptr{c_CholmodDense{Tv}}, Cint, Ptr{UInt8}),
          &D.c, p == 1 ? 1 :(p == Inf ? 1 : throw(ArgumentError("p must be 1 or Inf"))),cmn(Int32))
end

function CholmodSparse!{Tv<:CHMVTypes,Ti<:CHMITypes}(colpt::Vector{Ti},
                                                     rowval::Vector{Ti},
                                                     nzval::Vector{Tv},
                                                     m::Integer,
                                                     n::Integer,
                                                     stype::Signed)
    bb = colpt[1]
    if bb != 0 && bb != 1 throw(DimensionMismatch("colpt[1] is $bb, must be 0 or 1")) end
    if any(diff(colpt) .< 0) throw(ArgumentError("elements of colpt must be non-decreasing")) end
    if length(colpt) != n + 1 throw(DimensionMismatch("length(colptr) = $(length(colpt)), should be $(n+1)")) end
    if bool(bb)                         # one-based
        decrement!(colpt)
        decrement!(rowval)
    end
    nz = colpt[end]
    if length(rowval) != nz || length(nzval) != nz
        throw(DimensionMismatch("length(rowval) = $(length(rowval)) and length(nzval) = $(length(nzval)) should be $nz"))
    end
    if any(rowval .< 0) || any(rowval .>= m)
        throw(ArgumentError("all elements of rowval must be in the range [0,$(m-1)]"))
    end
    it = ityp(Ti)
    cs = CholmodSparse(c_CholmodSparse{Tv,Ti}(m,n,int(nz),convert(Ptr{Ti},colpt),
                                               convert(Ptr{Ti},rowval), C_NULL,
                                               convert(Ptr{Tv},nzval), C_NULL,
                                               int32(stype), ityp(Ti),
                                               xtyp(Tv),dtyp(Tv),
                                               CHOLMOD_FALSE,CHOLMOD_TRUE),
                        colpt,rowval,nzval)

    @isok isvalid(cs)

    cs = sort!(cs)

    return cs
end
function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(colpt::Vector{Ti},
                                                    rowval::Vector{Ti},
                                                    nzval::Vector{Tv},
                                                    m::Integer,
                                                    n::Integer,
                                                    stype::Signed)
    CholmodSparse!(copy(colpt),copy(rowval),copy(nzval),m,n,stype)
end
function CholmodSparse!{Tv<:CHMVTypes,Ti<:CHMITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Signed)
    CholmodSparse!(A.colptr,A.rowval,A.nzval,size(A,1),size(A,2),stype)
end
function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Signed)
    CholmodSparse!(copy(A.colptr),copy(A.rowval),copy(A.nzval),size(A,1),size(A,2),stype)
end
function CholmodSparse(A::SparseMatrixCSC)
    stype = ishermitian(A) ? 1 : 0
    CholmodSparse(stype > 0 ? triu(A) : A, stype)
end
function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodSparse{Tv,Ti}})
    csp = unsafe_load(cp)
    colptr0 = pointer_to_array(csp.ppt, (csp.n + 1,), true)
    nnz = int(colptr0[end])
    cms = CholmodSparse{Tv,Ti}(csp, colptr0,
                               pointer_to_array(csp.ipt, (nnz,), true),
                               pointer_to_array(csp.xpt, (nnz,), true))
    c_free(cp)
    cms
end
CholmodSparse!{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodSparse{Tv,Ti}}) = CholmodSparse(cp)
CholmodSparse{Tv<:CHMVTypes}(D::CholmodDense{Tv}) = CholmodSparse(D,1) # default Ti is Int

function CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}(tp::Ptr{c_CholmodTriplet{Tv,Ti}})
    ctp = unsafe_load(tp)
    i = pointer_to_array(ctp.i, (ctp.nnz,), true)
    j = pointer_to_array(ctp.j, (ctp.nnz,), true)
    x = pointer_to_array(ctp.x, (ctp.x == C_NULL ? 0 : ctp.nnz), true)
    ct = CholmodTriplet{Tv,Ti}(ctp, i, j, x)
    c_free(tp)
    ct
end

function chm_rdsp(fnm::AbstractString)
    fd = ccall(:fopen, Ptr{Void}, (Ptr{UInt8},Ptr{UInt8}), fnm, "r")
    res = ccall((:cholmod_read_sparse,:libcholmod), Ptr{c_CholmodSparse{Float64,Cint}},
                (Ptr{Void},Ptr{UInt8}),fd,cmn(Cint))
    ccall(:fclose, Cint, (Ptr{Void},), fd) # should do this in try/finally/end
    CholmodSparse(res)
end

for Ti in CholmodIndexTypes
    @eval begin
        function (*){Tv<:CHMVRealTypes}(A::CholmodSparse{Tv,$Ti},
                                    B::CholmodSparse{Tv,$Ti})
            CholmodSparse(ccall((@chm_nm "ssmult" $Ti
                                 , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{c_CholmodSparse{Tv,$Ti}},
                                 Cint,Cint,Cint,Ptr{UInt8}), &A.c,&B.c,0,true,true,cmn($Ti)))
        end
        function A_mul_Bc{Tv<:CHMVRealTypes}(A::CholmodSparse{Tv,$Ti},
                                                      B::CholmodSparse{Tv,$Ti})
            cm = cmn($Ti)
            aa = Array(Ptr{c_CholmodSparse{Tv,$Ti}}, 2)
            if !is(A,B)
                aa[1] = ccall((@chm_nm "transpose" $Ti
                               ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                              (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                              &B.c, 1, cm)
                ## result of ssmult will have stype==0, contain numerical values and be sorted
                aa[2] = ccall((@chm_nm "ssmult" $Ti
                               ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                              (Ptr{c_CholmodSparse{Tv,$Ti}},
                               Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Cint,Cint,Ptr{UInt8}),
                              &A.c, aa[1], 0, true, true, cm)
                @isok ccall((@chm_nm "free_sparse" $Ti
                                ,:libcholmod), Cint,
                               (Ptr{Ptr{c_CholmodSparse{Tv,$Ti}}}, Ptr{UInt8}), aa, cm)
                return CholmodSparse(aa[2])
            end

            ## The A*A' case is handled by cholmod_aat. This routine requires
            ## A->stype == 0 (storage of upper and lower parts). If neccesary
            ## the matrix A is first converted to stype == 0
            if A.c.stype != 0
                aa[1] = ccall((@chm_nm "copy" $Ti
                               , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                              (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Cint, Ptr{UInt8}),
                              &A.c, 0, 1, cm)

                aa[2] = ccall((@chm_nm "aat" $Ti
                               , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                              (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{Void}, Int, Cint, Ptr{UInt8}),
                              aa[1], C_NULL, 0, 1, cm)

                @isok ccall((@chm_nm "free_sparse" $Ti
                             , :libcholmod), Cint,
                            (Ptr{Ptr{c_CholmodSparse{Tv,$Ti}}}, Ptr{UInt8}), aa, cm)

            else
                aa[2] = ccall((@chm_nm "aat" $Ti
                               , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                              (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{Void}, Int, Cint, Ptr{UInt8}),
                              &A.c, C_NULL, 0, 1, cm)
            end

            cs = CholmodSparse(aa[2])

            ## According to the docs conversion to symmetric matrix can be done
            ## by changing the stype of the result (stype == 1: upper part).
            cs.c.stype = 1

            return sort!(cs)
        end
        function Ac_mul_B{Tv<:CHMVRealTypes}(A::CholmodSparse{Tv,$Ti},
                                              B::CholmodSparse{Tv,$Ti})
            cm = cmn($Ti)
            aa = Array(Ptr{c_CholmodSparse{Tv,$Ti}}, 2)
            aa[1] = ccall((@chm_nm "transpose" $Ti
                           ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                          (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                          &A.c, 1, cm)
            if is(A,B)
                Ac = CholmodSparse(aa[1])
                return A_mul_Bc(Ac,Ac)
            end
            ## result of ssmult will have stype==0, contain numerical values and be sorted
            aa[2] = ccall((@chm_nm "ssmult" $Ti
                           ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                          (Ptr{c_CholmodSparse{Tv,$Ti}},
                           Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Cint,Cint,Ptr{UInt8}),
                          aa[1],&B.c,0,true,true,cm)
            @isok ccall((@chm_nm "free_sparse" $Ti
                            , :libcholmod), Cint,
                           (Ptr{Ptr{c_CholmodSparse{Tv,$Ti}}}, Ptr{UInt8}), aa, cm)
            CholmodSparse(aa[2])
        end
        function A_mul_Bt{Tv<:CHMVRealTypes}(A::CholmodSparse{Tv,$Ti},
                                              B::CholmodSparse{Tv,$Ti})
            A_mul_Bc(A,B) # in the unlikely event of writing A*B.' instead of A*B'
        end
        function At_mul_B{Tv<:CHMVRealTypes}(A::CholmodSparse{Tv,$Ti},
                                              B::CholmodSparse{Tv,$Ti})
            Ac_mul_B(A,B) # in the unlikely event of writing A.'*B instead of A'*B
        end
        function CholmodDense{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodDense(ccall((@chm_nm "sparse_to_dense" $Ti
                                ,:libcholmod), Ptr{c_CholmodDense{Tv}},
                               (Ptr{c_CholmodSparse{Tv,Ti}},Ptr{UInt8}),
                               &A.c,cmn($Ti)))
        end
        function CholmodSparse{Tv<:CHMVTypes}(D::CholmodDense{Tv},i::$Ti)
            CholmodSparse(ccall((@chm_nm "dense_to_sparse" $Ti
                                 ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodDense{Tv}},Cint,Ptr{UInt8}),
                                &D.c,true,cmn($Ti)))
        end
        function CholmodSparse{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti})
            if bool(L.c.is_ll)
                return CholmodSparse(ccall((@chm_nm "factor_to_sparse" $Ti
                                            ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                           (Ptr{c_CholmodFactor{Tv,$Ti}},Ptr{UInt8}),
                                           &L.c,cmn($Ti)))
            end
            cm = cmn($Ti)
            Lcll = ccall((@chm_nm "copy_factor" $Ti
                          ,:libcholmod), Ptr{c_CholmodFactor{Tv,$Ti}},
                         (Ptr{c_CholmodFactor{Tv,$Ti}},Ptr{UInt8}),
                         &L.c,cm)
            @isok ccall((@chm_nm "change_factor" $Ti
                            ,:libcholmod), Cint,
                           (Cint,Cint,Cint,Cint,Cint,Ptr{c_CholmodFactor{Tv,$Ti}},Ptr{UInt8}),
                           L.c.xtype,true,L.c.is_super,true,true,Lcll,cm)
            val = CholmodSparse(ccall((@chm_nm "factor_to_sparse" $Ti
                                       ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                      (Ptr{c_CholmodFactor{Tv,$Ti}},Ptr{UInt8}),
                                      Lcll,cmn($Ti)))
            @isok ccall((@chm_nm "free_factor" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{Ptr{c_CholmodFactor{Tv,$Ti}}},Ptr{UInt8}),
                           [Lcll],cm)
            val
        end
        function CholmodSparse{Tv<:CHMVTypes,Ti<:$Ti}(T::CholmodTriplet{Tv,Ti})
            CholmodSparse(ccall((@chm_nm "triplet_to_sparse" $Ti
                                 ,:libcholmod), Ptr{c_CholmodSparse{Tv,Ti}},
                               (Ptr{c_CholmodTriplet{Tv,Ti}},Ptr{UInt8}),
                               &T.c,cmn($Ti)))
        end
        function CholmodTriplet{Tv<:CHMVTypes,Ti<:$Ti}(A::CholmodSparse{Tv,Ti})
            CholmodTriplet(ccall((@chm_nm "sparse_to_triplet" $Ti
                                 ,:libcholmod), Ptr{c_CholmodTriplet{Tv,Ti}},
                               (Ptr{c_CholmodSparse{Tv,Ti}},Ptr{UInt8}),
                               &A.c,cmn($Ti)))
        end
        function isvalid{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti})
            bool(ccall((@chm_nm "check_factor" $Ti
                        ,:libcholmod), Cint,
                       (Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                       &L.c, cmn($Ti)))
        end
        function isvalid{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            bool(ccall((@chm_nm "check_sparse" $Ti
                        ,:libcholmod), Cint,
                       (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}),
                       &A.c, cmn($Ti)))
        end
        function isvalid{Tv<:CHMVTypes}(T::CholmodTriplet{Tv,$Ti})
            bool(ccall((@chm_nm "check_triplet" $Ti
                        ,:libcholmod), Cint,
                       (Ptr{c_CholmodTriplet{Tv,$Ti}}, Ptr{UInt8}),
                       &T.c, cmn($Ti)))
        end
        function cholfact{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti}, ll::Bool)
            cm = cmn($Ti)
            ## may need to change final_asis as well as final_ll
            if ll cm[chm_final_ll_inds] = reinterpret(UInt8, [one(Cint)]) end
            Lpt = ccall((@chm_nm "analyze" $Ti
                         ,:libcholmod), Ptr{c_CholmodFactor{Tv,$Ti}},
                        (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}), &A.c, cm)
            @isok ccall((@chm_nm "factorize" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}},
                            Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                           &A.c, Lpt, cm)
            CholmodFactor(Lpt)
        end
        function cholfact{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},beta::Tv,ll::Bool)
            cm = cmn($Ti)
            ## may need to change final_asis as well as final_ll
            if ll cm[chm_final_ll_inds] = reinterpret(UInt8, [one(Cint)]) end
            Lpt = ccall((@chm_nm "analyze" $Ti
                         ,:libcholmod), Ptr{c_CholmodFactor{Tv,$Ti}},
                        (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}), &A.c, cm)
            @isok ccall((@chm_nm "factorize_p" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{Tv}, Ptr{Cint}, Csize_t,
                            Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                           &A.c, &beta, C_NULL, zero(Csize_t), Lpt, cmn($Ti))
            CholmodFactor(Lpt)
        end
        function chm_analyze{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodFactor(ccall((@chm_nm "analyze" $Ti
                                 ,:libcholmod), Ptr{c_CholmodFactor{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}), &A.c, cmn($Ti)))
        end
        function cholfact!{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti},A::CholmodSparse{Tv,$Ti})
            @isok ccall((@chm_nm "factorize" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}},
                            Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                           &A.c, &L.c, cmn($Ti))
            L
        end
        function cholfact!{Tv<:Float64}(L::CholmodFactor{Tv,$Ti},A::CholmodSparse{Tv,$Ti},
                                        beta::Tv)
            @isok ccall((@chm_nm "factorize_p" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{Tv}, Ptr{Cint}, Csize_t,
                            Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                           &A.c, &beta, C_NULL, zero(Csize_t), &L.c, cmn($Ti))
            L
        end
        function chm_pack!{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti})
            @isok ccall((@chm_nm "pack_factor" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}),
                           &L.c,cmn($Ti))
            L
        end
        function chm_print{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti},lev,nm)
            @isok isvalid(L)

            cm = cmn($Ti)
            orig = cm[chm_prt_inds]
            cm[chm_prt_inds] = reinterpret(UInt8, [int32(lev)])
            @isok ccall((@chm_nm "print_factor" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodFactor{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                           &L.c, nm, cm)
            cm[chm_prt_inds] = orig
        end
        function chm_print{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},lev,nm)
            @isok isvalid(A)

            cm = cmn($Ti)
            orig = cm[chm_prt_inds]
            cm[chm_prt_inds] = reinterpret(UInt8, [int32(lev)])
            @isok ccall((@chm_nm "print_sparse" $Ti
                           ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                           &A.c, nm, cm)
            cm[chm_prt_inds] = orig
        end
        function chm_scale!{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},
                                           S::CholmodDense{Tv},
                                           typ::Integer)
            @isok ccall((@chm_nm "scale" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodDense{Tv}},Cint,Ptr{c_CholmodSparse{Tv,$Ti}},
                            Ptr{UInt8}), &S.c, typ, &A.c, cmn($Ti))
            A
        end
        function chm_sdmult{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},
                                           trans::Bool,
                                           alpha::Real,
                                           beta::Real,
                                           X::CholmodDense{Tv})
            m,n = size(A)
            nc = trans ? m : n
            nr = trans ? n : m
            if nc != size(X,1)
                throw(DimensionMismatch("Incompatible dimensions, $nc and $(size(X,1)), in chm_sdmult"))
            end
            aa = float64([alpha, 0.])
            bb = float64([beta, 0.])
            Y = CholmodDense(zeros(Tv,nr,size(X,2)))
            @isok ccall((@chm_nm "sdmult" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Ptr{Cdouble},Ptr{Cdouble},
                            Ptr{c_CholmodDense{Tv}}, Ptr{c_CholmodDense{Tv}}, Ptr{UInt8}),
                           &A.c,trans,aa,bb,&X.c,&Y.c,cmn($Ti))
            Y
        end
        function chm_speye{Tv<:CHMVTypes,Ti<:$Ti}(m::Ti, n::Ti, x::Tv)
            CholmodSparse(ccall((@chm_nm "speye" $Ti
                                 , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Int, Int, Cint, Ptr{UInt8}),
                                m, n, xtyp(Tv), cmn($Ti)))
        end
        function chm_spzeros{Tv<:Union(Float64,Complex128)}(m::$Ti, n::$Ti, nzmax::$Ti, x::Tv)
            CholmodSparse(ccall((@chm_nm "spzeros" $Ti
                         , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Int, Int, Int, Cint, Ptr{UInt8}),
                                m, n, nzmax, xtyp(Tv), cmn($Ti)))
        end
## add chm_xtype and chm_pack
        function copy{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti})
            CholmodFactor(ccall((@chm_nm "copy_factor" $Ti
                                 ,:libcholmod), Ptr{c_CholmodFactor{Tv,$Ti}},
                                (Ptr{c_CholmodFactor{Tv,$Ti}},Ptr{UInt8}), &L.c, cmn($Ti)))
        end
        function copy{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodSparse(ccall((@chm_nm "copy_sparse" $Ti
                                 ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{UInt8}), &A.c, cmn($Ti)))
        end
        function copy{Tv<:CHMVTypes}(T::CholmodTriplet{Tv,$Ti})
            CholmodTriplet(ccall((@chm_nm "copy_triplet" $Ti
                                 ,:libcholmod), Ptr{c_CholmodTriplet{Tv,$Ti}},
                                (Ptr{c_CholmodTriplet{Tv,$Ti}},Ptr{UInt8}), &T.c, cmn($Ti)))
        end
        function ctranspose{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodSparse(ccall((@chm_nm "transpose" $Ti
                                 ,:libcholmod),Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                                &A.c, 2, cmn($Ti)))
        end
        function etree{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            tr = Array($Ti,size(A,2))
            @isok ccall((@chm_nm "etree" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{$Ti},Ptr{UInt8}),
                           &A.c,tr,cmn($Ti))
            tr
        end
        function hcat{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},B::CholmodSparse{Tv,$Ti})
            ccall((@chm_nm "horzcat" $Ti
                   , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                  (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Ptr{UInt8}),
                  &A.c,&B.c,true,cmn($Ti))
        end
        function nnz{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            ccall((@chm_nm "nnz" $Ti
                   ,:libcholmod), Int, (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{UInt8}),&A.c,cmn($Ti))
        end
        function norm{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},p::Real)
            ccall((@chm_nm "norm_sparse" $Ti
                   , :libcholmod), Float64,
                  (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                  &A.c,p == 1 ? 1 :(p == Inf ? 1 : throw(ArgumentError("p must be 1 or Inf"))),cmn($Ti))
        end
        function solve{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti},
                                      B::CholmodDense{Tv}, typ::Integer)
            size(L,1) == size(B,1) || throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(L,1)) rows, but RHS has $(size(B,1)) rows."))
            CholmodDense(ccall((@chm_nm "solve" $Ti
                                ,:libcholmod), Ptr{c_CholmodDense{Tv}},
                               (Cint, Ptr{c_CholmodFactor{Tv,$Ti}},
                                Ptr{c_CholmodDense{Tv}}, Ptr{UInt8}),
                               typ, &L.c, &B.c, cmn($Ti)))
        end
        function solve{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$Ti},
                                      B::CholmodSparse{Tv,$Ti},
                                      typ::Integer)
            size(L,1) == size(B,1) || throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(L,1)) rows, but RHS has $(size(B,1)) rows."))
            CholmodSparse(ccall((@chm_nm "spsolve" $Ti
                                 ,:libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Cint, Ptr{c_CholmodFactor{Tv,$Ti}},
                                 Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}),
                                typ, &L.c, &B.c, cmn($Ti)))
        end
        function sort!{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            @isok ccall((@chm_nm "sort" $Ti
                            ,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$Ti}}, Ptr{UInt8}),
                           &A.c, cmn($Ti))
            A
        end
        function copysym{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodSparse(ccall((@chm_nm "copy" $Ti
                                 ,:libcholmod),Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Cint,Ptr{UInt8}),
                                &A.c,0,1,cmn($Ti)))
        end
        function transpose{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti})
            CholmodSparse(ccall((@chm_nm "transpose" $Ti
                                 ,:libcholmod),Ptr{c_CholmodSparse{Tv,$Ti}},
                                (Ptr{c_CholmodSparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                                &A.c, 1, cmn($Ti)))
        end
        function vcat{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$Ti},B::CholmodSparse{Tv,$Ti})
            ccall((@chm_nm "vertcat" $Ti
                   , :libcholmod), Ptr{c_CholmodSparse{Tv,$Ti}},
                  (Ptr{c_CholmodSparse{Tv,$Ti}},Ptr{c_CholmodSparse{Tv,$Ti}},Cint,Ptr{UInt8}),
                  &A.c,&B.c,true,cmn($Ti))
        end
    end
end

(*){Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::CholmodDense{Tv}) = chm_sdmult(A,false,1.,0.,B)
(*){Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::VecOrMat{Tv}) = chm_sdmult(A,false,1.,0.,CholmodDense(B))

function Ac_mul_B{Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::CholmodDense{Tv})
    chm_sdmult(A,true,1.,0.,B)
end
function Ac_mul_B{Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::VecOrMat{Tv})
    chm_sdmult(A,true,1.,0.,CholmodDense(B))
end


A_ldiv_B!(L::CholmodFactor, B) = L\B # Revisit this to see if allocation can be avoided. It should be possible at least for the right hand side.
(\){T<:CHMVTypes}(L::CholmodFactor{T}, B::CholmodDense{T}) = solve(L, B, CHOLMOD_A)
(\){T<:CHMVTypes}(L::CholmodFactor{T}, b::Vector{T}) = reshape(solve(L, CholmodDense!(b), CHOLMOD_A).mat, length(b))
(\){T<:CHMVTypes}(L::CholmodFactor{T}, B::Matrix{T}) = solve(L, CholmodDense!(B),CHOLMOD_A).mat
function (\){Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::CholmodSparse{Tv,Ti})
    solve(L,B,CHOLMOD_A)
end
function (\){Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::SparseMatrixCSC{Tv,Ti})
    sparse!(solve(L,CholmodSparse(B),CHOLMOD_A))
end

Ac_ldiv_B{T<:CHMVTypes}(L::CholmodFactor{T},B::CholmodDense{T}) = solve(L,B,CHOLMOD_A)
Ac_ldiv_B{T<:CHMVTypes}(L::CholmodFactor{T},B::VecOrMat{T}) = solve(L,CholmodDense!(B),CHOLMOD_A).mat
function Ac_ldiv_B{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::CholmodSparse{Tv,Ti})
    solve(L,B,CHOLMOD_A)
end
function Ac_ldiv_B{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::SparseMatrixCSC{Tv,Ti})
    sparse!(solve(L,CholmodSparse(B),CHOLMOD_A))
end


cholfact{T<:CHMVTypes}(A::CholmodSparse{T},beta::T) = cholfact(A,beta,false)
cholfact(A::CholmodSparse) = cholfact(A,false)
cholfact(A::SparseMatrixCSC,ll::Bool) = cholfact(CholmodSparse(A),ll)
cholfact(A::SparseMatrixCSC) = cholfact(CholmodSparse(A),false)
function cholfact!{T<:CHMVTypes}(L::CholmodFactor{T},A::CholmodSparse{T},beta::Number)
    cholfact!(L,A,convert(T,beta))
end
function cholfact!{T<:CHMVTypes}(L::CholmodFactor{T},A::SparseMatrixCSC{T},beta::Number)
    cholfact!(L,CholmodSparse(A),convert(T,beta))
end
cholfact!{T<:CHMVTypes}(L::CholmodFactor{T},A::SparseMatrixCSC{T}) = cholfact!(L,CholmodSparse(A))

chm_analyze(A::SparseMatrixCSC) = chm_analyze(CholmodSparse(A))

chm_print(A::CholmodSparse, lev::Integer) = chm_print(A, lev, "")
chm_print(A::CholmodFactor, lev::Integer) = chm_print(L, lev, "")

function chm_scale!{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T},typ::Integer)
    chm_scale!(A,CholmodDense(b),typ)
    A
end
chm_scale{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T},typ::Integer) = chm_scale!(copy(A),b,typ)

chm_speye(m::Integer, n::Integer) = chm_speye(m, n, 1., 1) # default element type is Float32
chm_speye(n::Integer) = chm_speye(n, n, 1.)             # default shape is square

chm_spzeros(m::Integer,n::Integer,nzmax::Integer) = chm_spzeros(m,n,nzmax,1.)

function scale!{T<:CHMVTypes}(b::Vector{T}, A::CholmodSparse{T})
    chm_scale!(A,CholmodDense(b),CHOLMOD_ROW)
    A
end
scale{T<:CHMVTypes}(b::Vector{T}, A::CholmodSparse{T}) = scale!(b,copy(A))
function scale!{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T})
    chm_scale!(A,CholmodDense(b),CHOLMOD_COL)
    A
end
scale{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T}) = scale!(copy(A), b)

norm(A::CholmodSparse) = norm(A,1)

show(io::IO,L::CholmodFactor) = chm_print(L,int32(4),"")
show(io::IO,A::CholmodSparse) = chm_print(A,int32(4),"")

size(B::CholmodDense) = size(B.mat)
size(B::CholmodDense,d) = size(B.mat,d)
size(A::CholmodSparse) = (int(A.c.m), int(A.c.n))
function size(A::CholmodSparse, d::Integer)
    d == 1 ? A.c.m : (d == 2 ? A.c.n : 1)
end
size(L::CholmodFactor) = (n = int(L.c.n); (n,n))
size(L::CholmodFactor,d::Integer) = d < 1 ? throw(BoundsError("Dimension $d out of range")) : (d <= 2 ? int(L.c.n) : 1)

function solve{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},
                                            B::SparseMatrixCSC{Tv,Ti},typ::Integer)
    sparse!(solve(L,CholmodSparse(B),typ))
end
function solve{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::SparseMatrixCSC{Tv,Ti})
    sparse!(solve(L,CholmodSparse(B),CHOLMOD_A))
end
function solve{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::CholmodSparse{Tv,Ti})
    solve(L,B,CHOLMOD_A)
end
function (\){Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::CholmodSparse{Tv,Ti})
    solve(L,B,CHOLMOD_A)
end
function solve{T<:CHMVTypes}(L::CholmodFactor{T},B::Vector{T},typ::Integer)
    solve(L,CholmodDense!(B),typ).mat[:]
end
function solve{T<:CHMVTypes}(L::CholmodFactor{T},B::Matrix{T},typ::Integer)
    solve(L,CholmodDense!(B),typ).mat
end
solve{T<:CHMVTypes}(L::CholmodFactor{T},B::CholmodDense{T}) = solve(L,B,CHOLMOD_A)

function findnz{Tv,Ti}(A::CholmodSparse{Tv,Ti})
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

findnz(L::CholmodFactor) = findnz(CholmodSparse(L))

function diag{Tv}(A::CholmodSparse{Tv})
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

function diag{Tv}(L::CholmodFactor{Tv})
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

function logdet{Tv,Ti}(L::CholmodFactor{Tv,Ti},sub)
    res = zero(Tv)
    for d in diag(L)[sub] res += log(abs(d)) end
    bool(L.c.is_ll) ? 2res : res
end
logdet(L::CholmodFactor) = logdet(L, 1:L.c.n)
det(L::CholmodFactor) = exp(logdet(L))

full(A::CholmodSparse) = CholmodDense(A).mat
full(A::CholmodDense) = A.mat
function sparse(A::CholmodSparse)
    if bool(A.c.stype) return sparse!(copysym(A)) end
    SparseMatrixCSC(A.c.m, A.c.n, increment(A.colptr0), increment(A.rowval0), copy(A.nzval))
end
function sparse!(A::CholmodSparse)
    SparseMatrixCSC(A.c.m, A.c.n, increment!(A.colptr0), increment!(A.rowval0), A.nzval)
end
sparse(L::CholmodFactor) = sparse!(CholmodSparse(L))
sparse(D::CholmodDense) = sparse!(CholmodSparse(D))
sparse(T::CholmodTriplet) = sparse!(CholmodSparse(T))

isposdef!{Tv<:CHMVTypes,Ti}(A::SparseMatrixCSC{Tv,Ti}) = ishermitian(A) && cholfact(A).c.minor == size(A,1)

end #module

# placing factorize here for now. Maybe add a new file
function factorize(A::SparseMatrixCSC)
    m, n = size(A)
    if m == n
        Ac = cholfact(A)
        Ac.c.minor == m && ishermitian(A) && return Ac
    end
    return lufact(A)
end
