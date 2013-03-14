module CHOLMOD

using Base.LinAlg.UMFPACK               # for decrement, increment, etc.

export                                  # types
 CholmodDense,
 CholmodFactor,
 CholmodSparse,
 CholmodTriplet,

 CholmodSparse!                         # destructive constructor

import Base.(*)
import Base.(\)
import Base.Ac_ldiv_B
import Base.At_ldiv_B
import Base.Ac_mul_B
import Base.convert
import Base.copy
import Base.eltype
import Base.findn_nzs
import Base.getindex             
import Base.nnz
import Base.show
import Base.size
import Base.sort!
             
import LinAlg.Factorization
import LinAlg.cholfact
import LinAlg.cholfact!
import LinAlg.copy
import LinAlg.diagmm
import LinAlg.diagmm!
import LinAlg.logdet
import LinAlg.norm
import LinAlg.solve
 
const chm_com_sz = ccall((:jl_cholmod_common_size,:libsuitesparse_wrapper),Int,())
const chm_com    = ones(Uint8, chm_com_sz)

typealias CHMITypes Union(Int32,Int64)
typealias CHMVTypes Union(Complex64, Complex128, Float32, Float64)

type CholmodException <: Exception end

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

include("linalg/suitesparse_h.jl")
             
### These offsets should be reconfigured to be less error-prone in matches
const chm_com_offsets = Array(Int, length(ChmCommon.types))
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Uint8},), chm_com_offsets)
const chm_final_ll_inds = (1:4) + chm_com_offsets[7]
const chm_prt_inds = (1:4) + chm_com_offsets[13]
const chm_ityp_inds = (1:4) + chm_com_offsets[18]

### there must be an easier way but at least this works.
function ChmCommon(aa::Array{Uint8,1})
    typs = ChmCommon.types
    sz = map(sizeof, typs)
    args = map(i->reinterpret(typs[i], aa[chm_com_offsets[i] + (1:sz[i])])[1], 1:length(sz))
    eval(Expr(:call, unshift!(args, :ChmCommon), Any))
end

function set_chm_prt_lev(cm::Array{Uint8}, lev::Integer) # can probably be removed
    cm[(1:4) + chm_com_offsets[13]] = reinterpret(Uint8, [int32(lev)])
end

function cmn(::Type{Int32})
    ccall((:cholmod_start, :libcholmod), Cint, (Ptr{Uint8},), chm_com)
    chm_com
end
function cmn(::Type{Int64})             
    ccall((:cholmod_l_start, :libcholmod), Cint, (Ptr{Uint8},), chm_com)
    chm_com
end

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
    px::Ptr{Tv}
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
    px::Vector{Tv}
    s::Vector{Ti}
end

type c_CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    m::Int
    n::Int
    nzmax::Int
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
    stype:Cint
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

function CholmodDense{T<:CHMVTypes}(aa::VecOrMat{T})
    m = size(aa,1); n = size(aa,2)
    CholmodDense(c_CholmodDense{T}(m, n, m*n, stride(aa,2), convert(Ptr{T}, aa),
                                   C_NULL, xtyp(T), dtyp(T)),
                 length(size(aa)) == 2 ? aa : reshape(aa, (m,n)))
end

function CholmodDense{T<:CHMVTypes}(c::Ptr{c_CholmodDense{T}})
    cp = unsafe_ref(c)
    if cp.lda != cp.m || cp.nzmax != cp.m * cp.n
        error("overallocated cholmod_dense returned object of size $(cp.m) by $(cp.n) with leading dim $(cp.lda) and nzmax $(cp.nzmax)")
    end
    ## the true in the call to pointer_to_array means Julia will free the memory
    val = CholmodDense(cp, pointer_to_array(cp.xpt, (cp.m,cp.n), true))
    c_free(c)
    val
end
show(io::IO, cd::CholmodDense) = show(io, cd.mat)

function chm_check{T<:CHMVTypes}(cd::CholmodDense{T})
    status = ccall((:cholmod_check_dense, :libcholmod), Cint,
                   (Ptr{c_CholmodDense{T}}, Ptr{Uint8}), &cd.c, chm_com)
    if status != CHOLMOD_TRUE throw(CholmodException) end
end

function chm_ones{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_ones, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_ones(m::Integer, n::Integer) = chm_ones(m, n, 1.)

function chm_zeros{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_zeros, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_zeros(m::Integer, n::Integer) = chm_zeros(m, n, 1.)

function chm_eye{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_eye, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Cint, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_eye(m::Integer, n::Integer) = chm_eye(m, n, 1.)
chm_eye(n::Integer) = chm_eye(n, n, 1.)

function chm_print{T<:CHMVTypes}(cd::CholmodDense{T}, lev::Integer, nm::ASCIIString)
    orig = chm_com[chm_prt_inds]
    chm_com[chm_prt_inds] = reinterpret(Uint8, [int32(lev)])
    status = ccall((:cholmod_print_dense, :libcholmod), Cint,
                   (Ptr{c_CholmodDense{T}}, Ptr{Uint8}, Ptr{Uint8}),
                   &cd.c, nm, chm_com)
    chm_com[chm_prt_inds] = orig
    if status != CHOLMOD_TRUE throw(CholmodException) end
end
chm_print(cd::CholmodDense, lev::Integer) = chm_print(cd, lev, "")
chm_print(cd::CholmodDense) = chm_print(cd, int32(4), "")

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Integer)
    zerobased = A.colptr[1] == 0
    colptr0 = zerobased ? copy(A.colptr) : decrement(A.colptr)
    rowval0 = zerobased ? copy(A.rowptr) : decrement(A.rowval)
    nzval = copy(A.nzval)
    CholmodSparse{Tv,Ti}(c_CholmodSparse{Tv,Ti}(size(A,1),size(A,2),
                                                int(colptr0[end]),
                                                convert(Ptr{Ti}, colptr0),
                                                convert(Ptr{Ti}, rowval0), C_NULL,
                                                convert(Ptr{Tv}, nzval), C_NULL,
                                                int32(stype), ityp(Ti),
                                                xtyp(Tv), dtyp(Tv),
### Assuming that a SparseMatrixCSC always has sorted row indices. Need to check.
                                                CHOLMOD_TRUE, CHOLMOD_TRUE),
                         colptr0, rowval0, nzval)
end
function CholmodSparse(A::SparseMatrixCSC)
    stype = ishermitian(A) ? 1 : 0
    CholmodSparse(stype > 0 ? triu(A) : A, stype)
end
## this should probably be the base call for SparseMatrixCSC too
function CholmodSparse!{Tv<:CHMVTypes,Ti<:CHMITypes}(colpt::Vector{Ti},
                                                     rowval::Vector{Ti},
                                                     nzval::Vector{Tv},
                                                     m::Integer,
                                                     n::Integer,
                                                     stype::Signed)
    bb = colpt[1]
    if bb != 0 && bb != 1 error("colpt[1] is $bb, must be 0 or 1") end
    if any(diff(colpt) .< 0) error("elements of colpt must be non-decreasing") end
    if length(colpt) != n + 1 error("length(colptr) = $(length(colpt)), should be $(n+1)") end
    if bool(bb)                         # one-based 
        decrement!(colpt)
        decrement!(rowval)
    end
    nz = colpt[end]
    if length(rowval) != nz || length(nzval) != nz
        error("length(rowval) = $(length(rowval)) and length(nzval) = $(length(nzval)) should be $nz")
    end
    if any(rowval .< 0) || any(rowval .>= m)
        error("all elements of rowval must be in the range [0,$(m-1)]")
    end
    sort!(CholmodSparse(c_CholmodSparse{Tv,Ti}(m,n,int(nz),convert(Ptr{Ti},colpt),
                                               convert(Ptr{Ti},rowval), C_NULL,
                                               convert(Ptr{Tv},nzval), C_NULL,
                                               int32(stype), ityp(Ti), xtyp(Tv), dtyp(Tv),
                                               CHOLMOD_FALSE,CHOLMOD_TRUE),colpt,rowval,nzval))
end
function chm_rdsp(fnm::String)
    fd = ccall(:fopen, Ptr{Void}, (Ptr{Uint8},Ptr{Uint8}), fnm, "r")
    res = ccall((:cholmod_read_sparse,:libcholmod), Ptr{c_CholmodSparse{Float64,Cint}},
                (Ptr{Void},Ptr{Uint8}),fd,cmn(Cint))
    ccall(:fclose, Cint, (Ptr{Void},), fd)
    CholmodSparse(res)
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodSparse{Tv,Ti}})
    csp = unsafe_ref(cp)
    colptr0 = pointer_to_array(csp.ppt, (csp.n + 1,), true)
    nnz = int(colptr0[end])
    cms = CholmodSparse{Tv,Ti}(csp, colptr0,
                               pointer_to_array(csp.ipt, (nnz,), true),
                               pointer_to_array(csp.xpt, (nnz,), true))
    c_free(cp)
    cms
end

for (chk,faprt,spprt,srt,itype) in
    (("cholmod_check_sparse","cholmod_print_factor","cholmod_print_sparse","cholmod_sort",:Int32),
     ("cholmod_l_check_sparse","cholmod_l_print_factor","cholmod_l_print_sparse",
      "cholmod_l_sort",:Int64))
    @eval begin
        function chm_check{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$itype})
            bool(ccall(($chk,:libcholmod), Cint,
                       (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                       &A.c, cmn($itype)))
        end
        function chm_print{Tv<:CHMVTypes}(L::CholmodFactor{Tv,$itype},lev,nm)
            cmn($itype)
            orig = chm_com[chm_prt_inds]
            chm_com[chm_prt_inds] = reinterpret(Uint8, [int32(lev)])
            status = ccall(($faprt,:libcholmod), Cint,
                           (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}, Ptr{Uint8}),
                           &L.c, nm, chm_com)
            chm_com[chm_prt_inds] = orig
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function chm_print{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$itype},lev,nm)
            cmn($itype)
            orig = chm_com[chm_prt_inds]
            chm_com[chm_prt_inds] = reinterpret(Uint8, [int32(lev)])
            status = ccall(($spprt,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}, Ptr{Uint8}),
                           &A.c, nm, chm_com)
            chm_com[chm_prt_inds] = orig
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function sort!{Tv<:CHMVTypes}(A::CholmodSparse{Tv,$itype})
            status = ccall(($srt,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                           &A.c, cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
            A
        end
    end
end
chm_print(A::CholmodSparse, lev::Integer) = chm_print(A, lev, "")
chm_print(A::CholmodFactor, lev::Integer) = chm_print(L, lev, "")
show(io::IO,L::CholmodFactor) = chm_print(L,int32(4),"")
show(io::IO,A::CholmodSparse) = chm_print(A,int32(4),"")

nnz{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}) = int(cp.colptr0[end])
size{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}) = (int(cp.c.m), int(cp.c.n))
function size{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}, d::Integer)
    d == 1 ? cp.c.m : (d == 2 ? cp.c.n : 1)
end

for (aat,allocsp,cop,copsp,freesp,normsp,scl,sdmult,speye,ssmult,transsym,itype) in
    (("cholmod_aat","cholmod_allocate_sparse","cholmod_copy","cholmod_copy_sparse",
      "cholmod_free_sparse","cholmod_norm_sparse","cholmod_scale", "cholmod_sdmult",
      "cholmod_speye", "cholmod_ssmult","cholmod_transpose_sym",:Int32),
     ("cholmod_l_aat","cholmod_l_allocate_sparse","cholmod_l_copy","cholmod_l_copy_sparse",
      "cholmod_l_free_sparse","cholmod_l_norm_sparse","cholmod_l_scale",
      "cholmod_l_sdmult","cholmod_l_speye","cholmod_l_ssmult","cholmod_l_transpose_sym",:Int64))
    @eval begin
        function chm_aat{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            cm = cmn($itype)
            ## strangely the matrix returned by $aat is not marked as symmetric
            ## all of the code past the call to $aat is to create the symmetric-storage
            ## version of the result then transpose it to provide sorted columns
            aa = Array(Ptr{c_CholmodSparse{Tv,$itype}}, 2)
            aa[1] = ccall(($aat, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                          (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Void}, Int, Cint, Ptr{Uint8}),
                          &a, C_NULL, 0, 1, cm)
            ## Create the lower triangle unsorted
            aa[2] = ccall(($cop, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                          (Ptr{c_CholmodSparse{Tv,$itype}}, Cint, Cint, Ptr{Uint8}),
                          aa[1], -1, 1, cm)
            status = ccall(($freesp, :libcholmod), Cint,
                           (Ptr{Ptr{c_CholmodSparse{Tv,$itype}}}, Ptr{Uint8}), aa, cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            aa[1] = aa[2]
            r = unsafe_ref(aa[1])
            ## Now transpose the lower triangle to the upper triangle to do the sorting
            rpt = ccall(($allocsp,:libcholmod),Ptr{c_CholmodSparse{Tv,$itype}},
                        (Csize_t,Csize_t,Csize_t,Cint,Cint,Cint,Cint,Ptr{Cuchar}),
                        r.m,r.n,r.nzmax,r.sorted,r.packed,-r.stype,r.xtype,cm)
            status = ccall(($transsym,:libcholmod),Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Cint, Ptr{$itype},
                            Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                           aa[1],1,C_NULL,rpt,cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            status = ccall(($freesp, :libcholmod), Cint,
                           (Ptr{Ptr{c_CholmodSparse{Tv,$itype}}}, Ptr{Uint8}), aa, cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            CholmodSparse(rpt)
        end
        function chm_copy_sp{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            ccall(($copsp,:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                  (Ptr{c_CholmodSparse{Tv,$itype}},Ptr{Uint8}), &a, cmn($itype))
        end
        function norm{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},p::Number)
            ccall(($normsp, :libcholmod), Float64, 
                  (Ptr{c_CholmodSparse{Tv,$itype}}, Cint, Ptr{Uint8}),
                  &a,p == 1 ? 1 :(p == Inf ? 1 : error("p must be 1 or Inf")),cmn($itype))
        end
        function chm_sdmult{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},
                                           trans::Bool,
                                           alpha::Real,
                                           beta::Real,
                                           x::c_CholmodDense{Tv})
            nc = trans ? a.m : a.n
            nr = trans ? a.n : a.m
            if nc != x.m
                error("Incompatible dimensions, $nc and $(x.m), in chm_sdmult")
            end
            aa = float64([alpha, 0.])
            bb = float64([beta, 0.])
            Y = CholmodDense(zeros(Tv,nr,x.n))
            status = ccall(($sdmult,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}},Cint,Ptr{Cdouble},Ptr{Cdouble},
                            Ptr{c_CholmodDense{Tv}}, Ptr{c_CholmodDense{Tv}}, Ptr{Uint8}),
                           &a,trans,aa,bb,&x,[Y.c],cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
            Y
        end
        function chm_speye{Tv<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::Tv, i::$itype)
            CholmodSparse(ccall(($speye, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                                (Int, Int, Cint, Ptr{Uint8}),
                                m, n,
                                Tv<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                                cmn($itype)))
        end
        function (*){Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},
                                    b::c_CholmodSparse{Tv,$itype})
            CholmodSparse(ccall(($ssmult, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                                (Ptr{c_CholmodSparse{Tv,$itype}},Ptr{c_CholmodSparse{Tv,$itype}},
                                 Cint,Cint,Cint,Ptr{Uint8}), &a,&b,0,true,true,cmn($itype)))
        end
        function chm_scale!{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},
                                           s::c_CholmodDense{Tv},
                                           typ::Integer)
            status = ccall(($scl,:libcholmod), Cint,
                           (Ptr{c_CholmodDense{Tv}},Cint,Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{Uint8}), &s, typ, &a, cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
    end
end
(*){Tv<:CHMVTypes}(a::c_CholmodSparse{Tv},b::c_CholmodDense{Tv}) = chm_sdmult(a,false,1.,0.,b)
(*){Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::CholmodDense{Tv}) = chm_sdmult(A.c,false,1.,0.,B.c)
Ac_mul_B{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv},b::c_CholmodDense{Tv}) = chm_sdmult(a,true,1.,0.,b)
Ac_mul_B{Tv<:CHMVTypes}(A::CholmodSparse{Tv},B::CholmodDense{Tv}) = chm_sdmult(A.c,true,1.,0.,B.c)
(*){Tv<:CHMVTypes,Ti<:CHMITypes}(A::CholmodSparse{Tv,Ti},B::CholmodSparse{Tv,Ti}) = A.c * B.c
chm_speye(m::Integer, n::Integer) = chm_speye(m, n, 1., 1)
chm_speye(n::Integer) = chm_speye(n, n, 1., 1)
chm_aat(A::CholmodSparse) = chm_aat(A.c)
chm_aat(A::SparseMatrixCSC) = chm_aat(CholmodSparse(A).c)
norm(A::CholmodSparse,p::Number) = norm(A.c,p)
norm(A::CholmodSparse) = norm(A.c,1)
copy(A::CholmodSparse) = CholmodSparse(chm_copy_sp(A.c))
function chm_scale!{T<:CHMVTypes}(A::CholmodSparse{T},S::CholmodDense{T},typ::Integer)
    chm_scale!(A.c,S.c,typ)
end
function diagmm{T<:CHMVTypes}(b::Vector{T}, A::CholmodSparse{T})
    Acp = copy(A)
    chm_scale!(Acp,CholmodDense(b),CHOLMOD_ROW)
    Acp
end
function diagmm!{T<:CHMVTypes}(b::Vector{T}, A::CholmodSparse{T})
    chm_scale!(A,CholmodDense(b),CHOLMOD_ROW)
    A
end
function diagmm{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T})
    Acp = copy(A)
    chm_scale!(Acp,CholmodDense(b),CHOLMOD_COL)
    Acp
end
function diagmm!{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T})
    chm_scale!(A,CholmodDense(b),CHOLMOD_COL)
    A
end

function CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodFactor{Tv,Ti}})
    cfp = unsafe_ref(cp)
    Perm = pointer_to_array(cfp.Perm, (cfp.n,), true)
    ColCount = pointer_to_array(cfp.ColCount, (cfp.n,), true)
    p = pointer_to_array(cfp.p, (cfp.p == C_NULL ? 0 : cfp.n + 1,), true)
    i = pointer_to_array(cfp.i, (cfp.i == C_NULL ? 0 : cfp.nzmax,), true)
    x = pointer_to_array(cfp.x, (cfp.x == C_NULL ? 0 : cfp.nzmax,), true)
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

for (anl,chng,fac,slv,spslv,itype) in
    (("cholmod_analyze","cholmod_change_factor","cholmod_factorize",
      "cholmod_solve","cholmod_spsolve",:Int32),
     ("cholmod_l_analyze","cholmod_l_change_factor","cholmod_l_factorize",
      "cholmod_l_solve","cholmod_l_spsolve",:Int64))
    @eval begin
        function chm_analyze{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            ccall(($anl,:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                  (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &a, cmn($itype))
        end
        # update the factorization
        function chm_factorize!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                               a::c_CholmodSparse{Tv,$itype})
            status = ccall(($fac,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &a, &l, cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        # initialize a factorization
        function cholfact{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype}, ll::Bool)
            cmn($itype)
## may need to change final_asis as well as final_ll
            if ll chm_com[chm_final_ll_inds] = reinterpret(Uint8, [one(Cint)]) end
            Lpt = ccall(($anl,:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                        (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &a, chm_com)
            status = ccall(($fac,:libcholmod), Cint,
                           (Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &a, Lpt, chm_com)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            CholmodFactor(Lpt)
        end
        function solve{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                      b::c_CholmodDense{Tv}, typ::Integer)
            ccall(($slv,:libcholmod), Ptr{c_CholmodDense{Tv}},
                  (Cint, Ptr{c_CholmodFactor{Tv,$itype}},
                   Ptr{c_CholmodDense{Tv}}, Ptr{Uint8}),
                  typ, &l, &b, cmn($itype))
        end
        function solve{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                      b::c_CholmodSparse{Tv,$itype},
                                      typ::Integer)
            ccall(($spslv,:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                  (Cint, Ptr{c_CholmodFactor{Tv,$itype}},
                   Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                  typ, &l, &b, cmn($itype))
        end
    end
end
chm_analyze(ap::Ptr{c_CholmodSparse}) = chm_analyze(unsafe_ref(ap))
chm_analyze(A::CholmodSparse) = chm_analyze(A.c)
chm_analyze(A::SparseMatrixCSC) = chm_analyze(CholmodSparse(A).c)

cholfact(a::c_CholmodSparse) = cholfact(a,false) # LDL by default
cholfact(A::CholmodSparse,ll::Bool) = cholfact(A.c,ll)
cholfact(A::CholmodSparse) = cholfact(A.c,false) 
cholfact(A::SparseMatrixCSC,ll::Bool) = cholfact(CholmodSparse(A).c,ll)
cholfact(A::SparseMatrixCSC) = cholfact(CholmodSparse(A).c,false)
#cholfact!(A::SparseMatrixCSC,ll::Bool) = cholfact(CholmodSparse!(A).c,ll)
#cholfact!(A::SparseMatrixCSC) = cholfact(CholmodSparse!(A).c,false)
    
solve{T<:CHMVTypes}(l::c_CholmodFactor{T},b::c_CholmodDense{T}) = solve(l,b,CHOLMOD_A)
solve{T<:CHMVTypes}(L::CholmodFactor{T},B::CholmodDense{T}) = solve(L.c,B.c,CHOLMOD_A)
(\){T<:CHMVTypes}(L::CholmodFactor{T},B::CholmodDense{T}) = solve(L.c,B.c,CHOLMOD_A)
solve{Tv<:CHMVTypes,Ti<:CHMITypes}(l::c_CholmodFactor{Tv,Ti},b::c_CholmodSparse{Tv,Ti})=
    solve(l,b,CHOLMOD_A)
solve{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},B::CholmodSparse{Tv,Ti})=solve(L.c,B.c,CHOLMOD_A)
solve{T<:CHMVTypes}(l::c_CholmodFactor{T},b::VecOrMat{T},typ::Integer)=solve(l,CholmodDense(b),typ)    
solve{T<:CHMVTypes}(l::c_CholmodFactor{T},b::VecOrMat{T})=solve(l,CholmodDense(b),CHOLMOD_A)
solve{T<:CHMVTypes}(L::CholmodFactor{T},b::VecOrMat{T},typ::Integer)=solve(L.c,CholmodDense(b),typ)    
solve{T<:CHMVTypes}(L::CholmodFactor{T},b::VecOrMat{T})=solve(L.c,CholmodDense(b),CHOLMOD_A)
    
for (chng,pack,cop,chg_xtyp,f2s,itype) in
    (("cholmod_change_factor","cholmod_pack_factor",
      "cholmod_copy_factor","cholmod_factor_xtype",
      "cholmod_factor_to_sparse",:Int32),
     ("cholmod_l_change_factor","cholmod_l_pack_factor",
      "cholmod_l_copy_factor","cholmod_l_factor_xtype",
      "cholmod_l_factor_to_sparse",:Int64))
    @eval begin
        ## changing the factor is problematic because it reallocates the storage
        ## for the arrays and frees the old arrays but Julia retains the old pointers
        ## in the vectors (May get around this by passing an array of length 1 and not &l?)
        ## function chm_chng_fac!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
        ##                                       xt,ll,super,packed,monotonic)
        ##     status = ccall(($chng,:libcholmod), Cint,
        ##                    (Cint,Cint,Cint,Cint,Cint,
        ##                     Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
        ##                    xt,ll,super,packed,monotonic,&l,cmn(l))
        ##     if status != CHOLMOD_TRUE throw(CholmodException) end
        ## end
        function chm_copy_fac{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            ccall(($cop,:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                  (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}), &l,cmn($itype))
        end
        function chm_fac_to_sp{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            ccall(($f2s,:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                   (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}), &l,cmn($itype))
        end
        function chm_fac_xtype!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},to_xtype)
            status = ccall(($chg_xtyp,:libcholmod), Cint,
                           (Cint, Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           to_xtype,[l],cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function chm_pack_fac!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            status = ccall(($pack,:libcholmod), Cint,
                           (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &l,cmn($itype))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
    end
end

copy(L::CholmodFactor) = CholmodFactor(chm_copy_fac(L.c))
CholmodSparse(L::CholmodFactor) = CholmodSparse(chm_fac_to_sp(L.c))

function chm_fac_xtype!{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},to_xtype)
    chm_fac_xtype(L.c,to_xtype)
end

function CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}(tp::Ptr{c_CholmodTriplet{Tv,Ti}})
    ctp = unsafe_ref(tp)
    i = pointer_to_array(ctp.i, (ctp.nnz,), true)
    j = pointer_to_array(ctp.j, (ctp.nnz,), true)    
    x = pointer_to_array(ctp.x, (ctp.x == C_NULL ? 0 : ctp.nnz), true)
    ct = CholmodTriplet{Tv,Ti}(ctp, i, j, x)
    c_free(tp)
    ct
end
    
for (s2t,t2s,itype) in
    (("colmod_sparse_to_triplet","cholmod_triplet_to_sparse",:Int32),
     ("cholmod_l_sparse_to_triplet","cholmod_l_triplet_to_sparse",:Int64))
    @eval begin
        function convert{Tv<:CHMVTypes}(::Type{CholmodTriplet{Tv,$itype}},
                                        A::CholmodSparse{Tv,$itype})
            CholmodTriplet(ccall(($s2t, :libcholmod), Ptr{c_CholmodTriplet{Tv,$itype}},
                                 (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &A, cmn($itype)))
        end
        function convert{Tv<:CHMVTypes}(::Type{CholmodSparse{Tv,$itype}},
                                        A::CholmodTriplet{Tv,$itype})
            CholmodSparse(ccall(($t2s, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                                (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &A, cmn($itype)))
        end
    end
end

function findn_nzs{Tv,Ti}(A::CholmodSparse{Tv,Ti})
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
            println("Warning: sparse matrix contains explicitly stored zeros.")
        end
    end
    ind = ind[1:count]                  # ind is the indices of nonzeros in A.nzval
    (increment!(A.rowval0[ind]), jj[ind], A.nzval[ind])
end

findn_nzs(L::CholmodFactor) = findn_nzs(chm_fac_to_sp(L))

function diag{Tv}(A::CholmodSparse{Tv})
    minmn = min(size(A))
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
    if L.c.is_super != 0 error("Method for supernodal factors not yet written") end
    c0 = L.p
    r0 = L.i
    xv = L.x
    for j in 1:length(c0)-1
        jj = c0[j]+1
        assert(r0[jj] == j-1)
        res[j] = xv[jj]
    end
    res
end

function logdet{Tv,Ti}(L::CholmodFactor{Tv,Ti})
    if L.c.is_super != 0 error("Method for supernodal factors not yet written") end
    c0 = L.p
    r0 = L.i
    xv = L.x
    res = zero(Tv)
    for j in 1:length(c0)-1
        jj = c0[j]+1
        assert(r0[jj] == j-1)
        res += log(xv[jj])
    end
    L.c.is_ll != 0 ? 2res : res
end

end #module
