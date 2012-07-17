require("linalg_sparse.jl")
require("suitesparse_h.jl")

_jl_libsuitesparse_wrapper = dlopen("libsuitesparse_wrapper")
_jl_libcholmod = dlopen("libcholmod")
_jl_libumfpack = dlopen("libumfpack")

type MatrixIllConditionedException <: Exception end

function _jl_convert_to_0_based_indexing!(S::SparseMatrixCSC)
    for i=1:(S.colptr[end]-1); S.rowval[i] -= 1; end
    for i=1:length(S.colptr); S.colptr[i] -= 1; end
    return S
end

function _jl_convert_to_1_based_indexing!(S::SparseMatrixCSC)
    for i=1:length(S.colptr); S.colptr[i] += 1; end
    for i=1:(S.colptr[end]-1); S.rowval[i] += 1; end
    return S
end

_jl_convert_to_0_based_indexing(S) = _jl_convert_to_0_based_indexing!(copy(S))
_jl_convert_to_1_based_indexing(S) = _jl_convert_to_1_based_indexing!(copy(S))

## CHOLMOD

CHMVTypes = Union(Complex64, Complex128, Float32, Float64)
CHMITypes = Union(Int32, Int64)

# Wrapper for memory allocated by CHOLMOD. Carry along the value and index types.
## FIXME: CholmodPtr and UmfpackPtr should be amalgamated
type CholmodPtr{Tv<:CHMVTypes,Ti<:CHMITypes}
    val::Vector{Ptr{Void}} 
end

function _jl_cholmod_common_finalizer(x::Vector{Ptr{Void}})
    ccall(dlsym(_jl_libcholmod, :cholmod_finish), Int32, (Ptr{Void},), x[1])
    _c_free(x[1])
end

type CholmodCommon
    pt::Vector{Ptr{Void}}
    function CholmodCommon()
        pt = Array(Ptr{Void}, 1)
        ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_common), Void,
              (Ptr{Void},),
              pt)
        status = ccall(dlsym(_jl_libcholmod, :cholmod_start), Int,
                       (Ptr{Void}, ), pt[1])
        if status != 1 error("Error calling cholmod_start") end
        finalizer(pt, _jl_cholmod_common_finalizer)
        new(pt)
    end
end

function show(io, cm::CholmodCommon)
    ccall(dlsym(_jl_libcholmod, :cholmod_print_common),
          Int32, (Ptr{Uint8},Ptr{Void}), "", cm.pt[1])
end

type CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    ## cp contains a copy of the original matrix with 0-based indices
    cp::SparseMatrixCSC{Tv,Ti}
    stype::Int
    cm::CholmodCommon
    function CholmodSparse(m::SparseMatrixCSC{Tv,Ti}, stype::Int, cm::CholmodCommon)
        itype = Ti == Int32   ? _jl_CHOLMOD_INT : _jl_CHOLMOD_LONG
        xtype = Tv <: Complex ? _jl_CHOLMOD_COMPLEX : _jl_CHOLMOD_REAL
        dtype = Tv == Float32 || Tv == Complex64 ? _jl_CHOLMOD_SINGLE : _jl_CHOLMOD_DOUBLE
        pt = CholmodPtr{Tv,Ti}(Array(Ptr{Void}, 1))
        
        cp = _jl_convert_to_0_based_indexing(m)
        
        ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_sparse), Void,
              (Ptr{Void}, Uint, Uint, Uint, Ptr{Void}, Ptr{Void}, Ptr{Void},
               Ptr{Void}, Ptr{Void}, Int32, Int32, Int32, Int32, Int32, Int32),
              pt.val, cp.m, cp.n, length(cp.nzval), cp.colptr, cp.rowval, C_NULL,
              cp.nzval, C_NULL, stype, itype, xtype, dtype,
              _jl_CHOLMOD_TRUE, _jl_CHOLMOD_TRUE)
        finalizer(pt, x->_c_free(x.val[1]))
        new(pt, cp, int(stype), cm)
    end
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(m::SparseMatrixCSC{Tv,Ti}, stype::Int)
    CholmodSparse{Tv,Ti}(m, stype, CholmodCommon())
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(m::SparseMatrixCSC{Tv,Ti})
    m.m == m.n && ishermitian(m) ? CholmodSparse(triu(m), 1) : CholmodSparse(m, 0)
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(m::SparseMatrixCSC{Tv,Ti}, cm::CholmodCommon)
    m.m == m.n && ishermitian(m) ? CholmodSparse(triu(m), 1, cm) : CholmodSparse(m, 0, cm)
end

function show{Tv<:CHMVTypes,Ti<:CHMITypes}(io, cs::CholmodSparse{Tv,Ti})
    ccall(dlsym(_jl_libcholmod, :cholmod_print_sparse),
          Int32, (Ptr{Void}, Ptr{Uint8},Ptr{Void}), cs.pt.val[1], "", cs.cm.pt[1])
end

size{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti}) = size(cs.cp)

nnz{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti}) = nnz(cs.cp) + 1

function SparseMatrixCSC{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti})
    _jl_convert_to_1_based_indexing(cs.cp)
end

## Somehow the results come out backwards - p == Inf produces the 1 norm
## For testing only.  The infinity and 1 norms of a sparse matrix are simply
## the same norm applied to its nzval field.
function norm{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti}, p::Number)
    ccall(dlsym(_jl_libcholmod, :cholmod_norm_sparse), Float64,
          (Ptr{Void}, Int32, Ptr{Void}), cs.pt.val[1], p == Inf ? 0 : 1, cs.cm.pt[1])
end

norm{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti}) = norm(cs, Inf)

## Approximate minimal degree ordering
function amd{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti})
    aa = Array(Int32, cs.cp.m)
    st = cs.stype == 0 ? ccall(dlsym(_jl_libcholmod, :cholmod_colamd), Int32,
                               (Ptr{Void}, Ptr{Void}, Uint, Int32, Ptr{Int32}, Ptr{Void}),
                               cs.pt.val[1], C_NULL, 0, 1, aa, cs.cm.pt[1]) :
                         ccall(dlsym(_jl_libcholmod, :cholmod_amd), Int32,
                               (Ptr{Void}, Ptr{Void}, Uint, Ptr{Int32}, Ptr{Void}),
                               cs.pt.val[1], C_NULL, 0, aa, cs.cm.pt[1])
    if st != 1 error("CHOLMOD error in amd") end
    aa
end

type CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes} <: Factorization{Tv}
    pt::CholmodPtr{Tv,Ti}
    cs::CholmodSparse{Tv,Ti}
    function CholmodFactor(pt::CholmodPtr{Tv,Ti}, cs::CholmodSparse{Tv,Ti})
        ff = new(pt, cs)
        finalizer(ff, _jl_cholmod_factor_finalizer)
        ff
    end
end

function CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}(cs::CholmodSparse{Tv,Ti})
    pt = CholmodPtr{Tv,Ti}(Array(Ptr{Void}, 1))
    pt.val[1] = ccall(dlsym(_jl_libcholmod, :cholmod_analyze), Ptr{Void},
                      (Ptr{Void}, Ptr{Void}), cs.pt.val[1], cs.cm.pt[1])
    st = ccall(dlsym(_jl_libcholmod, :cholmod_factorize), Int32,
               (Ptr{Void}, Ptr{Void}, Ptr{Void}), cs.pt.val[1], pt.val[1], cs.cm.pt[1])
    if st != 1 error("CHOLMOD failure in factorize") end
    CholmodFactor{Tv,Ti}(pt, cs)
end

function _jl_cholmod_factor_finalizer{Tv<:CHMVTypes,Ti<:CHMITypes}(x::CholmodFactor{Tv,Ti})
    st = ccall(dlsym(_jl_libcholmod, :cholmod_free_factor), Int32,
               (Ptr{Void}, Ptr{Void}), x.pt.val, x.cs.cs.cm[1])
    if st != 1 error("CHOLMOD error in cholmod_free_factor") end
end

function show{Tv<:CHMVTypes,Ti<:CHMITypes}(io, cf::CholmodFactor{Tv,Ti})
    ccall(dlsym(_jl_libcholmod, :cholmod_print_factor), Int32,
          (Ptr{Void}, Ptr{Uint8}, Ptr{Void}), cf.pt.val[1], "", cf.cs.cm.pt[1])
end

type CholmodDense{T<:CHMVTypes}
    pt::Vector{Ptr{Void}}
    m::Int
    n::Int
    aa::VecOrMat{T}                     # original array
    cm::CholmodCommon
end

function CholmodDense{T<:CHMVTypes}(B::VecOrMat{T}, cm::CholmodCommon)
    m = size(B, 1)
    n = isa(B, Matrix) ? size(B, 2) : 1

    xtype = T <: Complex ? _jl_CHOLMOD_COMPLEX : _jl_CHOLMOD_REAL
    dtype = T == Float32 || T == Complex64 ? _jl_CHOLMOD_SINGLE : _jl_CHOLMOD_DOUBLE

    pt = Array(Ptr{Void}, 1)

    ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_dense), Void,
          (Ptr{Void}, Uint, Uint, Uint, Uint, Ptr{Void}, Ptr{Void}, Int32, Int32),
          pt, m, n, numel(B), m, B, C_NULL, xtype, dtype)
    finalizer(pt, x->_c_free(pt[1]))
    CholmodDense{T}(pt, m, n, B, cm)
end

size{T<:CHMVTypes}(cd::CholmodDense{T}) = (cd.m, cd.n)

function show{T<:CHMVTypes}(io, cd::CholmodDense{T})
    ccall(dlsym(_jl_libcholmod, :cholmod_print_dense),
          Int32, (Ptr{Void},Ptr{Uint8},Ptr{Void}), cd.pt[1], "", cd.cm.pt[1])
end

type CholmodDenseOut{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    m::Int
    n::Int
    cm::CholmodCommon
    function CholmodDenseOut(pt::CholmodPtr{Tv,Ti}, m::Int, n::Int, cm::CholmodCommon)
        dd = new(pt, m, n, cm)
        finalizer(dd, _jl_cholmod_denseout_finalizer)
        dd
    end
end

function _jl_cholmod_denseout_finalizer{Tv<:CHMVTypes,Ti<:CHMITypes}(cd::CholmodDenseOut{Tv,Ti})
    st = ccall(dlsym(_jl_libcholmod, :cholmod_free_dense), Int32,
               (Ptr{Void}, Ptr{Void}), cd.pt.val, cd.cm.pt[1])
    if st != 1 error("Error in cholmod_free_dense") end
    nothing
end

function convert{Tv<:CHMVTypes,Ti<:CHMITypes}(::Type{Array{Tv,2}}, cdo::CholmodDenseOut{Tv,Ti})
    mm = Array(Tv, (cdo.m, cdo.n))
    ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_dense_copy_out), Void,
          (Ptr{Void}, Ptr{Tv}), cdo.pt.val[1], mm)
    mm
end

function solve{Tv<:CHMVTypes,Ti<:CHMITypes}(cf::CholmodFactor{Tv,Ti}, B::CholmodDense{Tv}, solv::Integer)
    m, n = size(B)
    cdo = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cdo.val[1] = ccall(dlsym(_jl_libcholmod, :cholmod_solve), Ptr{Void},
                       (Int32, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                       solv, cf.pt.val[1], B.pt[1], cf.cs.cm.pt[1])
    CholmodDenseOut{Tv,Ti}(cdo, m, n, cf.cs.cm)
end

solve{Tv<:CHMVTypes,Ti<:CHMITypes}(cf::CholmodFactor{Tv,Ti}, B::CholmodDense{Tv}) = solve(cf, B, _jl_CHOLMOD_A)

(\){Tv<:CHMVTypes,Ti<:CHMITypes}(cf::CholmodFactor{Tv,Ti}, b::VecOrMat{Tv}) = convert(Matrix{Tv}, solve(cf, CholmodDense(b, cf.cs.cm)))

type CholmodSparseOut{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    m::Int
    n::Int
    cm::CholmodCommon
    function CholmodSparseOut(pt::CholmodPtr{Tv,Ti}, m::Int, n::Int, cm::CholmodCommon)
        cso = new(pt, m, n, cm)
        finalizer(cso, _jl_cholmod_sparseout_finalizer)
        cso
    end
end

function _jl_cholmod_sparseout_finalizer{Tv<:CHMVTypes,Ti<:CHMITypes}(cso::CholmodSparseOut{Tv,Ti})
    st = ccall(dlsym(_jl_libcholmod, :cholmod_free_sparse), Int32,
               (Ptr{Void}, Ptr{Void}), cso.pt.val, cso.cm.pt[1])
    if st != 1 error("Error in cholmod_free_sparse") end
    nothing
end

function nnz{Tv<:CHMVTypes,Ti<:CHMITypes}(cso::CholmodSparseOut{Tv,Ti})
    ccall(dlsym(_jl_libcholmod, :cholmod_nnz), Int32,
          (Ptr{Void}, Ptr{Void}), cso.pt.val[1], cso.cm.pt[1])
end

function solve{Tv<:CHMVTypes,Ti<:CHMITypes}(cf::CholmodFactor{Tv,Ti}, B::CholmodSparse{Tv,Ti}, solv::Integer)
    m, n = size(B)
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall(dlsym(_jl_libcholmod, :cholmod_spsolve), Ptr{Void},
                       (Int32, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                       solv, cf.pt.val[1], B.pt[1], B.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, m, n, cf.cs.cm)
end

function CholmodSparseOut{Tv<:CHMVTypes,Ti<:CHMITypes}(cf::CholmodFactor{Tv,Ti})
    n = size(cf.cs)[1]
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall(dlsym(_jl_libcholmod, :cholmod_factor_to_sparse), Ptr{Void},
                       (Ptr{Void}, Ptr{Void}),
                        cf.pt.val[1], cf.cs.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, n, n, cf.cs.cm)
end

function SparseMatrixCSC{Tv<:CHMVTypes,Ti<:CHMITypes}(cso::CholmodSparseOut{Tv,Ti})
    nz = nnz(cso)
    sp = SparseMatrixCSC{Tv,Ti}(cso.m, cso.n, Array(Ti, cso.n + 1), Array(Ti, nz), Array(Tv, nz))
    err = ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_sparse_copy_out), Int32,
                (Ptr{Void}, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}),
                cso.pt.val[1], sp.colptr, sp.rowval, sp.nzval)
    if err == 1 error("CholmodSparseOut object is not packed") end
    if err == 2 error("CholmodSparseOut object is not sorted") end # maybe do double transpose here?
    if err == 3 error("CholmodSparseOut object has INTLONG itype") end
    _jl_convert_to_1_based_indexing!(sp)
end

function show{Tv<:CHMVTypes,Ti<:CHMITypes}(io, cso::CholmodSparseOut{Tv,Ti})
    ccall(dlsym(_jl_libcholmod, :cholmod_print_sparse),
          Int32, (Ptr{Void}, Ptr{Uint8},Ptr{Void}), cso.pt.val[1], "", cso.cm.pt[1])
end

## call wrapper function to create cholmod_sparse objects
_jl_cholmod_sparse(S) = _jl_cholmod_sparse(S, 0)

function _jl_cholmod_sparse{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, stype::Int)
    cs = Array(Ptr{Void}, 1)

    if     Ti == Int32; itype = _jl_CHOLMOD_INT;
    elseif Ti == Int64; itype = _jl_CHOLMOD_LONG; end

    if     Tv == Float64    || Tv == Float32;    xtype = _jl_CHOLMOD_REAL;
    elseif Tv == Complex128 || Tv == Complex64 ; xtype = _jl_CHOLMOD_COMPLEX; end

    if     Tv == Float64 || Tv == Complex128; dtype = _jl_CHOLMOD_DOUBLE; 
    elseif Tv == Float32 || Tv == Complex64 ; dtype = _jl_CHOLMOD_SINGLE; end

    ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_sparse),
          Ptr{Void},
          (Ptr{Void}, Int, Int, Int, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void},
           Int32, Int32, Int32, Int32, Int32, Int32),
          cs, int(S.m), int(S.n), int(length(S.nzval)), S.colptr, S.rowval, C_NULL, S.nzval, C_NULL,
          int32(stype), itype, xtype, dtype, _jl_CHOLMOD_TRUE, _jl_CHOLMOD_TRUE
          )

    return cs
end

## Call wrapper function to create cholmod_dense objects
function _jl_cholmod_dense{T}(B::VecOrMat{T})
    m = size(B, 1)
    n = isa(B, Matrix) ? size(B, 2) : 1

    cd = Array(Ptr{Void}, 1)

    if     T == Float64    || T == Float32;    xtype = _jl_CHOLMOD_REAL;
    elseif T == Complex128 || T == Complex64 ; xtype = _jl_CHOLMOD_COMPLEX; end

    if     T == Float64 || T == Complex128; dtype = _jl_CHOLMOD_DOUBLE; 
    elseif T == Float32 || T == Complex64 ; dtype = _jl_CHOLMOD_SINGLE; end

    ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_dense),
          Ptr{Void},
          (Ptr{Void}, Int, Int, Int, Int, Ptr{T}, Ptr{Void}, Int32, Int32),
          cd, m, n, numel(B), m, B, C_NULL, xtype, dtype
          )

    return cd
end

function _jl_cholmod_dense_copy_out{T}(x::Ptr{Void}, sol::VecOrMat{T})
    ccall(dlsym(_jl_libsuitesparse_wrapper, :jl_cholmod_dense_copy_out),
          Void,
          (Ptr{Void}, Ptr{T}),
          x, sol
          )
    return sol
end

function _jl_cholmod_transpose_unsym{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, cm::Array{Ptr{Void}, 1})
    S_t = SparseMatrixCSC(Tv, S.n, S.m, nnz(S)+1)

    # Allocate space for a cholmod_sparse object
    cs = _jl_cholmod_sparse(S)
    cs_t = _jl_cholmod_sparse(S_t)
    
    status = ccall(dlsym(_jl_libcholmod, :cholmod_transpose_unsym),
                   Int32,
                   (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Int32}, Int32, Ptr{Void}, Ptr{Void}),
                   cs[1], int32(1), C_NULL, C_NULL, int32(-1), cs_t[1], cm[1]);

    # Deallocate space for cholmod_sparse objects
    _c_free(cs[1])
    _c_free(cs_t[1])

    return S_t
end

function _jl_cholmod_analyze{Tv<:Union(Float64,Complex128), Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})

    cs_factor = ccall(dlsym(_jl_libcholmod, :cholmod_analyze),
                       Ptr{Void},
                       (Ptr{Void}, Ptr{Void}),
                       cs[1], cm[1])
           
    return cs_factor
end

function _jl_cholmod_factorize{Tv<:Union(Float64,Complex128), Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cs_factor::Ptr{Void}, cm::Array{Ptr{Void},1})

    status = ccall(dlsym(_jl_libcholmod, :cholmod_factorize),
                   Int32,
                   (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                   cs[1], cs_factor, cm[1])

    if status == _jl_CHOLMOD_FALSE; error("CHOLMOD could not factorize the matrix"); end
end

function _jl_cholmod_solve(cs_factor::Ptr{Void}, cd_rhs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})

    sol = ccall(dlsym(_jl_libcholmod, :cholmod_solve),
                Ptr{Void},
                (Int32, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                _jl_CHOLMOD_A, cs_factor, cd_rhs[1], cm[1])

    return sol
end

## UMFPACK

# Wrapper for memory allocated by umfpack. Carry along the value and index types.
type UmfpackPtr{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}
    val::Vector{Ptr{Void}} 
end

type UmfpackLU{Tv<:Union(Float64,Complex128),Ti<:CHMITypes} <: Factorization{Tv}
    numeric::UmfpackPtr{Tv,Ti}
    mat::SparseMatrixCSC{Tv,Ti}
end

function show(io, f::UmfpackLU)
    printf("UMFPACK LU Factorization of a %d-by-%d sparse matrix\n",
           size(f.mat,1), size(f.mat,2))
    println(f.numeric)
    _jl_umfpack_report(f)
end

type UmfpackLUTrans{Tv<:Union(Float64,Complex128),Ti<:CHMITypes} <: Factorization{Tv}
    numeric::UmfpackPtr{Tv,Ti}
    mat::SparseMatrixCSC{Tv,Ti}
end

function show(io, f::UmfpackLUTrans)
    printf("UMFPACK LU Factorization of a transposed %d-by-%d sparse matrix\n",
           size(f.mat,1), size(f.mat,2))
    println(f.numeric)
    _jl_umfpack_report(f)
end

function UmfpackLU{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    Scopy = copy(S) 
    Scopy = _jl_convert_to_0_based_indexing!(Scopy)
    numeric = []

    try
        symbolic = _jl_umfpack_symbolic(Scopy)
        numeric = _jl_umfpack_numeric(Scopy, symbolic)
    catch e
        if is(e,MatrixIllConditionedException)
            error("Input matrix is ill conditioned or singular");
        else
            error("Error calling UMFPACK")
        end
    end
    
    return UmfpackLU(numeric,Scopy) 
end

function UmfpackLU!{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    Sshallow = SparseMatrixCSC(S.m,S.n,S.colptr,S.rowval,S.nzval)
    Sshallow = _jl_convert_to_0_based_indexing!(Sshallow)
    numeric = []

    try
        symbolic = _jl_umfpack_symbolic(Sshallow)
        numeric = _jl_umfpack_numeric(Sshallow, symbolic)
    catch e
        Sshallow = _jl_convert_to_1_based_indexing!(Sshallow)
        if is(e,MatrixIllConditionedException)
            error("Input matrix is ill conditioned or singular");
        else
            error("Error calling UMFPACK")
        end
    end

    S.rowval = []
    S.nzval = []
    S.colptr = ones(S.n+1)
    
    return UmfpackLU(numeric,Sshallow)
end

function UmfpackLUTrans(S::SparseMatrixCSC) 
    x = UmfpackLU(S)
    return UmfpackLUTrans(x.numeric, x.mat)
end

# Solve with Factorization

(\){T}(fact::UmfpackLU{T}, b::Vector) = fact \ convert(Array{T,1}, b)
(\){T}(fact::UmfpackLU{T}, b::Vector{T}) = _jl_umfpack_solve(fact.mat,b,fact.numeric)

(\){T}(fact::UmfpackLUTrans{T}, b::Vector) = fact \ convert(Array{T,1}, b)
(\){T}(fact::UmfpackLUTrans{T}, b::Vector{T}) = _jl_umfpack_transpose_solve(fact.mat,b,fact.numeric)

ctranspose(fact::UmfpackLU) = UmfpackLUTrans(fact.numeric, fact.mat)

# Solve directly with matrix

(\)(S::SparseMatrixCSC, b::Vector) = UmfpackLU(S) \ b
At_ldiv_B(S::SparseMatrixCSC, b::Vector) = UmfpackLUTrans(S) \ b
Ac_ldiv_B(S::SparseMatrixCSC, b::Vector) = UmfpackLUTrans(S) \ b

## Wrappers around UMFPACK routines

for (f_sym_r, f_sym_c, inttype) in
    (("umfpack_di_symbolic","umfpack_zi_symbolic",:Int32),
     ("umfpack_dl_symbolic","umfpack_zl_symbolic",:Int64))
    @eval begin

        function _jl_umfpack_symbolic{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti})
            # Pointer to store the symbolic factorization returned by UMFPACK
            Symbolic = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(dlsym(_jl_libumfpack, $f_sym_r),
                           Ti,
                           (Ti, Ti, 
                            Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           S.m, S.n, 
                           S.colptr, S.rowval, S.nzval, Symbolic.val, C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in symbolic factorization"); end
            finalizer(Symbolic,_jl_umfpack_free_symbolic)
            return Symbolic
        end

        function _jl_umfpack_symbolic{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti})
            # Pointer to store the symbolic factorization returned by UMFPACK
            Symbolic = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(dlsym(_jl_libumfpack, $f_sym_c),
                           Ti,
                           (Ti, Ti, 
                            Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.m, S.n, 
                           S.colptr, S.rowval, real(S.nzval), imag(S.nzval), Symbolic.val, 
                           C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in symbolic factorization"); end
            finalizer(Symbolic,_jl_umfpack_free_symbolic) # Check: do we need to free if there was an error?
            return Symbolic
        end

    end
end

for (f_num_r, f_num_c, inttype) in
    (("umfpack_di_numeric","umfpack_zi_numeric",:Int32),
     ("umfpack_dl_numeric","umfpack_zl_numeric",:Int64))
    @eval begin

        function _jl_umfpack_numeric{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
            # Pointer to store the numeric factorization returned by UMFPACK
            Numeric = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(dlsym(_jl_libumfpack, $f_num_r),
                           Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.colptr, S.rowval, S.nzval, Symbolic.val[1], Numeric.val, 
                           C_NULL, C_NULL)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != _jl_UMFPACK_OK; error("Error in numeric factorization"); end
            finalizer(Numeric,_jl_umfpack_free_numeric)
            return Numeric
        end

        function _jl_umfpack_numeric{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
            # Pointer to store the numeric factorization returned by UMFPACK
            Numeric = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(dlsym(_jl_libumfpack, $f_num_c),
                           Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.colptr, S.rowval, real(S.nzval), imag(S.nzval), Symbolic.val[1], Numeric.val, 
                           C_NULL, C_NULL)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != _jl_UMFPACK_OK; error("Error in numeric factorization"); end
            finalizer(Numeric,_jl_umfpack_free_numeric)
            return Numeric
        end

    end
end

for (f_sol_r, f_sol_c, inttype) in
    (("umfpack_di_solve","umfpack_zi_solve",:Int32),
     ("umfpack_dl_solve","umfpack_zl_solve",:Int64))
    @eval begin

        function _jl_umfpack_solve{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                             b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            x = similar(b)
            status = ccall(dlsym(_jl_libumfpack, $f_sol_r),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           _jl_UMFPACK_A, S.colptr, S.rowval, S.nzval, 
                           x, b, Numeric.val[1], C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in solve"); end
            return x
        end

        function _jl_umfpack_solve{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                                b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(dlsym(_jl_libumfpack, $f_sol_c),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           _jl_UMFPACK_A, S.colptr, S.rowval, real(S.nzval), imag(S.nzval), 
                           xr, xi, real(b), imag(b), Numeric.val[1], C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in solve"); end
            return complex(xr,xi)
        end

        function _jl_umfpack_transpose_solve{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                             b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            x = similar(b)
            status = ccall(dlsym(_jl_libumfpack, $f_sol_r),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           _jl_UMFPACK_At, S.colptr, S.rowval, S.nzval, 
                           x, b, Numeric.val[1], C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in solve"); end
            return x
        end

        function _jl_umfpack_transpose_solve{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                                b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(dlsym(_jl_libumfpack, $f_sol_c),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           _jl_UMFPACK_At, S.colptr, S.rowval, real(S.nzval), imag(S.nzval), 
                           xr, xi, real(b), imag(b), Numeric.val[1], C_NULL, C_NULL)
            if status != _jl_UMFPACK_OK; error("Error in solve"); end
            return complex(xr,xi)
        end

    end
end

for (f_report, elty, inttype) in
    (("umfpack_di_report_numeric", :Float64,    :Int32),
     ("umfpack_zi_report_numeric", :Complex128, :Int32),
     ("umfpack_dl_report_numeric", :Float64,    :Int64),
     ("umfpack_zl_report_numeric", :Complex128, :Int64))
     @eval begin

         function _jl_umfpack_report{Tv<:$elty,Ti<:$inttype}(slu::UmfpackLU{Tv,Ti})

             control = zeros(Float64, _jl_UMFPACK_CONTROL)
             control[_jl_UMFPACK_PRL] = 4
         
             ccall(dlsym(_jl_libumfpack, $f_report),
                   Ti,
                   (Ptr{Void}, Ptr{Float64}),
                   slu.numeric.val[1], control)
         end

    end
end


for (f_symfree, f_numfree, elty, inttype) in
    (("umfpack_di_free_symbolic","umfpack_di_free_numeric",:Float64,:Int32),
     ("umfpack_zi_free_symbolic","umfpack_zi_free_numeric",:Complex128,:Int32),
     ("umfpack_dl_free_symbolic","umfpack_dl_free_numeric",:Float64,:Int64),
     ("umfpack_zl_free_symbolic","umfpack_zl_free_numeric",:Complex128,:Int64))
    @eval begin

        _jl_umfpack_free_symbolic{Tv<:$elty,Ti<:$inttype}(Symbolic::UmfpackPtr{Tv,Ti}) =
        ccall(dlsym(_jl_libumfpack, $f_symfree), Void, (Ptr{Void},), Symbolic.val)
        
        _jl_umfpack_free_numeric{Tv<:$elty,Ti<:$inttype}(Numeric::UmfpackPtr{Tv,Ti}) =
        ccall(dlsym(_jl_libumfpack, $f_numfree), Void, (Ptr{Void},), Numeric.val)
        
    end
end
