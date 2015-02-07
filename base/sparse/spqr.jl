module SPQR
using Base.SparseMatrix, Base.SparseMatrix.CHOLMOD
import Base.SparseMatrix.CHOLMOD: cmn, c_CholmodSparse, c_CholmodDense
import Base.SparseMatrix: increment!
export SuiteSparseQR, SuiteSparseQRQ

include("spqr_h.jl")

if ccall((:jl_cholmod_sizeof_long,:libsuitesparse_wrapper),Csize_t,()) == 4
    typealias SuiteSparseLong Int32
else
    typealias SuiteSparseLong Int64
end

type SuiteSparseQR{Tv}
    fact::Ptr{Void}

    function SuiteSparseQR(fact::Ptr{Void})
        f = new(fact)
        finalizer(f, spqr_free)
        f
    end
end

immutable SuiteSparseQRQ{Tv}
    qr::SuiteSparseQR{Tv}
end

spqr_free(qr::SuiteSparseQR) =
    ccall(("SuiteSparseQR_C_free", :libspqr), Cint, (Ptr{Ptr{Void}}, Ptr{Uint8}),
          [ptr], cmn(SuiteSparseLong))

## qr
function _qr{Tv}(A::CholmodSparse{Tv,SuiteSparseLong}, ordering, tol, thin)
    outmat = Array(Ptr{c_CholmodSparse{Tv,SuiteSparseLong}}, 2)
    outcolperm = [Ptr{SuiteSparseLong}(0)]

    rnk = ccall(("SuiteSparseQR_C_QR",:libspqr), SuiteSparseLong,
          (Cint, Float64, SuiteSparseLong,
           Ptr{c_CholmodSparse{Tv,SuiteSparseLong}},
           Ptr{Ptr{c_CholmodSparse{Tv,SuiteSparseLong}}},
           Ptr{Ptr{c_CholmodSparse{Tv,SuiteSparseLong}}},
           Ptr{Ptr{SuiteSparseLong}}, Ptr{UInt8}),
          ordering, tol, thin ? min(size(A, 1), size(A, 2)) : max(size(A, 1), size(A, 2)), &A.c,
          pointer(outmat, 1), pointer(outmat, 2), pointer(outcolperm, 1),
          cmn(SuiteSparseLong))
    rnk == -1 && error("QR factorization failed")

    (sparse(CholmodSparse(outmat[1])), sparse(CholmodSparse(outmat[2])), outcolperm[1])
end

function Base.LinAlg.qr{Tv}(A::CholmodSparse{Tv,SuiteSparseLong}, 
                            pivot::Type{Val{true}}=Val{true},
                            tol=SPQR_DEFAULT_TOL; thin=true)
    Q, R, pptr = _qr(A, SPQR_ORDERING_DEFAULT, tol, thin)
    p = pptr != C_NULL ?
        increment!(pointer_to_array(pptr, size(A, 2), true)) : 
        SuiteSparseLong[1:size(A, 2)]
    (Q, R, p)
end

function Base.LinAlg.qr{Tv}(A::CholmodSparse{Tv,SuiteSparseLong}, 
                            pivot::Type{Val{false}},
                            tol=SPQR_DEFAULT_TOL; thin=true)
    Q, R = _qr(A, SPQR_ORDERING_FIXED, tol, thin)
    (Q, R)
end

Base.LinAlg.qr{Tv}(A::SparseMatrixCSC{Tv,SuiteSparseLong},
                   pivot::Union(Type{Val{true}},Type{Val{false}})=Val{true},
                   tol=SPQR_DEFAULT_TOL; thin=true) =
    qr(CholmodSparse(A), pivot, tol; thin=thin)

## qrfact
function Base.LinAlg.qrfact{Tv}(A::CholmodSparse{Tv,SuiteSparseLong},
                                pivot::Union(Type{Val{true}},Type{Val{false}})=Val{true},
                                tol=SPQR_DEFAULT_TOL)
    ptr = ccall(("SuiteSparseQR_C_factorize",:libspqr), Ptr{Void},
          (Cint, Float64, Ptr{c_CholmodSparse{Tv,SuiteSparseLong}}, Ptr{UInt8}),
          pivot == Val{true} ? SPQR_ORDERING_DEFAULT : SPQR_ORDERING_FIXED, tol, &A.c, cmn(SuiteSparseLong))
    ptr == C_NULL && error("QR factorization failed")
    SuiteSparseQR{Tv}(ptr)
end
Base.LinAlg.qrfact{Tv}(A::SparseMatrixCSC{Tv,SuiteSparseLong},
                   pivot::Union(Type{Val{true}},Type{Val{false}})=Val{true}, tol=SPQR_DEFAULT_TOL) =
    qrfact(CholmodSparse(A), pivot, tol)

## Q multiplication
getindex(L::SuiteSparseQR, what::Symbol) =
    what == :Q ? SuiteSparseQRQ(L) : throw(KeyError(what))

function spqr_qmult{Tv}(Q::SuiteSparseQRQ{Tv}, B::CholmodDense{Tv}, method)
    ptr = ccall(("SuiteSparseQR_C_qmult",:libspqr), Ptr{c_CholmodDense{Tv}},
          (Cint, Ptr{Void}, Ptr{c_CholmodDense{Tv}}, Ptr{UInt8}),
          method, Q.qr.fact, &B.c, cmn(SuiteSparseLong))
    ptr == C_NULL && error("Q multiply failed")
    CholmodDense(ptr)
end
spqr_qmult{Tv}(Q::SuiteSparseQRQ{Tv}, X::VecOrMat{Tv}, method) =
    full(spqr_qmult(Q, CholmodDense!(X), method))

A_mul_B{Tv}(Q::SuiteSparseQRQ{Tv}, X::Union(VecOrMat{Tv},CholmodDense{Tv})) = spqr_qmult(Q, X, SPQR_QX)
At_mul_B{Tv}(Q::SuiteSparseQRQ{Tv}, X::Union(VecOrMat{Tv},CholmodDense{Tv})) = spqr_qmult(Q, X, SPQR_QTX)
A_mul_B{Tv}(X::Union(VecOrMat{Tv},CholmodDense{Tv}), Q::SuiteSparseQRQ{Tv}) = spqr_qmult(Q, X, SPQR_XQ)
A_mul_Bt{Tv}(X::Union(VecOrMat{Tv},CholmodDense{Tv}), Q::SuiteSparseQRQ{Tv}) = spqr_qmult(Q, X, SPQR_XQT)

## \ solve
function \{Tv}(L::SuiteSparseQR{Tv}, B::CholmodDense{Tv})
    QtB = At_mul_B(SuiteSparseQRQ(L), B)
    ptr = ccall(("SuiteSparseQR_C_solve",:libspqr), Ptr{c_CholmodDense{Tv}},
          (Cint, Ptr{Void}, Ptr{c_CholmodDense{Tv}}, Ptr{UInt8}),
          SPQR_RETX_EQUALS_B, L.fact, &QtB.c, cmn(SuiteSparseLong))
    ptr == C_NULL && error("QR solve failed")
    CholmodDense(ptr)
end
\{Tv}(L::SuiteSparseQR{Tv}, B::VecOrMat{Tv}) = full(L\CholmodDense!(B))

end