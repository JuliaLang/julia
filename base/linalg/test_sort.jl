module test_sort

const liblapack = Base.liblapack_name

import Base.LinAlg: BlasFloat, BlasChar, BlasInt, blas_int, LAPACKException,
    DimensionMismatch, SingularException, PosDefException, chkstride1, chksquare

#Generic LAPACK error handlers
macro assertargsok() #Handle only negative info codes - use only if positive info code is useful!
    :(info[1]<0 && throw(ArgumentError("invalid argument #$(-info[1]) to LAPACK call")))
end
macro lapackerror() #Handle all nonzero info codes
    :(info[1]>0 ? throw(LAPACKException(info[1])) : @assertargsok )
end

macro assertnonsingular()
    :(info[1]>0 && throw(SingularException(info[1])))
end
macro assertposdef()
    :(info[1]>0 && throw(PosDefException(info[1])))
end

#Check that upper/lower (for special matrices) is correctly specified
macro chkuplo()
    :((uplo=='U' || uplo=='L') || throw(ArgumentError("""invalid uplo = $uplo

Valid choices are 'U' (upper) or 'L' (lower).""")))
end


(gges, elty) = (:dgges_, :Float64)
@eval begin
    function test_gges!(jobvsl::Char, jobvsr::Char,
                        A::StridedMatrix{$elty}, B::StridedMatrix{$elty},
                        sort::Char, selctg::Function)

    # *     .. Scalar Arguments ..
    #       CHARACTER          JOBVSL, JOBVSR, SORT
    #       INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM
    # *     ..
    # *     .. Array Arguments ..
    #       LOGICAL            BWORK( * )
    #       DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
    #      $                   B( LDB, * ), BETA( * ), VSL( LDVSL, * ),
    #      $                   VSR( LDVSR, * ), WORK( * )

        chkstride1(A, B)
        n, m = chksquare(A, B)
        n==m || throw(DimensionMismatch("Matrices are not of the same size"))
        sdim = blas_int(0)
        alphar = similar(A, $elty, n)
        alphai = similar(A, $elty, n)
        beta = similar(A, $elty, n)
        ldvsl = jobvsl == 'V' ? n : 1
        vsl = similar(A, $elty, ldvsl, n)
        ldvsr = jobvsr == 'V' ? n : 1
        vsr = similar(A, $elty, ldvsr, n)
        work = Array($elty, 1)
        lwork = blas_int(-1)
        info = Array(BlasInt, 1)
        for i = 1:2
            ccall(($(string(gges)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Void},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                    Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{Void},
                    Ptr{BlasInt}),
                &jobvsl, &jobvsr, &sort, &selctg,
                &n, A, &max(1,n), B,
                &max(1,n), &sdim, alphar, alphai,
                beta, vsl, &ldvsl, vsr,
                &ldvsr, work, &lwork, C_NULL,
                info)
            if i == 1
                lwork = blas_int(real(work[1]))
                work = Array($elty, lwork)
            end
        end
        @lapackerror
        A, B, complex(alphar, alphai), beta, vsl[1:(jobvsl == 'V' ? n : 0),:], vsr[1:(jobvsr == 'V' ? n : 0),:]
    end
end

end # module


test_schurfact{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T}, sort::Char='V', selctg::Function=C_NULL) = schurfact!(copy(A),copy(B), sort, selctg)
test_schurfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}, sort::Char='V', selctg::Function=C_NULL) = GeneralizedSchur(LinAlg.LAPACK.gges!('V', 'V', A, B, sort, selctg)...)
test_schurfact{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB}, sort::Char='V', selctg::Function=C_NULL) = (S = promote_type(Float32,typeof(one(TA)/norm(one(TA))),TB); schurfact!(S != TA ? convert(AbstractMatrix{S},A) : copy(A), S != TB ? convert(AbstractMatrix{S},B) : copy(B), sort, selctg))


function order_eigs(alphar, alphai, beta)
    if alphar/alphai > 1
        return true
    else
        return false
    end
end

# Test call
A = randn(3, 3)
B = reshape(1:9, 3, 3)

test_schurfact(A, B, 'S', order_eigs)