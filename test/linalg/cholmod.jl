using Base.Test

let # Issue 9160
    const CHOLMOD = Base.LinAlg.CHOLMOD

    for Ti in CHOLMOD.CHMITypes.types
        for elty in CHOLMOD.CHMVRealTypes.types

            A = sprand(10,10,0.1)
            A = convert(SparseMatrixCSC{elty,Ti},A)
            cmA = CHOLMOD.CholmodSparse(A)

            B = sprand(10,10,0.1)
            B = convert(SparseMatrixCSC{elty,Ti},B)
            cmB = CHOLMOD.CholmodSparse(B)

            # Ac_mul_B
            @test_approx_eq sparse(cmA'*cmB) A'*B

            # A_mul_Bc
            @test_approx_eq sparse(cmA*cmB') A*B'

            # A_mul_Ac
            @test_approx_eq sparse(cmA*cmA') A*A'

            # Ac_mul_A
            @test_approx_eq sparse(cmA'*cmA) A'*A

            # A_mul_Ac for symmetric A
            A = 0.5*(A + A')
            cmA = CHOLMOD.CholmodSparse(A)
            @test_approx_eq  sparse(cmA*cmA') A*A'
        end
    end
end
