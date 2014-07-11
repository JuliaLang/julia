using Base.Test

let
    A = rand(2,2)
    B = rand(2,3)
    C = A*B
    for Constr in (FixedArrayI, FixedArrayM)
        AI = Constr(A)
        BI = Constr(B)
        CI = AI*BI
        @test_approx_eq CI C
        CI = Constr(C)
        @test CI == C
    end
end
