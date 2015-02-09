using Base.Test

let # svds test
    A = sparse([1, 1, 2, 3, 4], [2, 1, 1, 3, 1], [2.0, -1.0, 6.1, 7.0, 1.5])
    S1 = svds(A, nsv = 2)
    S2 = svd(full(A))

    ## singular values match:
    @test_approx_eq S1[2] S2[2][1:2]

    ## 1st left singular vector
    s1_left = sign(S1[1][3,1]) * S1[1][:,1]
    s2_left = sign(S2[1][3,1]) * S2[1][:,1]
    @test_approx_eq s1_left s2_left

    ## 1st right singular vector
    s1_right = sign(S1[3][3,1]) * S1[3][:,1]
    s2_right = sign(S2[3][3,1]) * S2[3][:,1]
    @test_approx_eq s1_right s2_right
end

let # complex svds test
    A = sparse([1, 1, 2, 3, 4], [2, 1, 1, 3, 1], exp(im*[2.0:2:10]))
    S1 = svds(A, nsv = 2)
    S2 = svd(full(A))

    ## singular values match:
    @test_approx_eq S1[2] S2[2][1:2]

    ## left singular vectors
    s1_left = abs(S1[1][:,1:2])
    s2_left = abs(S2[1][:,1:2])
    @test_approx_eq s1_left s2_left

    ## right singular vectors
    s1_right = abs(S1[3][:,1:2])
    s2_right = abs(S2[3][:,1:2])
    @test_approx_eq s1_right s2_right
end
