let
    function myeq(A, B)
        size(A) == size(B) || return false
        for I in eachindex(B)
            A[I] == B[I] || return false
        end
        true
    end
    indextype{T,N,P,I}(::Reshaped.ReshapedArray{T,N,P,I}) = I

    A = Reshaped.myreshape(1:15, (3, 5))
    @test indextype(A) <: (Reshaped.IndexMD{1,2},)
    AA = reshape(1:15, (3, 5))
    @test A == AA
    @test myeq(A, AA)

    B = Reshaped.myreshape(AA, (15,))
    @test indextype(B) <: (Reshaped.IndexMD{1,1},)
    @test B == [1:15;]
    @test myeq(B, [1:15;])
    BB = Reshaped.myreshape(A, (15,))
    @test indextype(BB) <: (Reshaped.IndexMD{1,1},)  # because LinearFast
    @test BB == [1:15;]
    @test myeq(BB, [1:15;])

    C = reshape(1:16, (2,4,2))
    CR = Reshaped.myreshape(C, (4,4))
    @test indextype(CR) <: (Reshaped.IndexMD{1,2},)  # because LinearFast
    @test CR == reshape(1:16, 4, 4)
    @test myeq(CR, reshape(1:16, 4, 4))
    CR2 = Reshaped.myreshape(C, (8,2))
    @test indextype(CR) <: (Reshaped.IndexMD{1,2},)  # because LinearFast
    @test CR2 == reshape(1:16, 8, 2)
    @test myeq(CR2, reshape(1:16, 8, 2))

    V = sub(CR, 2:3,2:4)
    @test V == [6 10 14; 7 11 15]
    VR = Reshaped.myreshape(V, (3,2))
    @test indextype(VR) <: (Reshaped.IndexMD{2,2},)
    @test VR == [6 11; 7 14; 10 15]
    @test myeq(VR, [6 11; 7 14; 10 15])

    A3 = reshape(1:5*7*8, 5, 7, 8)
    S = sub(A3, 2:5, 1:3:7, 3:5)
    R = Reshaped.myreshape(S, (length(S),))
    @test indextype(R) <: (Reshaped.IndexMD{3,1},)
    Sc = copy(S)
    @test R == Sc[:]
    @test myeq(R, Sc[:])
    R = Reshaped.myreshape(S, (12,3))
    @test indextype(R) <: (Reshaped.IndexMD{2,1},Colon)
    @test R == reshape(Sc, (12,3))
    @test myeq(R, reshape(Sc, (12,3)))
    R = Reshaped.myreshape(S, (4,9))
    @test indextype(R) <: (Colon,Reshaped.IndexMD{2,1})
    @test R == reshape(Sc, (4,9))
    @test myeq(R, reshape(Sc, (4,9)))
    R = Reshaped.myreshape(S, (6,6))
    @test indextype(R) <: (Reshaped.IndexMD{3,2},)
    @test R == reshape(Sc, (6,6))
    @test myeq(R, reshape(Sc, (6,6)))

    # Cases with dimension sizes of 1, some of which require MultiplicativeInverse1
    A = reshape(1:15, (1,1,15))
    R = Reshaped.myreshape(A, (15,))
    @test indextype(R) <: (Reshaped.IndexMD{1,1},)
    @test R == [1:15;]
    @test myeq(R, [1:15;])
    R = Reshaped.myreshape(1:15, (1,1,15,))
    @test indextype(R) <: (Reshaped.IndexMD{1,3},)
    @test A == R
    @test myeq(A, R)
    R = Reshaped.myreshape(1:15, (15,1))
    @test indextype(R) <: (Reshaped.IndexMD{1,2},)
    A2 = reshape(1:15, 15, 1)
    @test R == A2
    @test myeq(R, A2)
    R = Reshaped.myreshape(1:15, (3,1,5))
    @test indextype(R) <: (Reshaped.IndexMD{1,3},)
    # Make sure we test LinearSlow
    A = reshape(1:90, 3, 2, 15)
    S = sub(A, 2, 1:2, :)
    R = Reshaped.myreshape(S, (2, 3, 5))
    @test indextype(R) <: (Reshaped.IndexMD{2,1}, Reshaped.IndexMD{1,2},)
    Sc = reshape(copy(S), (2, 3, 5))
    @test R == Sc
    @test myeq(R, Sc)
end
