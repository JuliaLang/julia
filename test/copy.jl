mainres = ([4, 5, 3],
           [1, 5, 3])
bitres = ([true, true, false],
          [false, true, false])

tsk(x) = @task for i in x; produce(i); end

for (dest, src, bigsrc, emptysrc, res) in [
    ([1, 2, 3], () -> [4, 5], () -> [1, 2, 3, 4, 5], () -> Int[], mainres),
    ([1, 2, 3], () -> 4:5, () -> 1:5, () -> 1:0, mainres),
    ([1, 2, 3], () -> tsk(4:5), () -> tsk(1:5), () -> tsk(1:0), mainres),
    (falses(3), () -> trues(2), () -> trues(5), () -> trues(0), bitres)]

    @test copy!(copy(dest), src()) == res[1]
    @test copy!(copy(dest), 1, src()) == res[1]
    @test copy!(copy(dest), 2, src(), 2) == res[2]
    @test copy!(copy(dest), 2, src(), 2, 1) == res[2]

    @test copy!(copy(dest), 99, src(), 99, 0) == dest

    @test copy!(copy(dest), 1, emptysrc()) == dest
    @test_throws BoundsError copy!(dest, 1, emptysrc(), 1)

    for idx in [0, 4]
        @test_throws BoundsError copy!(dest, idx, src())
        @test_throws BoundsError copy!(dest, idx, src(), 1)
        @test_throws BoundsError copy!(dest, idx, src(), 1, 1)
        @test_throws BoundsError copy!(dest, 1, src(), idx)
        @test_throws BoundsError copy!(dest, 1, src(), idx, 1)
    end

    @test_throws BoundsError copy!(dest, 1, src(), 1, -1)

    @test_throws BoundsError copy!(dest, bigsrc())

    @test_throws BoundsError copy!(dest, 3, src())
    @test_throws BoundsError copy!(dest, 3, src(), 1)
    @test_throws BoundsError copy!(dest, 3, src(), 1, 2)

    @test_throws BoundsError copy!(dest, 1, src(), 2, 2)
end
