# This file is a part of Julia. License is MIT: https://julialang.org/license

include("setup_Compiler.jl")

@testset "CachedMethodTable" begin
    # cache result should be separated per `limit` and `sig`
    # https://github.com/JuliaLang/julia/pull/46799
    interp = Compiler.NativeInterpreter()
    table = Compiler.method_table(interp)
    sig = Tuple{typeof(*), Any, Any}
    result1 = Compiler.findall(sig, table; limit=-1)
    result2 = Compiler.findall(sig, table; limit=Compiler.InferenceParams().max_methods)
    @test result1 !== nothing && !Compiler.isempty(result1)
    @test result2 === nothing
end

@testset "BitSetBoundedMinPrioritySet" begin
    bsbmp = Compiler.BitSetBoundedMinPrioritySet(5)
    Compiler.push!(bsbmp, 2)
    Compiler.push!(bsbmp, 2)
    iterateok = true
    cnt = 0
    @eval Compiler for v in $bsbmp
        if cnt == 0
            iterateok &= v == 2
        elseif cnt == 1
            iterateok &= v == 5
        else
            iterateok = false
        end
        cnt += 1
    end
    @test iterateok
    @test Compiler.popfirst!(bsbmp) == 2
    Compiler.push!(bsbmp, 1)
    @test Compiler.popfirst!(bsbmp) == 1
    @test Compiler.isempty(bsbmp)
end

@testset "basic heap functionality" begin
    v = [2,3,1]
    @test Compiler.heapify!(v, Compiler.Forward) === v
    @test Compiler.heappop!(v, Compiler.Forward) === 1
    @test Compiler.heappush!(v, 4, Compiler.Forward) === v
    @test Compiler.heappop!(v, Compiler.Forward) === 2
    @test Compiler.heappop!(v, Compiler.Forward) === 3
    @test Compiler.heappop!(v, Compiler.Forward) === 4
end

@testset "randomized heap correctness tests" begin
    order = Compiler.By(x -> -x[2])
    for i in 1:6
        heap = Tuple{Int, Int}[(rand(1:i), rand(1:i)) for _ in 1:2i]
        mock = copy(heap)
        @test Compiler.heapify!(heap, order) === heap
        sort!(mock, by=last)

        for _ in 1:6i
            if rand() < .5 && !isempty(heap)
                # The first entries may differ because heaps are not stable
                @test last(Compiler.heappop!(heap, order)) === last(pop!(mock))
            else
                new = (rand(1:i), rand(1:i))
                Compiler.heappush!(heap, new, order)
                push!(mock, new)
                sort!(mock, by=last)
            end
        end
    end
end

@testset "searchsorted" begin
    @test Compiler.searchsorted([1, 1, 2, 2, 3, 3], 0) === Compiler.UnitRange(1, 0)
    @test Compiler.searchsorted([1, 1, 2, 2, 3, 3], 1) === Compiler.UnitRange(1, 2)
    @test Compiler.searchsorted([1, 1, 2, 2, 3, 3], 2) === Compiler.UnitRange(3, 4)
    @test Compiler.searchsorted([1, 1, 2, 2, 3, 3], 4) === Compiler.UnitRange(7, 6)
    @test Compiler.searchsorted([1, 1, 2, 2, 3, 3], 2.5; lt=<) === Compiler.UnitRange(5, 4)

    @test Compiler.searchsorted(Compiler.UnitRange(1, 3), 0) === Compiler.UnitRange(1, 0)
    @test Compiler.searchsorted(Compiler.UnitRange(1, 3), 1) === Compiler.UnitRange(1, 1)
    @test Compiler.searchsorted(Compiler.UnitRange(1, 3), 2) === Compiler.UnitRange(2, 2)
    @test Compiler.searchsorted(Compiler.UnitRange(1, 3), 4) === Compiler.UnitRange(4, 3)

    @test Compiler.searchsorted([1:10;], 1, by=(x -> x >= 5)) === Compiler.UnitRange(1, 4)
    @test Compiler.searchsorted([1:10;], 10, by=(x -> x >= 5)) === Compiler.UnitRange(5, 10)
    @test Compiler.searchsorted([1:5; 1:5; 1:5], 1, 6, 10, Compiler.Forward) === Compiler.UnitRange(6, 6)
    @test Compiler.searchsorted(fill(1, 15), 1, 6, 10, Compiler.Forward) === Compiler.UnitRange(6, 10)

    for (rg,I) in Any[(Compiler.UnitRange(49, 57),   47:59),
                      (Compiler.StepRange(1, 2, 17), -1:19)]
        rg_r = Compiler.reverse(rg)
        rgv, rgv_r = Compiler.collect(rg), Compiler.collect(rg_r)
        for i = I
            @test Compiler.searchsorted(rg,i) === Compiler.searchsorted(rgv,i)
            @test Compiler.searchsorted(rg_r,i,rev=true) === Compiler.searchsorted(rgv_r,i,rev=true)
        end
    end
end

@testset "basic sort" begin
    v = [3,1,2]
    @test v == [3,1,2]
    @test Compiler.sort!(v) === v == [1,2,3]
    @test Compiler.sort!(v, by = x -> -x) === v == [3,2,1]
    @test Compiler.sort!(v, by = x -> -x, < = >) === v == [1,2,3]
end

@testset "randomized sorting tests" begin
    for n in [0, 1, 3, 10, 30, 100, 300], k in [0, 30, 2n]
        v = rand(-1:k, n)
        for by in [identity, x -> -x, x -> x^2 + .1x], lt in [<, >]
            @test sort(v; by, lt) == Compiler.sort!(copy(v); by, < = lt)
        end
    end
end
