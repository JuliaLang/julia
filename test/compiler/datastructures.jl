using Test

@testset "CachedMethodTable" begin
    # cache result should be separated per `limit` and `sig`
    # https://github.com/JuliaLang/julia/pull/46799
    interp = Core.Compiler.NativeInterpreter()
    table = Core.Compiler.method_table(interp)
    sig = Tuple{typeof(*), Any, Any}
    result1 = Core.Compiler.findall(sig, table; limit=-1)
    result2 = Core.Compiler.findall(sig, table; limit=Core.Compiler.InferenceParams().max_methods)
    @test result1 !== nothing && !Core.Compiler.isempty(result1.matches)
    @test result2 === nothing
end

@testset "BitSetBoundedMinPrioritySet" begin
    bsbmp = Core.Compiler.BitSetBoundedMinPrioritySet(5)
    Core.Compiler.push!(bsbmp, 2)
    Core.Compiler.push!(bsbmp, 2)
    @test Core.Compiler.popfirst!(bsbmp) == 2
    Core.Compiler.push!(bsbmp, 1)
    @test Core.Compiler.popfirst!(bsbmp) == 1
    @test Core.Compiler.isempty(bsbmp)
end

@testset "basic heap functionality" begin
    v = [2,3,1]
    @test Core.Compiler.heapify!(v, Core.Compiler.Forward) === v
    @test Core.Compiler.heappop!(v, Core.Compiler.Forward) === 1
    @test Core.Compiler.heappush!(v, 4, Core.Compiler.Forward) === v
    @test Core.Compiler.heappop!(v, Core.Compiler.Forward) === 2
    @test Core.Compiler.heappop!(v, Core.Compiler.Forward) === 3
    @test Core.Compiler.heappop!(v, Core.Compiler.Forward) === 4
end

@testset "randomized heap correctness tests" begin
    order = Core.Compiler.By(x -> -x[2])
    for i in 1:6
        heap = Tuple{Int, Int}[(rand(1:i), rand(1:i)) for _ in 1:2i]
        mock = copy(heap)
        @test Core.Compiler.heapify!(heap, order) === heap
        sort!(mock, by=last)

        for _ in 1:6i
            if rand() < .5 && !isempty(heap)
                # The first entries may differ because heaps are not stable
                @test last(Core.Compiler.heappop!(heap, order)) === last(pop!(mock))
            else
                new = (rand(1:i), rand(1:i))
                Core.Compiler.heappush!(heap, new, order)
                push!(mock, new)
                sort!(mock, by=last)
            end
        end
    end
end

@testset "searchsorted" begin
    @test Core.Compiler.searchsorted([1, 1, 2, 2, 3, 3], 0) === Core.Compiler.UnitRange(1, 0)
    @test Core.Compiler.searchsorted([1, 1, 2, 2, 3, 3], 1) === Core.Compiler.UnitRange(1, 2)
    @test Core.Compiler.searchsorted([1, 1, 2, 2, 3, 3], 2) === Core.Compiler.UnitRange(3, 4)
    @test Core.Compiler.searchsorted([1, 1, 2, 2, 3, 3], 4) === Core.Compiler.UnitRange(7, 6)
    @test Core.Compiler.searchsorted([1, 1, 2, 2, 3, 3], 2.5; lt=<) === Core.Compiler.UnitRange(5, 4)

    @test Core.Compiler.searchsorted(Core.Compiler.UnitRange(1, 3), 0) === Core.Compiler.UnitRange(1, 0)
    @test Core.Compiler.searchsorted(Core.Compiler.UnitRange(1, 3), 1) === Core.Compiler.UnitRange(1, 1)
    @test Core.Compiler.searchsorted(Core.Compiler.UnitRange(1, 3), 2) === Core.Compiler.UnitRange(2, 2)
    @test Core.Compiler.searchsorted(Core.Compiler.UnitRange(1, 3), 4) === Core.Compiler.UnitRange(4, 3)

    @test Core.Compiler.searchsorted([1:10;], 1, by=(x -> x >= 5)) === Core.Compiler.UnitRange(1, 4)
    @test Core.Compiler.searchsorted([1:10;], 10, by=(x -> x >= 5)) === Core.Compiler.UnitRange(5, 10)
    @test Core.Compiler.searchsorted([1:5; 1:5; 1:5], 1, 6, 10, Core.Compiler.Forward) === Core.Compiler.UnitRange(6, 6)
    @test Core.Compiler.searchsorted(fill(1, 15), 1, 6, 10, Core.Compiler.Forward) === Core.Compiler.UnitRange(6, 10)

    for (rg,I) in Any[(Core.Compiler.UnitRange(49, 57),   47:59),
                      (Core.Compiler.StepRange(1, 2, 17), -1:19)]
        rg_r = Core.Compiler.reverse(rg)
        rgv, rgv_r = Core.Compiler.collect(rg), Core.Compiler.collect(rg_r)
        for i = I
            @test Core.Compiler.searchsorted(rg,i) === Core.Compiler.searchsorted(rgv,i)
            @test Core.Compiler.searchsorted(rg_r,i,rev=true) === Core.Compiler.searchsorted(rgv_r,i,rev=true)
        end
    end
end

@testset "basic sort" begin
    v = [3,1,2]
    @test v == [3,1,2]
    @test Core.Compiler.sort!(v) === v == [1,2,3]
    @test Core.Compiler.sort!(v, by = x -> -x) === v == [3,2,1]
    @test Core.Compiler.sort!(v, by = x -> -x, < = >) === v == [1,2,3]
end

@testset "randomized sorting tests" begin
    for n in [0, 1, 3, 10, 30, 100, 300], k in [0, 30, 2n]
        v = rand(-1:k, n)
        for by in [identity, x -> -x, x -> x^2 + .1x], lt in [<, >]
            @test sort(v; by, lt) == Core.Compiler.sort!(copy(v); by, < = lt)
        end
    end
end
