using Test

@testset "CachedMethodTable" begin
    # cache result should be separated per `limit` and `sig`
    # https://github.com/JuliaLang/julia/pull/46799
    interp = Core.Compiler.NativeInterpreter()
    table = Core.Compiler.method_table(interp)
    sig = Tuple{typeof(*), Any, Any}
    result1 = Core.Compiler.findall(sig, table; limit=-1)
    result2 = Core.Compiler.findall(sig, table; limit=Core.Compiler.get_max_methods(*, @__MODULE__, interp))
    @test result1 !== Core.Compiler.missing && !Core.Compiler.isempty(result1.matches)
    @test result2 === Core.Compiler.missing
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
