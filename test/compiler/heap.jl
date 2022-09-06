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
