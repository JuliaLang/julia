using Statistics, Distributed, Test

# Create QueuePool to call `do_work()` on every submitted job
qp_workers = addprocs(4)

@testset "QueuePool" begin
    @testset "Simple push!()/take!()" begin
        qp = QueuePool(qp_workers, identity)

        # Queue many jobs (they will immediately start processing)
        for idx in 1:100
            push!(qp, idx)
        end

        # First, test that we can fetch results in the order we ask
        @test take!(qp, 1) == 1
        @test take!(qp, 4) == 4
        @test take!(qp, 10) == 10
        @test take!(qp, 100) == 100

        # Next, test fetching results in a greedy way gets literally anything
        # other than what we've already gotten:
        for idx in 1:50
            @test !in(take!(qp), [1, 4, 10, 100])
        end

        # Ensure we can explicitly call `close()`
        close(qp)
    end

    @testset "Setup/Work function" begin
        # Create test that uses functions that must be imported
        @everywhere function do_work(x, y)
            # Be sure to import Statistics for `mean()`
            sleep(rand(1:100)./10000.0)
            return mean((x .+ y).^2)
        end

        qp = QueuePool(qp_workers, do_work; setup = :(using Statistics))
        for idx in 1:4
            push!(qp, randn(10, 10), randn(10))
        end

        for idx in 1:4
            z = take!(qp)
            @test z > 0.0 && z < 10.0
        end
    end

    @testset "Timeouts" begin
        # Finally, test timeouts
        qp = QueuePool(qp_workers, x -> sleep(x); queue_size = 10)

        for idx in 0:4
            push!(qp, idx*1000)
        end
        @test take!(qp, 1; timeout=10) == nothing
        @test_throws ErrorException take!(qp, 2; timeout=10)
    end
end

rmprocs(qp_workers...; waitfor=10)

