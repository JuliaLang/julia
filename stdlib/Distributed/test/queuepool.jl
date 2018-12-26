using Statistics, Distributed, Test

#addprocs_with_testenv = addprocs

# Create QueuePool to call `do_work()` on every submitted job
qp_workers = addprocs_with_testenv(4)
qp = QueuePool(qp_workers, identity)

# Queue many jobs (they will immediately start processing)
for idx in 1:100
    push_job!(qp, idx)
end

# First, test that we can fetch results in the order we ask
@test fetch_result(qp, 1) == 1
@test fetch_result(qp, 4) == 4
@test fetch_result(qp, 10) == 10
@test fetch_result(qp, 100) == 100

# Next, test fetching results in a greedy way gets literally anything
# other than what we've already gotten:
for idx in 1:50
    @test !in(fetch_result(qp), [1, 4, 10, 100])
end

# Destroy the queuepool
close(qp)

# Create test that uses functions that must be imported
num_workers = 4
qp_workers = addprocs_with_testenv(num_workers)
@everywhere function do_work(x, y)
    # Be sure to import Statistics for `mean()`
    sleep(rand(1:100)./10000.0)
    return mean((x .+ y).^2)
end

qp = QueuePool(qp_workers, do_work; setup = :(using Statistics))
for idx in 1:4
    push_job!(qp, randn(10, 10), randn(10))
end

for idx in 1:4
    z = fetch_result(qp)
    @test z > 0.0 && z < 10.0
end
close(qp)


# Finally, test timeouts
qp_workers = addprocs_with_testenv(1)
qp = QueuePool(qp_workers, x -> sleep(x); queue_size = 10)

for idx in 0:4
    push_job!(qp, idx*1000)
end
@test fetch_result(qp, 1; timeout=10) == nothing
@test_throws ErrorException fetch_result(qp, 2; timeout=10)

close(qp)
