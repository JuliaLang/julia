# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test various constructors
c=Channel(1)
@test eltype(c) == Any
@test put!(c, 1) == 1
@test isready(c) == true
@test take!(c) == 1
@test isready(c) == false

@test eltype(Channel(1.0)) == Any

c=Channel{Int}(1)
@test eltype(c) == Int
@test_throws MethodError put!(c, "Hello")

c=Channel{Int}(Inf)
@test eltype(c) == Int
pvals = map(i->put!(c,i), 1:10^6)
tvals = Int[take!(c) for i in 1:10^6]
@test pvals == tvals

# Uncomment line below once deprecation support has been removed.
# @test_throws MethodError Channel()

@test_throws ArgumentError Channel(-1)
@test_throws InexactError Channel(1.5)

# Test multiple concurrent put!/take! on a channel for different sizes
function testcpt(sz)
    c = Channel{Int}(sz)
    size = 0
    inc() = size += 1
    dec() = size -= 1
    @sync for i = 1:10^4
        @async (sleep(rand()); put!(c, i); inc())
        @async (sleep(rand()); take!(c); dec())
    end
    @test size == 0
end
testcpt(0)
testcpt(1)
testcpt(32)
testcpt(Inf)

# Test multiple "for" loops waiting on the same channel which
# is closed after adding a few elements.
c=Channel(32)
results=[]
@sync begin
    for i in 1:20
        @async for i in c
            push!(results, i)
        end
    end
    sleep(1.0)
    for i in 1:5
        put!(c,i)
    end
    close(c)
end
@test sum(results) == 15

# Test channel iterator with done() being called multiple times
# This needs to be explicitly tested since `take!` is called
# in `done()` and not `next()`
c=Channel(32); foreach(i->put!(c,i), 1:10); close(c)
s=start(c)
@test done(c,s) == false
res = Int[]
while !done(c,s)
    @test done(c,s) == false
    v,s = next(c,s)
    push!(res,v)
end
@test res == Int[1:10...]


# Testing timedwait on multiple channels
@sync begin
    rr1 = Channel(1)
    rr2 = Channel(1)
    rr3 = Channel(1)

    callback() = all(map(isready, [rr1, rr2, rr3]))
    # precompile functions which will be tested for execution time
    @test !callback()
    @test timedwait(callback, 0.0) === :timed_out

    @async begin sleep(0.5); put!(rr1, :ok) end
    @async begin sleep(1.0); put!(rr2, :ok) end
    @async begin sleep(2.0); put!(rr3, :ok) end

    tic()
    timedwait(callback, Dates.Second(1))
    et=toq()
    # assuming that 0.5 seconds is a good enough buffer on a typical modern CPU
    try
        @assert (et >= 1.0) && (et <= 1.5)
        @assert !isready(rr3)
    catch
        warn("timedwait tests delayed. et=$et, isready(rr3)=$(isready(rr3))")
    end
    @test isready(rr1)
end
