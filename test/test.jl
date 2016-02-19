# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

# test file to test testing

# Test @test
@test true
@test 1 == 1
@test 1 != 2
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") != "hi"

a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@test a[1,1,1,1,1] == 10
@test a[1,1,1,1,1] != 2

@test rand() != rand()

# Test printing of Pass results
# Pass - constant
@test contains(sprint(show, @test true), "Expression: true")
# Pass - expression
@test contains(sprint(show, @test 10 == 2*5), "Evaluated: 10 == 10")
@test contains(sprint(show, @test !false), "Expression: !false")
# Pass - exception
@test contains(sprint(show, @test_throws ErrorException error()),
                "Thrown: ErrorException")

# Test printing of Fail results
type NoThrowTestSet <: Base.Test.AbstractTestSet
    results::Vector
    NoThrowTestSet(desc) = new([])
end
Base.Test.record(ts::NoThrowTestSet, t::Base.Test.Result) = (push!(ts.results, t); t)
Base.Test.finish(ts::NoThrowTestSet) = ts.results
fails = @testset NoThrowTestSet begin
    # Fail - wrong exception
    @test_throws OverflowError error()
    # Fail - no exception
    @test_throws OverflowError 1 + 1
    # Fail - const
    @test false
    # Fail - comparison
    @test 1+1 == 2+2
end
for i in 1:4
    @test isa(fails[i], Base.Test.Fail)
end
@test contains(sprint(show, fails[1]), "Thrown: ErrorException")
@test contains(sprint(show, fails[2]), "No exception thrown")
@test contains(sprint(show, fails[3]), "Evaluated: false")
@test contains(sprint(show, fails[4]), "Evaluated: 2 == 4")

# Test printing of a TestSetException
tse_str = sprint(show, Test.TestSetException(1,2,3))
@test contains(tse_str, "1 passed")
@test contains(tse_str, "2 failed")
@test contains(tse_str, "3 errored")

@test Test.finish(Test.FallbackTestSet()) != nothing

OLD_STDOUT = STDOUT
catch_out = IOStream("")
rd, wr = redirect_stdout()

# Check that the fallback test set throws immediately
@test_throws ErrorException (@test 1 == 2)

@testset "no errors" begin
    @test true
    @test 1 == 1
end

# Test entirely empty test set
@testset "outer" begin
    @testset "inner" begin
    end
end

try

@testset "outer" begin
    @testset "inner1" begin
        @test true
        @test false
        @test 1 == 1
        @test 2 == :foo
        @test 3 == 3
        @testset "d" begin
            @test 4 == 4
        end
        @testset begin
            @test :blank != :notblank
        end
    end
    @testset "inner1" begin
        @test 1 == 1
        @test 2 == 2
        @test 3 == :bar
        @test 4 == 4
        @test_throws ErrorException 1+1
        @test_throws ErrorException error()
        @test_throws RemoteException error()
        @testset "errrrr" begin
            @test "not bool"
            @test error()
        end

        error("exceptions in testsets should be caught")
        @test 1 == 1 # this test will not be run
    end

    @testset "loop with desc" begin
        @testset "loop1 $T" for T in (Float32, Float64)
            @test 1 == T(1)
        end
    end
    @testset "loops without desc" begin
        @testset for T in (Float32, Float64)
            @test 1 == T(1)
        end
        @testset for T in (Float32, Float64), S in (Int32,Int64)
            @test S(1) == T(1)
        end
    end
    srand(123)
    @testset "some loops fail" begin
        @testset for i in 1:5
            @test i <= rand(1:10)
        end
        # should add 3 errors and 3 passing tests
        @testset for i in 1:6
            iseven(i) || error("error outside of test")
            @test true # only gets run if the above passed
        end
    end
end
    # These lines shouldn't be called
    redirect_stdout(OLD_STDOUT)
    error("No exception was thrown!")
catch ex

    @test isa(ex, Test.TestSetException)
    @test ex.pass  == 24
    @test ex.fail  == 6
    @test ex.error == 6
end

# Test @test_approx_eq
# TODO
@test isapprox(.1+.1+.1, .3)
@test !isapprox(.1+.1+.1, .4)

@test_throws ErrorException Test.test_approx_eq(ones(10),ones(11),1e-8,"a","b")
@test_throws ErrorException Test.test_approx_eq(ones(10),zeros(10),1e-8,"a","b")

# Test @test_approx_eq_eps
# TODO

ts = @testset "@testset should return the testset" begin
    @test true
end
@test typeof(ts) == Base.Test.DefaultTestSet
@test typeof(ts.results[1]) == Base.Test.Pass

tss = @testset "@testset/for should return an array of testsets: $i" for i in 1:3
    @test true
end
@test length(tss) == 3
@test typeof(tss[1]) == Base.Test.DefaultTestSet
@test typeof(tss[1].results[1]) == Base.Test.Pass


DTS = Base.Test.DefaultTestSet   # So we need not import it...
global global_var = 0
nonglobal_var = 0
@testset "@testset should run in scope where macro called" begin
    @testset "no testset type specified" begin
        global global_var += 1
        nonglobal_var += 1
    end
    global global_var
    @test global_var == 1
    @test nonglobal_var == 1

    @testset DTS "default testset" begin
        global global_var += 2
        nonglobal_var += 3
    end
    @test global_var == 3
    @test nonglobal_var == 4
end


@testset "default testset takes a skip argument" begin
    skipped = true
    @testset skip=true begin
        skipped = false
    end
    @test skipped == true
end

counter = 0
@testset "default testset can repeatedly run the tests" begin
    @testset repeats=2 begin
        counter += 1
        @test true
    end
    @test counter == 2

    @testset repeats=11 begin
        counter += 1
        @test counter < 100
    end
    @test counter == (2+11)

    @testset skip=true repeats=33 "skip takes precedence over repeats" begin
        counter += 1
    end
    @test counter == (2+11) # Still same value since we skipped
end


# now we're done running tests with DefaultTestSet so we can go back to STDOUT
redirect_stdout(OLD_STDOUT)


# import the methods needed for defining our own testset type
import Base.Test: record, finish, should_run
using Base.Test: get_testset_depth, get_testset
using Base.Test: AbstractTestSet, Result, Pass, Fail, Error
immutable CustomTestSet <: Base.Test.AbstractTestSet
    description::AbstractString
    foo::Int
    results::Vector
    reps::Int
    # constructor takes a description string and options keyword arguments
    CustomTestSet(desc; foo=1, reps=1) = new(desc, foo, [], reps)
end

record(ts::CustomTestSet, child::AbstractTestSet) = push!(ts.results, child)
record(ts::CustomTestSet, res::Result) = push!(ts.results, res)
function finish(ts::CustomTestSet)
    # just record if we're not the top-level parent
    if get_testset_depth() > 0
        record(get_testset(), ts)
    end
    ts
end
function should_run(ts::CustomTestSet, nruns::Int)
    (nruns < ts.reps) # Repeatedly run tests `reps` times
end

# These need to be global since the assignment to ts below affects scoping
global counter_inner = 0
global counter_outer = 0
ts = @testset CustomTestSet "Testing custom testsets" begin
    # this testset should inherit the parent testset type
    @testset "custom testset inner 1" begin
        @test true
        @test false
        @test error("this error will be reported as an error")
        @test_throws ErrorException nothing
        @test_throws ErrorException error("this error is a success")
    end
    # this testset has its own testset type
    @testset CustomTestSet foo=4 "custom testset inner 2" begin
        # this testset should inherit the type, but not the argument. If a particular
        # testset type wants inheritance behavior they should implement it themselves
        # using get_testset() in the constructor
        @testset "custom testset inner 2 inner 1" begin
            @test true
        end
        # make sure the RHS can use computed values, also tests options without
        # specifying the testset type
        @testset foo=(1+2) "custom testset inner 2 inner 2" begin
            @test true
        end
    end
    # this testset should inherit the parent testset type but change the reps arg
    @testset reps=2 "custom testset inner 3" begin
        global counter_outer += 1
        @test true
        @test false
        @test error("this error will be reported as an error")
        @test_throws ErrorException nothing
        @test_throws ErrorException error("this error is a success")
        @testset reps=3 "custom testset inner 3.1" begin
            global counter_inner += 1
        end
    end
end

@test typeof(ts) == CustomTestSet
@test ts.foo == 1
@test ts.description == "Testing custom testsets"
@test typeof(ts.results[1]) == CustomTestSet
@test ts.results[1].description == "custom testset inner 1"
@test ts.results[1].foo == 1
@test typeof(ts.results[1].results[1]) == Pass
@test typeof(ts.results[1].results[2]) == Fail
@test typeof(ts.results[1].results[3]) == Error
@test typeof(ts.results[1].results[4]) == Fail
@test typeof(ts.results[1].results[5]) == Pass

@test typeof(ts.results[2]) == CustomTestSet
@test ts.results[2].description == "custom testset inner 2"
@test ts.results[2].foo == 4
@test typeof(ts.results[2].results[1]) == CustomTestSet
@test ts.results[2].results[1].foo == 1
@test typeof(ts.results[2].results[1].results[1]) == Pass
@test typeof(ts.results[2].results[2]) == CustomTestSet
@test ts.results[2].results[2].foo == 3

@test typeof(ts.results[3]) == CustomTestSet
@test ts.results[3].description == "custom testset inner 3"
@test counter_outer == 2
@test counter_inner == 6
@test length(ts.results[3].results) == 2*6 # 2 outer loops of 6 inner "tests" each
for j in 1:2
    i = (j-1)*6
    @test typeof(ts.results[3].results[i+1]) == Pass
    @test typeof(ts.results[3].results[i+2]) == Fail
    @test typeof(ts.results[3].results[i+3]) == Error
    @test typeof(ts.results[3].results[i+4]) == Fail
    @test typeof(ts.results[3].results[i+5]) == Pass
    @test typeof(ts.results[3].results[i+6]) == CustomTestSet
    @test ts.results[3].results[i+6].description == "custom testset inner 3.1"
    @test ts.results[3].results[i+6].reps == 3
end


# test custom testset types on testset/for
tss = @testset CustomTestSet foo=3 "custom testset $i" for i in 1:6
    @testset "inner testset $i-$j" for j in 1:3
        @test iseven(i + j)
    end
    # make sure a testset within a testset/for works
    @testset "inner testset $i" begin
        @test iseven(i)
    end
end


for i in 1:6
    @test typeof(tss[i]) == CustomTestSet
    @test tss[i].foo == 3
    for j in 1:3
        @test typeof(tss[i].results[j]) == CustomTestSet
        @test tss[i].results[j].foo == 1
        @test typeof(tss[i].results[j].results[1]) == (iseven(i+j) ? Pass : Fail)
    end
    @test typeof(tss[i].results[4]) == CustomTestSet
    @test typeof(tss[i].results[4].results[1]) == (iseven(i) ? Pass : Fail)
end


# A more useful test case for repeatable testsets is for finding rare bugs
# with random test data generation, aka property-based testing:
my_buggy_reverse(v) = (length(v) == 4) ? Int64[1] : reverse(v) # Obvious, seeded, unrealistic bug here
hit_one_of_length_4 = false
ts = @testset CustomTestSet reps=500 begin
    # Generate a random array of random length between 0-4.
    a = Int64[rand(1:10000) for i in 1:rand(0:4)]
    # A fundamental property of reverse is that if done twice we get orig ary
    @test my_buggy_reverse(my_buggy_reverse(a)) == a

    # Below code is only to handle the very unlikely case that our random testing
    # did not "hit" an array of length 4.
    # But we don't want to flag a failure below even if unlikely
    global hit_one_of_length_4
    hit_one_of_length_4 = hit_one_of_length_4 || (length(a) == 4)
end

@test length(ts.results) == 500
if hit_one_of_length_4 # Chance that we didn't is extremely small, 0.8^500
    numfails = find((r)->isa(r, Fail), ts.results)
    @test length(numfails) > 0
end
