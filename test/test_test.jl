# test file to test testing
# run this with julia -L extras/test.jl -e "tests(\"test/test_test.jl\")"
require("nearequal.jl")
require("test.jl")

test_context("Testing test tests")
# setup goes here

test_group("string tests")
@test strip("\t  hi   \n") == "hi"
@testfails strip("\t  this should fail   \n") == "hi" 

test_group("numeric tests")
@test isapprox(.1+.1+.1, .3)
@testfails isapprox(.1+.1+.1, .4)

test_group("array tests")
a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@test a[1,1,1,1,1] == 10
@testfails a[1,1,1,1,1] == 2


test_group("random tests")
@test rand() != rand() # not very likely to fail
@testfails rand() == rand() # very likely to fail

test_group("exception tests")
@testfails complex(1,2) > 0 # fail
@test throws_exception(complex(1,2) > 0, MethodError)
@testfails throws_exception(complex(1,2) > 0, SystemError) 

test_group("printing tests")
@test sprint(show, :(1+2)) == ":( +(1, 2) )"
@test prints(print_joined, ([1,2,3], " : "), "1 : 2 : 3") # prints is a helper
@testfails prints(print_joined, ([1,2,3], " ! "), "1 : 2 : 3")

test_group("performance tests")
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@test fib(20) == 6765
@testfails takes_less_than(fib(20), 1e-6) 

# shutdown goes here
