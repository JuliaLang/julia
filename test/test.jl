# test file to test testing


test_context("Testing test tests")
# setup goes here

test_group("string tests")
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") == "hi" #fail


test_group("numeric tests")
@test approx_eq(airy(1.8), 0.0470362)
@test approx_eq(airy(1, 1.8), 1 + -0.0685248) # fail, using helper function

test_group("array tests")
a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
@test a[1,1,1,1,1] == 10
@test a[1,1,1,1,1] == 2 # fail


test_group("random tests")
@test rand() != rand() # not very likely to fail
@test rand() == rand() # fail

test_group("exception tests")
@test complex(1,2) > 0 # fail

test_group("printing tests")
@test print_to_string(show, :(1+2)) == "+(1,2)"
# could we do something like:
# @test prints(show, ([1,2]), "[1, 2]")
# where prints() applies the 2nd-arg tuple to the first arg in print_to_string?

test_group("performance tests")
fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
@test fib(20) == 6765
# @test takes_less_than(fib(20), .1)

# shutdown goes here
