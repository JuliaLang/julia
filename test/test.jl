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


# Test with_handler
successflag = false
failureflag = false
errorflag = false
test_handler(r::Test.Success) = !successflag
test_handler(r::Test.Failure) = !failureflag
test_handler(r::Test.Error) = !errorflag

Test.with_handler(test_handler) do
    @test true
    @test successflag
    @test !failureflag
    @test !errorflag
    successflag = false
    @test false
    @test !successflag
    @test failureflag
    @test !errorflag
    failureflag = false
    @test error("throw error")
    @test !successflag
    @test !failureflag
    @test errorflag
end


# Test @test_throws
domainerror_thrower() = throw(DomainError())
boundserror_thrower() = throw(BoundsError())
error_thrower() = error("An error happened")
@test_throws DomainError domainerror_thrower()
@test_throws BoundsError boundserror_thrower()

failureflag = false
successflag = false
Test.with_handler(test_handler) do
    @test_throws DomainError boundserror_thrower()
    @test failureflag
    @test_throws DomainError domainerror_thrower()
    @test successflag
end


# Test @test_approx_eq
# TODO
@test isapprox(.1+.1+.1, .3)
@test !isapprox(.1+.1+.1, .4)


# Test @test_approx_eq_eps
# TODO

