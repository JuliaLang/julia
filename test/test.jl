# test file to test testing


test_context("Testing test tests")
# setup goes here

test_group("string tests")
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") == "hi" #fail


test_group("numeric tests")
@test approx_eq(airy(1.8), 0.0470362)
@test approx_eq(airy(1, 1.8), 1 + -0.0685248) # fail

test_group("array tests")



test_group("random tests")



test_group("exception tests")



test_group("printing tests")




# shutdown goes here
