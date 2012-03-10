# test file to test testing


test_context("Testing test tests")
# setup goes here

test_group("string tests")
@test strip("\t  hi   \n") == "hi"
@test strip("\t  this should fail   \n") == "hi"

test_group("array tests")



test_group("random tests")



test_group("exception tests")



test_group("printing tests")




# shutdown goes here
