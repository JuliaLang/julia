ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (Uint8,), x)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1
