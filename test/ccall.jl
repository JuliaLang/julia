ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (Int8,), x)
@assert ccall_test_func(3) == 1
@assert ccall_test_func(259) == 1
