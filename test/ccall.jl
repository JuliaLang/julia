ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (Uint8,), x)
@assert ccall_test_func(3) == 1
@assert ccall_test_func(259) == 1

@assert ccall("llvm.sqrt.f64", Float64, (Float64,), 2) == sqrt(2)
try
	ccall("llvm.nonexistentintrinsic", Float64, (Float64,), 2)
	error("ccall of non-existent intrinsic should throw")
catch
end
