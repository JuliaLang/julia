
function lambda_dump(f, types)
    lambda = getmethods(f, types)[1][3]
    # ccall(:jl_dump_lambda_info, Void, (Ptr{Void},), &lambda)
    llvm_dump(lambda)
end

f1(x::Int) = x + 55
