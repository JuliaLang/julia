
function llvm_dump(f, types)
    lambda = getmethods(f, types)[1][3]
    llvm_dump_lambda(lambda)
end


