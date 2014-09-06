# catch regressions like #8239

# redirect stdout and stderr to avoid spam during tests.
oldout = STDOUT
olderr = STDERR
redirect_stdout()
redirect_stderr()

@test code_native(ismatch, (Regex, String)) == nothing
@test code_native(+, (Int, Int)) == nothing
@test code_native(+, (Array{Float32}, Array{Float32})) == nothing

@test code_llvm(ismatch, (Regex, String)) == nothing
@test code_llvm(+, (Int, Int)) == nothing
@test code_llvm(+, (Array{Float32}, Array{Float32})) == nothing

redirect_stdout(oldout)
redirect_stdout(olderr)

@test_throws Exception code_native(+, Int, Int)
@test_throws Exception code_native(+, Array{Float32}, Array{Float32})

@test_throws Exception code_llvm(+, Int, Int)
@test_throws Exception code_llvm(+, Array{Float32}, Array{Float32})
