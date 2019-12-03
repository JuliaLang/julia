@testset "shrink small array" begin
    x = [1, 2, 3, 4]
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 4
    sizehint!(x, 10000)
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 10000
    sizehint!(x, 4)
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 4

    x = [1, 2, 3, 4]
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 4
    sizehint!(x, 1000000)
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 1000000
    sizehint!(x, 4)
    @test x[1] == 1
    @test x[2] == 2
    @test x[3] == 3
    @test x[4] == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == 4
    @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == 4
end
