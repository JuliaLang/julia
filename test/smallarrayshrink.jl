@testset "shrink small array" begin
    function check_array(x, size, capacity)
        @test x[1] == 1
        @test x[2] == 2
        @test x[3] == 3
        @test x[4] == 4
        @test ccall(:jl_array_size, Int, (Any, UInt), x, 0) == size
        @test ccall(:jl_array_size, Int, (Any, UInt), x, 1) == capacity
    end
    for hint_size = [10000, 1000000]
        x = [1, 2, 3, 4]
        check_array(x, 4, 4)
        sizehint!(x, hint_size)
        check_array(x, 4, hint_size)
        sizehint!(x, 4; shrink = false)
        check_array(x, 4, hint_size)
        sizehint!(x, 4)
        check_array(x, 4, 4)
    end
end
