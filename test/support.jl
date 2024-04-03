using Test

@testset "enforce_stable" begin
    @test @enforce_stable(2 + 2) == 4

    global x = 1
    @test_throws Exception @enforce_stable(2 + x) == 4

    @test @enforce_stable begin
        @allow_unstable begin
            2+x
        end
    end == 4

    @test_throws Exception @enforce_stable begin
        @allow_unstable begin
            2+x
        end
        x + 2
    end
end

function my_hot_function(args...)
    # ...setup...
    x,y = 1, 2 # ...
    z = 0
    # hot regions must stay performant
    @enforce_stable begin
        for i in 1:1000
            # ...
            z = x + y + z
            # ...
            i % 100 == 0 && @allow_unstable @info "computed $z"
        end
    end
    return z
end
@testset "@enforce_stable complex example" begin
    @test my_hot_function() == 3
end
