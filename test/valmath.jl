@testset "Val math" begin
    @testset "Val +" begin
        @testset for x in -10:1:10, y in -10:1:10
            @test Val{x} + Val{y} == Val{x + y}
        end
    end

    @testset "Val -" begin
        @testset for x in -10:1:10, y in -10:1:10
            @test Val{x} - Val{y} == Val{x - y}
        end
    end

    @testset "Val *" begin
        @testset for x in -10:1:10, y in -10:1:10
            @test Val{x} * Val{y} == Val{x * y}
        end
    end
end
