if Base.generating_output()
let
    function example_payload()
        @testset "example" begin
            @test 1 == 1
            @test_throws ErrorException error()
            @test_logs (:info, "Doing foo with n=2") @info "Doing foo with n=2"
            @test_broken 1 == 2
            @test 1 ≈ 1.0000000000000001
        end
    end

    redirect_stdout(example_payload, devnull)
end
end
