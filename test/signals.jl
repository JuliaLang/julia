using Base.Signals

@testset "signal_abbrev" begin
    @test signal_abbrev(0) === nothing
    for signum in 1:31
        sigabbrev = signal_abbrev(signum)
        @test !isempty(sigabbrev)
        @test all(c -> isuppercase(c) || isdigit(c), sigabbrev)
        @test !startswith(sigabbrev, "SIG")
    end
    @test signal_abbrev(32) === nothing
end

@testset "signal_name" begin
    @test signal_name(0) === nothing
    for signum in 1:31
        signame = signal_name(signum)
        @test length(signame) > 3
        @test all(c -> isuppercase(c) || isdigit(c), signame)
        @test startswith(signame, "SIG")
    end
    @test signal_name(32) === nothing
end
