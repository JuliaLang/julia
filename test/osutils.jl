@test !Base.is_unix(:Windows)
@test !Base.is_linux(:Windows)
@test Base.is_linux(:Linux)
@test Base.is_windows(:Windows)
@test Base.is_windows(:NT)
@test !Base.is_windows(:Darwin)
@test Base.is_apple(:Darwin)
@test Base.is_apple(:Apple)
@test !Base.is_apple(:Windows)
@test Base.is_unix(:Darwin)
@test Base.is_unix(:FreeBSD)
@test_throws ArgumentError Base.is_unix(:BeOS)
if !is_windows()
    @test Sys.windows_version() === (0, 0)
else
    @test (Sys.windows_version()::Tuple{Int,Int})[1] > 0
end

@test (@static true ? 1 : 2) === 1
@test (@static false ? 1 : 2) === 2
@test (@static if true 1 end) === 1
@test (@static if false 1 end) === nothing
@test (@static true && 1) === 1
@test (@static false && 1) === false
@test (@static true || 1) === true
@test (@static false || 1) === 1
