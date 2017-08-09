# This file is a part of Julia. License is MIT: https://julialang.org/license

@test !Sys.isunix(:Windows)
@test !Sys.islinux(:Windows)
@test Sys.islinux(:Linux)
@test Sys.iswindows(:Windows)
@test Sys.iswindows(:NT)
@test !Sys.iswindows(:Darwin)
@test Sys.isapple(:Darwin)
@test Sys.isapple(:Apple)
@test !Sys.isapple(:Windows)
@test Sys.isunix(:Darwin)
@test Sys.isunix(:FreeBSD)
@test_throws ArgumentError Sys.isunix(:BeOS)
if !Sys.iswindows()
    @test Sys.windows_version() == v"0.0.0"
else
    @test Sys.windows_version() >= v"1.0.0-"
end

@test (@static true ? 1 : 2) === 1
@test (@static false ? 1 : 2) === 2
@test (@static if true 1 end) === 1
@test (@static if false 1 end) === nothing
@test (@static true && 1) === 1
@test (@static false && 1) === false
@test (@static true || 1) === true
@test (@static false || 1) === 1

# test that path variables use correct path delimiters on windows
if Sys.iswindows()
    @test !contains(Base.SYSCONFDIR, "/")
    @test !contains(Base.DATAROOTDIR, "/")
    @test !contains(Base.DOCDIR, "/")
    @test !contains(Base.LIBDIR, "/")
    @test !contains(Base.PRIVATE_LIBDIR, "/")
    @test !contains(Base.INCLUDEDIR, "/")
end
