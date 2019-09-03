# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "Operating system predicates" begin
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
    for bsd in (:FreeBSD, :OpenBSD, :NetBSD, :DragonFly)
        f = Symbol("is", lowercase(String(bsd)))
        q = QuoteNode(bsd)
        @eval begin
            @test Sys.$f($q)
            @test Sys.isbsd($q)
            @test Sys.isunix($q)
            @test !Sys.isapple($q)
        end
    end
    @test_throws ArgumentError Sys.isunix(:BeOS)
    if !Sys.iswindows()
        @test Sys.windows_version() == v"0.0.0"
    else
        @test Sys.windows_version() >= v"1.0.0-"
    end
end

@testset "@static" begin
    @test (@static true ? 1 : 2) === 1
    @test (@static false ? 1 : 2) === 2
    @test (@static if true 1 end) === 1
    @test (@static if false 1 end) === nothing
    @test (@static true && 1) === 1
    @test (@static false && 1) === false
    @test (@static true || 1) === true
    @test (@static false || 1) === 1
    @test (@static if false 1 elseif true 2 end) === 2
    @test (@static if false 1 elseif false 2 end) === nothing
    @test (@static if false 1 elseif false 2 else 3 end) === 3
    @test (@static if false 1 elseif false 2 elseif true && false 3 else 4 end) === 4
    @test (@static if false 1 elseif false 2 elseif true && false 3 end) === nothing
end

if Sys.iswindows()
    @testset "path variables use correct path delimiters on windows" begin
        for path in (Base.SYSCONFDIR, Base.DATAROOTDIR, Base.DOCDIR,
                     Base.LIBDIR, Base.PRIVATE_LIBDIR, Base.INCLUDEDIR)
            @test !occursin("/", path)
            @test !occursin("\\\\", path)
        end
    end
end
