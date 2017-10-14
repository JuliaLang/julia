# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

script = """
# Issue #11948
f(x) = x+1
workspace()
@assert @__MODULE__() === Main
@assert isdefined(Main, :f)
@assert !@isdefined LastMain
@eval Core.Main begin
    @assert @__MODULE__() === Main
    @assert !isdefined(Main, :f)
    LastMain.f(2)

    # PR #12990
    io = IOBuffer()
    show(io, Pair)
    @assert String(take!(io)) == "Pair"
    @assert !Base.inbase(LastMain)
end
"""
exename = Base.julia_cmd()
@test success(pipeline(`$exename --startup-file=no -e $script`, stdout=STDOUT, stderr=STDERR))

# issue #17764
script2 = """
mutable struct Foo end
workspace()
@eval Core.Main begin
    mutable struct Foo end
    @assert Tuple{Type{LastMain.Foo}} !== Tuple{Type{Main.Foo}}
end
"""
@test success(pipeline(`$exename --startup-file=no -e $script2`, stdout=STDOUT, stderr=STDERR))

# Issue #22101
mktempdir() do dir
    withenv("JULIA_DEBUG_LOADING" => nothing) do
        # We need to ensure that the module does a nontrivial amount of work during precompilation
        write(joinpath(dir, "Test22101.jl"), """
            __precompile__()
            module Test22101
                export f22101
                f22101() = collect(1:10)
                f22101()
            end
        """)
        write(joinpath(dir, "testdriver.jl"), """
            using Test
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(Base.LOAD_CACHE_PATH, 1, $(repr(dir)))
            @test !isdefined(Main, :f22101)
            @test !isdefined(Main, :LastMain)
            begin
                using Test22101
                @test isdefined(Main, :f22101)
                @test f22101()::Vector{Int} == collect(1:10)
                @eval workspace() using Test22101
                @test f22101()::Vector{Int} == collect(1:10)
                @test isdefined(Main, :f22101)
            end
            @test isdefined(Main, :f22101)
            @test !isdefined(Main, :LastMain)
            @test isdefined(Core.Main, :f22101)
            nothing
        """)
        # Ensure that STDIO doesn't get swallowed (helps with debugging)
        cmd = `$(Base.julia_cmd()) --startup-file=no --sysimage-native-code=yes --compiled-modules=yes $(joinpath(dir, "testdriver.jl"))`
        @test success(pipeline(cmd, stdout=STDOUT, stderr=STDERR))
    end
end
