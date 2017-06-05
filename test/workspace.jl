# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Test

script = """
# Issue #11948
f(x) = x+1
workspace()
@assert !isdefined(:f)
LastMain.f(2)

# PR #12990
io = IOBuffer()
show(io, Pair)
@assert String(take!(io)) == "Pair"
@assert !Base.inbase(LastMain)
"""
exename = Base.julia_cmd()
run(`$exename --startup-file=no -e $script`)

# issue #17764
script2 = """
mutable struct Foo end
workspace()
mutable struct Foo end
@assert Tuple{Type{LastMain.Foo}} !== Tuple{Type{Main.Foo}}
"""
run(`$exename --startup-file=no -e $script2`)

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
            insert!(LOAD_PATH, 1, $(repr(dir)))
            insert!(Base.LOAD_CACHE_PATH, 1, $(repr(dir)))
            try
                using Test22101
                f22101()
                workspace()
                using Test22101
            finally
                splice!(LOAD_PATH, 1)
                splice!(Base.LOAD_CACHE_PATH, 1)
            end
            exit(isdefined(Main, :f22101) ? 0 : 1)
        """)
        # Ensure that STDIO doesn't get swallowed (helps with debugging)
        cmd = `$(Base.julia_cmd()) --startup-file=no --precompiled=yes --compilecache=yes $(joinpath(dir, "testdriver.jl"))`
        @test success(pipeline(cmd, stdout=STDOUT, stderr=STDERR))
    end
end
