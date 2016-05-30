# This file is a part of Julia. License is MIT: http://julialang.org/license

# Note: Tests usually run with `--depwarn=error` (2)

let
    @noinline a_16617() = backtrace()
    @noinline b_16617() = a_16617()
    bt = b_16617()

    caller = Base.firstcaller(bt, :a_16617)  # Determine the caller of `a_16617`
    @test first(StackTraces.lookup(caller)).func == :b_16617
end

# Calling the same deprecated function from different callers should generate multiple
# warnings
let
    io = IOBuffer()
    @noinline deprecated_16617() = Base.depwarn(io, "message", :deprecated_16617, mode=1)
    @noinline f1_16617() = deprecated_16617()
    @noinline f2_16617() = deprecated_16617()
    @noinline function f3_16617()
        f1_16617()
        f2_16617()
    end
    f3_16617()  # This line should print two depwarn
    @test length(matchall(r"WARNING", readstring(seekstart(io)))) == 2
end

# Calling the same deprecated function from the same caller should result in one warning.
let
    io = IOBuffer()
    @noinline deprecated_16617() = Base.depwarn(io, "message", :deprecated_16617, mode=1)
    @noinline g1_16617() = deprecated_16617()
    @noinline function g2_16617()
        g1_16617()
        g1_16617()
    end
    g2_16617()  # This line should print only one depwarn
    @test length(matchall(r"WARNING", readstring(seekstart(io)))) == 1
end
