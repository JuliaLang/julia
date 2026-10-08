# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests the output of the embedding example is correct
using Test
using Libdl

if Sys.iswindows()
    # libjulia needs to be in the same directory as the embedding executable or in path
    ENV["PATH"] = string(Sys.BINDIR, ";", ENV["PATH"])
end

function checknum(s, rx, cond)
    m = match(rx, s)
    if m === nothing
        return false
    else
        num = m[1]
        return cond(parse(UInt, num))
    end
end

@test length(ARGS) == 1
@testset "gcext example" begin
    out = Pipe()
    err = Pipe()
    p = run(pipeline(Cmd(ARGS), stdin=devnull, stdout=out, stderr=err), wait=false)
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err_task = @async readlines(err)
    # @test success(p)
    errlines = fetch(err_task)
    lines = fetch(out_task)
    @test isempty(errlines)
    # @test length(lines) == 6
    @test length(lines) == 5
    @test checknum(lines[2], r"([0-9]+) full collections", n -> n >= 10)
    @test checknum(lines[3], r"([0-9]+) partial collections", n -> n > 0)
    @test checknum(lines[4], r"([0-9]+) object sweeps", n -> n > 0)
    # @test checknum(lines[5], r"([0-9]+) internal object scan failures",
    #     n -> n == 0)
    # @test checknum(lines[6], r"([0-9]+) corrupted auxiliary roots",
    #    n -> n == 0)
    @test checknum(lines[5], r"([0-9]+) corrupted auxiliary roots",
        n -> n == 0)
end

# Pool statistics must include live objects on pages moved to either partial-page bank.
@static if Base.USING_STOCK_GC
    const count_pool_lib = joinpath(ENV["BINDIR"], "foreignlib.$(Libdl.dlext)")

    mutable struct PoolCountObject
        payload::NTuple{31, UInt64}
    end

    @noinline function pool_count_objects()
        objects = [PoolCountObject(ntuple(_ -> UInt64(i), Val(31))) for i in 1:160_000]
        Core.donotdelete(objects)
        return objects[1:16:end]
    end

    # Run `gc_count_pool` from the root scanner of a full collection, i.e. while the world
    # is stopped and the page lists are stable, and return the bytes it counted.
    function counted_pool_bytes()
        output = mktemp() do _, io
            redirect_stderr(io) do
                ccall((:set_count_pool, count_pool_lib), Cvoid, (Ptr{Cvoid},),
                      cglobal(:gc_count_pool))
                try
                    GC.gc(true)
                finally
                    ccall((:set_count_pool, count_pool_lib), Cvoid, (Ptr{Cvoid},), C_NULL)
                end
            end
            seekstart(io)
            read(io, String)
        end
        stats_pattern = r"\*{6} Pool stat: \*{6}\n" *
                        r"bits\(0\): (\d+)\nbits\(1\): (\d+)\n" *
                        r"bits\(2\): (\d+)\nbits\(3\): (\d+)\n" *
                        r"free pages: +\d+\n\*{24}\n"
        stats = collect(eachmatch(stats_pattern, output))
        @test length(stats) >= 1
        @test isempty(replace(output, stats_pattern => ""))
        return sum(s -> parse(Int64, s), stats[end].captures)
    end

    @testset "Pool statistics include partial pages" begin
        GC.gc(true)
        baseline = counted_pool_bytes()
        objects = pool_count_objects()
        # This sweep leaves every page of `objects` mostly free, so the pages move to the
        # partial-page bank, where they stay unless this thread adopts them for 256-byte
        # allocations before the next count.
        GC.gc(true)
        counted = GC.@preserve objects counted_pool_bytes()
        @test length(objects) == 10_000
        @test sizeof(PoolCountObject) == 248 # the 256-byte size class, with the tag
        @test all(eachindex(objects)) do i
            objects[i].payload == ntuple(_ -> UInt64(16 * (i - 1) + 1), Val(31))
        end
        # `gc_count_pool` sums the size of every cell of every page it visits, so the
        # pages that only `objects` keep alive add at least their live cells to the count;
        # without the partial-page bank they would not be visited at all.
        @test counted - baseline >= length(objects) * 256
    end
end

@testset "Package with foreign type" begin
    load_path = copy(LOAD_PATH)
    push!(LOAD_PATH, joinpath(@__DIR__, "Foreign"))
    push!(LOAD_PATH, joinpath(@__DIR__, "DependsOnForeign"))
    try
        # Force recaching
        Base.compilecache(Base.identify_package("Foreign"))
        Base.compilecache(Base.identify_package("DependsOnForeign"))

        push!(LOAD_PATH, joinpath(@__DIR__, "ForeignObjSerialization"))
        @test_throws ErrorException  Base.compilecache(Base.identify_package("ForeignObjSerialization"), Base.DevNull())
        pop!(LOAD_PATH)

        (@eval (using Foreign))
        @test Base.invokelatest(Foreign.get_nmark)  == 0
        @test Base.invokelatest(Foreign.get_nsweep) == 0

        obj = Base.invokelatest(Foreign.FObj)
        GC.@preserve obj begin
            GC.gc(true)
        end
        @test Base.invokelatest(Foreign.get_nmark)  > 0
        # Bugfix: the following used to crash
        summarysize = Base.summarysize(Foreign.FObj())
        @test summarysize >= sizeof(Ptr)
        @time Base.invokelatest(Foreign.test, 10)
        GC.gc(true)
        @test Base.invokelatest(Foreign.get_nsweep) > 0
        (@eval (using DependsOnForeign))
        Base.invokelatest(DependsOnForeign.f, obj)
    finally
        copy!(LOAD_PATH, load_path)
    end
end
