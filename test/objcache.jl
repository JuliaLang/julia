# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for the LMDB object cache (src/objcache.cpp).

function objcache_session(env, script)
    cmd = addenv(`$(Base.julia_cmd()[1]) --startup-file=no -e $script`, env...)
    local out
    elapsed = @elapsed out = read(cmd, String)
    return out, elapsed
end

function objcache_fill_script(n, prefix)
    return """
        for i in 1:$n
            f = Symbol(:$prefix, i)
            @eval \$f(x) = x + \$i
            @eval \$f(1)
        end
        print("OK")
    """
end

# Construct an artificially over-capacity cache by shrinking the capacity to a
# couple of MiB and compiling a few hundred distinct functions, then check that
# LRU eviction makes progress in bounded batches and that the process still
# exits promptly with cache writes pending.
@testset "over-capacity eviction and shutdown" begin
    mktempdir() do dir
        logfile = joinpath(dir, "objcache.csv")
        env = ("JULIA_OBJCACHE" => "1",
               "JULIA_OBJCACHE_PATH" => joinpath(dir, "cache"),
               "JULIA_OBJCACHE_CAPACITY" => string(2 << 20),
               "JULIA_OBJCACHE_LOG" => logfile)
        out, elapsed = objcache_session(env, objcache_fill_script(600, :small_))
        @test out == "OK"
        @test elapsed < 600
        nevict = count(startswith("evict,"), eachline(logfile))
        @test nevict > 0
    end
end

# Generate the pathological cache shape behind the objcache shutdown
# livelock synthetically: jl_objcache_test_populate fills the LMDB map
# completely and punches out every other entry, leaving a full map whose
# free space exists only as a large fragmented freelist. A session with a
# much smaller capacity then compiles fresh code, forcing eviction and page
# allocation to operate entirely out of that freelist, with cache writes
# still pending at exit. Eviction must make real progress here: with
# unbounded single-transaction eviction, the commit does not fit on the
# full map, the abort rolls every delete back, and each write then repeats
# the full O(entries) walk — observable as an eviction count hundreds of
# times the entry count (and, at real-world map sizes, as an unbounded
# stall inside the commit's freelist bookkeeping).
@testset "full-map eviction storm" begin
    mktempdir() do dir
        logfile = joinpath(dir, "objcache.csv")
        mkenv(capacity) = ("JULIA_OBJCACHE" => "1",
                           "JULIA_OBJCACHE_PATH" => joinpath(dir, "cache"),
                           "JULIA_OBJCACHE_CAPACITY" => string(capacity),
                           "JULIA_OBJCACHE_LOG" => logfile)

        populate = """
            print(ccall(:jl_objcache_test_populate, Int64,
                        (UInt64, UInt64, UInt64), 100_000, 6144, 2))
        """
        out, _ = objcache_session(mkenv(128 << 20), populate)
        npop = parse(Int, out)
        @test npop > 1000

        out, elapsed = objcache_session(mkenv(8 << 20), objcache_fill_script(200, :storm_))
        @test out == "OK"
        @test elapsed < 300
        nevict = count(startswith("evict,"), eachline(logfile))
        @test nevict > 0
        # A correct eviction pass removes each entry at most about once; the
        # quadratic failure mode shows up as hundreds of evictions per entry.
        @test nevict < 20 * npop

        # The cache must still be functional after the storm: with breathing
        # room restored, a fresh entry stored by one session is hit by the
        # next.
        marker = """
            objcache_marker(x) = x + 42
            objcache_marker(1)
            print("OK")
        """
        out, _ = objcache_session(mkenv(64 << 20), marker)
        @test out == "OK"
        nlines = countlines(logfile)
        out, _ = objcache_session(mkenv(64 << 20), marker)
        @test out == "OK"
        newhits = count(l -> startswith(l, "lookup,") && contains(l, ",hit,"),
                        Iterators.drop(eachline(logfile), nlines))
        @test newhits > 0
    end
end
