# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Profile, Serialization, Logging
using Base.StackTraces: StackFrame

@test_throws "The profiling data buffer is not initialized. A profile has not been requested this session." Profile.print()

Profile.clear()
Profile.init()

let iobuf = IOBuffer()
    for fmt in (:tree, :flat)
        Test.@test_logs (:warn, r"^There were no samples collected\.") Profile.print(iobuf, format=fmt, C=true)
        Test.@test_logs (:warn, r"^There were no samples collected\.") Profile.print(iobuf, Profile.add_fake_meta([0x0000000000000001, 0x0000000000000000]), Dict(0x0000000000000001 => [Base.StackTraces.UNKNOWN]), format=fmt, C=false)
    end
end

@noinline function busywait(t, n_tries)
    iter = 0
    init_data = Profile.len_data()
    while iter < n_tries && Profile.len_data() == init_data
        iter += 1
        tend = time_ns() + 1e9 * t
        while time_ns() < tend end
    end
end

@noinline function sleeping_tasks(ch::Channel)
    for _ in 1:100
        Threads.@spawn take!(ch)
    end
    sleep(10)
end

function test_profile()
    let r = Profile.retrieve()
        mktemp() do path, io
            serialize(io, r)
            close(io)
            open(path) do io
                @test isa(deserialize(io), Tuple{Vector{UInt},Dict{UInt64,Vector{Base.StackTraces.StackFrame}}})
            end
        end
    end
end

function test_has_task_profiler_sample_in_buffer()
    let r = Profile.retrieve()
        mktemp() do path, io
            serialize(io, r)
            close(io)
            open(path) do io
                all = deserialize(io)
                data = all[1]
                startframe = length(data)
                for i in startframe:-1:1
                    (startframe - 1) >= i >= (startframe - (Profile.nmeta + 1)) && continue # skip metadata (its read ahead below) and extra block end NULL IP
                    if Profile.is_block_end(data, i)
                        thread_sleeping_state = data[i - Profile.META_OFFSET_SLEEPSTATE]
                        @test thread_sleeping_state == 0x3
                    end
                end
            end
        end
    end
end

busywait(0, 0) # compile

@profile_walltime busywait(1, 20)
test_profile()

Profile.clear()

ch = Channel(1)
@profile_walltime sleeping_tasks(ch)
test_profile()
close(ch)
test_has_task_profiler_sample_in_buffer()

Profile.clear()

@profile busywait(1, 20)
test_profile()

# test printing options
for options in ((format=:tree, C=true),
                (format=:tree, maxdepth=2),
                (format=:flat, C=true),
                (),
                (format=:flat, sortedby=:count),
                (format=:tree, recur=:flat),
               )
    iobuf = IOBuffer()
    Profile.print(iobuf; options...)
    str = String(take!(iobuf))
    @test !isempty(str)
    file, _ = mktemp()
    Profile.print(file; options...)
    @test filesize(file) > 0
end

@testset "Profile.print() groupby options" begin
    iobuf = IOBuffer()
    with_logger(NullLogger()) do
        @testset for format in [:flat, :tree]
            @testset for threads in Any[1:typemax(Int), 1, 1:1, 1:2, [1,2]]
                @testset for groupby in Any[:none, :thread, :task, [:thread, :task], [:task, :thread]]
                    Profile.print(iobuf; groupby, threads, format)
                    @test !isempty(String(take!(iobuf)))
                end
            end
        end
    end
end

@testset "Profile.fetch() with and without meta" begin
    data_without = Profile.fetch(include_meta = false)
    data_with = Profile.fetch()
    @test data_without[1] == data_with[1]
    @test data_without[end] == data_with[end]
    nblocks = count(Base.Fix1(Profile.is_block_end, data_with), eachindex(data_with))
    @test length(data_without) == length(data_with) - nblocks * (Profile.nmeta + 1)

    data_with_fake = Profile.add_fake_meta(data_without)
    @test_throws "input already has metadata" Profile.add_fake_meta(data_with)
    data_stripped = Profile.strip_meta(data_with_fake)
    @test data_stripped == data_without
    # ideally the test below would be a test for equality, but real sample ips can be nulls, and thus
    # adding metadata back in can convert those ips to new block ends, and the length is then longer
    @test length(data_with_fake) >= length(data_with)

end

Profile.clear()
@test isempty(Profile.fetch())

let
    @test Profile.callers("\\") !== nothing
    @test Profile.callers(\) !== nothing
    # linerange with no filename provided should fail
    @test_throws ArgumentError Profile.callers(\; linerange=10:50)
end

# issue #13229
module I13229
using Test, Profile
global z = 0
@timed @profile for i = 1:5
    function f(x)
        return x + i
    end
    global z = f(i)
end
@test z == 10
end

@testset "@profile no scope" begin
    @profile no_scope_57858_1 = 1
    @test @isdefined no_scope_57858_1
    Profile.clear()

    @profile_walltime no_scope_57858_1 = 1
    @test @isdefined no_scope_57858_1
    Profile.clear()

    Profile.Allocs.@profile no_scope_57858_2 = 1
    @test @isdefined no_scope_57858_2
    Profile.Allocs.clear()
end

@testset "setting sample count and delay in init" begin
    n_, delay_ = Profile.init()
    n_original = n_
    sample_size_bytes = sizeof(Ptr)
    def_n = Sys.iswindows() && Sys.WORD_SIZE == 32 ? 1_000_000 : 10_000_000
    if Sys.WORD_SIZE == 32 && (def_n * sample_size_bytes) > 2^29
        @test n_ * sample_size_bytes <= 2^29
    else
        @test n_ == def_n
    end

    def_delay = Sys.iswindows() && Sys.WORD_SIZE == 32 ? 0.01 : 0.001
    @test delay_ == def_delay
    Profile.init(n=1_000_001, delay=0.0005)
    n_, delay_ = Profile.init()
    if Sys.WORD_SIZE == 32 && (1_000_001 * sample_size_bytes) > 2^29
        @test n_ * sample_size_bytes <= 2^29
    else
        @test n_ == 1_000_001
    end
    @test delay_ == 0.0005
    Profile.init(n=n_original, delay=def_delay)
end

@testset "warning for buffer full" begin
    n_, delay_ = Profile.init()
    Profile.init(n=17)
    @test_logs (:warn, r"The profile data buffer is full") Profile.fetch()
    Profile.init(n=n_, delay=delay_)
end

@testset "Line number correction" begin
    @profile busywait(1, 20)
    _, fdict0 = Profile.flatten(Profile.retrieve()...)
    Base.update_stackframes_callback[] = function(list)
        modify((sf, n)) = sf.func === :busywait ? (StackTraces.StackFrame(sf.func, sf.file, sf.line+2, sf.linfo, sf.from_c, sf.inlined, sf.pointer), n) : (sf, n)
        map!(modify, list, list)
    end
    _, fdictc = Profile.flatten(Profile.retrieve()...)
    Base.update_stackframes_callback[] = identity
    function getline(sfs)
        for sf in sfs
            sf.func === :busywait && return sf.line
        end
        nothing
    end
    @test getline(values(fdictc)) == getline(values(fdict0)) + 2
end

import InteractiveUtils

@generated function compile_takes_1_second(x)
    t = time_ns()
    while time_ns() < t + 1e9
        # busy wait for 1 second
    end
    return :(x)
end
@testset "Module short names" begin
    Profile.clear()
    @profile begin
        @eval compile_takes_1_second(1) # to increase chance of profiling hitting compilation code
        InteractiveUtils.peakflops()
    end
    io = IOBuffer()
    ioc = IOContext(io, :displaysize=>(1000,1000))
    Profile.print(ioc, C=true)
    str = String(take!(io))
    slash = Sys.iswindows() ? "\\" : "/"
    @test occursin("@Compiler" * slash, str)
    @test occursin("@Base" * slash, str)
    @test occursin("@InteractiveUtils" * slash, str)
    @test occursin("@LinearAlgebra" * slash, str)
    @test occursin("@juliasrc" * slash, str)
    @test occursin("@julialib" * slash, str)
end

# Profile deadlocking in compilation (debuginfo registration)
let cmd = Base.julia_cmd()
    script = """
        using Profile
        f(::Val) = GC.safepoint()
        @profile for i = 1:10^3
            println(i)
            f(Val(i))
        end
        println("done")
        print(Profile.len_data())
        """
    # use multiple threads here to ensure that profiling works with threading
    p = open(`$cmd -t2 -e $script`)
    t = Timer(120) do t
        # should be under 10 seconds, so give it 2 minutes then report failure
        println("KILLING debuginfo registration test BY PROFILE TEST WATCHDOG\n")
        kill(p, Base.SIGQUIT)
        sleep(30)
        kill(p, Base.SIGQUIT)
        sleep(30)
        kill(p, Base.SIGKILL)
    end
    s = read(p, String)
    close(t)
    @test success(p)
    @test !isempty(s)
    @test occursin("done", s)
    @test parse(Int, split(s, '\n')[end]) > 100
end

if Sys.isbsd() || Sys.islinux()
    @testset "SIGINFO/SIGUSR1 profile triggering" begin
        let cmd = Base.julia_cmd()
            script = """
                print(stderr, "started\n")
                eof(stdin)
                """
            iob = Base.BufferStream() # make an unbounded buffer, so we can just read after waiting for exit
            notify_exit = Base.PipeEndpoint()
            p = run(`$cmd -e $script`, notify_exit, devnull, iob, wait=false)
            eof = @async try # set up a monitor task to set EOF on iob after p exits
                wait(p)
            finally
                closewrite(iob)
            end
            t = Timer(120) do t
                # should be under 10 seconds, so give it 2 minutes then report failure
                println("KILLING siginfo/sigusr1 test BY PROFILE TEST WATCHDOG\n")
                kill(p, Base.SIGQUIT)
                sleep(30)
                kill(p, Base.SIGQUIT)
                sleep(30)
                kill(p, Base.SIGKILL)
                close(notify_exit)
            end
            try
                s = readuntil(iob, "started", keep=true)
                @assert occursin("started", s)
                @assert process_running(p)
                for i in 1:2
                    i > 1 && sleep(5)
                    if Sys.isbsd()
                        kill(p, 29) # SIGINFO
                    elseif Sys.islinux()
                        kill(p, 10) # SIGUSR1
                    end
                    s = readuntil(iob, "Overhead ╎", keep=true)
                    @test process_running(p)
                    readavailable(iob)
                    @test occursin("Overhead ╎", s)
                end
                close(notify_exit) # notify test finished
                wait(eof) # wait for test completion
                s = read(iob, String) # consume test output from buffer
                close(t)
            catch
                close(notify_exit)
                wait(eof) # wait for test completion
                errs = read(iob, String) # consume test output
                isempty(errs) || println("CHILD STDERR after test failure: ", errs)
                close(t)
                rethrow()
            end
            @test success(p)
        end
    end
end

@testset "FlameGraphs" begin
    # FlameGraphs makes use of some Profile's internals. Detect possible breakage by mimicking some of its tests.
    # Breakage is acceptable since these internals are not part of the stable API, but it's better to know, and ideally
    # should be paired with an issue or PR in FlameGraphs.
    #
    # This also improves the thoroughness of our overall Profile tests.
    stackframe(func, file, line; C=false) = StackFrame(Symbol(func), Symbol(file), line, nothing, C, false, 0)

    backtraces = UInt64[   4, 3, 2, 1,   # order: callees then caller
                        0, 6, 5, 1,
                        0, 8, 7,
                        0, 4, 3, 2, 1,
                        0]
    backtraces = Profile.add_fake_meta(backtraces)
    lidict = Dict{UInt64,StackFrame}(1=>stackframe(:f1, :file1, 1),
                                     2=>stackframe(:f2, :file1, 5),
                                     3=>stackframe(:f3, :file2, 1),
                                     4=>stackframe(:f2, :file1, 15),
                                     5=>stackframe(:f4, :file1, 20),
                                     6=>stackframe(:f5, :file3, 1),
                                     7=>stackframe(:f1, :file1, 2),
                                     8=>stackframe(:f6, :file3, 10))
    root = Profile.StackFrameTree{StackFrame}()
    Profile.tree!(root, backtraces, lidict, #= C =# true, :off)
    @test length(root.down) == 2
    for k in keys(root.down)
        @test k.file === :file1
        @test k.line ∈ (1, 2)
    end
    node = root.down[stackframe(:f1, :file1, 2)]
    @test only(node.down).first == lidict[8]
end

# FIXME: Issue #57103: heap snapshots are currently not supported in MMTk
@static if Base.USING_STOCK_GC
@testset "HeapSnapshot" begin
    tmpdir = mktempdir()

    # ensure that we can prevent redacting data
    fname = cd(tmpdir) do
        read(`$(Base.julia_cmd()) --startup-file=no -e "using Profile; const x = \"redact_this\"; print(Profile.take_heap_snapshot(; redact_data=false))"`, String)
    end

    @test isfile(fname)

    sshot = read(fname, String)
    @test sshot != ""
    @test contains(sshot, "redact_this")

    rm(fname)

    # ensure that string data is redacted by default
    fname = cd(tmpdir) do
        read(`$(Base.julia_cmd()) --startup-file=no -e "using Profile; const x = \"redact_this\"; print(Profile.take_heap_snapshot())"`, String)
    end

    @test isfile(fname)

    sshot = read(fname, String)
    @test sshot != ""
    @test !contains(sshot, "redact_this")

    rm(fname)
    rm(tmpdir, force = true, recursive = true)
end
end

@testset "PageProfile" begin
    fname = "$(getpid())_$(time_ns())"
    fpath = joinpath(tempdir(), fname)
    Profile.take_page_profile(fpath)
    open(fpath) do fs
        @test readline(fs) != ""
    end
    rm(fpath)
end

include("allocs.jl")

@testset "Docstrings" begin
    undoc = Docs.undocumented_names(Profile)
    @test_broken isempty(undoc)
    @test undoc == [:Allocs]
end
include("heapsnapshot_reassemble.jl")
