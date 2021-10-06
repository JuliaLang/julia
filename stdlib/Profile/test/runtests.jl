# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Profile, Serialization, Logging

Profile.clear()
Profile.init()

let iobuf = IOBuffer()
    for fmt in (:tree, :flat)
        Test.@test_logs (:warn, r"^There were no samples collected\.") Profile.print(iobuf, format=fmt, C=true)
        Test.@test_logs (:warn, r"^There were no samples collected\.") Profile.print(iobuf, [0x0000000000000001], Dict(0x0000000000000001 => [Base.StackTraces.UNKNOWN]), format=fmt, C=false)
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

busywait(0, 0) # compile
@profile busywait(1, 20)

let r = Profile.retrieve()
    mktemp() do path, io
        serialize(io, r)
        close(io)
        open(path) do io
            @test isa(deserialize(io), Tuple{Vector{UInt},Dict{UInt64,Vector{Base.StackTraces.StackFrame}}})
        end
    end
end

let iobuf = IOBuffer()
    Profile.print(iobuf, format=:tree, C=true)
    str = String(take!(iobuf))
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:tree, maxdepth=2)
    str = String(take!(iobuf))
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:flat, C=true)
    str = String(take!(iobuf))
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf)
    @test !isempty(String(take!(iobuf)))
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:flat, sortedby=:count)
    @test !isempty(String(take!(iobuf)))
    Profile.print(iobuf, format=:tree, recur=:flat)
    str = String(take!(iobuf))
    @test !isempty(str)
    truncate(iobuf, 0)
end

@testset "Profile.print() groupby options" begin
    iobuf = IOBuffer()
    with_logger(NullLogger()) do
        @testset for format in [:flat, :tree]
            @testset for threads in [1:Threads.nthreads(), 1, 1:1, 1:2, [1,2]]
                @testset for groupby in [:none, :thread, :task, [:thread, :task], [:task, :thread]]
                    Profile.print(iobuf; groupby, threads, format)
                    @test !isempty(String(take!(iobuf)))
                end
            end
        end
    end
end

@testset "Profile.fetch() with and without meta" begin
    data_without = Profile.fetch()
    data_with = Profile.fetch(include_meta = true)
    @test data_without[1] == data_with[1]
    @test data_without[end] == data_with[end]
    @test length(data_without) < length(data_with)
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

@testset "setting sample count and delay in init" begin
    n_, delay_ = Profile.init()
    n_original = n_
    nthreads = Sys.iswindows() ? 1 : Threads.nthreads()
    sample_size_bytes = sizeof(Ptr)
    def_n = Sys.iswindows() && Sys.WORD_SIZE == 32 ? 1_000_000 : 10_000_000
    if Sys.WORD_SIZE == 32 && (def_n * nthreads * sample_size_bytes) > 2^29
        @test n_ * nthreads * sample_size_bytes <= 2^29
    else
        @test n_ == def_n
    end

    def_delay = Sys.iswindows() && Sys.WORD_SIZE == 32 ? 0.01 : 0.001
    @test delay_ == def_delay
    Profile.init(n=1_000_001, delay=0.0005)
    n_, delay_ = Profile.init()
    if Sys.WORD_SIZE == 32 && (1_000_001 * nthreads * sample_size_bytes) > 2^29
        @test n_ * nthreads * sample_size_bytes <= 2^29
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
        modify((sf, n)) = sf.func == :busywait ? (StackTraces.StackFrame(sf.func, sf.file, sf.line+2, sf.linfo, sf.from_c, sf.inlined, sf.pointer), n) : (sf, n)
        map!(modify, list, list)
    end
    _, fdictc = Profile.flatten(Profile.retrieve()...)
    Base.update_stackframes_callback[] = identity
    function getline(sfs)
        for sf in sfs
            sf.func == :busywait && return sf.line
        end
        nothing
    end
    @test getline(values(fdictc)) == getline(values(fdict0)) + 2
end

# Profile deadlocking in compilation (debuginfo registration)
let cmd = Base.julia_cmd()
    script = """
        using Profile
        f(::Val) = GC.safepoint()
        @profile for i = 1:10^3; f(Val(i)); end
        print(Profile.len_data())
        """
    p = open(`$cmd -e $script`)
    t = Timer(120) do t
        # should be under 10 seconds, so give it 2 minutes then report failure
        println("KILLING BY PROFILE TEST WATCHDOG\n")
        kill(p, Base.SIGTERM)
        sleep(10)
        kill(p, Base.SIGKILL)
    end
    s = read(p, String)
    close(t)
    @test success(p)
    @test parse(Int, s) > 100
end
