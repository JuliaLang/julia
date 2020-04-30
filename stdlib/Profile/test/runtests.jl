# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Profile, Serialization

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
    @test n_ == 1_000_000
    def_delay = Sys.iswindows() ? 0.01 : 0.001
    @test delay_ == def_delay
    Profile.init(n=1_000_001, delay=0.0005)
    n_, delay_ = Profile.init()
    @test n_ == 1_000_001
    @test delay_ == 0.0005
    Profile.init(n=1_000_000, delay=def_delay)
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
