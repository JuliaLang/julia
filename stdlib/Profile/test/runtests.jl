# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Profile, Serialization

function busywait(t, n_tries)
    iter = 0
    while iter < n_tries && Profile.len_data() == 0
        iter += 1
        tend = time() + t
        while time() < tend end
    end
end

Profile.clear()
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
    Profile.clear()
    @test isempty(Profile.fetch())
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
