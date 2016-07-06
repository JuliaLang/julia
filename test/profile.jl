# This file is a part of Julia. License is MIT: http://julialang.org/license

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
let iobuf = IOBuffer()
    Profile.print(iobuf, format=:tree, C=true)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:tree, maxdepth=2)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:flat, C=true)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    truncate(iobuf, 0)
    Profile.print(iobuf)
    @test !isempty(takebuf_string(iobuf))
    truncate(iobuf, 0)
    Profile.print(iobuf, format=:flat, sortedby=:count)
    @test !isempty(takebuf_string(iobuf))
    Profile.clear()
    @test isempty(Profile.fetch())
end
