function busywait(t)
    tend = time() + t
    while time() < tend end
end

@profile busywait(1)
let iobuf = IOBuffer()
    Profile.print(iobuf)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    Profile.print(iobuf, format=:flat)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    Profile.print(iobuf, format=:tree, C=true)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    Profile.clear()
    @test isempty(Profile.fetch())
end
