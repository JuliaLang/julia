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
    Profile.print(iobuf, format=:flat, C=true)
    str = takebuf_string(iobuf)
    @test !isempty(str)
    Profile.print(iobuf)
    Profile.print(iobuf, format=:flat)
    Profile.clear()
    @test isempty(Profile.fetch())
end
