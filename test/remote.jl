# Check that serializer hasn't gone out-of-frame
@test Base._jl_ser_tag[Symbol] == 2
@test Base._jl_ser_tag[()] == 49
@test Base._jl_ser_tag[false] == 125

# issue #1770
let
    a = ['T', 'e', 's', 't']
    f = memio()
    serialize(f, a)
    seek(f, 0)
    @test deserialize(f) == a
    f = IOString()
    serialize(f, a)
    seek(f, 0)
    @test deserialize(f) == a
end
