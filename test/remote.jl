# Check that serializer hasn't gone out-of-frame
@test Base.ser_tag[Symbol] == 2
@test Base.ser_tag[()] == 47
@test Base.ser_tag[false] == 123

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
