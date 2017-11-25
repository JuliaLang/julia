@test @inferred(MappedArray(identity, [1,2,3]))::MappedArray == [1,2,3]
@test @inferred(MappedVector(identity, [1,2,3]))::MappedArray == [1,2,3]
@test @inferred(MappedArray(identity, [1 2; 3 4]))::MappedArray == [1 2; 3 4]
@test @inferred(MappedMatrix(identity, [1 2; 3 4]))::MappedArray == [1 2; 3 4]

a = [1,2,3]
b = @inferred(MappedArray(x -> x + 10, a))::MappedArray
@test b[2] === 12
@test_throws ErrorException b[2] = 20
c = @inferred(MappedArray(x -> x + 10, x -> x - 10, a))::MappedArray
@test c[2] === 12
@test (c[2] = 20; a[2] === 10)
@test (c[3] = 30.0; a[3] === 20)

@test Base.IndexStyle(b) === Base.IndexStyle(a)