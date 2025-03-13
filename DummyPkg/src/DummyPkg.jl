module DummyPkg

const Arr = Int[123, 456, 789]

const Small = 42

const Str = "Lorem ipsum"

const Big = BigInt(10)^123

struct MyType
    a::NTuple{16,Int}
    b::Float64
    c::Float32
    d::Float16
end

const TN = Base.typename(MyType)

end # module DummyPkg
