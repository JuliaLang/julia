world::String = "world!"
const str = OncePerProcess{String}() do
    return "Hello, " * world
end

abstract type Shape end
struct Square <: Shape
    side::Float64
end
struct Circle <: Shape
    radius::Float64
end
area(s::Square) = s.side^2
area(c::Circle) = pi*c.radius^2

sum_areas(v::Vector{Shape}) = sum(area, v)

function @main(args::Vector{String})::Cint
    println(Core.stdout, str())
    println(Core.stdout, PROGRAM_FILE)
    foreach(x->println(Core.stdout, x), args)

    # test map/mapreduce; should work but relies on inlining and other optimizations
    # test that you can dispatch to some number of concrete cases
    println(Core.stdout, sum_areas(Shape[Circle(1), Square(2)]))

    arr = rand(10)
    sorted_arr = sort(arr)
    tot = sum(sorted_arr)
    tot = prod(sorted_arr)
    a = any(x -> x > 0, sorted_arr)
    b = all(x -> x >= 0, sorted_arr)
    c = map(x -> x^2, sorted_arr)
    d = mapreduce(x -> x^2, +, sorted_arr)
    # e = reduce(xor, rand(Int, 10))
    return 0
end
