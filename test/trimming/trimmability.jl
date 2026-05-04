# Test that various constructs support trimming

using Sockets

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

mutable struct Foo; x::Int; end
const storage = Foo[]
function add_one(x::Cint)::Cint
    push!(storage, Foo(x))
    return x + 1
end

function _test_cat()
    # hcat
    _cat1a = hcat(randn(3), rand(3), randn(3))
    _cat1b = [randn(3) rand(3) randn(3)]
    _cat1c = hcat(randn(3,3), rand(3,3), randn(3,3))
    _cat1d = [randn(3,3) rand(3,3) randn(3,3)]
    _cat1e = hcat(randn(3,3,3), rand(3,3,3), randn(3,3,3))
    _cat1f = [randn(3,3,3) rand(3,3,3) randn(3,3,3)]

    # v_cat
    _cat2a = vcat(randn(3), rand(3), randn(3))
    _cat2b = [randn(3); rand(3); randn(3)]
    _cat2c = vcat(randn(3,3), rand(3,3), randn(3,3))
    _cat2d = [randn(3,3); rand(3,3); randn(3,3)]
    _cat2e = vcat(randn(3,3,3), rand(3,3,3), randn(3,3,3))
    _cat2f = [randn(3,3,3); rand(3,3,3); randn(3,3,3)]

    # hvcat
    _cat3a = hvcat((2,2), rand(3,2), randn(3,4), rand(1,2), randn(1,4))
    _cat3b = [rand(3,2) randn(3,4); rand(1,2) randn(1,4)]
    _cat3c = hvcat((2, 2), rand(5,2,3), rand(5,4,3), rand(1,2,3), rand(1,4,3))
    _cat3d = [rand(5,2,3) rand(5,4,3); rand(1,2,3) rand(1,4,3)]

    # cat
    _cat4a = cat(randn(3), randn(3); dims = 1)
    _cat4b = cat(randn(3,3,3), randn(3,3,3); dims = 2)
    _cat4c = cat(randn(3), randn(3,3); dims = 2)
    _cat4d = cat(randn(3), randn(3), rand(3), rand(3), randn(3), randn(3); dims = (1,))
    _cat4e = cat(randn(3,3), randn(3,3), rand(3,3), rand(3,3), randn(3,3), randn(3,3); dims = (1,2))
    _cat4f = cat(randn(3,3,3), randn(3,3); dims=(1,3))

    # hvncat
    _cat5a = hvncat(2, randn(3), randn(3), randn(3))
    _cat5b = [randn(3) ;; randn(3) ;; randn(3)]
    _cat5c = hvncat(2, randn(3,3), randn(3,3), randn(3,3))
    _cat5d = [randn(3,3) ;; randn(3,3) ;; randn(3,3)]
    _cat5e = hvncat((1, 2, 2), false, randn(2,3), randn(2,3), randn(2,3), randn(2,3))
    _cat5f = [randn(2,3) ;; randn(2,3) ;;; randn(2,3) ;; randn(2,3)]

    # stack
    _cat6a = stack([randn(3), randn(3), randn(3)])
    _cat6b = stack([randn(3), randn(3), randn(3)]; dims=1)
    _cat6c = stack([randn(2,3), randn(2,3)]; dims=3)
    _cat6d = stack(x -> x .^ 2, [randn(3), randn(3)])

    # repeat
    _cat7a = repeat(randn(3), 2)
    _cat7b = repeat(randn(2,3), 2, 3)
    _cat7c = repeat(randn(2,3); inner=(2,1), outer=(1,3))
    _cat7d = repeat(randn(3,3,3), 1, 2, 1)

    # aggregate to prevent deletion
    _cat1 = _cat1a[1] + _cat1b[1] + _cat1c[1] + _cat1d[1] + _cat1e[1] + _cat1f[1]
    _cat2 = _cat2a[1] + _cat2b[1] + _cat2c[1] + _cat2d[1] + _cat2e[1] + _cat2f[1]
    _cat3 = _cat3a[1] + _cat3b[1] + _cat3c[1] + _cat3d[1]
    _cat4 = _cat4a[1] + _cat4b[1] + _cat4c[1] + _cat4d[1] + _cat4e[1] + _cat4f[1]
    _cat5 = _cat5a[1] + _cat5b[1] + _cat5c[1] + _cat5d[1] + _cat5e[1] + _cat5f[1]
    _cat6 = _cat6a[1] + _cat6b[1] + _cat6c[1] + _cat6d[1]
    _cat7 = _cat7a[1] + _cat7b[1] + _cat7c[1] + _cat7d[1]

    return _cat1 + _cat2 + _cat3 + _cat4 + _cat5 + _cat6 + _cat7
end


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

    println(Core.stdout, _test_cat())

    for i = 1:10
        # https://github.com/JuliaLang/julia/issues/60846
        add_one(Cint(i))
        GC.gc()
    end

    try
        sock = connect("localhost", 4900)
        if isopen(sock)
            write(sock, "Hello")
            flush(sock)
            close(sock)
        end
    catch
    end

    Base.donotdelete(reshape([1,2,3],:,1,1))

    return 0
end
