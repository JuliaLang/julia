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

# Exercise task starters that are only reachable through task registration.
module TrimTaskModule
const task_ran = Ref(false)
plain_entry()::Nothing = (task_ran[] = true; nothing)
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

    task = Task(TrimTaskModule.plain_entry)
    schedule(task)
    wait(task)
    println(Core.stdout, TrimTaskModule.task_ran[])

    spawned_ran = Ref(false)
    spawned = Threads.@spawn begin
        spawned_ran[] = true
        nothing
    end
    wait(spawned)
    println(Core.stdout, spawned_ran[])

    return 0
end
