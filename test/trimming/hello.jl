world::String = "world!"
const str = OncePerProcess{String}() do
    return "Hello, " * world
end

function @main(args::Vector{String})::Cint
    println(Core.stdout, str())
    foreach(x->println(Core.stdout, x), args)
    return 0
end
