world::String = "world!"
const str = OncePerProcess{String}() do
    return "Hello, " * world
end

function @main(args::Vector{String})::Cint
    println(Core.stdout, str())
    return 0
end
