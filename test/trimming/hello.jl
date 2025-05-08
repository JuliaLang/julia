module MyApp

world::String = "world!"
const str = OncePerProcess{String}() do
    return "Hello, " * world
end

Base.@ccallable function main()::Cint
    println(Core.stdout, str())
    return 0
end

end
