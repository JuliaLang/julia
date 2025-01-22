module MyApp

Base.@ccallable function foo()::Cint
    Cint(1)
end

Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    u = foo()
    println(Core.stdout, u)
    return 0
end
end
