module MyApp
Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    return 0
end
end
