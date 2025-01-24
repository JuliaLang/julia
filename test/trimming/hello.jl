module MyApp

# a ccallable that is used but not declared as an entrypoint. should not have a C name generated.
Base.@ccallable function foo()::Cint
    Cint(1)
end

# a ccallable that is an entrypoint but uses const_api.
Base.@ccallable function foo2()::Cint
    Cint(2)
end

Base.Experimental.entrypoint(foo2, ())

Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    u = foo()
    println(Core.stdout, u)
    return 0
end
end
