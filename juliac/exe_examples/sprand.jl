module MyApp

using SparseArrays

Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    sprand(10,10,0.1)[1]
    return 0
end

end