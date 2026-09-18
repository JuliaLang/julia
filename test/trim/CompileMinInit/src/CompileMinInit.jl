# Test that `__init__` of a `compile=min` module survives trimming and runs at startup
module CompileMinInit

using MinInitDep

function @main(args::Vector{String})::Cint
    println(Core.stdout, "initialized: ", MinInitDep.initialized[])
    return 0
end

end
