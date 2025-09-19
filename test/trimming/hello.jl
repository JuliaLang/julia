# Test that minimal executable size stays low

function @main(args::Vector{String})::Cint
    println(Core.stdout, "Hello, world!")
    return 0
end
