using Pkg

function with_output_on_failure(f)
    output = IOBuffer()
    try
        return f(output)
    catch
        seekstart(output)
        write(stderr, read(output))
        rethrow()
    end
end

Pkg.activate(dirname(@__DIR__)) do
    with_output_on_failure() do output
        Pkg.instantiate(; io=output)
    end
    include("runtests.jl")
end
