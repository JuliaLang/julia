using Test

function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    Core.include("$name.jl")
end

macro timeit(ex,name)
    quote
        t = Inf
        for i = 1:5
            t = min(t, @elapsed $(esc(ex)))
        end
        println(rpad(string($name,":"), 20), t)
    end
end

function propagate_errors(a,b)
    if isa(a,Exception)
        rethrow(a)
    end
    if isa(b,Exception)
        rethrow(b)
    end
    nothing
end

shift!(LOAD_PATH) # looking in . messes things up badly
