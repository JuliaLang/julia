using Test

function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    #flush(OUTPUT_STREAM)
    include("$name.jl")
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

shift!(LOAD_PATH) # looking in . messes things up badly

for t in ARGS
    runtests(t)
end
println("    \033[32;1mSUCCESS\033[0m")
