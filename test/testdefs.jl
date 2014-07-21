using Base.Test

function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    Core.include("$name.jl")
    nothing
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

# looking in . messes things up badly
filter!(x->x!=".", LOAD_PATH)
