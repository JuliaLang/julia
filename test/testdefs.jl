using Base.Test

function runtests(name)
    @printf("     \033[1m*\033[0m \033[31m%-20s\033[0m", name)
    tt = @elapsed Core.include("$name.jl")
    @printf(" in %6.2f seconds\n", tt)
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
