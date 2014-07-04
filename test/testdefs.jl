using Base.Test

function runtests(name)
    println("  Running ", name)
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
