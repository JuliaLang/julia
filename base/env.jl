# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    withenv(f::Function, kv::Pair...)

Execute `f` in an environment that is temporarily modified (not replaced as in `setenv`)
by zero or more `"var"=>val` arguments `kv`. `withenv` is generally used via the
`withenv(kv...) do ... end` syntax. A value of `nothing` can be used to temporarily
unset an environment variable (if it is set). When `withenv` returns, the original
environment has been restored.
"""
function withenv(f::Function, keyvals::Pair{T}...) where T<:AbstractString
    old = Dict{T,Any}()
    for (key, val) in keyvals
        old[key] = get(ENV, key, nothing)
        if val === nothing
            delete!(ENV, key)
        else
            ENV[key] = val
        end
    end
    try f()
    finally
        for (key, val) in old
            if val === nothing
                delete!(ENV, key)
            else
                ENV[key] = val
            end
        end
    end
end
withenv(f::Function) = f() # handle empty keyvals case; see #10853
