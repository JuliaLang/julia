# This file is a part of Julia. License is MIT: https://julialang.org/license

module InitSupport

    # MUST ITSELF BE TYPE STABLE TO AVOID INFINITE RECURSION, since called from dispatch
    Base.@ccallable function is_enforce_static()::Cint
        # Type stable, thanks to the type assert in getproperty(Task, :storage).
        tls = task_local_storage()
        return haskey(tls, :nhd_is_enforce_static) ? 1 : 0
    end

    function __init__()
        fptr = reinterpret(Ptr{Int}, cglobal(:is_enforce_static))
        unsafe_store!(fptr, @cfunction(is_enforce_static, Cint, ()))
    end
end

macro enforce_stable(expr)
    return quote
        has_prev = haskey(task_local_storage(), :nhd_is_enforce_static)
        !has_prev && task_local_storage(:nhd_is_enforce_static, true)
        try
            $(esc(expr))
        finally
            !has_prev && delete!(task_local_storage(), :nhd_is_enforce_static)
        end
    end
end

macro allow_unstable(expr)
    return quote
        has_prev = haskey(task_local_storage(), :nhd_is_enforce_static)
        delete!(task_local_storage(), :nhd_is_enforce_static)
        try
            $(esc(expr))
        finally
            has_prev && task_local_storage(:nhd_is_enforce_static, true)
        end
    end
end

