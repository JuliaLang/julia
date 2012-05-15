include("os_detect.jl")

macro windows_only(ex)
    if(CURRENT_OS == :Windows)
        return ex
    else
        return :nothing
    end
end

macro unix_only(ex)
    if(_jl_is_unix(CURRENT_OS))
        return ex
    else
        return :nothing
    end
end

_jl_os_name(os::Symbol) = string(os)
