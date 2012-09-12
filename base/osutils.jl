include("os_detect.jl")

macro windows_only(ex)
    if(OS_NAME == :Windows)
        return esc(ex)
    else
        return :nothing
    end
end

macro unix_only(ex)
    if(_jl_is_unix(OS_NAME))
        return esc(ex)
    else
        return :nothing
    end
end

_jl_os_name(os::Symbol) = string(os)
