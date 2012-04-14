include("os_detect.jl")

macro windows_only(ex)
    if(CURRENT_OS == JL_OS_Windows)
        return ex
    else
        return :nothing
    end
end

macro unix_only(ex)
    if(CURRENT_OS != JL_OS_Windows)
        return ex
    else
        return :nothing
    end
end
