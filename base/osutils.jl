function _jl_is_unix(os::Symbol)
    if (os==:Windows) return false; 
    elseif (os==:Linux) return true; 
    elseif (os==:FreeBSD) return true; 
    elseif (os==:Darwin) return true; 
    else error("Unknown Operating System")
    end
end

macro windows_only(ex)
    OS_NAME == :Windows ? esc(ex) : :nothing
end

macro unix_only(ex)
    _jl_is_unix(OS_NAME) ? esc(ex) : :nothing
end

macro osx_only(ex)
    OS_NAME == :Darwin ? esc(ex) : :nothing
end

macro linux_only(ex)
    _jl_is_unix(OS_NAME) && OS_NAME != :Darwin ? esc(ex) : :nothing
end

_jl_os_name(os::Symbol) = string(os)
