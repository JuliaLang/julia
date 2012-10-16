include("os_detect.jl")

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
