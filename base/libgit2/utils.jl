function version()
    major = Ref{Cint}(0)
    minor = Ref{Cint}(0)
    patch = Ref{Cint}(0)
    ccall((:git_libgit2_version, :libgit2), Void,
          (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), major, minor, patch)
    return VersionNumber(major[], minor[], patch[])
end

isset(val::Integer, flag::Integer) = (val & flag == flag)

function prompt(msg::AbstractString; default::AbstractString="", password::Bool=false)
    msg = !isempty(default) ? msg*" [$default]:" : msg*":"
    uinput = if password
        bytestring(ccall(:getpass, Cstring, (Cstring,), msg))
    else
        print(msg)
        chomp(readline(STDIN))
    end
    isempty(uinput) ? default : uinput
end
