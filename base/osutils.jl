function is_unix(os::Symbol)
    if (os==:Windows) return false;
    elseif (os==:Linux) return true;
    elseif (os==:FreeBSD) return true;
    elseif (os==:Darwin) return true;
    else error("unknown operating system")
    end
end

function _os_test(qm,ex,test)
    @assert qm == :?
    @assert isa(ex,Expr)
    @assert ex.head == :(:)
    @assert length(ex.args) == 2
    if test
        return esc(ex.args[1])
    else
        return esc(ex.args[2])
    end
end

macro windows(qm,ex)
    _os_test(qm, ex, OS_NAME===:Windows)
end
macro unix(qm,ex)
    _os_test(qm, ex, is_unix(OS_NAME))
end
macro osx(qm,ex)
    _os_test(qm, ex, OS_NAME===:Darwin)
end
macro linux(qm,ex)
    _os_test(qm, ex, OS_NAME===:Linux)
end

macro windows_only(ex)
    @windows? esc(ex) : nothing
end
macro unix_only(ex)
    @unix? esc(ex) : nothing
end
macro osx_only(ex)
    @osx? esc(ex) : nothing
end
macro linux_only(ex)
    @linux? esc(ex) : nothing
end

# Windows version macros

@windows_only function windows_version()
    verinfo = ccall(:GetVersion, UInt32, ())
    (verinfo & 0xFF, (verinfo >> 8) & 0xFF)
end
@unix_only windows_version() = (0,0)

WINDOWS_XP_VER = (5,1)

macro windowsxp(qm,ex)
    _os_test(qm, ex, OS_NAME===:Windows && windows_version() <= WINDOWS_XP_VER)
end

macro windowsxp_only(ex)
    @windowsxp? esc(ex) : nothing
end

macro non_windowsxp_only(ex)
    @windowsxp? nothing : esc(ex)
end
