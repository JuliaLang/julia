function version()
    major, minor, patch = Cint[0], Cint[0], Cint[0]
    ccall((:git_libgit2_version, :libgit2), Void,
          (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), major, minor, patch)
    return VersionNumber(major[1], minor[1], patch[1])
end

isset(val::Integer, flag::Integer) = (val & flag == flag)