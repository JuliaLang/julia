# check that libgit2 has been installed correctly

const LIBGIT2_VER = v"0.21+"

function check_version()
    major, minor, patch = Cint[0], Cint[0], Cint[0]
    ccall((:git_libgit2_version, :libgit2), Void,
          (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), major, minor, patch)
    v = VersionNumber(major[1], minor[1], patch[1])
    if v.major == LIBGIT2_VER.major && v.minor == LIBGIT2_VER.minor
        return true
    else
        return false
    end
end

@test check_version()
