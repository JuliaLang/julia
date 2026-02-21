# This converts the ground-truth JSON files to the Julia repr format so
# we can use that without requiring a JSON parser during testing.

using Downloads
using Tar
using p7zip_jll

const url = "https://github.com/KristofferC/toml-test-julia/archive/refs/tags/v1.2.0.tar.gz"
const tarname = basename(url)
const version = lstrip(split(tarname, ".tar.gz")[1], 'v')

# From Pkg
function exe7z()
    # If the JLL is available, use the wrapper function defined in there
    if p7zip_jll.is_available()
        return p7zip_jll.p7zip()
    end
    return Cmd([find7z()])
end

function find7z()
    name = "7z"
    Sys.iswindows() && (name = "$name.exe")
    for dir in (joinpath("..", "libexec"), ".")
        path = normpath(Sys.BINDIR::String, dir, name)
        isfile(path) && return path
    end
    path = Sys.which(name)
    path !== nothing && return path
    error("7z binary not found")
end

function get_data()
    tmp = mktempdir()
    path = joinpath(tmp, basename(url))
    retry(Downloads.download, delays=fill(10,5))(url, path)
    Tar.extract(`$(exe7z()) x $path -so`, joinpath(tmp, "testfiles"))
    return joinpath(tmp, "testfiles", "toml-test-julia-$version", "testfiles")
end
