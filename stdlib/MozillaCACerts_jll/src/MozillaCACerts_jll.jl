# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MozillaCACerts_jll.jl

baremodule MozillaCACerts_jll
using Base
Base.Experimental.@compiler_options compile=min optimize=0 infer=false

const PATH_list = String[]
const LIBPATH_list = String[]

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
cacert = ""

function __init__()
    global artifact_dir = dirname(Sys.BINDIR)
    global cacert = normpath(Sys.BINDIR::String, Base.DATAROOTDIR, "julia", "cert.pem")
end

# JLLWrappers API compatibility shims.  Note that not all of these will really make sense.
# For instance, `find_artifact_dir()` won't actually be the artifact directory, because
# there isn't one.  It instead returns the overall Julia prefix.
is_available() = true
find_artifact_dir() = artifact_dir
dev_jll() = error("stdlib JLLs cannot be dev'ed")
best_wrapper = nothing

end # module
