# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MozillaCACerts_jll.jl

module MozillaCACerts_jll

# These get calculated in __init__()
PATH = Ref("")
LIBPATH = Ref("")
artifact_dir = ""
cacert = ""

function __init__()
	global artifact_dir = dirname(Sys.BINDIR)
	global LIBPATH[] = joinpath(Sys.BINDIR, Base.LIBDIR, "julia")
	global cacert = normpath(Sys.BINDIR::String, Base.DATAROOTDIR, "julia", "cert.pem")
end

is_available() = true

end # module
