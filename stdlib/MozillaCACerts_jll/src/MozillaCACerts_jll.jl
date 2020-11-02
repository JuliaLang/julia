# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/MozillaCACerts_jll.jl

module MozillaCACerts_jll

function __init__()
	global cacert = normpath(Sys.BINDIR::String, Base.DATAROOTDIR, "julia", "cert.pem")
end

end # module
