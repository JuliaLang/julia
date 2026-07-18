# Custom build: produce a trimmed C-callable shared library (with an ABI export)
# and compile a C application that loads it via `dlopen` at run-time.
#
# Included in-process by `test/trim.jl`, so `run_juliac` and `run_cc` (defined
# there) are in scope. `ARGS[1]` is the output/bundle directory.
outdir = ARGS[1]
projdir = @__DIR__

libname = "libsimple"
libout = joinpath(outdir, libname)
abiout = joinpath(outdir, "bindinginfo_libsimple.json")

run_juliac(String[
    "--output-lib", libout,
    "--project", projdir,
    "--compile-ccallable",
    "--trim=safe",
    "--experimental",
    joinpath(projdir, "src", "LibSimple.jl"),
    "--export-abi", abiout,
    "--bundle", outdir,
])

# Locate the bundled shared library (Windows places libraries under `bin/`).
dlext = Base.BinaryPlatforms.platform_dlext()
libdir = Sys.iswindows() ? "bin" : "lib"
libpath = joinpath(outdir, libdir, libname * "." * dlext)
isfile(libpath) || error("expected bundled library at $libpath")

# Compile the C application that dlopens the library.
bindir = joinpath(outdir, "bin")
mkpath(bindir)
exe = joinpath(bindir, "capplication" * (Sys.iswindows() ? ".exe" : ""))
csrc = joinpath(projdir, "capplication.c")
if Sys.islinux()
    run_cc(["-o", exe, csrc, "-ldl"])
else
    run_cc(["-o", exe, csrc])
end
