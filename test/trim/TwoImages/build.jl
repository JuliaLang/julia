# Custom build: produce two trimmed C-callable shared libraries from the sibling
# projects `TwoImagesA` and `TwoImagesB`, plus the C driver that loads them into
# one process. The libraries are not bundled, so both resolve the same libjulia
# from the build tree.
#
# Included in-process by `test/trim.jl`, so `run_juliac` and `run_cc` (defined
# there) are in scope. `ARGS[1]` is the output directory.
outdir = ARGS[1]
harnessdir = @__DIR__

for (name, pkg) in (("twoimages_a", "TwoImagesA"), ("twoimages_b", "TwoImagesB"))
    pkgdir = joinpath(harnessdir, pkg)
    run_juliac(String[
        "--output-lib", joinpath(outdir, name),
        "--project", pkgdir,
        "--compile-ccallable",
        "--trim=safe",
        "--experimental",
        joinpath(pkgdir, "src", pkg * ".jl"),
    ])
end

dlext = Base.BinaryPlatforms.platform_dlext()
for name in ("twoimages_a", "twoimages_b")
    isfile(joinpath(outdir, name * "." * dlext)) ||
        error("expected library at ", joinpath(outdir, name * "." * dlext))
end

bindir = joinpath(outdir, "bin")
mkpath(bindir)
exe = joinpath(bindir, "twoimages" * (Sys.iswindows() ? ".exe" : ""))
csrc = joinpath(harnessdir, "twoimages.c")
if Sys.islinux()
    run_cc(["-o", exe, csrc, "-ldl", "-lpthread"])
elseif Sys.isunix()
    run_cc(["-o", exe, csrc, "-lpthread"])
else
    run_cc(["-o", exe, csrc])
end
