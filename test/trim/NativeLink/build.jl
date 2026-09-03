# Custom build: compile the source as a script, so its top-level registration of the
# foreign link policy runs in the compiler process. Included in-process by
# `test/trim.jl`, so `run_juliac` is in scope. `ARGS[1]` is the bundle directory.
outdir = ARGS[1]
projdir = @__DIR__

run_juliac(String[
    "--output-exe", "nativelink",
    "--project", projdir,
    "--trim=safe",
    "--experimental",
    joinpath(projdir, "src", "NativeLink.jl"),
    "--bundle", outdir,
])
