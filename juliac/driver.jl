cmd = Base.julia_cmd()
shared_lib = false
small_image = true
help = findfirst(x->x == "--help", ARGS)

if help !== nothing
    println("Usage: julia driver.jl [-shared] [-not-small] <file.jl>")
    exit(0)
end

idx = findfirst(x->x == "-shared", ARGS)
if idx !== nothing
    shared_lib = true
    deleteat!(ARGS, idx)
end
idx = findfirst(x->x == "-not-small", ARGS)
if idx !== nothing
    small_image = false
    println("Not using small image. This is for debugging")
    deleteat!(ARGS, idx)
end

if length(ARGS) != 1
    println("Unexpected number of arguments, usage is: julia driver.jl [-shared] <file.jl>")
    exit(1)
end
file = ARGS[1]
if shared_lib
    println("Compiling shared library based on $file")
else
    println("Compiling executable based on $file")
end
absfile = abspath(file)
cflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --cflags `)
cflags = Base.shell_split(cflags)
allflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --allflags`)
allflags = Base.shell_split(allflags)
tmpdir = mktempdir(cleanup=false)
init_path = joinpath(tmpdir, "init.a")
img_path = joinpath(tmpdir, "img.a")
bc_path = joinpath(tmpdir, "img-bc.a")
tmp,io = mktemp(tmpdir, cleanup=false)
write(io, """
    Base.reinit_stdio()
    Sys.__init__()
    copy!(LOAD_PATH, ["."]) # Only allow loading packages from current project
    Base.init_depot_path()
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae
    uuid_tuple = (UInt64(0), UInt64(0))
    ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
    ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)
    Core.Compiler.track_newly_inferred.x = true
    Base.include(Base.__toplevel__, "$absfile")
    Core.Compiler.track_newly_inferred.x = false
""")
close(io)
if small_image
    withenv("JULIA_SMALL_IMAGE" => 1, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1) do
        global result = run(`$cmd --project --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $tmp`)
    end

else
    withenv( "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1) do
        global result = run(`$cmd --project  --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $tmp`)
    end
end
result.exitcode == 0 || error("Failed to compile $file")

run(`cc $(cflags) -g -c -o $init_path $(joinpath(@__DIR__, "init.c"))`)

if !shared_lib
    run(`cc $(allflags) -o ./test-o -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path -ljulia -ljulia-internal`)
else
    run(`cc $(allflags) -o ./libtest.$(Base.BinaryPlatforms.platform_dlext()) -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path  -ljulia -ljulia-internal`)
end
