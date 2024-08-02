using Test

tmpdir = mktempdir()
img_path = joinpath(tmpdir, "img.a")
init_path = joinpath(tmpdir, "init.a")
exe_path = joinpath(tmpdir, "hello")

cmd = Base.julia_cmd()
cmd = `$cmd --startup-file=no --history-file=no`

cflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --cflags `)
cflags = Base.shell_split(cflags)
allflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --allflags`)
allflags = Base.shell_split(allflags)

cmd = addenv(`$cmd --output-o $img_path --output-incremental=no --strip-ir --strip-metadata --static-call-graph $(joinpath(@__DIR__,"buildscript.jl")) hello.jl --output-exe true`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)

@test success(cmd)

@test success(`cc $(cflags) -g -c -o $init_path $(joinpath(@__DIR__, "init.c"))`)

julia_libs = Base.shell_split(Base.isdebugbuild() ? "-ljulia-debug -ljulia-internal-debug" : "-ljulia -ljulia-internal")

@test success(`cc $(allflags) -o $exe_path -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path $(julia_libs)`)

@test readchomp(`$exe_path`) == "Hello, world!"

@test filesize(exe_path) < filesize(unsafe_string(Base.JLOptions().image_file))/10
