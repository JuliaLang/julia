# Julia compiler wrapper script
# NOTE: The interface and location of this script are considered unstable/experimental

cmd = Base.julia_cmd()
cmd = `$cmd --startup-file=no --history-file=no`
output_type = nothing  # exe, sharedlib, sysimage
trim = nothing
outname = nothing
file = nothing
add_ccallables = false

help = findfirst(x->x == "--help", ARGS)
if help !== nothing
    println(
        """
        Usage: julia juliac.jl [--output-exe | --output-lib | --output-sysimage] <name> [options] <file.jl>
        --trim=<no,safe,unsafe,unsafe-warn>  Only output code statically determined to be reachable
        --compile-ccallable  Include all methods marked `@ccallable` in output
        --verbose            Request verbose output
        """)
    exit(0)
end

let i = 1
    while i <= length(ARGS)
        arg = ARGS[i]
        if arg == "--output-exe" || arg == "--output-lib" || arg == "--output-sysimage"
            isnothing(output_type) || error("Multiple output types specified")
            global output_type = arg
            i == length(ARGS) && error("Output specifier requires an argument")
            global outname = ARGS[i+1]
            i += 1
        elseif startswith(arg, "--trim")
            arg = split(arg, '=')
            if length(arg) == 1
                global trim = "safe"
            else
                global trim = arg[2]
            end
        elseif arg == "--compile-ccallable"
            global add_ccallables = true
        else
            if arg[1] == '-' || !isnothing(file)
                println("Unexpected argument `$arg`")
                exit(1)
            end
            global file = arg
        end
        i += 1
    end
end

isnothing(outname) && error("No output file specified")
isnothing(file) && error("No input file specified")

absfile = abspath(file)
cflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --cflags `)
cflags = Base.shell_split(cflags)
allflags = readchomp(`$(cmd) $(joinpath(Sys.BINDIR, Base.DATAROOTDIR,"julia", "julia-config.jl")) --allflags`)
allflags = Base.shell_split(allflags)
tmpdir = mktempdir(cleanup=false)
initsrc_path = joinpath(tmpdir, "init.c")
init_path = joinpath(tmpdir, "init.a")
img_path = joinpath(tmpdir, "img.a")
bc_path = joinpath(tmpdir, "img-bc.a")

open(initsrc_path, "w") do io
    print(io, """
              #include <julia.h>
              __attribute__((constructor)) void static_init(void) {
                  if (jl_is_initialized())
                      return;
                  julia_init(JL_IMAGE_IN_MEMORY);
                  jl_exception_clear();
              }
              """)
end

static_call_graph_arg() = isnothing(trim) ?  `` : `--trim=$(trim)`
is_verbose() = verbose ? `--verbose-compilation=yes` : ``
cmd = addenv(`$cmd --project=$(Base.active_project()) --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $(static_call_graph_arg()) $(joinpath(@__DIR__,"juliac-buildscript.jl")) $absfile $output_type $add_ccallables`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)

if !success(pipeline(cmd; stdout, stderr))
    println(stderr, "\nFailed to compile $file")
    exit(1)
end

run(`cc $(cflags) -g -c -o $init_path $initsrc_path`)

if output_type == "--output-lib" || output_type == "--output-sysimage"
    of, ext = splitext(outname)
    soext = "." * Base.BinaryPlatforms.platform_dlext()
    if ext == ""
        outname = of * soext
    end
end

julia_libs = Base.shell_split(Base.isdebugbuild() ? "-ljulia-debug -ljulia-internal-debug" : "-ljulia -ljulia-internal")
try
    if output_type == "--output-lib"
        run(`cc $(allflags) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path  $(julia_libs)`)
    elseif output_type == "--output-sysimage"
        run(`cc $(allflags) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)             $(julia_libs)`)
    else
        run(`cc $(allflags) -o $outname -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path $(julia_libs)`)
    end
catch
    println("\nCompilation failed.")
    exit(1)
end
