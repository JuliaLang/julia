# Julia compiler wrapper script
# NOTE: The interface and location of this script are considered unstable/experimental

module JuliaConfig
    include(joinpath(@__DIR__, "julia-config.jl"))
end

julia_cmd = `$(Base.julia_cmd()) --startup-file=no --history-file=no`
cpu_target = get(ENV, "JULIA_CPU_TARGET", nothing)
julia_cmd_target =  `$(Base.julia_cmd(;cpu_target)) --startup-file=no --history-file=no`
output_type = nothing  # exe, sharedlib, sysimage
outname = nothing
file = nothing
add_ccallables = false
relative_rpath = false
verbose = false

help = findfirst(x->x == "--help", ARGS)
if help !== nothing
    println(
        """
        Usage: julia juliac.jl [--output-exe | --output-lib | --output-sysimage] <name> [options] <file.jl>
        --experimental --trim=<no,safe,unsafe,unsafe-warn>  Only output code statically determined to be reachable
        --compile-ccallable  Include all methods marked `@ccallable` in output
        --relative-rpath     Configure the library / executable to lookup all required libraries in an adjacent "julia/" folder
        --verbose            Request verbose output
        """)
    exit(0)
end

# arguments to forward to julia compilation process
julia_args = []

let i = 1
    while i <= length(ARGS)
        arg = ARGS[i]
        if arg == "--output-exe" || arg == "--output-lib" || arg == "--output-sysimage"
            isnothing(output_type) || error("Multiple output types specified")
            global output_type = arg
            i == length(ARGS) && error("Output specifier requires an argument")
            global outname = ARGS[i+1]
            i += 1
        elseif arg == "--compile-ccallable"
            global add_ccallables = true
        elseif arg == "--verbose"
            global verbose = true
        elseif arg == "--relative-rpath"
            global relative_rpath = true
        elseif startswith(arg, "--trim") || arg == "--experimental"
            # forwarded args
            push!(julia_args, arg)
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

function get_rpath(; relative::Bool = false)
    if relative
        if Sys.isapple()
            return "-Wl,-rpath,'@loader_path/julia/' -Wl,-rpath,'@loader_path/'"
        elseif Sys.islinux()
            return "-Wl,-rpath,'\$ORIGIN/julia/' -Wl,-rpath,'\$ORIGIN/'"
        else
            error("unimplemented")
        end
    else
        return JuliaConfig.ldrpath()
    end
end

absfile = abspath(file)
cflags = JuliaConfig.cflags(; framework=false)
cflags = Base.shell_split(cflags)
allflags = JuliaConfig.allflags(; framework=false, rpath=false)
allflags = Base.shell_split(allflags)
rpath = get_rpath(; relative = relative_rpath)
rpath = Base.shell_split(rpath)
tmpdir = mktempdir(cleanup=false)
initsrc_path = joinpath(tmpdir, "init.c")
init_path = joinpath(tmpdir, "init.a")
img_path = joinpath(tmpdir, "img.a")
bc_path = joinpath(tmpdir, "img-bc.a")


function precompile_env()
    # Pre-compile the environment
    # (otherwise obscure error messages will occur)
    cmd = addenv(`$julia_cmd --project=$(Base.active_project()) -e "using Pkg; Pkg.precompile()"`)
    verbose && println("Running: $cmd")
    if !success(pipeline(cmd; stdout, stderr))
        println(stderr, "\nError encountered during pre-compilation of environment.")
        exit(1)
    end
end

function compile_products()
    # Compile the Julia code
    cmd = addenv(`$julia_cmd_target --project=$(Base.active_project()) --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $julia_args $(joinpath(@__DIR__,"juliac-buildscript.jl")) $absfile $output_type $add_ccallables`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)
    verbose && println("Running: $cmd")
    if !success(pipeline(cmd; stdout, stderr))
        println(stderr, "\nFailed to compile $file")
        exit(1)
    end

    # Compile the initialization code
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
    run(`cc $(cflags) -g -c -o $init_path $initsrc_path`)
end

function link_products()
    global outname
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
            cmd2 = `cc $(allflags) $(rpath) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path  $(julia_libs)`
        elseif output_type == "--output-sysimage"
            cmd2 = `cc $(allflags) $(rpath) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)             $(julia_libs)`
        else
            cmd2 = `cc $(allflags) $(rpath) -o $outname -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path $(julia_libs)`
        end
        verbose && println("Running: $cmd2")
        run(cmd2)
    catch e
        println("\nCompilation failed: ", e)
        exit(1)
    end
end

precompile_env()
compile_products()
link_products()
