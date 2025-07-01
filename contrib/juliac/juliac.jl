# This file is a part of Julia. License is MIT: https://julialang.org/license

# Julia compiler wrapper script
# NOTE: The interface and location of this script are considered unstable/experimental

using LazyArtifacts

module JuliaConfig
    include(joinpath(@__DIR__, "..", "julia-config.jl"))
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

# Copied from PackageCompiler
# https://github.com/JuliaLang/PackageCompiler.jl/blob/1c35331d8ef81494f054bbc71214811253101993/src/PackageCompiler.jl#L147-L190
function get_compiler_cmd(; cplusplus::Bool=false)
    cc = get(ENV, "JULIA_CC", nothing)
    path = nothing
    @static if Sys.iswindows()
        path = joinpath(LazyArtifacts.artifact"mingw-w64",
                        "extracted_files",
                        (Int==Int64 ? "mingw64" : "mingw32"),
                        "bin",
                        cplusplus ? "g++.exe" : "gcc.exe")
        compiler_cmd = `$path`
    end
    if cc !== nothing
        compiler_cmd = Cmd(Base.shell_split(cc))
        path = nothing
    elseif !Sys.iswindows()
        compilers_cpp = ("g++", "clang++")
        compilers_c = ("gcc", "clang")
        found_compiler = false
        if cplusplus
            for compiler in compilers_cpp
                if Sys.which(compiler) !== nothing
                    compiler_cmd = `$compiler`
                    found_compiler = true
                    break
                end
            end
        end
        if !found_compiler
            for compiler in compilers_c
                if Sys.which(compiler) !== nothing
                    compiler_cmd = `$compiler`
                    found_compiler = true
                    if cplusplus && !WARNED_CPP_COMPILER[]
                        @warn "could not find a c++ compiler (g++ or clang++), falling back to $compiler, this might cause link errors"
                        WARNED_CPP_COMPILER[] = true
                    end
                    break
                end
            end
        end
        found_compiler || error("could not find a compiler, looked for ",
            join(((cplusplus ? compilers_cpp : ())..., compilers_c...), ", ", " and "))
    end
    if path !== nothing
        compiler_cmd = addenv(compiler_cmd, "PATH" => string(ENV["PATH"], ";", dirname(path)))
    end
    return compiler_cmd
end

# arguments to forward to julia compilation process
julia_args = []
enable_trim::Bool = false

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
        elseif startswith(arg, "--trim")
            global enable_trim = arg != "--trim=no"
            push!(julia_args, arg) # forwarded arg
        elseif arg == "--experimental"
            push!(julia_args, arg) # forwarded arg
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

cc = get_compiler_cmd()
absfile = abspath(file)
cflags = JuliaConfig.cflags(; framework=false)
cflags = Base.shell_split(cflags)
allflags = JuliaConfig.allflags(; framework=false, rpath=false)
allflags = Base.shell_split(allflags)
rpath = get_rpath(; relative = relative_rpath)
rpath = Base.shell_split(rpath)
tmpdir = mktempdir(cleanup=false)
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

function compile_products(enable_trim::Bool)

    # Only strip IR / metadata if not `--trim=no`
    strip_args = String[]
    if enable_trim
        push!(strip_args, "--strip-ir")
        push!(strip_args, "--strip-metadata")
    end

    # Compile the Julia code
    cmd = addenv(`$julia_cmd_target --project=$(Base.active_project()) --output-o $img_path --output-incremental=no $strip_args $julia_args $(joinpath(@__DIR__,"juliac-buildscript.jl")) $absfile $output_type $add_ccallables`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)
    verbose && println("Running: $cmd")
    if !success(pipeline(cmd; stdout, stderr))
        println(stderr, "\nFailed to compile $file")
        exit(1)
    end
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
            cmd2 = `$(cc) $(allflags) $(rpath) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)  $(julia_libs)`
        elseif output_type == "--output-sysimage"
            cmd2 = `$(cc) $(allflags) $(rpath) -o $outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)             $(julia_libs)`
        else
            cmd2 = `$(cc) $(allflags) $(rpath) -o $outname -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)  $(julia_libs)`
        end
        verbose && println("Running: $cmd2")
        run(cmd2)
    catch e
        println("\nCompilation failed: ", e)
        exit(1)
    end
end

precompile_env()
compile_products(enable_trim)
link_products()
