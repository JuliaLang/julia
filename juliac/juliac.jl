cmd = Base.julia_cmd()
output_type = nothing  # exe, sharedlib, sysimage
static_call_graph = false
strict = false
verbose = false
outname = nothing
file = nothing

help = findfirst(x->x == "--help", ARGS)
if help !== nothing
    println(
        """
        Usage: julia juliac.jl [--output-exe | --output-lib | --output-sysimage] <name> [options] <file.jl>
        --static-call-graph  Only output code statically determined to be reachable
        --strict             Error if call graph cannot be fully statically determined
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
        elseif arg == "--strict"
            global strict = true
        elseif arg == "--static-call-graph"
            global static_call_graph = true
        elseif arg == "--verbose"
            global verbose = true
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
init_path = joinpath(tmpdir, "init.a")
img_path = joinpath(tmpdir, "img.a")
bc_path = joinpath(tmpdir, "img-bc.a")
tmp,io = mktemp(tmpdir, cleanup=false)
write(io, """
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
    let mod = Base.include(Base.__toplevel__, "$absfile")
        if !isa(mod, Module)
            mod = Main
        end
        if $(output_type == "--output-exe") && isdefined(mod, :main)
            precompile(mod.main, ())
        end
        precompile(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{Base.SubString{String}, 1}, String))
        precompile(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{String, 1}, Char))
    end
    Core.Compiler.track_newly_inferred.x = false
    @eval Base begin
        _assert_tostring(msg) = nothing
        reinit_stdio() = nothing
        JuliaSyntax.enable_in_core!() = nothing
        set_active_project(projfile::Union{AbstractString,Nothing}) = ACTIVE_PROJECT[] = projfile
        disable_library_threading() = nothing
    end
""")
close(io)

is_small_image() = static_call_graph ? `--small-image=yes` : ``
is_strict() = strict ? `--no-dispatch-precompile=yes` : ``
is_verbose() = verbose ? `--verbose-compilation=yes` : ``
cmd = addenv(`$cmd --project --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $(is_small_image()) $(is_strict()) $(is_verbose()) $tmp`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)
result = run(cmd)

result.exitcode == 0 || error("Failed to compile $file")

run(`cc $(cflags) -g -c -o $init_path $(joinpath(@__DIR__, "init.c"))`)

if output_type == "--output-lib" || output_type == "--output-sysimage"
    of, ext = splitext(outname)
    soext = "." * Base.BinaryPlatforms.platform_dlext()
    if ext == ""
        outname = of * soext
    end
end

if output_type == "--output-lib"
    run(`cc $(allflags) -o ./$outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path  -ljulia -ljulia-internal`)
elseif output_type == "--output-sysimage"
    run(`cc $(allflags) -o ./$outname -shared -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path  -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE)             -ljulia -ljulia-internal`)
else
    run(`cc $(allflags) -o ./$outname -Wl,$(Base.Linking.WHOLE_ARCHIVE) $img_path -Wl,$(Base.Linking.NO_WHOLE_ARCHIVE) $init_path -ljulia -ljulia-internal`)
end
