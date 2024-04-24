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
julia_libs = Base.shell_split(Base.isdebugbuild() ? "-ljulia-debug -ljulia-internal-debug" : "-ljulia -ljulia-internal")
write(io, """
    Sys.__init__()
    Base.init_depot_path()
    Base.init_load_path()
    Base.init_active_project()
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae
    uuid_tuple = (UInt64(0), UInt64(0))
    ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
    ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)
    (f::Base.RedirectStdStream)(io::Core.CoreSTDOUT) = Base._redirect_io_global(io, f.unix_fd)
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
        _assert_tostring(msg) = ""
        reinit_stdio() = nothing
        JuliaSyntax.enable_in_core!() = nothing
        set_active_project(projfile::Union{AbstractString,Nothing}) = ACTIVE_PROJECT[] = projfile
        disable_library_threading() = nothing
        @inline function invokelatest(f::F, args...; kwargs...) where F
            return f(args...; kwargs...)
        end
        function sprint(f::F, args::Vararg{Any,N}; context=nothing, sizehint::Integer=0) where {F<:Function,N}
            s = IOBuffer(sizehint=sizehint)
            if context isa Tuple
                f(IOContext(s, context...), args...)
            elseif context !== nothing
                f(IOContext(s, context), args...)
            else
                f(s, args...)
            end
            String(_unsafe_take!(s))
        end
        show(io::IO, T::Type) = print(io, "Type")
    end
    @eval Core begin
        DomainError(@nospecialize(val), @nospecialize(msg::AbstractString)) = (@noinline; \$(Expr(:new, :DomainError, :val, :msg)))
    end
    @eval Base.Unicode begin
        function utf8proc_map(str::Union{String,SubString{String}}, options::Integer, chartransform::F = identity) where F
            nwords = utf8proc_decompose(str, options, C_NULL, 0, chartransform)
            buffer = Base.StringVector(nwords*4)
            nwords = utf8proc_decompose(str, options, buffer, nwords, chartransform)
            nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
            nbytes < 0 && utf8proc_error(nbytes)
            return String(resize!(buffer, nbytes))
        end
    end
    @eval Base.GMP begin
        function __init__()
            try
                ccall((:__gmp_set_memory_functions, libgmp), Cvoid,
                    (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}),
                    cglobal(:jl_gc_counted_malloc),
                    cglobal(:jl_gc_counted_realloc_with_old_size),
                    cglobal(:jl_gc_counted_free_with_size))
                ZERO.alloc, ZERO.size, ZERO.d = 0, 0, C_NULL
                ONE.alloc, ONE.size, ONE.d = 1, 1, pointer(_ONE)
            catch ex
                Base.showerror_nostdio(ex, "WARNING: Error during initialization of module GMP")
            end
            # This only works with a patched version of GMP, ignore otherwise
            try
                ccall((:__gmp_set_alloc_overflow_function, libgmp), Cvoid,
                    (Ptr{Cvoid},),
                    cglobal(:jl_throw_out_of_memory_error))
                ALLOC_OVERFLOW_FUNCTION[] = true
            catch ex
                # ErrorException("ccall: could not find function...")
                if typeof(ex) != ErrorException
                    rethrow()
                end
            end
        end
    end
    @eval Base.Sort begin
        issorted(itr;
            lt::T=isless, by::F=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) where {T,F} =
            issorted(itr, ord(lt,by,rev,order))
    end
    @eval Base.TOML begin
        function try_return_datetime(p, year, month, day, h, m, s, ms)
            return DateTime(year, month, day, h, m, s, ms)
        end
        function try_return_date(p, year, month, day)
            return Date(year, month, day)
        end
        function parse_local_time(l::Parser)
            h = @try parse_int(l, false)
            h in 0:23 || return ParserError(ErrParsingDateTime)
            _, m, s, ms = @try _parse_local_time(l, true)
            # TODO: Could potentially parse greater accuracy for the
            # fractional seconds here.
            return try_return_time(l, h, m, s, ms)
        end
        function try_return_time(p, h, m, s, ms)
            return Time(h, m, s, ms)
        end
    end
    let loaded = Symbol.(Base.loaded_modules_array())  # TODO better way to do this
        if :LinearAlgebra in loaded
            using LinearAlgebra
            @eval LinearAlgebra.BLAS begin
                check() = nothing #TODO: this might be unsafe but needs logging macro fixes
            end
        end
        if :SparseArrays in loaded
            using SparseArrays
            @eval SparseArrays.CHOLMOD begin
                function __init__()
                    ccall((:SuiteSparse_config_malloc_func_set, :libsuitesparseconfig),
                        Cvoid, (Ptr{Cvoid},), cglobal(:jl_malloc, Ptr{Cvoid}))
                    ccall((:SuiteSparse_config_calloc_func_set, :libsuitesparseconfig),
                        Cvoid, (Ptr{Cvoid},), cglobal(:jl_calloc, Ptr{Cvoid}))
                    ccall((:SuiteSparse_config_realloc_func_set, :libsuitesparseconfig),
                        Cvoid, (Ptr{Cvoid},), cglobal(:jl_realloc, Ptr{Cvoid}))
                    ccall((:SuiteSparse_config_free_func_set, :libsuitesparseconfig),
                    Cvoid, (Ptr{Cvoid},), cglobal(:jl_free, Ptr{Cvoid}))
                end
            end
        end
    end
""")
close(io)

is_small_image() = static_call_graph ? `--small-image=yes` : ``
is_strict() = strict ? `--no-dispatch-precompile=yes` : ``
is_verbose() = verbose ? `--verbose-compilation=yes` : ``
cmd = addenv(`$cmd --project=$(Base.active_project()) --output-o $img_path --output-incremental=no --strip-ir --strip-metadata $(is_small_image()) $(is_strict()) $(is_verbose()) $tmp`, "OPENBLAS_NUM_THREADS" => 1, "JULIA_NUM_THREADS" => 1)
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
