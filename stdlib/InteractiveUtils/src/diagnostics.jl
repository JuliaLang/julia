"""
    InteractiveUtils.diagnostics(io::IO=stderr)

Print a variety of useful debugging info.

!!! warning "Warning"
    The output of this function may contain sensitive information. Before sharing the output,
    please review the output and remove any data that should not be shared publicly.

See also: [`versioninfo`](@ref), [`stdlib_diagnostics`](@ref).
"""
diagnostics()                        = diagnostics(stderr)
diagnostics(modules::Vector{Module}) = diagnostics(stderr, modules)
diagnostics(m::Module)               = diagnostics(stderr, m)
function diagnostics(io::IO)
    # Get the list of all modules that are currently loaded
    loaded_modules = copy(Base.loaded_modules_array())
    sort!(loaded_modules; by = nameof)
    diagnostics(io, loaded_modules)
    return nothing
end
function diagnostics(io::IO, modules::Vector{Module})
    for m in modules
        diagnostics(io, m)
    end
    return nothing
end
function diagnostics(io::IO, m::Module)
    module_name = nameof(m)
    if isdefined(m, :__diagnostics__) && applicable(m.__diagnostics__, io)
        header = "# $(module_name)"
        println(io, header)
        try
            m.__diagnostics__(io)
        catch ex
            bt = catch_backtrace()
            msg = "Warning: caught an error while running $(module_name).__diagnostics__()"
            println(io, msg)
            Base.showerror(io, ex)
            Base.show_backtrace(io, bt)
        end
    end
    return nothing
end

"""
    InteractiveUtils.stdlib_diagnostics(io::IO=stderr)

Print a variety of useful debugging info, but only for the standard libraries (stdlibs).

!!! warning "Warning"
    The output of this function may contain sensitive information. Before sharing the output,
    please review the output and remove any data that should not be shared publicly.

See also: [`versioninfo`](@ref), [`diagnostics`](@ref).
"""
stdlib_diagnostics() = stdlib_diagnostics(stderr)
function stdlib_diagnostics(io::IO)
    stdlib_modules = _loaded_stdlibs()
    sort!(stdlib_modules; by = nameof)
    diagnostics(io, stdlib_modules)
    return nothing
end

# Copied from https://github.com/JuliaLang/Pkg.jl/blob/68ba0adc9b686db83dd65676cdd2c55313bd2520/src/utils.jl#L27
_stdlib_dir() = normpath(joinpath(Sys.BINDIR::String, "..", "share", "julia", "stdlib", "v$(VERSION.major).$(VERSION.minor)"))

_stdlib_names() = readdir(_stdlib_dir())

# Bake this into the sysimage
const _STDLIB_NAMES = _stdlib_names()

function _loaded_stdlibs()
    modules = copy(Base.loaded_modules_array())
    stdlib_names = copy(_STDLIB_NAMES)
    push!(stdlib_names, "Base")
    push!(stdlib_names, "Core")
    filter!(m -> String(nameof(m)) in stdlib_names, modules)
    return modules
end

# InteractiveUtils.__diagnostics__()
function __diagnostics__(io::IO)
    indent = "  "
    versioninfo(io; verbose = true) # InteractiveUtils.versioninfo()
    println(io, "Miscellaneous Info:")
    if !_is_tagged_commit()
        println(io, indent, "INFO: It looks like you are probably not using the official binaries from julialang.org")
    end
    startup_files = [
        "Global" => _global_julia_startup_file(),
        "Local"  => _local_julia_startup_file(),
    ]
    for (type, filename) in startup_files
        if filename === nothing
            description = "does not exist"
        else
            str = strip(read(filename, String))
            expr = Base.Meta.parse(str; raise = false)
            if expr === nothing
                description = "is empty"
            else
                description = "exists and contains code ($(filename))"
            end
        end
        println(io, indent, type, " startup file: ", description)
    end
    println(io, indent, "Base.julia_cmd(): ", Base.julia_cmd())
    return nothing
end

# We can't say with 100% certainty whether or not this is the official binary
# from julialang.org, but we can make an educated guess.
function _is_tagged_commit()
    tagged_commit = Base.GIT_VERSION_INFO.tagged_commit
    if !tagged_commit
        return false
    end
    banner = Base.TAGGED_RELEASE_BANNER
    if banner == "Official https://julialang.org/ release"
        return true
    end
    return false
end
