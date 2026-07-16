# This file is a part of Julia. License is MIT: https://julialang.org/license

# Package and source introspection.

function find_source_file(path::AbstractString)
    (isabspath(path) || isfile(path)) && return path
    base_path = joinpath(Sys.BINDIR, DATAROOTDIR, "julia", "base", path)
    return isfile(base_path) ? normpath(base_path) : nothing
end

function get_pkgversion_from_path(path)
    project_file = locate_project_file(path)
    project_file isa String || return nothing
    version = get(parsed_toml(project_file), "version", nothing)
    return version === nothing ? nothing : VersionNumber(version::String)
end

"""
    pathof(m::Module)

Return the path of the `m.jl` file that was used to `import` module `m`,
or `nothing` if `m` was not imported from a package.

Use [`dirname`](@ref) to get the directory part and [`basename`](@ref)
to get the file name part of the path.

See also [`pkgdir`](@ref).
"""
function pathof(m::Module)
    @lock require_lock begin
    pkgid = PkgId(m)
    origin = get(pkgorigins, pkgid, nothing)
    origin === nothing && return nothing
    path = origin.path
    path === nothing && return nothing
    return fixup_stdlib_path(path)
    end
end

"""
    pkgdir(m::Module[, paths::String...])

Return the root directory of the package that declared module `m`,
or `nothing` if `m` was not declared in a package. Optionally further
path component strings can be provided to construct a path within the
package root.

To get the root directory of the package that implements the current module
the form `pkgdir(@__MODULE__)` can be used.

If an extension module is given, the root of the parent package is returned.

```julia-repl
julia> pkgdir(Foo)
"/path/to/Foo.jl"

julia> pkgdir(Foo, "src", "file.jl")
"/path/to/Foo.jl/src/file.jl"
```

See also [`pathof`](@ref).

!!! compat "Julia 1.7"
    The optional argument `paths` requires at least Julia 1.7.
"""
function pkgdir(m::Module, paths::String...)
    rootmodule = moduleroot(m)
    path = pathof(rootmodule)
    path === nothing && return nothing
    original = path
    path, base = splitdir(dirname(path))
    if base == "src"
        # package source in `../src/Foo.jl`
    elseif base == "ext"
        # extension source in `../ext/FooExt.jl`
    elseif basename(path) == "ext"
        # extension source in `../ext/FooExt/FooExt.jl`
        path = dirname(path)
    else
        error("Unexpected path structure for module source: $original")
    end
    return joinpath(path, paths...)
end

"""
    pkgversion(m::Module)

If the module `m` belongs to a versioned package, return the
version number of that package. Otherwise return `nothing`.

The version is read from the package's Project.toml during package
load.

To get the version of the package that imported the current module
the form `pkgversion(@__MODULE__)` can be used.

!!! compat "Julia 1.9"
    This function was introduced in Julia 1.9.
"""
function pkgversion(m::Module)
    @lock require_lock begin
        pkgorigin = get(pkgorigins, PkgId(moduleroot(m)), nothing)
        if pkgorigin !== nothing && pkgorigin.version !== nothing
            return pkgorigin.version
        end
        path = pkgdir(m)
        path === nothing && return nothing
        v = get_pkgversion_from_path(path)
        if pkgorigin !== nothing
            pkgorigin.version = v
        end
        return v
    end
end
