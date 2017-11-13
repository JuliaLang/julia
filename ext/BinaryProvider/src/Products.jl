export Product, LibraryProduct, FileProduct, ExecutableProduct, satisfied, locate, @write_deps_file

"""
A `Product` is an expected result after building or installation of a package.
"""
abstract Product

"""
A `LibraryProduct` is a special kind of `Product` that not only needs to exist,
but needs to be `dlopen()`'able.  You must know which directory the library
will be installed to, and its name, e.g. to build a `LibraryProduct` that
refers to `"/lib/libnettle.so"`, the "directory" would be "/lib", and the
"libname" would be "libnettle".
"""
immutable LibraryProduct <: Product
    dir_path::String
    libname::String

    """
    `LibraryProduct(prefix::Prefix, libname::AbstractString)`

    Declares a `LibraryProduct` that points to a library located within the
    `libdir` of the given `Prefix`, with a name containing `libname`.  As an
    example, given that `libdir(prefix)` is equal to `usr/lib`, and `libname`
    is equal to `libnettle`, this would be satisfied by the following paths:

        usr/lib/libnettle.so
        usr/lib/libnettle.so.6
        usr/lib/libnettle.6.dylib
        usr/lib/libnettle-6.dll

    Libraries matching the search pattern are rejected if they are not
    `dlopen()`'able.
    """
    function LibraryProduct(prefix::Prefix, libname::AbstractString)
        return LibraryProduct(libdir(prefix), libname)
    end

    """
    `LibraryProduct(dir_path::AbstractString, libname::AbstractString)`

    For finer-grained control over `LibraryProduct` locations, you may directly
    pass in the `dir_path` instead of auto-inferring it from `libdir(prefix)`.
    """
    function LibraryProduct(dir_path::AbstractString, libname::AbstractString)
        return new(dir_path, libname)
    end
end

"""
locate(lp::LibraryProduct; verbose::Bool = false,
        platform::Platform = platform_key())

If the given library exists (under any reasonable name) and is `dlopen()`able,
(assuming it was built for the current platform) return its location.  Note
that the `dlopen()` test is only run if the current platform matches the given
`platform` keyword argument, as cross-compiled libraries cannot be `dlopen()`ed
on foreign platforms.
"""
function locate(lp::LibraryProduct; verbose::Bool = false,
                platform::Platform = platform_key())
    if !isdir(lp.dir_path)
        if verbose
            info("Directory $(lp.dir_path) does not exist!")
        end
        return nothing
    end
    for f in readdir(lp.dir_path)
        # Skip any names that aren't a valid dynamic library for the given
        # platform (note this will cause problems if something compiles a `.so`
        # on OSX, for instance)
        if !valid_dl_path(f, platform)
            continue
        end

        if verbose
            info("Found a valid dl path $(f) while looking for $(lp.libname)")
        end

        # If we found something that is a dynamic library, let's check to see
        # if it matches our libname:
        if startswith(basename(f), lp.libname)
            dl_path = abspath(joinpath(lp.dir_path), f)
            if verbose
                info("$(dl_path) matches our search criteria of $(lp.libname)")
            end

            # If it does, try to `dlopen()` it if the current platform is good
            if platform == platform_key()
                hdl = Libdl.dlopen_e(dl_path)
                if hdl == C_NULL
                    if verbose
                        info("$(dl_path) cannot be dlopen'ed")
                    end
                else
                    # Hey!  It worked!  Yay!
                    Libdl.dlclose(hdl)
                    return dl_path
                end
            else
                # If the current platform doesn't match, then just trust in our
                # cross-compilers and go with the flow
                return dl_path
            end
        end
    end

    if verbose
        info("Could not locate $(lp.libname) inside $(lp.dir_path)")
    end
    return nothing
end

"""
An `ExecutableProduct` is a `Product` that represents an executable file.

On all platforms, an ExecutableProduct checks for existence of the file.  On
non-Windows platforms, it will check for the executable bit being set.  On
Windows platforms, it will check that the file ends with ".exe", (adding it on
automatically, if it is not already present).
"""
immutable ExecutableProduct <: Product
    path::AbstractString

    """
    `ExecutableProduct(prefix::Prefix, binname::AbstractString)`

    Declares an `ExecutableProduct` that points to an executable located within
    the `bindir` of the given `Prefix`, named `binname`.
    """
    function ExecutableProduct(prefix::Prefix, binname::AbstractString)
        return ExecutableProduct(joinpath(bindir(prefix), binname))
    end

    """
    `ExecutableProduct(binpath::AbstractString)`

    For finer-grained control over `ExecutableProduct` locations, you may directly
    pass in the full `binpath` instead of auto-inferring it from `bindir(prefix)`.
    """
    function ExecutableProduct(binpath::AbstractString)
        return new(binpath)
    end
end

"""
`locate(fp::FileProduct; platform::Platform = platform_key(),
                         verbose::Bool = false)`

If the given executable file exists and is executable, return its path.

On all platforms, an ExecutableProduct checks for existence of the file.  On
non-Windows platforms, it will check for the executable bit being set.  On
Windows platforms, it will check that the file ends with ".exe", (adding it on
automatically, if it is not already present).
"""
function locate(ep::ExecutableProduct; platform::Platform = platform_key(),
                verbose::Bool = false)
    # On windows, we always slap an .exe onto the end if it doesn't already
    # exist, as Windows won't execute files that don't have a .exe at the end.
    path = if platform isa Windows && !endswith(ep.path, ".exe")
        "$(ep.path).exe"
    else
        ep.path
    end

    if !isfile(path)
        if verbose
            info("$(ep.path) does not exist, reporting unsatisfied")
        end
        return nothing
    end

    # If the file is not executable, fail out (unless we're on windows since
    # windows doesn't honor these permissions on its filesystems)
    @static if !is_windows()
        if uperm(path) & 0x1 == 0
            if verbose
                info("$(path) is not executable, reporting unsatisfied")
            end
            return nothing
        end
    end

    return path
end

"""
A `FileProduct` represents a file that simply must exist to be satisfied.
"""
immutable FileProduct <: Product
    path::AbstractString
end

"""
locate(fp::FileProduct; platform::Platform = platform_key(),
                        verbose::Bool = false)

If the given file exists, return its path.  The platform argument is ignored
here, but included for uniformity.
"""
function locate(fp::FileProduct; platform::Platform = platform_key(),
                                 verbose::Bool = false)
    if isfile(fp.path)
        if verbose
            info("FileProduct $(fp.path) does not exist")
        end
        return fp.path
    end
    return nothing
end


"""
`satisfied(p::Product; platform::Platform = platform_key(),
                       verbose::Bool = false)`

Given a `Product`, return `true` if that `Product` is satisfied, e.g. whether
a file exists that matches all criteria setup for that `Product`.
"""
function satisfied(p::Product; platform::Platform = platform_key(),
                               verbose::Bool = false)
    return locate(p; platform=platform, verbose=verbose) != nothing
end


"""
`@write_deps_file(products...)`

Helper macro to generate a `deps.jl` file out of a mapping of variable name
to  `Product` objects. Call using something like:

    fooifier = ExecutableProduct(...)
    libbar = LibraryProduct(...)
    @write_deps_file fooifier libbar

If any `Product` object cannot be satisfied (e.g. `LibraryProduct` objects must
be `dlopen()`-able, `FileProduct` objects must exist on the filesystem, etc...)
this macro will error out.  Ensure that you have used `install()` to install
the binaries you wish to write a `deps.jl` file for, and, optionally that you
have used `activate()` on the `Prefix` in which the binaries were installed so
as to make sure that the binaries are locatable.

The result of this macro call is a `deps.jl` file containing variables named
the same as the keys of the passed-in dictionary, holding the full path to the
installed binaries.  Given the example above, it would contain code similar to:

    global const fooifier = "<pkg path>/deps/usr/bin/fooifier"
    global const libbar = "<pkg path>/deps/usr/lib/libbar.so"

This file is intended to be `include()`'ed from within the `__init__()` method
of your package.  Note that all files are checked for consistency on package
load time, and if an error is discovered, package loading will fail, asking
the user to re-run `Pkg.build("package_name")`.
"""
macro write_deps_file(capture...)
    # props to @tshort for his macro wizardry
    const names = :($(capture))
    const products = esc(Expr(:tuple, capture...))

    # We have to create this dummy_source, because we cannot, in a single line,
    # have both `@__FILE__` and `__source__` interpreted by the same julia.
    const dummy_source = VERSION >= v"0.7.0-" ? __source__.file : ""

    return quote
        # First pick up important pieces of information from the call-site
        const source = VERSION >= v"0.7.0-" ? $("$(dummy_source)") : @__FILE__
        const depsjl_path = joinpath(dirname(source), "deps.jl")
        const package_name = basename(dirname(dirname(source)))
        const platform = platform_key()
        escape_path = path -> replace(path, "\\", "\\\\")

        const rebuild = strip("""
        Please re-run Pkg.build(\\\"$(package_name)\\\"), and restart Julia.
        """)

        # Begin by ensuring that we can satisfy every product RIGHT NOW
        for product in $(products)
            # Check to make sure that we've passed in the right kind of
            # objects, e.g. subclasses of `Product`
            if !(typeof(product) <: Product)
                msg = "Cannot @write_deps_file for $product, which is " *
                        "of type $(typeof(product)), which is not a " *
                        "subtype of `Product`!"
                error(msg)
            end

            if !satisfied(product; verbose=true)
                error("$product is not satisfied, cannot generate deps.jl!")
            end
        end

        # If things look good, let's generate the `deps.jl` file
        open(depsjl_path, "w") do depsjl_file
            # First, dump the preamble
            println(depsjl_file, strip("""
            ## This file autogenerated by BinaryProvider.@write_deps_file.
            ## Do not edit.
            """))

            # Next, spit out the paths of all our products
            for idx in 1:$(length(capture))
                product = $(products)[idx]
                name = $(names)[idx]

                # Escape the location so that e.g. Windows platforms are happy
                # with the backslashes in a string literal
                escaped_path = escape_path(locate(product, platform=platform))
                println(depsjl_file, strip("""
                const $(name) = \"$(escaped_path)\"
                """))
            end

            # Next, generate a function to check they're all on the up-and-up
            println(depsjl_file, "function check_deps()")

            for idx in 1:$(length(capture))
                product = $(products)[idx]
                name = $(names)[idx]

                # Add a `global $(name)`
                println(depsjl_file, "    global $(name)");

                # Check that any file exists
                println(depsjl_file, """
                    if !isfile($(name))
                        error("\$($(name)) does not exist, $(rebuild)")
                    end
                """)

                # For Library products, check that we can dlopen it:
                if typeof(product) <: LibraryProduct
                    println(depsjl_file, """
                        if Libdl.dlopen_e($(name)) == C_NULL
                            error("\$($(name)) cannot be opened, $(rebuild)")
                        end
                    """)
                end
            end

            # Close the `check_deps()` function
            println(depsjl_file, "end")
        end
    end
end
