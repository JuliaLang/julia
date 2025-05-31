using Pkg, Test, Libdl

# Remove `.X.dylib` or just `.dylib`
function strip_soversion_macos(lib)
    m = match(r"^(.*?)(\.\d+)*\.dylib$", lib)
    if m !== nothing
        return m.captures[1]
    end
    return lib
end

# Remove `.so.X` or just `.so`
function strip_soversion_linux(lib)
    m = match(r"^(.*?)\.so(\.\d+)*$", lib)
    if m !== nothing
        return m.captures[1]
    end
    return lib
end

# Remove `-X.dll` or just `.dll`
function strip_soversion_windows(lib)
    m = match(r"^(.*?)(-\d+)*\.dll$", lib)
    if m !== nothing
        return m.captures[1]
    end
    return lib
end


function get_deps_otool(lib_path::String)
    libs = split(readchomp(`otool -L $(lib_path)`), "\n")[2:end]
    # Get rid of `(compatibility version x.y.x)` at the end
    libs = first.(split.(libs, (" (compatibility",)))
    # Get rid of any `@rpath/` stuff at the beginning
    libs = last.(split.(libs, ("@rpath/",)))

    # If there are any absolute paths left, get rid of them here
    libs = basename.(strip.(libs))

    # Now that we've got the basenames of each library, remove `.x.dylib` if it exists:
    libs = strip_soversion_macos.(libs)

    # Get rid of any self-referential links
    self_lib = strip_soversion_macos(basename(lib_path))
    libs = filter(!=(self_lib), libs)
    return libs
end

function is_system_lib_macos(lib)
    system_libs = [
        "libSystem.B",
        "libc++", # While we package libstdc++, we do NOT package libc++.
        "libiconv", # some things (like git) link against system libiconv

        # macOS frameworks used by things like LibCurl
        "CoreFoundation",
        "CoreServices",
        "Security",
        "SystemConfiguration"
    ]
    return lib ∈ system_libs
end

function is_system_lib_linux(lib)
    system_libs = [
        "libdl",
        "libc",
        "libm",
        "librt",
        "libpthread",
        "ld-linux-x86-64",
        "ld-linux-x86",
        "ld-linux-aarch64",
        "ld-linux-armhf",
        "ld-linux-i386",
    ]
    return lib ∈ system_libs
end

function is_system_lib_freebsd(lib)
    system_libs = [
        "libdl",
        "libc",
        "libm",
        "libthr",      # primary threading library
        "libpthread",  # alias kept for compatibility
        "librt",
        "libutil",
        "libexecinfo",
        "libc++",
        "libcxxrt",
    ]
    return lib ∈ system_libs
end

function get_deps_readelf(lib_path::String)
    # Split into lines
    libs = split(readchomp(`readelf -d $(lib_path)`), "\n")

    # Only keep `(NEEDED)` lines
    needed_str = Sys.isfreebsd() ? "NEEDED" : "(NEEDED)"
    libs = filter(contains(needed_str), libs)

    # Grab the SONAME from "Shared library: [$SONAME]"
    libs = map(libs) do lib
        m = match(r"Shared library: \[(.*)\]$", lib)
        if m !== nothing
            return basename(m.captures[1])
        end
        return ""
    end
    libs = filter(!isempty, strip.(libs))

    # Get rid of soversions in the filenames
    libs = strip_soversion_linux.(libs)
    return libs
end


skip = false
# On linux, we need `readelf` available, otherwise we refuse to attempt this
if Sys.islinux() || Sys.isfreebsd()
    if Sys.which("readelf") === nothing
        @debug("Silently skipping stdlib_dependencies.jl as `readelf` not available.")
        skip = true
    end
    get_deps = get_deps_readelf
    strip_soversion = strip_soversion_linux
    is_system_lib = Sys.islinux() ? is_system_lib_linux : is_system_lib_freebsd
elseif Sys.isapple()
    # On macOS, we need `otool` available
    if Sys.which("otool") === nothing
        @debug("Silently skipping stdlib_dependencies.jl as `otool` not available.")
        skip = true
    end
    get_deps = get_deps_otool
    strip_soversion = strip_soversion_macos
    is_system_lib = is_system_lib_macos
else
    @debug("Don't know how to run `stdlib_dependencies.jl` on this platform")
    skip = true
end

if !skip
    # Iterate over all JLL stdlibs, check their lazy libraries to ensure
    # that they list all valid library dependencies, avoiding a situation
    # where the JLL wrapper code has fallen out of sync with the binaries
    # themselves.
    @testset "Stdlib JLL dependency check" begin
        for (_, (stdlib_name, _)) in Pkg.Types.stdlibs()
            if !endswith(stdlib_name, "_jll")
                continue
            end

            # Import the stdlib, skip it if it's not available on this platform
            m = eval(Meta.parse("import $(stdlib_name); $(stdlib_name)"))
            if !Base.invokelatest(getproperty(m, :is_available))
                continue
            end

            for prop_name in names(m)
                prop = getproperty(m, prop_name)
                if isa(prop, Libdl.LazyLibrary)
                    lib_path = dlpath(prop)
                    lazy_lib_deps = strip_soversion.(basename.(dlpath.(prop.dependencies)))
                    real_lib_deps = filter(!is_system_lib, get_deps(lib_path))

                    # See if there are missing dependencies in the lazy library deps
                    missing_deps = setdiff(real_lib_deps, lazy_lib_deps)
                    extraneous_deps = setdiff(lazy_lib_deps, real_lib_deps)

                    # We expect there to be no missing or extraneous deps
                    deps_mismatch = !isempty(missing_deps) || !isempty(extraneous_deps)

                    # This is a manually-managed special case
                    if stdlib_name == "libblastrampoline_jll" &&
                    prop_name == :libblastrampoline &&
                    extraneous_deps == ["libopenblas64_"]
                        deps_mismatch = false
                    end

                    @test !deps_mismatch

                    # Print out the deps mismatch if we find one
                    if deps_mismatch
                        @warn("Dependency mismatch",
                            jll=stdlib_name,
                            library=string(prop_name),
                            missing_deps=join(missing_deps, ", "),
                            extraneous_deps=join(extraneous_deps, ", "),
                            actual_deps=join(real_lib_deps, ", "),
                        )
                    end
                end
            end
        end
    end
end
