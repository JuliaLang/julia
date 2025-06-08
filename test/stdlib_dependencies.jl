using Libdl
using Pkg
using Test
prev_env = Base.active_project()
Pkg.activate(temp=true)
Pkg.add(Pkg.PackageSpec(name="ObjectFile", uuid="d8793406-e978-5875-9003-1fc021f44a92", version="0.4"))
using ObjectFile
try

    strip_soversion(lib::AbstractString) = Base.BinaryPlatforms.parse_dl_name_version(lib)[1]

    function get_deps_objectfile_macos(lib_path::String)
        open(lib_path, "r") do io
            obj_handles = readmeta(io)
            obj = only(obj_handles)  # If more than one its unclear what to do
            raw_libs = String[]

            # For Mach-O files, get load commands
            if isa(obj, ObjectFile.MachOHandle)
                for lc in ObjectFile.MachOLoadCmds(obj)
                    if lc isa ObjectFile.MachO.MachOLoadDylibCmd
                        # Extract the library name from the load command
                        lib_name = ObjectFile.dylib_name(lc)
                        if lib_name !== nothing
                            # Remove @rpath/ prefix if present
                            lib_name = last(split(lib_name, "@rpath/"))
                            # Get basename
                            lib_name = basename(lib_name)
                            isempty(splitext(lib_name)[2]) && continue # skip frameworks
                            push!(raw_libs, lib_name)
                        end
                    end
                end
            end
            libs = strip_soversion.(raw_libs)
            # Get rid of any self-referential links
            self_lib = strip_soversion(basename(lib_path))
            libs = filter(!=(self_lib), libs)
            return libs
        end
    end

    function get_deps_objectfile_linux_freebsd(lib_path::String)
        open(lib_path, "r") do io
            obj_handles = readmeta(io)
            obj = first(obj_handles)  # Take the first handle from the vector
            raw_libs = String[]

            # For ELF files, get dynamic dependencies
            if isa(obj, ObjectFile.ELFHandle)
                # Get all dynamic entries
                dyn_entries = ObjectFile.ELFDynEntries(obj)
                for entry in dyn_entries
                    # Check if the entry is of type DT_NEEDED
                    if ObjectFile.dyn_entry_type(entry) == ObjectFile.ELF.DT_NEEDED
                        lib_name = ObjectFile.strtab_lookup(entry)
                        if lib_name !== nothing && !isempty(lib_name)
                            push!(raw_libs, basename(lib_name))
                        end
                    end
                end
            end

            libs = strip_soversion.(raw_libs)
            # Self-reference is typically not listed in NEEDED for ELF, so no explicit filter here.
            return libs
        end
    end

    function get_deps_objectfile_windows(lib_path::String)
        open(lib_path, "r") do io
            obj_handles = readmeta(io)
            obj = first(obj_handles)  # Take the first handle from the vector
            raw_libs_set = Set{String}() # Use Set for uniqueness of DLL names

            # For COFF/PE files, get import table
            if isa(obj, ObjectFile.COFFHandle)
                # Get dynamic links
                dls = ObjectFile.DynamicLinks(obj)
                for link in dls
                    lib_name = ObjectFile.path(link)
                    if lib_name !== nothing && !isempty(lib_name)
                        # COFF library names are case-insensitive
                        push!(raw_libs_set, lowercase(lib_name))
                    end
                end
            end

            libs = strip_soversion.(collect(raw_libs_set))
            # Get rid of any self-referential links
            self_lib = strip_soversion(lowercase(basename(lib_path)))
            libs = filter(!=(self_lib), libs)
            return libs
        end
    end

    function get_deps_objectfile(lib_path::String)
        if Sys.isapple()
            return get_deps_objectfile_macos(lib_path)
        elseif Sys.islinux() || Sys.isfreebsd()
            return get_deps_objectfile_linux_freebsd(lib_path)
        elseif Sys.iswindows()
            return get_deps_objectfile_windows(lib_path)
        else
            error("Unsupported platform for ObjectFile.jl dependency extraction")
        end
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
            "ld-linux",
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

    function is_system_lib_windows(lib)
        system_libs = [
            "kernel32",
            "user32",
            "gdi32",
            "advapi32",
            "ole32",
            "oleaut32",
            "shell32",
            "ws2_32",
            "comdlg32",
            "shlwapi",
            "rpcrt4",
            "msvcrt",
            "comctl32",
            "ucrtbase",
            "vcruntime140",
            "msvcp140",
            "libwinpthread",
            "ntdll",
            "crypt32",
            "bcrypt",
            "winhttp",
            "secur32",
        ]
        return any(syslib -> lowercase(lib) == syslib, system_libs)
    end

    # Set up platform-specific functions
    if Sys.islinux() || Sys.isfreebsd()
        is_system_lib = Sys.islinux() ? is_system_lib_linux : is_system_lib_freebsd
    elseif Sys.isapple()
        is_system_lib = is_system_lib_macos
    elseif Sys.iswindows()
        is_system_lib = is_system_lib_windows
    else
        error("Unsupported platform for `stdlib_dependencies.jl`. Only Linux, FreeBSD, macOS, and Windows are supported.")
    end

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
                    real_lib_deps = filter(!is_system_lib, get_deps_objectfile(lib_path))

                    # See if there are missing dependencies in the lazy library deps
                    missing_deps = setdiff(real_lib_deps, lazy_lib_deps)
                    extraneous_deps = setdiff(lazy_lib_deps, real_lib_deps)

                    # We expect there to be no missing or extraneous deps
                    deps_mismatch = !isempty(missing_deps) || !isempty(extraneous_deps)

                    # This is a manually-managed special case
                    if stdlib_name == "libblastrampoline_jll" &&
                       prop_name == :libblastrampoline &&
                       extraneous_deps in (["libopenblas64_"], ["libopenblas"])
                        deps_mismatch = false
                    end

                    @test !deps_mismatch

                    # Print out the deps mismatch if we find one
                    if deps_mismatch
                        @warn("Dependency mismatch",
                            jll = stdlib_name,
                            library = string(prop_name),
                            missing_deps = join(missing_deps, ", "),
                            extraneous_deps = join(extraneous_deps, ", "),
                            actual_deps = join(real_lib_deps, ", "),
                        )
                    end
                end
            end
            if isdefined(m, :eager_mode)
                # If the JLL has an eager_mode function, call it
                Base.invokelatest(getproperty(m, :eager_mode))
            end
        end
    end

finally
    if prev_env !== nothing
        Pkg.activate(prev_env)
    else
        # If no previous environment, activate the default one
        Pkg.activate()
    end
end
