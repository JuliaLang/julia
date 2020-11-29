# This file is a part of Julia. License is MIT: https://julialang.org/license

# load some stdlib packages but don't put their names in Main
let
    # Stdlibs sorted in dependency, then alphabetical, order by contrib/print_sorted_stdlibs.jl
    stdlibs = [
        # No dependencies
        "Artifacts",
        "Base64",
        "CRC32c",
        "FileWatching",
        "Libdl",
        "Logging",
        "Mmap",
        "SHA",
        "Serialization",
        "Sockets",
        "Unicode",
        # 1-depth packages
        "LinearAlgebra",
        "Printf",
        "Random",
        # 2-depth packages
        "Dates",
        "Future",
        "SparseArrays",
        "UUIDs",
    ]

    extralibs = [
        # No dependencies
        "ArgTools",
        "MozillaCACerts_jll",
        "NetworkOptions",
        # 1-depth packages
        "DelimitedFiles",
        "LibCURL_jll",
        "Markdown",
        "Tar",
        # 2-depth packages
        "Distributed",
        "InteractiveUtils",
        "LibCURL",
        "LibGit2",
        "Profile",
        # 3-depth packages
        "Downloads",
        "REPL",
        "SharedArrays",
        "Statistics",
        "SuiteSparse",
        "TOML",
        "Test",
        # 4-depth packages
        "Pkg",
        # 5-depth packages
        "LazyArtifacts",
    ]

    fullstdlibs = [stdlibs...; extralibs...]

    depot = Base.DEPOT_PATH[end]
    rm(joinpath(depot, "compiled"), recursive=true, force=true)

    maxlen = reduce(max, textwidth.(string.(fullstdlibs)); init=0)

    tot_time_stdlib = 0.0
    print_time = (mod, t) -> (print(rpad(string(mod) * "  ", maxlen + 3, "─"));
                              Base.time_print(t * 10^9); println())
    print_time(Base, (Base.end_base_include - Base.start_base_include) * 10^(-9))

    tot_time_stdlib = @elapsed for stdlib in fullstdlibs
        tt = @elapsed Base.require(Main, Symbol(stdlib))
        print_time(stdlib, tt)
    end

    print_time("Stdlibs total", tot_time_stdlib)

    tot_time_base = (Base.end_base_include - Base.start_base_include) * 10.0^(-9)
    tot_time = tot_time_base + tot_time_stdlib

    println("Sysimage built. Summary:")
    print("Total ─────── "); Base.time_print(tot_time               * 10^9); print(" \n");
    print("Base: ─────── "); Base.time_print(tot_time_base          * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_base          / tot_time) * 100); println("%")
    print("Stdlibs: ──── "); Base.time_print(tot_time_stdlib * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_stdlib / tot_time) * 100); println("%")

    info = Dict{String,Any}()
    for (pkg, origin) in Base.pkgorigins
        entrypath, entryfile = Base.cache_file_entry(pkg)
        ji = joinpath(depot, entrypath, "$(entryfile)_A.ji")
        mkpath(joinpath(depot, entrypath))
        cp(origin.cachepath, ji, follow_symlinks=true)
        info[pkg.name] = (pkg.uuid, origin.path, ji)
    end

    header = """
        Base.loaded_modules[Base.PkgId("")] = Base.__toplevel__
        for (uuid, name, path, ji) in [
        """
    trailer = """
            ]
            local id = Base.PkgId(uuid, name)
            if !Base.haskey(Base.loaded_modules, id)
                local e = Base._require_from_serialized(ji)
                e isa Exception && throw(e)
                for chi in Base.parse_cache_header(ji)[2][1]
                    push!(Base._included_files, (Base.loaded_modules[chi.id], chi.filename))
                end
                Base.pkgorigins[id] = Base.PkgOrigin(path, ji)
            end
        end
        Base.delete!(Base.loaded_modules, Base.PkgId(""))
        """

    open("generate_minimal.jl.tmp", "w") do io
        print(io, header)
        for stdlib in stdlibs
            uuid, path, ji = info[stdlib]
            println(io, """(Base.$(repr(uuid)), $(repr(stdlib)), $(repr(path)), $(repr(ji))),""")
        end
        print(io, trailer)
    end

    open("generate_full.jl.tmp", "w") do io
        print(io, header)
        for stdlib in fullstdlibs
            uuid, path, ji = info[stdlib]
            println(io, """(Base.$(repr(uuid)), $(repr(stdlib)), $(repr(path)), $(repr(ji))),""")
        end
        print(io, trailer)
    end
end
