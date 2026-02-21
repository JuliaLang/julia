# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test


include("testenv.jl")
include("tempdepot.jl")


function test_harness(@nospecialize(fn); empty_load_path=true, empty_depot_path=true)
    load_path = copy(LOAD_PATH)
    depot_path = copy(DEPOT_PATH)
    try
        empty_load_path && empty!(LOAD_PATH)
        empty_depot_path && empty!(DEPOT_PATH)
        fn()
    finally
        copy!(LOAD_PATH, load_path)
        copy!(DEPOT_PATH, depot_path)
    end
end

# We test relocation with these dummy pkgs:
# - RelocationTestPkg1 - pkg with no include_dependency
# - RelocationTestPkg2 - pkg with include_dependency tracked by `mtime`
# - RelocationTestPkg3 - pkg with include_dependency tracked by content
# - RelocationTestPkg4 - pkg with no dependencies; will be compiled such that the pkgimage is
#                        not relocatable, but no repeated recompilation happens upon loading

if !test_relocated_depot

    @testset "edge cases when inserting @depot tag in path" begin

        # insert @depot only once for first match
        test_harness() do
            mkdepottempdir() do dir
                pushfirst!(DEPOT_PATH, dir)
                if Sys.iswindows()
                    # dirs start with a drive letter instead of a path separator
                    path = dir*Base.Filesystem.pathsep()*dir
                    @test Base.replace_depot_path(path) == "@depot"*Base.Filesystem.pathsep()*dir
                else
                    path = dir*dir
                    @test Base.replace_depot_path(path) == "@depot"*dir
                end
            end

            # 55340
            empty!(DEPOT_PATH)
            mkdepottempdir() do dir
                jlrc = joinpath(dir, "julia-rc2")
                jl   = joinpath(dir, "julia")
                mkdir(jl)
                push!(DEPOT_PATH, jl)
                @test Base.replace_depot_path(jl) == "@depot"
                @test Base.replace_depot_path(string(jl,Base.Filesystem.pathsep())) ==
                            string("@depot",Base.Filesystem.pathsep())
                @test Base.replace_depot_path(jlrc) != "@depot-rc2"
                @test Base.replace_depot_path(jlrc) == jlrc
            end
        end

        # deal with and without trailing path separators
        test_harness() do
            mkdepottempdir() do dir
                pushfirst!(DEPOT_PATH, dir)
                path = joinpath(dir, "foo")
                if isdirpath(DEPOT_PATH[1])
                    DEPOT_PATH[1] = dirname(DEPOT_PATH[1]) # strip trailing pathsep
                end
                tag = string("@depot", Base.Filesystem.pathsep())
                @test startswith(Base.replace_depot_path(path), tag)
                DEPOT_PATH[1] = string(DEPOT_PATH[1], Base.Filesystem.pathsep())
                @test startswith(Base.replace_depot_path(path), tag)
                popfirst!(DEPOT_PATH)
                @test !startswith(Base.replace_depot_path(path), tag)
            end
        end

    end

    @testset "restore path from @depot tag" begin

        tmp = tempdir()

        path = joinpath("@depot", "foo", "bar")
        tmppath = joinpath(tmp, "foo", "bar")
        @test Base.restore_depot_path(path, tmp) == tmppath

        path = joinpath("no@depot", "foo", "bar")
        @test Base.restore_depot_path(path, tmp) == path

        path = joinpath("@depot", "foo", "bar\n", "@depot", "foo")
        tmppath = joinpath(tmp, "foo", "bar\n", "@depot", "foo")
        @test Base.restore_depot_path(path, tmp) == tmppath

    end

    @testset "precompile RelocationTestPkg1" begin
        pkgname = "RelocationTestPkg1"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__) # make src files available for relocation
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because not precompiled
            Base.require(pkg)
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
            @test Base.isrelocatable(pkg) == true
        end
    end

    @testset "precompile RelocationTestPkg2" begin
        pkgname = "RelocationTestPkg2"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__) # make src files available for relocation
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            rm(joinpath(@__DIR__, pkgname, "src", "foodir"), force=true, recursive=true)
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because not precompiled
            touch(joinpath(@__DIR__, pkgname, "src", "foo.txt"))
            mkdir(joinpath(@__DIR__, pkgname, "src", "foodir"))
            Base.require(pkg)
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
            @test Base.isrelocatable(pkg) == false # because tracked by mtime
        end
    end

    @testset "precompile RelocationTestPkg3" begin
        pkgname = "RelocationTestPkg3"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__) # make src files available for relocation
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            rm(joinpath(@__DIR__, pkgname, "src", "bardir"), force=true, recursive=true)
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because not precompiled
            touch(joinpath(@__DIR__, pkgname, "src", "bar.txt"))
            mkdir(joinpath(@__DIR__, pkgname, "src", "bardir"))
            Base.require(pkg)
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
            @test Base.isrelocatable(pkg) == true
        end
    end

    @testset "precompile RelocationTestPkg4" begin
        # test for #52346 and https://github.com/JuliaLang/julia/issues/53859#issuecomment-2027352004
        # If a pkgimage is not relocatable, no repeated precompilation should occur.
        pkgname = "RelocationTestPkg4"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            # skip this dir to make the pkgimage not relocatable
            filter!(DEPOT_PATH) do depot
                !startswith(@__DIR__, depot)
            end
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because not precompiled
            Base.require(pkg)
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
            @test Base.isrelocatable(pkg) == false
        end
    end

    @testset "#52161" begin
        # Take the src files from two pkgs Example1 and Example2,
        # which are each located in depot1 and depot2, respectively, and
        # add them as include_dependency()s to a new pkg Foo, which will be precompiled into depot3.
        # After loading the include_dependency()s of Foo should refer to depot1 depot2 each.
        test_harness() do
            mkdepottempdir() do depot1
                # precompile Example in depot1
                example1_root = joinpath(depot1, "Example1")
                mkpath(joinpath(example1_root, "src"))
                open(joinpath(example1_root, "src", "Example1.jl"); write=true) do io
                    println(io, """
                    module Example1
                    greet() = println("Hello from Example1!")
                    end
                    """)
                end
                open(joinpath(example1_root, "Project.toml"); write=true) do io
                    println(io, """
                    name = "Example1"
                    uuid = "00000000-0000-0000-0000-000000000001"
                    version = "1.0.0"
                    """)
                end
                pushfirst!(LOAD_PATH, depot1); pushfirst!(DEPOT_PATH, depot1)
                pkg = Base.identify_package("Example1"); Base.require(pkg)
                mkdepottempdir() do depot2
                    # precompile Example in depot2
                    example2_root = joinpath(depot2, "Example2")
                    mkpath(joinpath(example2_root, "src"))
                    open(joinpath(example2_root, "src", "Example2.jl"); write=true) do io
                        println(io, """
                        module Example2
                        greet() = println("Hello from Example2!")
                        end
                        """)
                    end
                    open(joinpath(example2_root, "Project.toml"); write=true) do io
                        println(io, """
                        name = "Example2"
                        uuid = "00000000-0000-0000-0000-000000000002"
                        version = "1.0.0"
                        """)
                    end
                    pushfirst!(LOAD_PATH, depot2); pushfirst!(DEPOT_PATH, depot2)
                    pkg = Base.identify_package("Example2"); Base.require(pkg)
                    mkdepottempdir() do depot3
                        # precompile Foo in depot3
                        open(joinpath(depot3, "Module52161.jl"), write=true) do io
                            println(io, """
                            module Module52161
                            using Example1
                            using Example2
                            srcfile1 = joinpath(pkgdir(Example1), "src", "Example1.jl")
                            srcfile2 = joinpath(pkgdir(Example2), "src", "Example2.jl")
                            include_dependency(srcfile1)
                            include_dependency(srcfile2)
                            end
                            """)
                        end
                        pushfirst!(LOAD_PATH, depot3); pushfirst!(DEPOT_PATH, depot3)
                        pkg = Base.identify_package("Module52161"); Base.compilecache(pkg)
                        cachefile = joinpath(depot3, "compiled",
                                             "v$(VERSION.major).$(VERSION.minor)", "Module52161.ji")
                        _, (deps, _, _), _... = Base.parse_cache_header(cachefile)
                        @test map(x -> x.filename, deps) ==
                            [ joinpath(depot3, "Module52161.jl"),
                              joinpath(depot1, "Example1", "src", "Example1.jl"),
                              joinpath(depot2, "Example2", "src", "Example2.jl") ]
                    end
                end
            end
        end
    end


else

    @testset "load stdlib from test/relocatedepot" begin
        test_harness() do
            push!(LOAD_PATH, "@stdlib")
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia"))
            # stdlib should be already precompiled
            pkg = Base.identify_package("DelimitedFiles")
            @test Base.isprecompiled(pkg) == true
            @test Base.isrelocatable(pkg) == true
        end
    end

    @testset "load RelocationTestPkg1 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg1"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == true
            @test Base.isrelocatable(pkg) == true
        end
    end

    @testset "load RelocationTestPkg2 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg2"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == false # moving depot changes mtime of include_dependency
            @test Base.isrelocatable(pkg) == false # because not precompiled
            Base.require(pkg)
            @test Base.isprecompiled(pkg) == true
            @test Base.isrelocatable(pkg) == false # because tracked by mtime
            touch(joinpath(@__DIR__, "relocatedepot", "RelocationTestPkg2", "src", "foodir", "foofoo"))
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because tracked by mtime
        end
    end

    @testset "load RelocationTestPkg3 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg3"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == true
            @test Base.isrelocatable(pkg) == true
            touch(joinpath(@__DIR__, "relocatedepot", "RelocationTestPkg3", "src", "bardir", "barbar"))
            @test Base.isprecompiled(pkg) == false
            @test Base.isrelocatable(pkg) == false # because not precompiled
        end
    end

    @testset "load RelocationTestPkg4 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg4"
        test_harness() do
            push!(LOAD_PATH, @__DIR__, "relocatedepot")
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            # precompiled but not relocatable
            @test Base.isprecompiled(pkg) == true
            @test Base.isrelocatable(pkg) == false
        end
    end

end
