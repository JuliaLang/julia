# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Tests for @__LINE__ inside and outside of macros
@test (@__LINE__) == 6

macro macro_caller_lineno()
    @test 9 == (@__LINE__) != __source__.line > 12
    return __source__.line
end

@test @macro_caller_lineno() == (@__LINE__) > 12

# @__LINE__ in a macro expands to the location of the macrocall in the source
# while __source__.line is the location of the macro caller
macro nested_LINE_expansion()
    return quote
        return (@emit_LINE, $(__source__.line))
    end
end
macro nested_LINE_expansion2()
    return :((@emit_LINE, $(__source__.line)))
end
macro emit_LINE()
    return quote
        (@__LINE__, $(__source__.line))
    end
end
@test (@emit_LINE) == ((@__LINE__) - 3, @__LINE__)
@test @nested_LINE_expansion() == ((@__LINE__() - 4, @__LINE__() - 12), @__LINE__())
@test @nested_LINE_expansion2() == ((@__LINE__() - 5, @__LINE__() - 9), @__LINE__())

loaded_files = String[]
push!(Base.include_callbacks, (mod::Module, fn::String) -> push!(loaded_files, fn))
include("test_sourcepath.jl")
@test length(loaded_files) == 1 && endswith(loaded_files[1], "test_sourcepath.jl")
pop!(Base.include_callbacks)
thefname = "the fname!//\\&\1*"
include_string_test_func = include_string(@__MODULE__, "include_string_test() = @__FILE__", thefname)
@test include_string_test_func() == thefname
@test include_string(@__MODULE__, "Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)

@test isdir(@__DIR__)
@test @__DIR__() == dirname(@__FILE__)
let exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no`,
    wd = sprint(show, abspath(pwd(), "")),
    s_dir = sprint(show, joinpath(realpath(tempdir()), ""))
    @test wd != s_dir
    @test readchomp(`$exename -E "@__DIR__" -i`) == wd
    @test readchomp(`$exename -E "cd(()->eval(:(@__DIR__)), $s_dir)" -i`) == s_dir
    @test readchomp(`$exename -E "@__DIR__"`) == wd # non-interactive
end

# Issue #5789 and PR #13542:
mktempdir() do dir
    cd(dir) do
        let true_filename = "cAsEtEsT.jl", lowered_filename="casetest.jl"
            touch(true_filename)
            @test Base.isfile_casesensitive(true_filename)
            @test !Base.isfile_casesensitive(lowered_filename)

            # check that case-sensitivity only applies to basename of a path:
            if isfile(lowered_filename) # case-insensitive filesystem
                mkdir("cAsEtEsT")
                touch(joinpath("cAsEtEsT", true_filename))
                @test Base.isfile_casesensitive(joinpath("casetest", true_filename))
                @test !Base.isfile_casesensitive(joinpath("casetest", lowered_filename))
            end
        end

        # Test Unicode normalization; pertinent for OS X
        let nfc_name = "\U00F4.jl"
            touch(nfc_name)
            @test Base.isfile_casesensitive(nfc_name)
        end
    end
end

import Base: UUID, SHA1, PkgId, load_path, identify_package, locate_package, version_slug

saved_load_path = copy(LOAD_PATH)
saved_depot_path = copy(DEPOT_PATH)
push!(empty!(LOAD_PATH), "project")
push!(empty!(DEPOT_PATH), "depot")

@test load_path() == [abspath("project","Project.toml")]

@testset "project & manifest identify_package & locate_package" begin
    local path
    for (names, uuid, path) in [
        ("Foo",     "767738be-2f1f-45a9-b806-0234f3164144", "project/deps/Foo1/src/Foo.jl"       ),
        ("Bar.Foo", "6f418443-bd2e-4783-b551-cdbac608adf2", "project/deps/Foo2.jl/src/Foo.jl"    ),
        ("Bar",     "2a550a13-6bab-4a91-a4ee-dff34d6b99d0", "project/deps/Bar/src/Bar.jl"        ),
        ("Foo.Baz", "6801f525-dc68-44e8-a4e8-cabd286279e7", "depot/packages/9HkB/TCSb/src/Baz.jl"),
        ("Foo.Qux", "b5ec9b9c-e354-47fd-b367-a348bdc8f909", "project/deps/Qux.jl"                ),
    ]
        n = map(String, split(names, '.'))
        pkg = identify_package(n...)
        @test pkg == PkgId(UUID(uuid), n[end])
        @test joinpath(@__DIR__, normpath(path)) == locate_package(pkg)
    end
    @test identify_package("Baz") == nothing
    @test identify_package("Qux") == nothing
    @testset "equivalent package names" begin
        local classes = [
            ["Foo"],
            ["Bar", "Foo.Bar"],
            ["Foo.Baz", "Bar.Baz", "Foo.Bar.Baz"],
            ["Bar.Foo", "Foo.Bar.Foo", "Foo.Baz.Foo", "Bar.Baz.Foo"],
            ["Foo.Qux", "Foo.Baz.Qux", "Bar.Baz.Qux", "Foo.Bar.Foo.Qux",
             "Bar.Foo.Qux", "Foo.Baz.Foo.Qux", "Bar.Baz.Foo.Qux", "Foo.Bar.Baz.Foo.Qux"],
            ["Baz", "Qux", "Bar.Qux", "Bar.Baz.Bar", "Bar.Foo.Bar", "Bar.Foo.Baz",
             "Bar.Foo.Qux.Foo", "Bar.Foo.Qux.Bar", "Bar.Foo.Qux.Baz"],
        ]
        for i = 1:length(classes)
            A = classes[i]
            for x in A
                X = identify_package(map(String, split(x, '.'))...)
                for y in A
                    Y = identify_package(map(String, split(y, '.'))...)
                    @test X == Y
                end
                for j = i+1:length(classes)
                    B = classes[j]
                    for z in B
                        Z = identify_package(map(String, split(z, '.'))...)
                        @test X != Z
                    end
                end
            end
        end
    end
end

@testset "project & manifest import" begin
    @test !@isdefined Foo
    @test !@isdefined Bar
    import Foo
    @test @isdefined Foo
    @test !@isdefined Bar
    import Bar
    @test @isdefined Foo
    @test @isdefined Bar

    @testset "module graph structure" begin
        local classes = Dict(
            "Foo1" => [Foo],
            "Bar"  => [Bar, Foo.Bar],
            "Baz"  => [Foo.Baz, Bar.Baz, Foo.Bar.Baz],
            "Foo2" => [Bar.Foo, Foo.Bar.Foo, Foo.Baz.Foo, Bar.Baz.Foo],
            "Qux"  => [Foo.Qux, Foo.Baz.Qux, Bar.Baz.Qux, Foo.Bar.Foo.Qux,
                       Bar.Foo.Qux, Foo.Baz.Foo.Qux, Bar.Baz.Foo.Qux,
                       Foo.Bar.Baz.Foo.Qux],
        )
        for (i, (this, mods)) in enumerate(classes)
            for x in mods
                @test x.this == this
                for y in mods
                    @test x === y
                end
                for (j, (that, mods′)) in enumerate(classes)
                    i == j && continue
                    for z in mods′
                        @test x !== z
                    end
                end
            end
        end
    end
    @test Foo.which == "path"
end

function gen_entry_point(entry::String, pkg::PkgId)
    mkpath(dirname(entry))
    open(entry, "w") do io
        print(io, """
        __precompile__(true)
        module $(pkg.name)
        uuid = $(pkg.uuid === nothing ? "nothing" : "Base.UUID(\"$(pkg.uuid)\")")
        name = "$(pkg.name)"
        end
        """)
    end
end

function gen_project_file(project_file::String, pkg::PkgId, deps::Pair{String,UUID}...)
    mkpath(dirname(project_file))
    open(project_file, "w") do io
        println(io, "name = $(repr(pkg.name))")
        pkg.uuid !== nothing && println(io, "uuid = $(repr(string(pkg.uuid)))")
        println(io, "\n[deps]")
        for (name, uuid) in deps
            println(io, "$name = ", repr(string(uuid)))
        end
    end
end

function gen_implicit(dir::String, pkg::PkgId, proj::Bool, deps::Pair{String,UUID}...)
    entry_point = joinpath(dir, pkg.name, "src", "$(pkg.name).jl")
    gen_entry_point(entry_point, pkg)
    proj && gen_project_file(joinpath(dir, pkg.name, "Project.toml"), pkg, deps...)
    return entry_point
end

function gen_depot_ver(depot::String, pkg::PkgId, deps::Pair{String,UUID}...)
    pkg.uuid === nothing && error("package UUID required in explicit env")
    tree = SHA1(rand(UInt8, 20)) # fake tree hash
    dir = joinpath(depot, "packages", version_slug(pkg.uuid, tree))
    entry = joinpath(dir, "src", "$(pkg.name).jl")
    gen_entry_point(entry, pkg)
    gen_project_file(joinpath(dir, "Project.toml"), pkg, deps...)
    return tree, entry
end

let n = 0
    global function gen_explicit(dir::String, pkg::PkgId=PkgId("Env$(n += 1)"))
        gen_project_file(joinpath(dir, "Project.toml"), pkg)
        close(open(joinpath(dir, "Manifest.toml"), "w"))
    end
end

function gen_manifest(dir::String, name::String, uuid::UUID, tree::SHA1,
    deps::Pair{String,UUID}...; toplevel::Bool = true)
    toplevel && open(joinpath(dir, "Project.toml"), "a") do io
        println(io, "$name = \"$uuid\"")
    end
    open(joinpath(dir, "Manifest.toml"), "a") do io
        println(io, "[[$name]]")
        println(io, "uuid = \"$uuid\"")
        println(io, "hash-sha1 = \"$(bytes2hex(tree.bytes))\"")
        if !isempty(deps)
            println(io, "    [$name.deps]")
            for (n, u) in deps
                println(io, "    $n = \"$u\"")
            end
        end
        println(io)
    end
end

const name = "Flarp"
const uuidA = UUID("b2cb3794-8625-4058-bcde-7eeb13ac1c8b")
const uuidB = UUID("1513c021-3639-4616-a37b-ee45c9d2f773")
const uuids = [uuidA, uuidB]

ft(::UUID)    =  true:true
ft(::Nothing) = false:true

@testset "direct dependency loading: implict + implicit" begin
    for uuid1 in [nothing; uuids], proj1 in ft(uuid1),
        uuid2 in [nothing; uuids], proj2 in ft(uuid2)
        pkg1 = uuid1 === nothing ? PkgId(name) : PkgId(uuid1, name)
        pkg2 = uuid2 === nothing ? PkgId(name) : PkgId(uuid2, name)
        empty!(LOAD_PATH)
        mktempdir() do dir1
            push!(LOAD_PATH, dir1)
            path1 = gen_implicit(dir1, pkg1, proj1)
            @test identify_package(name) == pkg1
            @test locate_package(pkg1) == path1
            path = uuid1 == coalesce(uuid2, uuid1) ? path1 : nothing
            @test locate_package(pkg2) == path
            mktempdir() do dir2
                push!(LOAD_PATH, dir2)
                path2 = gen_implicit(dir2, pkg2, proj2)
                @test identify_package(name) == pkg1
                @test locate_package(pkg1) == path1
                path = uuid1 == coalesce(uuid2, uuid1) ? path1 : path2
                @test locate_package(pkg2) == path
            end
        end
    end
end

@testset "direct dependency loading: explicit + explicit" begin
    mktempdir() do depot
        push!(empty!(DEPOT_PATH), depot)
        pkgs  = [PkgId(uuid, name) for uuid in uuids]
        pairs = [gen_depot_ver(depot, pkg) for pkg in pkgs, _ in 1:2]
        trees = first.(pairs)
        paths = last.(pairs)
        for i = 1:length(uuids), k = 1:2,
            j = 1:length(uuids), l = 1:2
            empty!(LOAD_PATH)
            mktempdir() do dir1
                push!(LOAD_PATH, dir1)
                gen_explicit(dir1)
                gen_manifest(dir1, name, uuids[i], trees[i,k])
                @test identify_package(name) == pkgs[i]
                @test locate_package(pkgs[i]) == paths[i,k]
                path = uuids[i] == uuids[j] ? paths[i,k] : nothing
                @test locate_package(pkgs[j]) == path
                mktempdir() do dir2
                    push!(LOAD_PATH, dir2)
                    gen_explicit(dir2)
                    gen_manifest(dir2, name, uuids[j], trees[j,l])
                    @test identify_package(name) == pkgs[i]
                    @test locate_package(pkgs[i]) == paths[i,k]
                    path = uuids[i] == uuids[j] ? paths[i,k] : paths[j,l]
                    @test locate_package(pkgs[j]) == path
                end
            end
        end
    end
end

const uuidT = UUID("a54bd003-d8dc-4161-b186-d5516cd448e9")

@testset "indirect dependency loading: explicit + explicit" begin
    mktempdir() do depot
        push!(empty!(DEPOT_PATH), depot)
        # generate top-level package
        top = PkgId(uuidT, "TopLevel")
        top_tree, _ = gen_depot_ver(depot, top)
        # generate dependency packages
        pkgs  = [PkgId(uuid, name) for uuid in uuids]
        pairs = [gen_depot_ver(depot, pkg) for pkg in pkgs, _ in 1:2]
        trees = first.(pairs)
        paths = last.(pairs)
        for i = 1:length(uuids), k = 1:2, s = false:true,
            j = 1:length(uuids), l = 1:2, t = false:true
            empty!(LOAD_PATH)
            mktempdir() do dir1
                push!(LOAD_PATH, dir1)
                gen_explicit(dir1)
                gen_manifest(dir1, name, uuids[i], trees[i,k], toplevel=false)
                s && gen_manifest(dir1, top.name, top.uuid, top_tree, name => uuids[i])
                @test identify_package(top, name) == (s ? pkgs[i] : nothing)
                @test locate_package(pkgs[i]) == paths[i,k]
                path = uuids[i] == uuids[j] ? paths[i,k] : nothing
                @test locate_package(pkgs[j]) == path
                mktempdir() do dir2
                    push!(LOAD_PATH, dir2)
                    gen_explicit(dir2)
                    t && gen_manifest(dir1, top.name, top.uuid, top_tree, name => uuids[j])
                    gen_manifest(dir2, name, uuids[j], trees[j,l], toplevel=false)
                    pkg = (s ? pkgs[i] : t ? pkgs[j] : nothing)
                    @test identify_package(top, name) == pkg
                    @test locate_package(pkgs[i]) == paths[i,k]
                    path = uuids[i] == uuids[j] ? paths[i,k] : paths[j,l]
                    @test locate_package(pkgs[j]) == path
                end
            end
        end
    end
end

append!(empty!(DEPOT_PATH), saved_depot_path)
append!(empty!(LOAD_PATH), saved_load_path)
