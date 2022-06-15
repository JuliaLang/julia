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
@test !endswith(@__DIR__, Base.Filesystem.path_separator)
let exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no --color=no`,
    wd = sprint(show, pwd())
    s_dir = sprint(show, realpath(tempdir()))
    @test wd != s_dir
    @test readchomp(`$exename -E "@__DIR__" -i`) == wd
    @test readchomp(`$exename -E "cd(()->eval(:(@__DIR__)), $s_dir)" -i`) == s_dir
    @test readchomp(`$exename -E "@__DIR__"`) == wd # non-interactive
    @test !endswith(wd, Base.Filesystem.path_separator)
    @test !endswith(s_dir, Base.Filesystem.path_separator)
end

@test Base.in_sysimage(Base.PkgId(Base.UUID("cf7118a7-6976-5b1a-9a39-7adc72f591a4"), "UUIDs"))
@test Base.in_sysimage(Base.PkgId(Base.UUID("3a7fdc7e-7467-41b4-9f64-ea033d046d5b"), "NotAPackage")) == false

## Unit tests for safe file operations ##

@test Base.isaccessiblefile("/root/path/doesn't/exist") == false
@test Base.isaccessiblepath("/root/path/doesn't/exist") == false
@test Base.isaccessibledir("/root/path/doesn't/exist") == false

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

## unit tests of project parsing ##

import Base: SHA1, PkgId, load_path, identify_package, locate_package, version_slug, dummy_uuid
import UUIDs: UUID, uuid4, uuid_version
import Random: shuffle, randstring
using Test

let shastr = "ab"^20
    hash = SHA1(shastr)
    @test hash == eval(Meta.parse(repr(hash))) # check show method
    @test string(hash) == shastr
    @test "check $hash" == "check $shastr"
end

let shastr1 = "ab"^20, shastr2 = "ac"^20
    hash1 = SHA1(shastr1)
    hash2 = SHA1(shastr2)
    @test isless(hash1, hash2)
    @test !isless(hash2, hash1)
    @test !isless(hash1, hash1)
end

# Test bad SHA1 values
@test_throws ArgumentError SHA1("this is not a valid SHA1")
@test_throws ArgumentError parse(SHA1, "either is this")
@test tryparse(SHA1, "nor this") === nothing

let uuidstr = "ab"^4 * "-" * "ab"^2 * "-" * "ab"^2 * "-" * "ab"^2 * "-" * "ab"^6
    uuid = UUID(uuidstr)
    @test uuid == eval(Meta.parse(repr(uuid))) # check show method
    @test string(uuid) == uuidstr == sprint(print, uuid)
    @test "check $uuid" == "check $uuidstr"
    @test UUID(UInt128(uuid)) == uuid
    @test UUID(uuid) === uuid
    @test UUID(convert(NTuple{2, UInt64}, uuid)) == uuid
    @test UUID(convert(NTuple{4, UInt32}, uuid)) == uuid

    uuidstr2 = "ba"^4 * "-" * "ba"^2 * "-" * "ba"^2 * "-" * "ba"^2 * "-" * "ba"^6
    uuid2 = UUID(uuidstr2)
    uuids = [uuid, uuid2]
    @test (uuids .== uuid) == [true, false]

    @test parse(UUID, uuidstr2) == uuid2
end
@test_throws ArgumentError UUID("@"^4 * "-" * "@"^2 * "-" * "@"^2 * "-" * "@"^2 * "-" * "@"^6)
@test_throws ArgumentError parse(UUID, "not a UUID")
@test tryparse(UUID, "either is this") === nothing

@testset "explicit_project_deps_get" begin
    mktempdir() do dir
        project_file = joinpath(dir, "Project.toml")
        touch(project_file) # dummy_uuid calls realpath
        # various UUIDs to work with
        proj_uuid = dummy_uuid(project_file)
        root_uuid = uuid4()
        this_uuid = uuid4()

        old_load_path = copy(LOAD_PATH)
        try
            copy!(LOAD_PATH, [project_file])
            write(project_file, """
            name = "Root"
            uuid = "$root_uuid"
            [deps]
            This = "$this_uuid"
            """)
            # look up various packages by name
            root = Base.identify_package("Root")
            this = Base.identify_package("This")
            that = Base.identify_package("That")

            @test root.uuid == root_uuid
            @test this.uuid == this_uuid
            @test that == nothing

            write(project_file, """
            name = "Root"
            This = "$this_uuid"
            [deps]
            """)
            # look up various packages by name
            root = Base.identify_package("Root")
            this = Base.identify_package("This")
            that = Base.identify_package("That")

            @test root.uuid == proj_uuid
            @test this == nothing
            @test that == nothing
        finally
            copy!(LOAD_PATH, old_load_path)
        end
    end
end

# extras
@testset "extras" begin
    mktempdir() do dir
        project_file = joinpath(dir, "Project.toml")
        touch(project_file) # dummy_uuid calls realpath
        # various UUIDs to work with
        proj_uuid = dummy_uuid(project_file)
        root_uuid = uuid4()
        this_uuid = uuid4()

        old_load_path = copy(LOAD_PATH)
        try
            copy!(LOAD_PATH, [project_file])
            write(project_file, """
            name = "Root"
            uuid = "$root_uuid"
            [extras]
            This = "$this_uuid"
            """)
            # look up various packages by name
            root = Base.identify_package("Root")
            this = Base.identify_package("This")
            that = Base.identify_package("That")

            @test root.uuid == root_uuid
            @test this == nothing
            @test that == nothing

            @test Base.get_uuid_name(project_file, this_uuid) == "This"
        finally
            copy!(LOAD_PATH, old_load_path)
        end
    end
end


## functional testing of package identification, location & loading ##

saved_load_path = copy(LOAD_PATH)
saved_depot_path = copy(DEPOT_PATH)
saved_active_project = Base.ACTIVE_PROJECT[]
watcher_counter = Ref(0)
push!(Base.active_project_callbacks, () -> watcher_counter[] += 1)
push!(Base.active_project_callbacks, () -> error("broken"))

push!(empty!(LOAD_PATH), joinpath(@__DIR__, "project"))
append!(empty!(DEPOT_PATH), [mktempdir(), joinpath(@__DIR__, "depot")])
@test watcher_counter[] == 0
@test_logs (:error, r"active project callback .* failed") Base.set_active_project(nothing)
@test watcher_counter[] == 1
pop!(Base.active_project_callbacks)

@test load_path() == [joinpath(@__DIR__, "project", "Project.toml")]

# locate `tail(names)` package by following the search path graph through `names` starting from `where`
function recurse_package(where::PkgId, name::String, names::String...)
    pkg = identify_package(where, name)
    pkg === nothing && return nothing
    return recurse_package(pkg, names...)
end

recurse_package(pkg::String) = identify_package(pkg)
recurse_package(where::PkgId, pkg::String) = identify_package(where, pkg)

function recurse_package(name::String, names::String...)
    pkg = identify_package(name)
    pkg === nothing && return nothing
    return recurse_package(pkg, names...)
end

@testset "project & manifest identify_package & locate_package" begin
    local path
    for (names, uuid, path) in [
        ("Foo",     "767738be-2f1f-45a9-b806-0234f3164144", "project/deps/Foo1/src/Foo.jl"       ),
        ("Bar.Foo", "6f418443-bd2e-4783-b551-cdbac608adf2", "project/deps/Foo2.jl/src/Foo.jl"    ),
        ("Bar",     "2a550a13-6bab-4a91-a4ee-dff34d6b99d0", "project/deps/Bar/src/Bar.jl"        ),
        ("Foo.Baz", "6801f525-dc68-44e8-a4e8-cabd286279e7", "depot/packages/Baz/81oLe/src/Baz.jl"),
        ("Foo.Qux", "b5ec9b9c-e354-47fd-b367-a348bdc8f909", "project/deps/Qux.jl"                ),
    ]
        n = map(String, split(names, '.'))
        pkg = recurse_package(n...)
        @test pkg == PkgId(UUID(uuid), n[end])
        @test joinpath(@__DIR__, normpath(path)) == locate_package(pkg)
        @test Base.compilecache_path(pkg, UInt64(0)) == Base.compilecache_path(pkg, UInt64(0))
    end
    @test identify_package("Baz") == nothing
    @test identify_package("Qux") == nothing
    @testset "equivalent package names" begin
         classes = [
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
                X = recurse_package(map(String, split(x, '.'))...)
                for y in A
                    Y = recurse_package(map(String, split(y, '.'))...)
                    @test X == Y
                end
                for j = i+1:length(classes)
                    B = classes[j]
                    for z in B
                        Z = recurse_package(map(String, split(z, '.'))...)
                        @test X != Z
                    end
                end
            end
        end
    end
end

module NotPkgModule; end

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

    @testset "pathof" begin
        @test pathof(Foo) == normpath(abspath(@__DIR__, "project/deps/Foo1/src/Foo.jl"))
        @test pathof(NotPkgModule) === nothing
    end

    @testset "pkgdir" begin
        @test pkgdir(Foo) == normpath(abspath(@__DIR__, "project/deps/Foo1"))
        @test pkgdir(Foo.SubFoo1) == normpath(abspath(@__DIR__, "project/deps/Foo1"))
        @test pkgdir(Foo.SubFoo2) == normpath(abspath(@__DIR__, "project/deps/Foo1"))
        @test pkgdir(NotPkgModule) === nothing

        @test pkgdir(Foo, "src") == normpath(abspath(@__DIR__, "project/deps/Foo1/src"))
        @test pkgdir(Foo.SubFoo1, "src") == normpath(abspath(@__DIR__, "project/deps/Foo1/src"))
        @test pkgdir(Foo.SubFoo2, "src") == normpath(abspath(@__DIR__, "project/deps/Foo1/src"))
        @test pkgdir(NotPkgModule, "src") === nothing
    end

    @testset "pkgversion" begin
        @test pkgversion(Foo) == v"1.2.3"
        @test pkgversion(Foo.SubFoo1) == v"1.2.3"
        @test pkgversion(Foo.SubFoo2) == v"1.2.3"
        @test pkgversion(NotPkgModule) === nothing
    end

end

## systematic generation of test environments ##

const M = 3 # number of node names
const N = 12 # different UUIDs per name
const NODES = 1:M*N
const NAMES = map(string, ('A':'Z')[1:M])
const UUIDS = [uuid4() for i = 1:M, j = 1:N]
const KIND = [(i + j) % 3 for i = 1:M, j = 1:N]
const KIND0 = filter(i -> KIND[i] == 0, NODES)
const KIND2 = filter(i -> KIND[i] == 2, NODES)

# kind 0: no project file
# kind 1: project file without name or uuid
# kind 2: project file with name and uuid

# explicit env: root can be anything, everything else kind 2
# implicit env: nodes can be anything, names must be unique

# allowed dependencies between kinds (explicit and implicit):
allowed(i::Int, j::Int) = KIND[i] ≤ KIND[j] && !(KIND[i] == KIND[j] == 1)

# node names/labels
L(i::Int) = NAMES[mod1(i, M)]

# first generate random dependency graphs

const graphs = Any[]

while length(graphs) < 100
    if (flat = rand(Bool))
        root = rand(KIND0)
        pool = root ∪ filter(i -> L(i) ≠ L(root), NODES)
        size = rand(1:N)
    else
        root = rand(NODES)
        pool = filter(i -> i ≠ root, KIND2)
        size = rand(1:length(NODES)÷2)
    end
    graph = Dict(root => Int[])
    KIND[root] ≠ 0 && push!(graph[root], root)
    for _ = 1:size
        i = rand(keys(graph))
        J = filter(pool) do j
            allowed(i, j) && all(L(j) ≠ L(k) for k in graph[i])
        end
        isempty(J) && continue
        j = rand(J)
        push!(graph[i], j)
        if j ∉ keys(graph)
            graph[j] = [j]
            flat && filter!(k -> L(k) ≠ L(j), pool)
            isempty(pool) && break
        end
    end
    roots = flat ? reduce(∪, values(graph)) : graph[root]
    for i in keys(graph)
        KIND[i] == 0 && delete!(graph, i)
    end
    t = (flat, root,
        Dict(L(i) => i for i in roots),
        Dict(i => Dict(L(j) => j for j in deps) for (i, deps) in graph))
    t in graphs || push!(graphs, t)
end

# materialize dependency graphs as explicit environments

function make_entry_point(path::String, name::String, uuid::UUID)
    mkpath(dirname(path))
    open(path, "w") do io
        print(io, """
        module $name
        name = $(repr(name))
        uuid = $(repr(string(uuid)))
        end
        """)
    end
end

function make_env(flat, root, roots, graph, paths, dummies)
    pkg(i::Int) = PkgId(KIND[i] == 2 ? UUIDS[i] : get(dummies, i, nothing), L(i))
    return (flat, pkg(root),
        Dict(n => pkg(i) for (n, i) in roots),
        Dict(pkg(i) => Dict(n => pkg(j) for (n, j) in d) for (i, d) in graph),
        Dict{PkgId,Union{Nothing,String}}(pkg(i) => path for (i, path) in paths),
    )
end

const depots = [mktempdir() for _ = 1:3]
const envs = Dict{String,Any}()

append!(empty!(DEPOT_PATH), depots)

@testset "load code uniqueness" begin
    @test allunique(UUIDS)
    @test allunique(depots)
    @test allunique(DEPOT_PATH)
end

for (flat, root, roots, graph) in graphs
    if flat
        all(KIND[i] == 2 for i in values(roots)) || continue
        all(KIND[i] == 2 for i in keys(graph)) || continue
    end
    dir = mktempdir()
    dummies = Dict{Int,UUID}()
    paths = Dict{Int,Union{Nothing,String}}()
    root_path = rand([false, true, joinpath("src", "$(randstring()).jl")])

    # generate project file
    project_file = joinpath(dir, "Project.toml")
    open(project_file, "w") do io
        name, uuid, kind = L(root), UUIDS[root], KIND[root]
        kind != 0 && println(io, "name = ", repr(name))
        kind == 2 && println(io, "uuid = ", repr(string(uuid)))
        kind == 1 && (dummies[root] = dummy_uuid(project_file))
        root_path isa String && println(io, "path = ", repr(root_path))
        println(io, "[deps]")
        for (n, i) in roots
            i == root && continue
            @assert KIND[i] == 2
            println(io, "$n = ", repr(string(UUIDS[i])))
        end
        kind == 0 && return
        # generate entry point
        if root_path == false
            kind == 2 && (paths[root] = nothing)
        else
            root_path == true && (root_path = joinpath("src", "$name.jl"))
            root_path = joinpath(dir, root_path)
            make_entry_point(root_path, name, uuid)
            paths[root] = root_path
        end
    end

    # count manifest entries
    counts = Dict(name => 0 for name in NAMES)
    for (i, _) in graph
        i == root && continue
        @assert KIND[i] == 2
        counts[L(i)] += 1
    end

    # generate manifest file
    open(joinpath(dir, "Manifest.toml"), "w") do io
        for (i, deps) in graph
            i == root && continue
            name, uuid = L(i), UUIDS[i]
            println(io, "[[$name]]")
            println(io, "uuid = ", repr(string(uuid)))
            if rand() < 2/3
                sha1 = SHA1(rand(UInt8, 20))
                println(io, "git-tree-sha1 = ", repr(string(sha1)))
                path = joinpath(
                    rand(depots), "packages",
                    name, version_slug(uuid, sha1),
                    "src", "$name.jl",
                )
                make_entry_point(path, name, uuid)
                paths[i] = path # may get overwritten below
            end
            if rand() < 1/4
                path = joinpath("deps", "$(randstring()).jl")
                println(io, "path = ", repr(path))
                path = joinpath(dir, path)
                make_entry_point(path, name, uuid)
                paths[i] = path # may overwrite path above
            end
            # neither can occur, i.e. no entry in paths
            deps = delete!(copy(deps), name)
            isempty(deps) && continue
            if all(counts[n] == 1 for n in keys(deps))
                println(io, "deps = ", repr(keys(deps)))
            else
                println(io, "  [$name.deps]")
                for (n, j) in deps
                    @assert KIND[j] == 2
                    println(io, "  $n = ", repr(string(UUIDS[j])))
                end
            end
        end
    end

    envs[dir] = make_env(flat, root, roots, graph, paths, dummies)
end

# materialize dependency graphs as implicit environments (if possible)

for (flat, root, roots, graph) in graphs
    flat || continue
    dir = mktempdir()
    dummies = Dict{Int,UUID}()
    paths = Dict{Int,Union{Nothing,String}}()

    for (name, i) in roots
        uuid, kind = UUIDS[i], KIND[i]
        # generate package entry point
        entry = joinpath(dir, name, "src", "$name.jl")
        make_entry_point(entry, name, uuid)
        paths[i] = entry
        kind == 0 && continue
        deps = delete!(copy(graph[i]), name)
        # generate project file
        project_file = joinpath(dir, name, "Project.toml")
        open(project_file, "w") do io
            kind != 0 && println(io, "name = ", repr(name))
            kind == 2 && println(io, "uuid = ", repr(string(uuid)))
            kind != 2 && (dummies[i] = dummy_uuid(project_file))
            isempty(deps) || println(io, "[deps]")
            for (n, j) in deps
                @assert KIND[j] == 2
                println(io, "  $n = ", repr(string(UUIDS[j])))
            end
        end
    end

    envs[dir] = make_env(flat, root, roots, graph, paths, dummies)
end

## use generated environments to test package loading ##

function test_find(
        roots::Dict{String,PkgId},
        graph::Dict{PkgId,Dict{String,PkgId}},
        paths::Dict{PkgId,Union{Nothing,String}},
    )
    # check direct dependencies
    for name in NAMES
        id = identify_package(name)
        @test id == get(roots, name, nothing)
        path = id === nothing ? nothing : locate_package(id)
        @test path == get(paths, id, nothing)
    end
    # check indirect dependencies
    for where in keys(graph)
        where.uuid === nothing && continue
        deps = get(graph, where, Dict(where.name => where))
        for name in NAMES
            id = identify_package(where, name)
            @test id == get(deps, name, nothing)
            path = id === nothing ? nothing : locate_package(id)
            @test path == get(paths, id, nothing)
        end
    end
end

@testset "find_package with one env in load path" begin
    for (env, (_, _, roots, graph, paths)) in envs
        push!(empty!(LOAD_PATH), env)
        test_find(roots, graph, paths)
    end
end

@testset "find_package with two envs in load path" begin
    for x = false:true,
        (env1, (_, _, roots1, graph1, paths1)) in (x ? envs : rand(envs, 10)),
        (env2, (_, _, roots2, graph2, paths2)) in (x ? rand(envs, 10) : envs)
        push!(empty!(LOAD_PATH), env1, env2)
        roots = merge(roots2, roots1)
        graph = merge(graph2, graph1)
        paths = merge(paths2, paths1)
        test_find(roots, graph, paths)
    end
end

@testset "find_package with three envs in load path" begin
    for (env1, (_, _, roots1, graph1, paths1)) in rand(envs, 10),
        (env2, (_, _, roots2, graph2, paths2)) in rand(envs, 10),
        (env3, (_, _, roots3, graph3, paths3)) in rand(envs, 10)
        push!(empty!(LOAD_PATH), env1, env2, env3)
        roots = merge(roots3, roots2, roots1)
        graph = merge(graph3, graph2, graph1)
        paths = merge(paths3, paths2, paths1)
        test_find(roots, graph, paths)
    end
end

# normalization of paths by include (#26424)
@test begin
    exc = try; include("./notarealfile.jl"); "unexpectedly reached!"; catch exc; exc; end
    @test exc isa SystemError
    exc.prefix
end == "opening file $(repr(joinpath(@__DIR__, "notarealfile.jl")))"

old_act_proj = Base.ACTIVE_PROJECT[]
pushfirst!(LOAD_PATH, "@")
try
    Base.set_active_project(joinpath(@__DIR__, "TestPkg"))
    @eval using TestPkg
finally
    Base.set_active_project(old_act_proj)
    popfirst!(LOAD_PATH)
end
@test Base.pkgorigins[Base.PkgId(UUID("69145d58-7df6-11e8-0660-cf7622583916"), "TestPkg")].version == v"1.2.3"

@testset "--project and JULIA_PROJECT paths should be absolutified" begin
    mktempdir() do dir; cd(dir) do
        mkdir("foo")
        script = """
        using Test
        old = Base.active_project()
        cd("foo")
        @test Base.active_project() == old
        """
        @test success(`$(Base.julia_cmd()) --startup-file=no --project=foo -e $(script)`)
        withenv("JULIA_PROJECT" => "foo") do
            @test success(`$(Base.julia_cmd()) --startup-file=no -e $(script)`)
        end
    end; end
end

# Base.active_project when version directory exist in depot, but contains no project file
mktempdir() do dir
    vdir = Base.DEFAULT_LOAD_PATH[2]
    vdir = replace(vdir, "#" => VERSION.major, count = 1)
    vdir = replace(vdir, "#" => VERSION.minor, count = 1)
    vdir = replace(vdir, "#" => VERSION.patch, count = 1)
    vdir = vdir[2:end] # remove @
    vpath = joinpath(dir, "environments", vdir)
    mkpath(vpath)
    withenv("JULIA_DEPOT_PATH" => dir) do
        script = "@assert startswith(Base.active_project(), $(repr(vpath)))"
        @test success(`$(Base.julia_cmd()) --startup-file=no -e $(script)`)
    end
end

@testset "expansion of JULIA_LOAD_PATH" begin
    s = Sys.iswindows() ? ';' : ':'
    tmp = "/foo/bar"
    cases = Dict{Any,Vector{String}}(
        nothing => Base.DEFAULT_LOAD_PATH,
        "" => [],
        "$s" => Base.DEFAULT_LOAD_PATH,
        "$tmp$s" => [tmp; Base.DEFAULT_LOAD_PATH],
        "$s$tmp" => [Base.DEFAULT_LOAD_PATH; tmp],
        )
    for (env, result) in pairs(cases)
        withenv("JULIA_LOAD_PATH" => env) do
            script = "LOAD_PATH == $(repr(result)) || error()"
            @test success(`$(Base.julia_cmd()) --startup-file=no -e $script`)
        end
    end
end

@testset "expansion of JULIA_DEPOT_PATH" begin
    s = Sys.iswindows() ? ';' : ':'
    tmp = "/foo/bar"
    DEFAULT = Base.append_default_depot_path!(String[])
    cases = Dict{Any,Vector{String}}(
        nothing => DEFAULT,
        "" => [],
        "$s" => DEFAULT,
        "$tmp$s" => [tmp; DEFAULT],
        "$s$tmp" => [DEFAULT; tmp],
        )
    for (env, result) in pairs(cases)
        withenv("JULIA_DEPOT_PATH" => env) do
            script = "DEPOT_PATH == $(repr(result)) || error()"
            @test success(`$(Base.julia_cmd()) --startup-file=no -e $script`)
        end
    end
end

## cleanup after tests ##

for env in keys(envs)
    rm(env, force=true, recursive=true)
end
for depot in depots
    rm(depot, force=true, recursive=true)
end

append!(empty!(LOAD_PATH), saved_load_path)
append!(empty!(DEPOT_PATH), saved_depot_path)
pop!(Base.active_project_callbacks)
Base.set_active_project(saved_active_project)
@test watcher_counter[] == 3

# issue #28190
module Foo28190; import Libdl; end
import .Foo28190.Libdl; import Libdl
@test Foo28190.Libdl === Libdl

@testset "include with mapexpr" begin
    let exprs = Any[]
        @test 13 === include_string(@__MODULE__, "1+1\n3*4") do ex
            ex isa LineNumberNode || push!(exprs, ex)
            Meta.isexpr(ex, :call) ? :(1 + $ex) : ex
        end
        @test exprs == [:(1 + 1), :(3 * 4)]
    end
    # test using test_exec.jl, just because that is the shortest handy file
    for incl in (include, (mapexpr,path) -> Base.include(mapexpr, @__MODULE__, path))
        let exprs = Any[]
            incl("test_exec.jl") do ex
                ex isa LineNumberNode || push!(exprs, ex)
                Meta.isexpr(ex, :macrocall) ? :nothing : ex
            end
            @test length(exprs) == 2 && exprs[1] == :(using Test)
            @test Meta.isexpr(exprs[2], :macrocall) &&
                  exprs[2].args[[1,3]] == [Symbol("@test"), :(1 == 2)]
        end
    end
end

@testset "`Base.project_names` and friends" begin
    # Some functions in Pkg assumes that these tuples have the same length
    n = length(Base.project_names)
    @test length(Base.manifest_names) == n
    @test length(Base.preferences_names) == n
end

@testset "Manifest formats" begin
    deps = Dict{String,Any}(
        "Serialization" => Any[Dict{String, Any}("uuid"=>"9e88b42a-f829-5b0c-bbe9-9e923198166b")],
        "Random"        => Any[Dict{String, Any}("deps"=>["Serialization"], "uuid"=>"9a3f8284-a2c9-5f02-9a11-845980a1fd5c")],
        "Logging"       => Any[Dict{String, Any}("uuid"=>"56ddb016-857b-54e1-b83d-db4d58db5568")]
    )

    @testset "v1.0" begin
        env_dir = joinpath(@__DIR__, "manifest", "v1.0")
        manifest_file = joinpath(env_dir, "Manifest.toml")
        isfile(manifest_file) || error("Reference manifest is missing")
        raw_manifest = Base.parsed_toml(manifest_file)
        @test Base.is_v1_format_manifest(raw_manifest)
        @test Base.get_deps(raw_manifest) == deps
    end

    @testset "v2.0" begin
        env_dir = joinpath(@__DIR__, "manifest", "v2.0")
        manifest_file = joinpath(env_dir, "Manifest.toml")
        isfile(manifest_file) || error("Reference manifest is missing")
        raw_manifest = Base.parsed_toml(manifest_file)
        @test Base.is_v1_format_manifest(raw_manifest) == false
        @test Base.get_deps(raw_manifest) == deps
    end
end

@testset "error message loading pkg bad module name" begin
    mktempdir() do tmp
        old_loadpath = copy(LOAD_PATH)
        try
            push!(LOAD_PATH, tmp)
            write(joinpath(tmp, "BadCase.jl"), "module badcase end")
            @test_throws ErrorException("package `BadCase` did not define the expected module `BadCase`, \
                                        check for typos in package module name") (@eval using BadCase)
        finally
            copy!(LOAD_PATH, old_loadpath)
        end
    end
end

@testset "Preferences loading" begin
    mktempdir() do dir
        this_uuid = uuid4()
        that_uuid = uuid4()

        # First, create outer environment with exported preferences
        mkpath(joinpath(dir, "outer_env"))
        open(joinpath(dir, "outer_env", "Project.toml"), write=true) do io
            write(io, """
            [deps]
            This = "$(this_uuid)"
            That = "$(that_uuid)"

            [preferences.This]
            pref1 = "outer-project"
            pref2 = "outer-project"
            pref3 = "outer-project"
            pref4 = "outer-project"
            pref5 = "outer-project"
            pref6 = "outer-project"

            [preferences.That]
            pref1 = "outer-project"
            """)
        end

        # Override some of those preferences above here:
        open(joinpath(dir, "outer_env", "JuliaLocalPreferences.toml"), write=true) do io
            write(io, """
            [This]
            pref2 = "outer-jlp"
            """)
        end

        # Ensure that a `JuliaLocalPreferences.toml` disables `LocalPreferences.toml`
        # We test that both overriding `pref2` and trying to clear `pref5` are ignored
        open(joinpath(dir, "outer_env", "LocalPreferences.toml"), write=true) do io
            write(io, """
            [This]
            pref2 = "outer-lp"
            __clear__ = ["pref5"]
            """)
        end

        # Next, set up an inner environment that will override some of the preferences
        # set by the outer environment, even clearing `pref6`.
        mkpath(joinpath(dir, "inner_env"))
        open(joinpath(dir, "inner_env", "Project.toml"), write=true) do io
            write(io, """
            name = "Root"
            uuid = "$(uuid4())"

            [extras]
            This = "$(this_uuid)"

            [preferences.This]
            pref3 = "inner-project"
            pref4 = "inner-project"
            __clear__ = ["pref6"]
            """)
        end

        # And have an override here as well, this time only LocalPreferences.toml
        open(joinpath(dir, "inner_env", "LocalPreferences.toml"), write=true) do io
            write(io, """
            [This]
            pref4 = "inner-lp"
            """)
        end

        # Finally, we load preferences with a stacked environment, and ensure that
        # we get the appropriate outputs:
        old_load_path = copy(LOAD_PATH)
        try
            copy!(LOAD_PATH, [joinpath(dir, "inner_env", "Project.toml"), joinpath(dir, "outer_env", "Project.toml")])

            function test_this_prefs(this_prefs)
                @test this_prefs["pref1"] == "outer-project"
                @test this_prefs["pref2"] == "outer-jlp"
                @test this_prefs["pref3"] == "inner-project"
                @test this_prefs["pref4"] == "inner-lp"
                @test this_prefs["pref5"] == "outer-project"
                @test !haskey(this_prefs, "pref6")
            end

            # Test directly loading the UUID we're interested in
            test_this_prefs(Base.get_preferences(this_uuid))

            # Also test loading _all_ preferences
            all_prefs = Base.get_preferences()
            @test haskey(all_prefs, "This")
            @test haskey(all_prefs, "That")
            @test all_prefs["That"]["pref1"] == "outer-project"

            # Ensure that the sub-tree of `This` still satisfies our tests
            test_this_prefs(all_prefs["This"])
        finally
            copy!(LOAD_PATH, old_load_path)
        end
    end
end
