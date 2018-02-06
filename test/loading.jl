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

import Base: SHA1, PkgId, load_path, identify_package, locate_package, version_slug
import UUIDs: UUID, uuid4, uuid_version
import Random: shuffle, randstring
using Test

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
    pkg.uuid === nothing && return nothing, nothing
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
        println(io, "git-tree-sha1 = \"$(bytes2hex(tree.bytes))\"")
        if !isempty(deps)
            println(io, "    [$name.deps]")
            for (n, u) in deps
                println(io, "    $n = \"$u\"")
            end
        end
        println(io)
    end
end

false && let name = "Flarp"
uuidA = UUID("b2cb3794-8625-4058-bcde-7eeb13ac1c8b")
uuidB = UUID("1513c021-3639-4616-a37b-ee45c9d2f773")
uuids = [nothing, uuidA, uuidB]

ft(::UUID)    =  true:true
ft(::Nothing) = false:true

@testset "direct dependency loading: implict + implicit" begin
    for uuid1 in uuids, proj1 in ft(uuid1),
        uuid2 in uuids, proj2 in ft(uuid2)
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
            uuids[i] !== nothing && uuids[j] !== nothing || continue
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

@testset "direct dependency loading: explicit + implicit" begin
    mktempdir() do depot
        push!(empty!(DEPOT_PATH), depot)
        pkgs  = [PkgId(uuid, name) for uuid in uuids]
        pairs = [gen_depot_ver(depot, pkg) for pkg in pkgs, _ in 1:2]
        trees = first.(pairs)
        paths = last.(pairs)
        for i = 1:length(uuids), k = 1:2,
            j = 1:length(uuids), l = 1:2, proj in ft(uuids[j])
            uuids[i] !== nothing || continue
            empty!(LOAD_PATH)
            mktempdir() do dir1
                push!(LOAD_PATH, dir1)
                gen_explicit(dir1)
                gen_manifest(dir1, name, uuids[i], trees[i,k])
                @test identify_package(name) == pkgs[i]
                @test locate_package(pkgs[i]) == paths[i,k]
                path = uuids[i] == coalesce(uuids[j], uuids[i]) ? paths[i,k] : nothing
                @test locate_package(pkgs[j]) == path
                mktempdir() do dir2
                    push!(LOAD_PATH, dir2)
                    path2 = gen_implicit(dir2, pkgs[j], proj)
                    @test identify_package(name) == pkgs[i]
                    @test locate_package(pkgs[i]) == paths[i,k]
                    path = uuids[i] == coalesce(uuids[j], uuids[i]) ? paths[i,k] : path2
                    @test locate_package(pkgs[j]) == path
                end
            end
        end
    end
end

uuidT = UUID("a54bd003-d8dc-4161-b186-d5516cd448e9")

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
            uuids[i] !== nothing && uuids[j] !== nothing || continue
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
end # let

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

const envs = Dict{String,eltype(graphs)}()

for (flat, root, roots, graph) in graphs
    if flat
        all(KIND[i] == 2 for i in values(roots)) || continue
        all(KIND[i] == 2 for i in keys(graph)) || continue
    end
    dir = mktempdir()
    envs[dir] = (flat, root, roots, graph)

    # generate project file
    open(joinpath(dir, "Project.toml"), "w") do io
        name, uuid, kind = L(root), UUIDS[root], KIND[root]
        kind != 0 && println(io, "name = ", repr(name))
        kind == 2 && println(io, "uuid = ", repr(string(uuid)))
        println(io, "[deps]")
        for (n, i) in roots
            i == root && continue
            @assert KIND[i] == 2
            println(io, "$n = ", repr(string(UUIDS[i])))
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
end

# materialize dependency graphs as implicit environments (if possible)

for (flat, root, roots, graph) in graphs
    flat || continue
    dir = mktempdir()
    envs[dir] = (flat, root, roots, graph)

    for (name, i) in roots
        uuid, kind = UUIDS[i], KIND[i]
        # generate package entry point
        entry = joinpath(dir, name, "src", "$name.jl")
        mkpath(dirname(entry))
        open(entry, "w") do io
            print(io, """
            module $name
            name = $(repr(name))
            uuid = $(repr(string(uuid)))
            end
            """)
        end
        kind == 0 && continue
        deps = delete!(copy(graph[i]), name)
        # generate project file
        open(joinpath(dir, name, "Project.toml"), "w") do io
            kind != 0 && println(io, "name = ", repr(name))
            kind == 2 && println(io, "uuid = ", repr(string(uuid)))
            isempty(deps) || println(io, "[deps]")
            for (n, j) in deps
                @assert KIND[j] == 2
                println(io, "  $n = ", repr(string(UUIDS[j])))
            end
        end
    end
end

## use generated environments to test package loading ##

function pkg_id(id::Int)
    PkgId(KIND[id] == 2 ? UUIDS[id] : nothing, L(id))
end
function pkg_id(ids::Dict{String,Int}, name::String)
    haskey(ids, name) ? pkg_id(ids[name]) : nothing
end

function ≊(a::PkgId, b::PkgId)
    a.name == b.name || return false
    a.uuid == b.uuid && return true
    a.uuid == nothing && uuid_version(b.uuid) == 5 ||
    b.uuid == nothing && uuid_version(a.uuid) == 5
end
≊(a::Union{Nothing,PkgId}, b::Union{Nothing,PkgId}) = a == b

function add_id!(ids::Vector{Pair{Int,PkgId}},
    nodes::Dict{String,Int}, name::String, id::PkgId)
    node = nodes[name]
    any(node == i for (i, _) in ids) && return ids
    push!(ids, node => id)
end
function add_id!(ids::Vector{Pair{Int,PkgId}},
    nodes::Dict{String,Int}, name::String, ::Nothing)
    return ids # no op
end

function test_identify(roots::Dict{String,Int}, graph::Dict{Int,Dict{String,Int}})
    ids = Pair{Int,PkgId}[]
    # check & add named roots
    for name in NAMES
        id = identify_package(name)
        @test id ≊ pkg_id(roots, name)
        add_id!(ids, roots, name, id)
    end
    # add nodes reachable by uuid
    for (node, deps) in graph
        KIND[node] == 2 || continue
        add_id!(ids, deps, L(node), pkg_id(node))
    end
    # check all nodes reachable via `where`
    let i = 0
        while (i += 1) ≤ length(ids)
            node, where = ids[i]
            deps = get(graph, node, roots)
            for name in NAMES
                id = identify_package(where, name)
                @test id ≊ pkg_id(deps, name)
                add_id!(ids, deps, name, id)
            end
        end
    end
    # check all other package ids return nothing
    let ids = Dict(ids)
        for node in NODES
            node in keys(ids) && continue
            where = pkg_id(node)
            where.uuid == nothing && continue
            for name in NAMES
                id = where.name == name ? where : nothing
                @test identify_package(where, name) == id
            end
        end
    end
end

empty!(DEPOT_PATH)

@testset "identify_package with one env in load path" begin
    for (env, (_, _, roots, graph)) in envs
        push!(empty!(LOAD_PATH), env)
        test_identify(roots, graph)
    end
end

@testset "identify_package with two envs in load path" begin
    for x = false:true,
        (env1, (_, _, roots1, graph1)) in (x ? envs : rand(envs, 10)),
        (env2, (_, _, roots2, graph2)) in (x ? rand(envs, 10) : envs)
        push!(empty!(LOAD_PATH), env1, env2)
        roots = merge(roots2, roots1)
        graph = merge(graph2, graph1)
        test_identify(roots, graph)
    end
end

@testset "identify_package with three envs in load path" begin
    for (env1, (_, _, roots1, graph1)) in rand(envs, 10),
        (env2, (_, _, roots2, graph2)) in rand(envs, 10),
        (env3, (_, _, roots3, graph3)) in rand(envs, 10)
        push!(empty!(LOAD_PATH), env1, env2, env3)
        roots = merge(roots3, roots2, roots1)
        graph = merge(graph3, graph2, graph1)
        test_identify(roots, graph)
    end
end

## cleanup after tests ##

for env in keys(envs)
    rm(env, force=true, recursive=true)
end

append!(empty!(DEPOT_PATH), saved_depot_path)
append!(empty!(LOAD_PATH), saved_load_path)
