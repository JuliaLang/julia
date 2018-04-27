# This file is a part of Julia. License is MIT: https://julialang.org/license

module ResolveTest

using Test
using Pkg3.Types
using Pkg3.GraphType
using Pkg3.Types: VersionBound
using Pkg3.Resolve
using Pkg3.Resolve.VersionWeights
import Pkg3.Types: uuid5, uuid_package

# print info, stats etc.
const VERBOSE = false

# Check that VersionWeight keeps the same ordering as VersionNumber

vlst = [
    v"0.0.0",
    v"0.0.1",
    v"0.1.0",
    v"0.1.1",
    v"1.0.0",
    v"1.0.1",
    v"1.1.0",
    v"1.1.1"
    ]

for v1 in vlst, v2 in vlst
    vw1 = VersionWeight(v1)
    vw2 = VersionWeight(v2)
    clt = v1 < v2
    @test clt == (vw1 < vw2)
    ceq = v1 == v2
    @test ceq == (vw1 == vw2)
end

# auxiliary functions
pkguuid(p::String) = uuid5(uuid_package, p)
function storeuuid(p::String, uuid_to_name::Dict{UUID,String})
    uuid = p == "julia" ? Types.uuid_julia : pkguuid(p)
    if haskey(uuid_to_name, uuid)
        @assert uuid_to_name[uuid] == p
    else
        uuid_to_name[uuid] = p
    end
    return uuid
end
wantuuids(want_data) = Dict{UUID,VersionNumber}(pkguuid(p) => v for (p,v) in want_data)

function load_package_data_raw(T::Type, input::String)
    toml = Types.TOML.parse(input)
    data = Dict{VersionRange,Dict{String,T}}()
    for (v, d) in toml, (key, value) in d
        vr = VersionRange(v)
        dict = get!(data, vr, Dict{String,T}())
        haskey(dict, key) && cmderror("$ver/$key is duplicated in $path")
        dict[key] = T(value)
    end
    return data
end

function gen_versionranges(dict::Dict{K,Set{VersionNumber}}, srtvers::Vector{VersionNumber}) where {K}
    vranges = Dict{K,Vector{VersionRange}}()
    for (vreq,vset) in dict
        vranges[vreq] = VersionRange[]
        while !isempty(vset)
            vn0 = minimum(vset)
            i = findfirst(isequal(vn0), srtvers)
            @assert i ≠ 0
            pop!(vset, vn0)
            vn1 = vn0
            pushed = false
            j = i + 1
            while j ≤ length(srtvers)
                vn = srtvers[j]
                if vn ∈ vset
                    pop!(vset, vn)
                    vn1 = vn
                    j += 1
                else
                    # vn1 =  srtvers[j-1]
                    push!(vranges[vreq], VersionRange(VersionBound(vn0),VersionBound(vn1)))
                    pushed = true
                    break
                end
            end
            !pushed && push!(vranges[vreq], VersionRange(VersionBound(vn0),VersionBound(vn1)))
        end
    end
    allvranges = unique(vcat(collect(values(vranges))...))
    return vranges, allvranges
end

function graph_from_data(deps_data, uuid_to_name = Dict{UUID,String}(Types.uuid_julia=>"julia"))
    uuid(p) = storeuuid(p, uuid_to_name)
    # deps = DepsGraph(uuid_to_name)
    fixed = Dict(Types.uuid_julia => Fixed(VERSION))
    all_versions = Dict{UUID,Set{VersionNumber}}(fp => Set([fx.version]) for (fp,fx) in fixed)
    all_deps = Dict{UUID,Dict{VersionRange,Dict{String,UUID}}}(fp => Dict(VersionRange(fx.version)=>Dict()) for (fp,fx) in fixed)
    all_compat = Dict{UUID,Dict{VersionRange,Dict{String,VersionSpec}}}(fp => Dict(VersionRange(fx.version)=>Dict()) for (fp,fx) in fixed)

    deps = Dict{String,Dict{VersionNumber,Dict{String,VersionSpec}}}()
    for d in deps_data
        p, vn, r = d[1], d[2], d[3:end]
        if !haskey(deps, p)
            deps[p] = Dict{VersionNumber,Dict{String,VersionSpec}}()
        end
        if !haskey(deps[p], vn)
            deps[p][vn] = Dict{String,VersionSpec}()
        end
        isempty(r) && continue
        rp = r[1]
        rvs = VersionSpec(r[2:end])
        deps[p][vn][rp] = rvs
    end
    for p in keys(deps)
        u = uuid(p)
        all_versions[u] = Set(keys(deps[p]))
        srtvers = sort!(collect(keys(deps[p])))

        deps_pkgs = Dict{String,Set{VersionNumber}}()
        for (vn,vreq) in deps[p], rp in keys(vreq)
            push!(get!(deps_pkgs, rp, Set{VersionNumber}()), vn)
        end
        vranges, allvranges = gen_versionranges(deps_pkgs, srtvers)
        all_deps[u] = Dict{VersionRange,Dict{String,UUID}}(VersionRange()=>Dict{String,UUID}("julia"=>Types.uuid_julia))
        for vrng in allvranges
            all_deps[u][vrng] = Dict{String,UUID}()
            for (rp,vvr) in vranges
                vrng ∈ vvr || continue
                all_deps[u][vrng][rp] = uuid(rp)
            end
        end

        deps_reqs = Dict{Pair{String,VersionSpec},Set{VersionNumber}}()
        for (vn,vreq) in deps[p], (rp,rvs) in vreq
            push!(get!(deps_reqs, (rp=>rvs), Set{VersionNumber}()), vn)
        end
        vranges, allvranges = gen_versionranges(deps_reqs, srtvers)
        all_compat[u] = Dict{VersionRange,Dict{String,VersionSpec}}()
        for vrng in allvranges
            all_compat[u][vrng] = Dict{String,VersionSpec}()
            for (req,vvr) in vranges
                vrng ∈ vvr || continue
                rp,rvs = req
                all_compat[u][vrng][rp] = rvs
            end
        end
    end
    return Graph(all_versions, all_deps, all_compat, uuid_to_name, Requires(), fixed, VERBOSE)
end
function reqs_from_data(reqs_data, graph::Graph)
    reqs = Dict{UUID,VersionSpec}()
    function uuid_check(p)
        uuid = pkguuid(p)
        @assert graph.data.uuid_to_name[uuid] == p
        return uuid
    end
    for r in reqs_data
        p = uuid_check(r[1])
        reqs[p] = VersionSpec(r[2:end])
    end
    reqs
end
function sanity_tst(deps_data, expected_result; pkgs=[])
    if VERBOSE
        println()
        @info("sanity check")
        # @show deps_data
        # @show pkgs
    end
    graph = graph_from_data(deps_data)
    id(p) = pkgID(pkguuid(p), graph)
    result = sanity_check(graph, Set(pkguuid(p) for p in pkgs), VERBOSE)

    length(result) == length(expected_result) || return false
    expected_result_uuid = [(id(p), vn) for (p,vn) in expected_result]
    for r in result
        r ∈ expected_result_uuid || return  false
    end
    return true
end
sanity_tst(deps_data; kw...) = sanity_tst(deps_data, []; kw...)

function resolve_tst(deps_data, reqs_data, want_data = nothing; clean_graph = false)
    if VERBOSE
        println()
        @info("resolving")
        # @show deps_data
        # @show reqs_data
    end
    graph = graph_from_data(deps_data)
    reqs = reqs_from_data(reqs_data, graph)
    add_reqs!(graph, reqs)
    simplify_graph!(graph, clean_graph = clean_graph)
    want = resolve(graph)
    return want == wantuuids(want_data)
end

@testset "schemes" begin
    VERBOSE && @info("SCHEME 1")
    ## DEPENDENCY SCHEME 1: TWO PACKAGES, DAG
    deps_data = Any[
        ["A", v"1", "B", "1-*"],
        ["A", v"2", "B", "2-*"],
        ["B", v"1"],
        ["B", v"2"]
    ]

    @test sanity_tst(deps_data)
    @test sanity_tst(deps_data, pkgs=["A", "B"])
    @test sanity_tst(deps_data, pkgs=["B"])
    @test sanity_tst(deps_data, pkgs=["A"])

    # require just B
    reqs_data = Any[
        ["B", "*"]
    ]

    want_data = Dict("B"=>v"2")
    resolve_tst(deps_data, reqs_data, want_data)
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just A: must bring in B
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)


    VERBOSE && @info("SCHEME 2")
    ## DEPENDENCY SCHEME 2: TWO PACKAGES, CYCLIC
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["A", v"2", "B", "1-*"],
        ["B", v"1", "A", "2-*"],
        ["B", v"2", "A", "1-*"]
    ]

    @test sanity_tst(deps_data)

    # require just A
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just B, force lower version
    reqs_data = Any[
        ["B", "1"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just A, force lower version
    reqs_data = Any[
        ["A", "1"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)


    VERBOSE && @info("SCHEME 3")
    ## DEPENDENCY SCHEME 3: THREE PACKAGES, CYCLIC, TWO MUTUALLY EXCLUSIVE SOLUTIONS
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["A", v"2", "B", "1"],
        ["B", v"1", "C", "2-*"],
        ["B", v"2", "C", "1"],
        ["C", v"1", "A", "1"],
        ["C", v"2", "A", "2-*"]
    ]

    @test sanity_tst(deps_data)

    # require just A (must choose solution which has the highest version for A)
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"1", "C"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just B (must choose solution which has the highest version for B)
    reqs_data = Any[
        ["B", "*"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2", "C"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just A, force lower version
    reqs_data = Any[
        ["A", "1"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2", "C"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require A and C, incompatible versions
    reqs_data = Any[
        ["A", "1"],
        ["C", "2-*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)


    VERBOSE && @info("SCHEME 4")
    ## DEPENDENCY SCHEME 4: TWO PACKAGES, DAG, WITH TRIVIAL INCONSISTENCY
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["B", v"1"]
    ]

    @test sanity_tst(deps_data, [("A", v"1")])
    @test sanity_tst(deps_data, pkgs=["B"])

    # require B (must not give errors)
    reqs_data = Any[
        ["B", "*"]
    ]
    want_data = Dict("B"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)


    VERBOSE && @info("SCHEME 5")
    ## DEPENDENCY SCHEME 5: THREE PACKAGES, DAG, WITH IMPLICIT INCONSISTENCY
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["A", v"1", "C", "2-*"],
        # ["A", v"1", "julia", "10"],
        ["A", v"2", "B", "1"],
        ["A", v"2", "C", "1"],
        ["B", v"1", "C", "2-*"],
        ["B", v"2", "C", "2-*"],
        ["C", v"1"],
        ["C", v"2"]
    ]

    @test sanity_tst(deps_data, [("A", v"2")])
    @test sanity_tst(deps_data, pkgs=["B"])
    @test sanity_tst(deps_data, pkgs=["C"])

    # require A, any version (must use the highest non-inconsistent)
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2", "C"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require A, force highest version (impossible)
    reqs_data = Any[
        ["A", "2-*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)


    VERBOSE && @info("SCHEME 6")
    ## DEPENDENCY SCHEME 6: TWO PACKAGES, CYCLIC, TOTALLY INCONSISTENT
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["A", v"2", "B", "1"],
        ["B", v"1", "A", "1"],
        ["B", v"2", "A", "2-*"]
    ]

    @test sanity_tst(deps_data, [("A", v"1"), ("A", v"2"),
                                ("B", v"1"), ("B", v"2")])

    # require A (impossible)
    reqs_data = Any[
        ["A", "*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)

    # require B (impossible)
    reqs_data = Any[
        ["B", "*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)


    VERBOSE && @info("SCHEME 7")
    ## DEPENDENCY SCHEME 7: THREE PACKAGES, CYCLIC, WITH INCONSISTENCY
    deps_data = Any[
        ["A", v"1", "B", "1"],
        ["A", v"2", "B", "2-*"],
        ["B", v"1", "C", "1"],
        ["B", v"2", "C", "2-*"],
        ["C", v"1", "A", "2-*"],
        ["C", v"2", "A", "2-*"],
    ]

    @test sanity_tst(deps_data, [("A", v"1"), ("B", v"1"),
                                ("C", v"1")])

    # require A
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2", "C"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require C
    reqs_data = Any[
        ["C", "*"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2", "C"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require C, lowest version (impossible)
    reqs_data = Any[
        ["C", "1"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)


    VERBOSE && @info("SCHEME 8")
    ## DEPENDENCY SCHEME 8: THREE PACKAGES, CYCLIC, TOTALLY INCONSISTENT
    deps_data = Any[
        ["A", v"1", "B", "1"],
        ["A", v"2", "B", "2-*"],
        ["B", v"1", "C", "1"],
        ["B", v"2", "C", "2-*"],
        ["C", v"1", "A", "2-*"],
        ["C", v"2", "A", "1"],
    ]

    @test sanity_tst(deps_data, [("A", v"1"), ("A", v"2"),
                                ("B", v"1"), ("B", v"2"),
                                ("C", v"1"), ("C", v"2")])

    # require A (impossible)
    reqs_data = Any[
        ["A", "*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)

    # require B (impossible)
    reqs_data = Any[
        ["B", "*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)

    # require C (impossible)
    reqs_data = Any[
        ["C", "*"]
    ]
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)

    VERBOSE && @info("SCHEME 9")
    ## DEPENDENCY SCHEME 9: SIX PACKAGES, DAG
    deps_data = Any[
        ["A", v"1"],
        ["A", v"2"],
        ["A", v"3"],
        ["B", v"1", "A", "1"],
        ["B", v"2", "A", "*"],
        ["C", v"1", "A", "2"],
        ["C", v"2", "A", "2-*"],
        ["D", v"1", "B", "1-*"],
        ["D", v"2", "B", "2-*"],
        ["E", v"1", "D", "*"],
        ["F", v"1", "A", "1-2"],
        ["F", v"1", "E", "*"],
        ["F", v"2", "C", "2-*"],
        ["F", v"2", "E", "*"],
    ]

    @test sanity_tst(deps_data)

    # require just F
    reqs_data = Any[
        ["F", "*"]
    ]
    want_data = Dict("A"=>v"3", "B"=>v"2", "C"=>v"2",
                    "D"=>v"2", "E"=>v"1", "F"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just F, lower version
    reqs_data = Any[
        ["F", "1"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2", "D"=>v"2",
                    "E"=>v"1", "F"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require F and B; force lower B version -> must bring down F, A, and D versions too
    reqs_data = Any[
        ["F", "*"],
        ["B", "1"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"1", "D"=>v"1",
                    "E"=>v"1", "F"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require F and D; force lower D version -> must not bring down F version
    reqs_data = Any[
        ["F", "*"],
        ["D", "1"]
    ]
    want_data = Dict("A"=>v"3", "B"=>v"2", "C"=>v"2",
                    "D"=>v"1", "E"=>v"1", "F"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require F and C; force lower C version -> must bring down F and A versions
    reqs_data = Any[
        ["F", "*"],
        ["C", "1"]
    ]
    want_data = Dict("A"=>v"2", "B"=>v"2", "C"=>v"1",
                    "D"=>v"2", "E"=>v"1", "F"=>v"1")
    @test resolve_tst(deps_data, reqs_data, want_data)

    VERBOSE && @info("SCHEME 10")
    ## DEPENDENCY SCHEME 10: FIVE PACKAGES, SAME AS SCHEMES 5 + 1, UNCONNECTED
    deps_data = Any[
        ["A", v"1", "B", "2-*"],
        ["A", v"1", "C", "2-*"],
        ["A", v"2", "B", "1"],
        ["A", v"2", "C", "1"],
        ["B", v"1", "C", "2-*"],
        ["B", v"2", "C", "2-*"],
        ["C", v"1"],
        ["C", v"2"],
        ["D", v"1", "E", "1-*"],
        ["D", v"2", "E", "2-*"],
        ["E", v"1"],
        ["E", v"2"]
    ]

    @test sanity_tst(deps_data, [("A", v"2")])
    @test sanity_tst(deps_data, pkgs=["B"])
    @test sanity_tst(deps_data, pkgs=["D"])
    @test sanity_tst(deps_data, pkgs=["E"])
    @test sanity_tst(deps_data, pkgs=["B", "D"])

    # require A, any version (must use the highest non-inconsistent)
    reqs_data = Any[
        ["A", "*"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2", "C"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)

    # require just D: must bring in E
    reqs_data = Any[
        ["D", "*"]
    ]
    want_data = Dict("D"=>v"2", "E"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)


    # require A and D, must be the merge of the previous two cases
    reqs_data = Any[
        ["A", "*"],
        ["D", "*"]
    ]
    want_data = Dict("A"=>v"1", "B"=>v"2", "C"=>v"2", "D"=>v"2", "E"=>v"2")
    @test resolve_tst(deps_data, reqs_data, want_data)
end

@testset "realistic" begin
    VERBOSE && @info("SCHEME REALISTIC")
    ## DEPENDENCY SCHEME 11: A REALISTIC EXAMPLE
    ## ref Julia issue #21485

    include("resolvedata1.jl")

    @test sanity_tst(ResolveData.deps_data, ResolveData.problematic_data)
    @test resolve_tst(ResolveData.deps_data, ResolveData.reqs_data, ResolveData.want_data)
end

@testset "nasty" begin
    VERBOSE && @info("SCHEME NASTY")
    ## DEPENDENCY SCHEME 12: A NASTY CASE

    include("NastyGenerator.jl")
    deps_data, reqs_data, want_data, problematic_data = NastyGenerator.generate_nasty(5, 20, q=20, d=4, sat = true)

    @test sanity_tst(deps_data, problematic_data)
    @test resolve_tst(deps_data, reqs_data, want_data)

    deps_data, reqs_data, want_data, problematic_data = NastyGenerator.generate_nasty(5, 20, q=20, d=4, sat = false)

    @test sanity_tst(deps_data, problematic_data)
    @test_throws ResolverError resolve_tst(deps_data, reqs_data)
end

end # module
