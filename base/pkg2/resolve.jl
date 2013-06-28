module Resolve

include("pkg2/resolve/pkgstruct.jl")
include("pkg2/resolve/maxsum.jl")

using ..Types, .PkgStructs, .MaxSum

export resolve, sanity_check, MetadataError

# Use the max-sum algorithm to resolve packages dependencies
function resolve(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

    # fetch data
    reqsstruct = ReqsStruct(reqs, deps)

    # init structures
    pkgstruct = PkgStruct(reqsstruct)

    prune_versions!(reqsstruct, pkgstruct)

    graph = Graph(reqsstruct, pkgstruct)
    msgs = Messages(reqsstruct, pkgstruct, graph)

    # find solution
    local sol::Vector{Int}
    try
        sol = maxsum(graph, msgs)
    catch err
        if isa(err, UnsatError)
            p = reqsstruct.pkgs[err.info]
            msg = "Unsatisfiable package requirements detected: " *
                  "no feasible version could be found for package: $p"
            if msgs.num_nondecimated != graph.np
                msg *= "\n  (you may try increasing the value of the" *
                       "\n   JULIA_PKGRESOLVE_ACCURACY environment variable)"
            end
            error(msg)
        end
        rethrow(err)
    end

    # verify solution (debug code) and enforce its optimality
    verify_sol(reqsstruct, pkgstruct, sol)
    enforce_optimality(reqsstruct, pkgstruct, sol)

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(reqsstruct, pkgstruct, sol)
end

# Scan dependencies for (explicit or implicit) contradictions
function sanity_check(deps::Dict{ByteString,Dict{VersionNumber,Available}})

    reqsstruct0 = ReqsStruct((ByteString=>VersionSet)[], deps)
    pkgstruct0 = PkgStruct(reqsstruct0)

    eq_classes_map = prune_versions!(reqsstruct0, pkgstruct0, false)

    pkgs = reqsstruct0.pkgs
    deps = reqsstruct0.deps
    np = reqsstruct0.np
    spp = pkgstruct0.spp
    pdict = pkgstruct0.pdict
    pvers = pkgstruct0.pvers
    vdict = pkgstruct0.vdict

    pdeps = [ [ Available[] for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]
    pndeps = [ zeros(Int,spp[p0]-1) for p0 = 1:np ]
    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            push!(pdeps[p0][v0], a)
            pndeps[p0][v0] += 1
        end
    end

    rev_eq_classes_map = [ (VersionNumber=>Vector{VersionNumber})[] for p0 = 1:np ]
    for p0 = 1:np
        eqclass0 = eq_classes_map[p0]
        reveqclass0 = rev_eq_classes_map[p0]
        for (v,vtop) in eqclass0
            if !haskey(reveqclass0, vtop)
                reveqclass0[vtop] = VersionNumber[v]
            else
                push!(reveqclass0[vtop], v)
            end
        end
    end

    vers = Array((ByteString,VersionNumber), 0)
    for (p,d) in deps, (vn,_) in d
        push!(vers, (p,vn))
    end
    function vrank(p::ByteString, vn::VersionNumber)
        p0 = pdict[p]
        v0 = vdict[p0][vn]
        return -pndeps[p0][v0]
    end
    sortby!(vers, x->vrank(x...))

    nv = length(vers)

    svdict = ((ByteString,VersionNumber)=>Int)[]
    i = 1
    for v in vers
        svdict[v] = i
        i += 1
    end
    checked = falses(nv)

    insane_ids = Array((Int,Int),0)
    problematic_pkgs = String[]

    i = 1
    psl = 0
    for (p,vn) in vers
        vr = -vrank(p,vn)
        if vr == 0
            break
        end
        if checked[i]
            i += 1
            continue
        end
        reqsstruct, pkgstruct = substructs(reqsstruct0, pkgstruct0, pdeps, p, vn)

        graph = Graph(reqsstruct, pkgstruct)
        msgs = Messages(reqsstruct, pkgstruct, graph)

        red_pkgs = reqsstruct.pkgs
        red_np = reqsstruct.np
        red_spp = pkgstruct.spp
        red_pvers = pkgstruct.pvers

        local sol::Vector{Int}
        try
            sol = maxsum(graph, msgs)
            verify_sol(reqsstruct, pkgstruct, sol)

            for p0 = 1:red_np
                s0 = sol[p0]
                if s0 != red_spp[p0]
                    j = svdict[(red_pkgs[p0], red_pvers[p0][s0])]
                    checked[j] = true
                end
            end
            checked[i] = true
        catch err
            if isa(err, UnsatError)
                pp = red_pkgs[err.info]
                p0 = pdict[p]
                v0 = vdict[p0][vn]
                push!(insane_ids, (p0, v0))
                push!(problematic_pkgs, pp)
            else
                rethrow(err)
            end
        end
        i += 1
    end

    insane = Array((ByteString,VersionNumber,ByteString), 0)
    if !isempty(insane_ids)
        i = 1
        for (p0,v0) in insane_ids
            p = pkgs[p0]
            vn = pvers[p0][v0]
            pp = problematic_pkgs[i]
            for vneq in rev_eq_classes_map[p0][vn]
                push!(insane, (p, vneq, pp))
            end
            i += 1
        end
        sortby!(insane, x->x[1])
        throw(MetadataError(insane))
    end

    return
end

end # module
