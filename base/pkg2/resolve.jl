module Resolve

include("pkg2/resolve/interface.jl")
include("pkg2/resolve/maxsum.jl")

using ..Types, .PkgToMaxSumInterface, .MaxSum

export resolve, sanity_check, MetadataError

# Use the max-sum algorithm to resolve packages dependencies
function resolve(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

    # init structures
    interface = Interface(reqs, deps)

    prune_versions!(interface)

    graph = Graph(interface)
    msgs = Messages(interface, graph)

    # find solution
    local sol::Vector{Int}
    try
        sol = maxsum(graph, msgs)
    catch err
        if isa(err, UnsatError)
            p = interface.pkgs[err.info]
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
    verify_sol(interface, sol)
    enforce_optimality(interface, sol)

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(interface, sol)
end

# Scan dependencies for (explicit or implicit) contradictions
function sanity_check(deps::Dict{ByteString,Dict{VersionNumber,Available}})

    interface0 = Interface((ByteString=>VersionSet)[], deps)

    eq_classes_map = prune_versions!(interface0, false)

    pkgs = interface0.pkgs
    deps = interface0.deps
    np = interface0.np
    spp = interface0.spp
    pdict = interface0.pdict
    pvers = interface0.pvers
    vdict = interface0.vdict

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
        interface = reduce_interface(interface0, pdeps, p, vn)

        graph = Graph(interface)
        msgs = Messages(interface, graph)

        red_pkgs = interface.pkgs
        red_np = interface.np
        red_spp = interface.spp
        red_pvers = interface.pvers

        local sol::Vector{Int}
        try
            sol = maxsum(graph, msgs)
            verify_sol(interface, sol)

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
