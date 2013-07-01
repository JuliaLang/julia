module PkgToMaxSumInterface

using ...Types, ...Query

export MetadataError, Interface, compute_output_dict,
       verify_solution, enforce_optimality!

# Error type used to signal that there was some
# problem with the metadata info passed to resolve
type MetadataError <: Exception
    info
end

# The numeric type used to determine how the different
# versions of a package should be weighed
typealias VersionWeight Int

# A collection of objects which allow interfacing external (Pkg) and
# internal (MaxSum) representation
type Interface
    # requirements and dependencies, in external representation
    reqs::Requires
    deps::Dict{ByteString,Dict{VersionNumber,Available}}

    # packages list
    pkgs::Vector{ByteString}

    # number of packages
    np::Int

    # states per package: one per version + uninstalled
    spp::Vector{Int}

    # pakage dict: associates an index to each package name
    pdict::Dict{ByteString,Int}

    # package versions: for each package, keep the list of the
    #                   possible version numbers; this defines a
    #                   mapping from version numbers of a package
    #                   to indices
    pvers::Vector{Vector{VersionNumber}}

    # versions dict: associates a version index to each package
    #                version; such that
    #                  pvers[p0][vdict[p0][vn]] = vn
    vdict::Vector{Dict{VersionNumber,Int}}

    # version weights: the weight for each version of each package
    #                  (versions include the uninstalled state; the
    #                   higher the weight, the more favored the version)
    vweight::Vector{Vector{VersionWeight}}

    # equivalence classes: after version pruning, for each package and each
    #                      version keeps a vector with all versions in the
    #                      equivalence class
    eq_classes_map::Dict{ByteString,Dict{VersionNumber,Vector{VersionNumber}}}

    function Interface(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

        # check reqs for unknown packages
        for rp in keys(reqs)
            haskey(deps, rp) || throw(MetadataError("required package $rp has no version compatible with fixed requirements"))
        end

        # reduce deps by version pruning
        deps, eq_classes_map = Query.prune_versions(reqs, deps)

        # generate pkgs
        pkgs = sort!(ByteString[Set{ByteString}(keys(deps)...)...])

        np = length(pkgs)

        # generate pdict
        pdict = (ByteString=>Int)[ pkgs[i] => i for i = 1:np ]

        # generate spp and pvers
        spp = Array(Int, np)

        pvers = [ VersionNumber[] for i = 1:np ]

        for (p,depsp) in deps, vn in keys(depsp)
            p0 = pdict[p]
            push!(pvers[p0], vn)
        end
        for p0 = 1:np
            sort!(pvers[p0])
            spp[p0] = length(pvers[p0]) + 1
        end

        # generate vdict
        vdict = [(VersionNumber=>Int)[] for p0 = 1:np]
        for (p,depsp) in deps
            p0 = pdict[p]
            vdict0 = vdict[p0]
            for vn in keys(depsp)
                for v0 in 1:length(pvers[p0])
                    if pvers[p0][v0] == vn
                        vdict0[vn] = v0
                        break
                    end
                end
            end
        end

        # generate wveights:
        # the version weights are just progressive integer numbers,
        # there is no difference between major, minor, patch etc.
        # TODO: change this to weigh differently major, minor etc. ?
        # NOTE: the computation uses eq_classes_map so that version
        # pruning has no effect on the final output and the end result
        # is the same as if it was computed as
        #   vweight = [ [ v0-1 for v0 = 1:spp[p0] ] for p0 = 1:np ]
        # before pruning, and pruning was performed afterwards
        vweight = [ Array(Int, spp[p0]) for p0 = 1:np ]
        for p0 = 1:np
            p = pkgs[p0]
            @assert haskey(eq_classes_map, p)
            eqclass0 = eq_classes_map[p]
            allv = vcat([cl for (_,cl) in eqclass0]...)
            vweight0 = vweight[p0]
            pvers0 = pvers[p0]
            for v0 = 1:spp[p0]-1
                vn = pvers0[v0]
                vweight0[v0] = nnz(allv .< vn)
            end
            vweight0[spp[p0]] = length(allv)
        end

        return new(reqs, deps, pkgs, np, spp, pdict, pvers, vdict, vweight, eq_classes_map)
    end
end

# The output format is a Dict which associates a VersionNumber to each installed package name
function compute_output_dict(sol::Vector{Int}, interface::Interface)

    pkgs = interface.pkgs
    np = interface.np
    pvers = interface.pvers
    spp = interface.spp

    want = (ByteString=>VersionNumber)[]
    for p0 = 1:np
        p = pkgs[p0]
        s = sol[p0]
        if s != spp[p0]
            v = pvers[p0][s]
            want[p] = v
        end
    end

    return want
end

# verifies that the solution fulfills all hard constraints
# (requirements and dependencies)
function verify_solution(sol::Vector{Int}, interface::Interface)

    reqs = interface.reqs
    deps = interface.deps
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    vdict = interface.vdict

    # verify requirements
    for (p,vs) in reqs
        p0 = pdict[p]
        @assert sol[p0] != spp[p0]
        vn = pvers[p0][sol[p0]]
        @assert contains(vs, vn)
    end

    # verify dependencies
    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            if sol[p0] == v0
                for (rp, rvs) in a.requires
                    p1 = pdict[rp]
                    @assert sol[p1] != spp[p1]
                    vn = pvers[p1][sol[p1]]
                    @assert contains(rvs, vn)
                end
            end
        end
    end

end

# Push the given solution to a local optimium if needed
function enforce_optimality!(sol::Vector{Int}, interface::Interface)
    np = interface.np

    reqs = interface.reqs
    deps = interface.deps
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    vdict = interface.vdict

    # prepare some useful structures
    # pdeps[p0][v0] has all dependencies of package p0 version v0
    pdeps = [ Array(Requires, spp[p0]-1) for p0 = 1:np ]
    # prevdeps[p1][p0][v0] is the VersionSet of package p1 which package p0 version v0
    # depends upon
    prevdeps = [ (Int=>Dict{Int,VersionSet})[] for p0 = 1:np ]

    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            pdeps[p0][v0] = a.requires
            for (rp, rvs) in a.requires
                p1 = pdict[rp]
                if !haskey(prevdeps[p1], p0)
                    prevdeps[p1][p0] = (Int=>VersionSet)[]
                end
                prevdeps[p1][p0][v0] = rvs
            end
        end
    end

    restart = true
    while restart
        restart = false
        for p0 = 1:np
            s0 = sol[p0]
            if s0 >= spp[p0] - 1
                # either the package is not installed,
                # or it's already at the maximum version
                continue
            end
            viol = false
            # check if the higher version has a depencency which
            # would be violated by the state of the remaining packages
            for (p,vs) in pdeps[p0][s0+1]
                p1 = pdict[p]
                if sol[p1] == spp[p1]
                    # the dependency is violated because
                    # the other package is not being installed
                    viol = true
                    break
                end
                vn = pvers[p1][sol[p1]]
                if !contains(vs, vn)
                    # the dependency is violated because
                    # the other package version is invalid
                    viol = true
                    break
                end
            end
            if viol
                continue
            end

            # check if bumping the version would violate some
            # dependency of another package
            for (p1,d) in prevdeps[p0]
                vs = get(d, sol[p1], nothing)
                if vs == nothing
                    continue
                end
                vn = pvers[p0][s0+1]
                if !contains(vs, vn)
                    # bumping the version would violate
                    # the dependency
                    viol = true
                    break
                end
            end
            if viol
                continue
            end
            # So the solution is non-optimal: we bump it manually
            #warn("nonoptimal solution for package $(interface.pkgs[p0]): sol=$s0")
            sol[p0] += 1
            restart = true
        end
    end
    return
end

end
