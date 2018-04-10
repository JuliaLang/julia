# This file is a part of Julia. License is MIT: https://julialang.org/license

module PkgToMaxSumInterface

using ...Types, ...Query, ..VersionWeights

export Interface, compute_output_dict, greedysolver,
       verify_solution, enforce_optimality!

# A collection of objects which allow interfacing external (Pkg) and
# internal (MaxSum) representation
mutable struct Interface
    # requirements and dependencies, in external representation
    reqs::Requires
    deps::Dict{String,Dict{VersionNumber,Available}}

    # packages list
    pkgs::Vector{String}

    # number of packages
    np::Int

    # states per package: one per version + uninstalled
    spp::Vector{Int}

    # pakage dict: associates an index to each package name
    pdict::Dict{String,Int}

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

    function Interface(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}})
        # generate pkgs
        pkgs = sort!(String[keys(deps)...])

        np = length(pkgs)

        # generate pdict
        pdict = Dict{String,Int}(pkgs[i] => i for i = 1:np)

        # generate spp and pvers
        spp = Vector{Int}(undef, np)

        pvers = [VersionNumber[] for i = 1:np]

        for (p,depsp) in deps, vn in keys(depsp)
            p0 = pdict[p]
            push!(pvers[p0], vn)
        end
        for p0 = 1:np
            sort!(pvers[p0])
            spp[p0] = length(pvers[p0]) + 1
        end

        # generate vdict
        vdict = [Dict{VersionNumber,Int}() for p0 = 1:np]
        for (p,depsp) in deps
            p0 = pdict[p]
            vdict0 = vdict[p0]
            pvers0 = pvers[p0]
            for vn in keys(depsp)
                for v0 in 1:length(pvers0)
                    if pvers0[v0] == vn
                        vdict0[vn] = v0
                        break
                    end
                end
            end
        end

        ## generate wveights:
        vweight = Vector{Vector{VersionWeight}}(undef, np)
        for p0 = 1:np
            pvers0 = pvers[p0]
            spp0 = spp[p0]
            vweight0 = vweight[p0] = Vector{VersionWeight}(undef, spp0)
            for v0 = 1:spp0-1
                vweight0[v0] = VersionWeight(pvers0[v0])
            end
            vweight0[spp0] = VersionWeight(v"0") # last version means uninstalled
        end

        return new(reqs, deps, pkgs, np, spp, pdict, pvers, vdict, vweight)
    end
end

# The output format is a Dict which associates a VersionNumber to each installed package name
function compute_output_dict(sol::Vector{Int}, interface::Interface)
    pkgs = interface.pkgs
    np = interface.np
    pvers = interface.pvers
    spp = interface.spp

    want = Dict{String,VersionNumber}()
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

# Produce a trivial solution: try to maximize each version;
# bail out as soon as some non-trivial requirements are detected.
function greedysolver(interface::Interface)
    reqs = interface.reqs
    deps = interface.deps
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    np = interface.np

    # initialize solution: all uninstalled
    sol = [spp[p0] for p0 = 1:np]

    # set up required packages to their highest allowed versions
    for (rp,rvs) in reqs
        rp0 = pdict[rp]
        # look for the highest version which satisfies the requirements
        rv = spp[rp0] - 1
        while rv > 0
            rvn = pvers[rp0][rv]
            rvn ∈ rvs && break
            rv -= 1
        end
        @assert rv > 0
        sol[rp0] = rv
    end

    # we start from required packages and explore the graph
    # following dependencies
    staged = Set{String}(keys(reqs))
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{String}()
        for p in staged
            p0 = pdict[p]
            @assert sol[p0] < spp[p0]
            vn = pvers[p0][sol[p0]]
            a = deps[p][vn]

            # scan dependencies
            for (rp,rvs) in a.requires
                rp0 = pdict[rp]
                # look for the highest version which satisfies the requirements
                rv = spp[rp0] - 1
                while rv > 0
                    rvn = pvers[rp0][rv]
                    rvn ∈ rvs && break
                    rv -= 1
                end
                # if we found a version, and the package was uninstalled
                # or the same version was already selected, we're ok;
                # otherwise we can't be sure what the optimal configuration is
                # and we bail out
                if rv > 0 && (sol[rp0] == spp[rp0] || sol[rp0] == rv)
                    sol[rp0] = rv
                else
                    return (false, Int[])
                end

                rp ∈ seen || push!(staged_next, rp)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    @assert verify_solution(sol, interface)

    return true, sol
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
        sol[p0] != spp[p0] || return false
        vn = pvers[p0][sol[p0]]
        vn ∈ vs || return false
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
                    if sol[p1] == spp[p1]
                        println("""
                                VERIFICATION ERROR: REQUIRED DEPENDENCY NOT INSTALLED
                                    package p=$p (p0=$p0) version=$vn (v0=$v0) requires package rp=$rp in version set rvs=$rvs
                                    but package $rp is not being installed (p1=$p1 sol[p1]=$(sol[p1]) == spp[p1]=$(spp[p1]))
                                """)
                        return false
                    end
                    vn1 = pvers[p1][sol[p1]]
                    if vn1 ∉ rvs
                        println("""
                                VERIFICATION ERROR: INVALID VERSION
                                    package p=$p (p0=$p0) version=$vn (v0=$v0) requires package rp=$rp in version set rvs=$rvs
                                    but package $rp version is being set to $vn1 (p1=$p1 sol[p1]=$(sol[p1]) spp[p1]=$(spp[p1]))
                                """)
                        return false
                    end
                end
            end
        end
    end
    return true
end

# Push the given solution to a local optimium if needed
function enforce_optimality!(sol::Vector{Int}, interface::Interface)
    np = interface.np

    reqs = interface.reqs
    deps = interface.deps
    pkgs = interface.pkgs
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    vdict = interface.vdict

    # prepare some useful structures
    # pdeps[p0][v0] has all dependencies of package p0 version v0
    pdeps = [Vector{Requires}(undef, spp[p0]-1) for p0 = 1:np]
    # prevdeps[p1][p0][v0] is the VersionSet of package p1 which package p0 version v0
    # depends upon
    prevdeps = [Dict{Int,Dict{Int,VersionSet}}() for p0 = 1:np]

    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            pdeps[p0][v0] = a.requires
            for (rp, rvs) in a.requires
                p1 = pdict[rp]
                if !haskey(prevdeps[p1], p0)
                    prevdeps[p1][p0] = Dict{Int,VersionSet}()
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
                if vn ∉ vs
                    # the dependency is violated because
                    # the other package version is invalid
                    viol = true
                    break
                end
            end
            viol && continue

            # check if bumping the version would violate some
            # dependency of another package
            for (p1,d) in prevdeps[p0]
                vs = get(d, sol[p1], nothing)
                vs === nothing && continue
                vn = pvers[p0][s0+1]
                if vn ∉ vs
                    # bumping the version would violate
                    # the dependency
                    viol = true
                    break
                end
            end
            viol && continue
            # So the solution is non-optimal: we bump it manually
            #@debug "Nonoptimal solution for package `$(interface.pkgs[p0])`: sol=$s0"
            sol[p0] += 1
            restart = true
        end
    end

    # Finally uninstall unneeded packages:
    # start from the required ones and keep only
    # the packages reachable from them along the graph
    uninst = trues(np)
    staged = Set{String}(keys(reqs))
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{String}()
        for p in staged
            p0 = pdict[p]
            uninst[p0] = false
            @assert sol[p0] < spp[p0]
            vn = pvers[p0][sol[p0]]
            a = deps[p][vn]

            # scan dependencies
            for (rp,rvs) in a.requires
                rp0 = pdict[rp]
                @assert sol[rp0] < spp[rp0] && pvers[rp0][sol[rp0]] ∈ rvs
                rp ∈ seen || push!(staged_next, rp)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    for p0 in findall(uninst)
        sol[p0] = spp[p0]
    end

    return
end

end
