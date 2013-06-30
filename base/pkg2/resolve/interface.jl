module PkgToMaxSumInterface

using ...Types

export MetadataError, Interface,
       prune_versions!, compute_output_dict,
       verify_sol, enforce_optimality, reduce_interface

# Error type used to signal that there was some
# problem with the metadata info passed to resolve
type MetadataError <: Exception
    info
end

# The numeric type used to determine how the different
# versions of a package should be weighed
typealias VersionWeight Int

# A collection of objects which allow interfacing external (Pkg) and
# internal (MaxSum) representation, and doing some manipulation
# (i.e. version pruning, reduction)
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

    # has version pruning been performed?
    waspruned::Bool

    function Interface(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

        pkgs_set = Set{ByteString}()
        for (p,_) in deps add!(pkgs_set, p) end

        for (p,_) in reqs
            if !contains(pkgs_set, p)
                throw(MetadataError("required package $p has no version compatible with fixed requirements"))
            end
        end

        pkgs = ByteString[pkgs_set...]
        np = length(pkgs)

        pdict = (ByteString=>Int)[ pkgs[i] => i for i = 1:np ]

        spp, pvers = gen_pvers(np, pdict, deps)
        vdict = gen_vdict(pdict, pvers, deps)

        # the version weights are just progressive integer numbers,
        # there is no difference between major, minor, patch etc.
        # TODO: change this to weigh differently major, minor etc. ?
        vweight = [ [ v0-1 for v0 = 1:spp[p0] ] for p0 = 1:np ]

        return new(reqs, deps, pkgs, np, spp, pdict, pvers, vdict, vweight, false)
    end
end

# Generate the pvers field in Interface; used by the
# constructor and within `prune_versions!`
function gen_pvers(np::Int, pdict::Dict{ByteString,Int}, deps::Dict{ByteString,Dict{VersionNumber,Available}})
    spp = ones(Int, np)

    pvers = [ VersionNumber[] for i = 1:np ]

    for (p,d) in deps, (vn,_) in d
        p0 = pdict[p]
        spp[p0] += 1
        push!(pvers[p0], vn)
    end
    for p0 = 1:np
        sort!(pvers[p0])
    end

    return spp, pvers
end

# Generate the vdict field in Interface; used by the
# constructor and within `prune_versions!`
function gen_vdict(pdict::Dict{ByteString,Int}, pvers::Vector{Vector{VersionNumber}},
                   deps::Dict{ByteString,Dict{VersionNumber,Available}})

    np = length(pvers)
    vdict = [(VersionNumber=>Int)[] for p0 = 1:np]
    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,_) in d
            for v0 in 1:length(pvers[p0])
                if pvers[p0][v0] == vn
                    vdict0[vn] = v0
                    break
                end
            end
        end
    end
    return vdict
end

# Reduce the number of versions by creating equivalence classes, and retaining
# only the highest version for each equivalence class.
# Two versions are equivalent if:
#   1) They appear together as dependecies of another package (i.e. for each
#      dependency relation, they are both required or both not required)
#   2) They have the same dependencies
# Also, for each package explicitly required, dicards all versions outside
# the allowed range (checking for impossible ranges while at it).
# This function mutates interface.
function prune_versions!(interface::Interface, prune_reqs::Bool = true)

    np = interface.np
    reqs = interface.reqs
    pkgs = interface.pkgs
    deps = interface.deps
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    vdict = interface.vdict
    vweight = interface.vweight

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = [ [ BitVector(0) for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]

    # From the point of view of resolve(), VersionInterval() and
    # VersionInterval(v0) are equivelent if v0 is the first
    # available version
    function contains_any(vs::VersionSet, first::VersionNumber)
        all(i->(isempty(i) || (i.lower == first && i.upper == typemax(VersionNumber))), vs.intervals)
    end

    # Parse requirements and store allowed versions.
    allowed = [ trues(spp[p0]-1) for p0 = 1:np ]
    if prune_reqs
        for (p,vs) in reqs
            p0 = pdict[p]
            pvers0 = pvers[p0]
            allowed0 = allowed[p0]
            for v0 = 1:spp[p0]-1
                vn = pvers0[v0]
                allowed0[v0] = contains(vs, vn)
            end
        end
        for p0 = 1:np
            allowed0 = allowed[p0]
            if !any(allowed0)
                error("Invalid requirements: no version allowed for package $(pkgs[p0])")
            end
        end
    end

    # Parse the dependency list, segregate them according to the
    # dependant package and version
    pdeps = [ [ Array((ByteString,VersionSet),0) for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]
    for (p,d) in deps
        p0 = pdict[p]
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            if !allowed[p0][v0]
                continue
            end
            for (rp, rvs) in a.requires
                p1 = pdict[rp]
                if contains_any(rvs, pvers[p1][1])
                    rvs = VersionSet()
                end
                push!(pdeps[p0][v0], (rp,rvs))
            end
        end
    end

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Array((ByteString,VersionSet),0)
    for p0 = 1:np
        pdeps0 = pdeps[p0]

        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(pdeps0)

        # Store all dependencies seen so far for later use
        for dd in uniqdepssets, v in dd
            push!(alldeps, v)
        end

        # If the package has just one version, it's uninteresting
        if spp[p0] == 2
            continue
        end

        # Grow the pattern by the number of classes
        ff = falses(length(uniqdepssets))
        vmask0 = vmask[p0]
        vmind_base = length(vmask0[1])
        for vm in vmask0
            append!(vm, ff)
        end

        # For each version, determine to which class it belongs and
        # store that info in the patterns
        for v0 = 1:spp[p0]-1
            if !allowed[p0][v0]
                continue
            end
            vmind = findfirst(uniqdepssets, pdeps0[v0])
            @assert vmind >= 0
            vmind += vmind_base
            vm = vmask0[v0]
            vm[vmind] = true
        end
    end

    # Produce dependency patterns. Ideally, one would use unique(alldeps)
    # here (or better still, use a Set from the beginning), but it takes
    # more time than it saves - better waste a little extra memory.
    for (p,vs) in alldeps
        p0 = pdict[p]

        # packages with just one version, or dependencies
        # which do not distiguish between versions, are not
        # interesting
        if spp[p0] == 2 || contains_any(vs, pvers[p0][1])
            continue
        end

        pvers0 = pvers[p0]

        # Grow the patterns by one bit
        vmask0 = vmask[p0]
        for vm in vmask0
            resize!(vm, length(vm)+1)
        end

        # Store the dependency info in the patterns
        for v0 = 1:spp[p0]-1
            v = pvers0[v0]
            vm = vmask0[v0]
            vm[end] = contains(vs, v)
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers_id = [ Int[] for p0 = 1:np ]
    eq_classes_map = [ (VersionNumber=>VersionNumber)[] for p0 = 1:np ]
    for p0 = 1:np
        vmask0 = vmask[p0]
        vmask0_uniq = unique(vmask0)
        nc = length(vmask0_uniq)
        classes = [ Int[] for c0 = 1:nc ]
        for v0 = 1:spp[p0]-1
            if !allowed[p0][v0]
                continue
            end
            vm = vmask0[v0]
            c0 = findfirst(vmask0_uniq, vm)
            push!(classes[c0], v0)
        end

        # For each nonempty class, we store only the last entry (i.e. the
        # highest version)
        pruned0 = pruned_vers_id[p0]
        eqclass0 = eq_classes_map[p0]
        pvers0 = pvers[p0]
        for cl in classes
            if !isempty(cl)
                vtop0 = cl[end]
                push!(pruned0, vtop0)
                if !prune_reqs
                    vtop = pvers0[vtop0]
                    for v0 in cl
                        eqclass0[pvers0[v0]] = vtop
                    end
                end
            end
        end
        sort!(pruned0)
    end

    # All that follows is just recomputing the structures' fields
    # by throwing away unnecessary versions

    # Recompute pvers
    new_pvers = [ pvers[p0][pruned_vers_id[p0]] for p0 = 1:np ]

    # Reompute deps. We could simplify them, but it's not worth it
    new_deps = Dict{ByteString,Dict{VersionNumber,Available}}()

    for (p,d) in deps
        p0 = pdict[p]
        @assert !haskey(new_deps, p)
        new_deps[p] = Dict{VersionNumber,Available}()
        vdict0 = vdict[p0]
        for (vn,a) in d
            v0 = vdict0[vn]
            if !contains(pruned_vers_id[p0], v0)
                continue
            end
            new_deps[p][vn] = a
        end
    end

    interface.deps = new_deps

    # Finally, mutate remaining interface fields by regenerating pvers, vdict
    # and vweights

    new_spp, new_pvers = gen_pvers(np, pdict, new_deps)
    new_vdict = gen_vdict(pdict, new_pvers, new_deps)

    new_vweight = [ Array(VersionWeight,new_spp[p0]) for p0 = 1:np ]
    for p0 = 1:np
        vweight0 = vweight[p0]
        new_vweight0 = new_vweight[p0]
        pversid0 = pruned_vers_id[p0]
        for v0 = 1:length(pversid0)
            new_vweight0[v0] = vweight0[pversid0[v0]]
        end
        new_vweight0[end] = vweight0[end]
    end

    interface.spp = new_spp
    interface.pvers = new_pvers
    interface.vdict = new_vdict
    interface.vweight = new_vweight
    if prune_reqs
        interface.waspruned = true
    end

    #println("pruning stats:")
    #numvers = 0
    #numdeps = 0
    #for (p,d) in deps, (vn,a) in d
    #    numvers += 1
    #    for r in a.requires
    #        numdeps += 1
    #    end
    #end
    #numnewvers = 0
    #numnewdeps = 0
    #for (p,d) in new_deps, (vn,a) in d
    #    numnewvers += 1
    #    for r in a.requires
    #        numnewdeps += 1
    #    end
    #end
    #println("  before: vers=$numvers deps=$numdeps")
    #println("  after: vers=$numnewvers deps=$numnewdeps")

    return eq_classes_map
end

# The output format is a dict which associates sha1's to each installed package name
function compute_output_dict(interface::Interface, sol::Vector{Int})

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
function verify_sol(interface::Interface, sol::Vector{Int})

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
function enforce_optimality(interface::Interface, sol::Vector{Int})
    np = interface.np

    reqs = interface.reqs
    deps = interface.deps
    spp = interface.spp
    pdict = interface.pdict
    pvers = interface.pvers
    vdict = interface.vdict
    waspruned = interface.waspruned

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
            if !waspruned
                # check if bumping would violate a requirement
                for (p,vs) in reqs
                    if p0 != pdict[p]
                        continue
                    end
                    vn = pvers[p0][s0+1]
                    if !contains(vs, vn)
                        viol = true
                        break
                    end
                end
                if viol
                    continue
                end
            end
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
            #println(STDERR, "Warning: nonoptimal solution for package $(interface.pkgs[p0]): sol=$s0")
            sol[p0] += 1
            restart = true
        end
    end
    return
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package (used in sanity_check)
function reduce_interface(interface0::Interface, pdeps::Vector, p::ByteString, vn::VersionNumber)

    pkgs = interface0.pkgs
    deps = interface0.deps
    np = interface0.np
    spp = interface0.spp
    pdict = interface0.pdict
    pvers = interface0.pvers
    vdict = interface0.vdict

    nvn = VersionNumber(vn.major, vn.minor, vn.patch + 1, vn.prerelease, vn.build)

    reqs = (ByteString=>VersionSet)[p=>VersionSet([vn, nvn])]

    p0 = pdict[p]
    staged = IntSet(p0)
    pset = IntSet(p0)
    while !isempty(staged)
        staged_next = IntSet()
        for p0 in staged
            for av in pdeps[p0], a in av, (rp,_) in a.requires
                p1 = pdict[rp]
                if !contains(pset, p1)
                    add!(staged_next, p1)
                end
            end
        end
        pset = union(pset, staged_next)
        staged = staged_next
    end

    red_deps = Dict{ByteString,Dict{VersionNumber,Available}}()
    for p0 in pset
        pdeps0 = pdeps[p0]
        pvers0 = pvers[p0]
        p = pkgs[p0]
        for v0 = 1:spp[p0]-1
            vn = pvers0[v0]
            for a in pdeps0[v0]
                if !haskey(red_deps, p)
                    red_deps[p] = Dict{VersionNumber,Available}()
                end
                red_deps[p][vn] = a
            end
        end
    end

    return Interface(reqs, red_deps)
end

end
