# This file is a part of Julia. License is MIT: http://julialang.org/license

module Query

import ...Pkg
using ..Types

function requirements(reqs::Dict, fix::Dict, avail::Dict)
    for (p1,f1) in fix
        for p2 in keys(f1.requires)
            haskey(avail, p2) || haskey(fix, p2) || error("unknown package $p2 required by $p1")
        end
        satisfies(p1, f1.version, reqs) ||
            warn("$p1 is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])")
        for (p2,f2) in fix
            satisfies(p1, f1.version, f2.requires) ||
                warn("$p1 is fixed at $(f1.version) conflicting with requirement for $p2: $(f2.requires[p1])")
        end
    end
    reqs = deepcopy(reqs)
    for (p1,f1) in fix
        merge_requires!(reqs, f1.requires)
    end
    for (p,f) in fix
        delete!(reqs, p)
    end
    reqs
end

function dependencies(avail::Dict, fix::Dict = Dict{UTF8String,Fixed}("julia"=>Fixed(VERSION)))
    avail = deepcopy(avail)
    conflicts = Dict{UTF8String,Set{UTF8String}}()
    for (fp,fx) in fix
        delete!(avail, fp)
        for (ap,av) in avail, (v,a) in copy(av)
            if satisfies(fp, fx.version, a.requires)
                delete!(a.requires, fp)
            else
                haskey(conflicts, ap) || (conflicts[ap] = Set{UTF8String}())
                push!(conflicts[ap], fp)
                delete!(av, v)
            end
        end
    end
    again = true
    while again
        again = false
        deleted_pkgs = UTF8String[]
        for (ap,av) in avail
            if isempty(av)
                delete!(avail, ap)
                push!(deleted_pkgs, ap)
                again = true
            end
        end
        for dp in deleted_pkgs
            for (ap,av) in avail, (v,a) in copy(av)
                if haskey(a.requires, dp)
                    haskey(conflicts, ap) || (conflicts[ap] = Set{UTF8String}())
                    union!(conflicts[ap], conflicts[dp])
                    delete!(av, v)
                end
            end
        end
    end
    avail, conflicts
end

typealias PackageState Union{Void,VersionNumber}

function diff(have::Dict, want::Dict, avail::Dict, fixed::Dict)
    change = Array(Tuple{UTF8String,Tuple{PackageState,PackageState}},0)
    remove = Array(Tuple{UTF8String,Tuple{PackageState,PackageState}},0)

    for pkg in collect(union(keys(have),keys(want)))
        h, w = haskey(have,pkg), haskey(want,pkg)
        if h && w
            if have[pkg] != want[pkg]
                push!(change, (pkg,(have[pkg], want[pkg])))
            end
        elseif h
            push!(remove, (pkg,(have[pkg],nothing)))
        elseif w
            push!(change, (pkg,(nothing,want[pkg])))
        end
    end
    append!(sort!(change), sort!(remove))
end

function check_requirements{T<:ByteString}(reqs::Requires, deps::Dict{T,Dict{VersionNumber,Available}}, fix::Dict)
    for (p,vs) in reqs
        if !any(vn->(vn in vs), keys(deps[p]))
            remaining_vs = VersionSet()
            err_msg = "fixed packages introduce conflicting requirements for $p: \n"
            available_list = sort(collect(keys(deps[p])))
            for (p1,f1) in fix
                f1r = f1.requires
                haskey(f1r, p) || continue
                err_msg *= "         $p1 requires versions $(f1r[p])"
                if !any([vn in f1r[p] for vn in available_list])
                    err_msg *= " [none of the available versions can satisfy this requirement]"
                end
                err_msg *= "\n"
                remaining_vs = intersect(remaining_vs, f1r[p])
            end
            if isempty(remaining_vs)
                err_msg *= "       the requirements are unsatisfiable because their intersection is empty"
            else
                err_msg *= "       available versions are $(join(available_list, ", ", " and "))"
            end
            error(err_msg)
        end
    end
end

# If there are explicitly required packages, dicards all versions outside
# the allowed range (checking for impossible ranges while at it).
# This is a pre-pruning step, so it also creates some structures which are later used by pruning
function filter_versions{T<:ByteString}(reqs::Requires, deps::Dict{T,Dict{VersionNumber,Available}})

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = Dict{UTF8String,Dict{VersionNumber, BitVector}}()

    # Parse requirements and store allowed versions.
    allowed = Dict{UTF8String,Dict{VersionNumber, Bool}}()
    for (p,vs) in reqs
        @assert !haskey(allowed, p)
        allowed[p] = Dict{VersionNumber,Bool}()
        allowedp = allowed[p]
        for vn in keys(deps[p])
            allowedp[vn] = vn in vs
        end
        @assert !isempty(allowedp)
        @assert any(values(allowedp))
    end

    filtered_deps = Dict{UTF8String,Dict{VersionNumber,Available}}()
    for (p,depsp) in deps
        filtered_deps[p] = Dict{VersionNumber,Available}()
        allowedp = get(allowed, p, Dict{VersionNumber,Bool}())
        fdepsp = filtered_deps[p]
        for (vn,a) in depsp
            if !isempty(allowedp) && !allowedp[vn]
                continue
            end
            fdepsp[vn] = a
        end
    end

    return filtered_deps, allowed, vmask
end

# Reduce the number of versions by creating equivalence classes, and retaining
# only the highest version for each equivalence class.
# Two versions are equivalent if:
#   1) They appear together as dependecies of another package (i.e. for each
#      dependency relation, they are both required or both not required)
#   2) They have the same dependencies
# Preliminarily calls filter_versions.
function prune_versions{T<:ByteString}(reqs::Requires, deps::Dict{T,Dict{VersionNumber,Available}})

    filtered_deps, allowed, vmask = filter_versions(reqs, deps)

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Dict{UTF8String,Set{VersionSet}}()
    for (p, fdepsp) in filtered_deps

        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(values(fdepsp))

        # Store all dependencies seen so far for later use
        for a in uniqdepssets, (rp,rvs) in a.requires
            haskey(alldeps, rp) || (alldeps[rp] = Set{VersionSet}())
            push!(alldeps[rp], rvs)
        end

        # If the package has just one version, it's uninteresting
        if length(deps[p]) == 1
            continue
        end

        # Grow the pattern by the number of classes
        luds = length(uniqdepssets)
        @assert !haskey(vmask, p)
        vmask[p] = Dict{VersionNumber,BitVector}()
        vmaskp = vmask[p]
        for vn in keys(fdepsp)
            vmaskp[vn] = falses(luds)
        end
        for (vn,a) in fdepsp
            vmind = findfirst(uniqdepssets, a)
            @assert vmind >= 0
            vm = vmaskp[vn]
            vm[vmind] = true
        end
    end

    # Produce dependency patterns.
    for (p,vss) in alldeps, vs in vss
        # packages with just one version, or dependencies
        # which do not distiguish between versions, are not
        # interesting
        if length(deps[p]) == 1 || vs == VersionSet()
            continue
        end

        # Store the dependency info in the patterns
        @assert haskey(vmask, p)
        vmaskp = vmask[p]
        for (vn,vm) in vmaskp
            push!(vm, in(vn, vs))
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers = Dict{UTF8String,Vector{VersionNumber}}()
    eq_classes = Dict{UTF8String,Dict{VersionNumber,Vector{VersionNumber}}}()
    for (p, vmaskp) in vmask
        vmask0_uniq = unique(values(vmaskp))
        nc = length(vmask0_uniq)
        classes = [ VersionNumber[] for c0 = 1:nc ]
        for (vn,vm) in vmaskp
            c0 = findfirst(vmask0_uniq, vm)
            push!(classes[c0], vn)
        end
        map(sort!, classes)

        # For each nonempty class, we store only the highest version)
        pruned_vers[p] = VersionNumber[]
        prunedp = pruned_vers[p]
        eq_classes[p] = Dict{VersionNumber,Vector{VersionNumber}}()
        eqclassp = eq_classes[p]
        for cl in classes
            if !isempty(cl)
                vtop = maximum(cl)
                push!(prunedp, vtop)
                @assert !haskey(eqclassp, vtop)
                eqclassp[vtop] = cl
            end
        end
        sort!(prunedp)
    end
    # Put non-allowed versions into eq_classes
    for (p, allowedp) in allowed
        haskey(eq_classes, p) || continue
        eqclassp = eq_classes[p]
        for (vn, a) in allowedp
            a && continue
            eqclassp[vn] = [vn]
        end
    end
    # Put all remaining packages into eq_classes
    for (p, depsp) in deps
        haskey(eq_classes, p) && continue
        eq_classes[p] = Dict{VersionNumber,Vector{VersionNumber}}()
        eqclassp = eq_classes[p]
        for vn in keys(depsp)
            eqclassp[vn] = [vn]
        end
    end


    # Recompute deps. We could simplify them, but it's not worth it
    new_deps = Dict{UTF8String,Dict{VersionNumber,Available}}()

    for (p,depsp) in deps
        @assert !haskey(new_deps, p)
        if !haskey(pruned_vers, p)
            new_deps[p] = depsp
            continue
        end
        new_deps[p] = Dict{VersionNumber,Available}()
        pruned_versp = pruned_vers[p]
        for (vn,a) in depsp
            in(vn, pruned_versp) || continue
            new_deps[p][vn] = a
        end
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
    #println()

    return new_deps, eq_classes
end
prune_versions{T<:ByteString}(deps::Dict{T,Dict{VersionNumber,Available}}) =
    prune_versions(Dict{UTF8String,VersionSet}(), deps)

# Build a graph restricted to a subset of the packages
function subdeps{T<:ByteString}(deps::Dict{T,Dict{VersionNumber,Available}}, pkgs::Set{T})

    sub_deps = Dict{UTF8String,Dict{VersionNumber,Available}}()
    for p in pkgs
        haskey(sub_deps, p) || (sub_deps[p] = Dict{VersionNumber,Available}())
        sub_depsp = sub_deps[p]
        for (vn, a) in deps[p]
            sub_depsp[vn] = a
        end
    end

    return sub_deps
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package set
function dependencies_subset{T<:ByteString}(deps::Dict{T,Dict{VersionNumber,Available}}, pkgs::Set{T})

    staged = pkgs
    allpkgs = copy(pkgs)
    while !isempty(staged)
        staged_next = Set{UTF8String}()
        for p in staged, a in values(deps[p]), rp in keys(a.requires)
            if !(rp in allpkgs)
                push!(staged_next, rp)
            end
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

# Build a subgraph incuding only the (direct and indirect) dependencies and dependants
# of a given package set
function undirected_dependencies_subset{T<:ByteString}(deps::Dict{T,Dict{VersionNumber,Available}}, pkgs::Set{T})

    graph = Dict{UTF8String, Set{UTF8String}}()

    for (p,d) in deps
        haskey(graph, p) || (graph[p] = Set{UTF8String}())
        for a in values(d), rp in keys(a.requires)
            push!(graph[p], rp)
            haskey(graph, rp) || (graph[rp] = Set{UTF8String}())
            push!(graph[rp], p)
        end
    end

    staged = pkgs
    allpkgs = copy(pkgs)
    while !isempty(staged)
        staged_next = Set{UTF8String}()
        for p in staged, rp in graph[p]
            if !(rp in allpkgs)
                push!(staged_next, rp)
            end
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

function filter_dependencies{T<:ByteString}(reqs::Requires, deps::Dict{T,Dict{VersionNumber,Available}})
    deps = dependencies_subset(deps, Set{UTF8String}(keys(reqs)))
    deps, _, _ = filter_versions(reqs, deps)

    return deps
end

function prune_dependencies{T<:ByteString}(reqs::Requires, deps::Dict{T,Dict{VersionNumber,Available}})
    deps = dependencies_subset(deps, Set{UTF8String}(keys(reqs)))
    deps, _ = prune_versions(reqs, deps)

    return deps
end

end # module
