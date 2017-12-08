# This file is a part of Julia. License is MIT: https://julialang.org/license

module Query

using ..Types
import Pkg3.equalto

function init_resolve_backtrace(uuid_to_name::Dict{UUID,String}, reqs::Requires, fix::Dict{UUID,Fixed} = Dict{UUID,Fixed}())
    bktrc = ResolveBacktrace(uuid_to_name)
    for (p,f) in fix
        bktrc[p] = ResolveBacktraceItem(:fixed, f.version)
    end
    for (p,vs) in reqs
        bktrcp = get!(bktrc, p) do; ResolveBacktraceItem() end
        push!(bktrcp, :required, vs)
    end
    return bktrc
end

function check_fixed(reqs::Requires, fix::Dict{UUID,Fixed}, deps::DepsGraph, uuid_to_name::Dict{UUID,String})
    id(p) = pkgID(p, uuid_to_name)
    for (p1,f1) in fix
        for p2 in keys(f1.requires)
            if !(haskey(deps, p2) || haskey(fix, p2))
                throw(PkgError("unknown package $(id(p1)) required by $(id(p2))"))
            end
        end
        if !satisfies(p1, f1.version, reqs)
            warn("$(id(p1)) is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])")
        end
        for (p2,f2) in fix
            if !satisfies(p1, f1.version, f2.requires)
                warn("$(id(p1)) is fixed at $(f1.version) conflicting with requirement for $(id(p2)): $(f2.requires[p1])")
            end
        end
    end
end

function propagate_fixed!(reqs::Requires, bktrc::ResolveBacktrace, fix::Dict{UUID,Fixed})
    for (p,f) in fix
        merge_requires!(reqs, f.requires)
        for (rp,rvs) in f.requires
            bktrc_rp = get!(bktrc, rp) do; ResolveBacktraceItem() end
            push!(bktrc_rp, p=>bktrc[p], rvs)
        end
    end
    for (p,f) in fix
        delete!(reqs, p)
    end
    reqs
end

# Specialized copy for the deps argument below because the deepcopy is slow
function depscopy(deps::DepsGraph)
    new_deps = similar(deps)
    for (p,depsp) in deps
        new_depsp = similar(depsp)
        for (vn,vdep) in depsp
            new_depsp[vn] = copy(vdep)
        end
        new_deps[p] = new_depsp
    end
    return new_deps
end

# Generate a reverse dependency graph (package names only)
function gen_backdeps(deps::DepsGraph)
    backdeps = Dict{UUID,Set{UUID}}()
    for (p,depsp) in deps, (vn,vdep) in depsp, rp in keys(vdep)
        s = get!(backdeps, rp) do; Set{UUID}() end
        push!(s, p)
    end
    return backdeps
end

function dependencies(deps::DepsGraph, fix::Dict = Dict{UUID,Fixed}(julia_UUID=>Fixed(VERSION)))
    deps = depscopy(deps)
    conflicts = Dict{UUID,Set{UUID}}()
    to_expunge = VersionNumber[]
    emptied = UUID[]
    backdeps = gen_backdeps(deps)

    for (fp,fx) in fix
        delete!(deps, fp)
        haskey(backdeps, fp) || continue
        for p in backdeps[fp]
            haskey(deps, p) || continue
            depsp = deps[p]
            empty!(to_expunge)
            for (vn,vdep) in depsp
                if satisfies(fp, fx.version, vdep)
                    delete!(vdep, fp)
                else
                    conflicts_p = get!(conflicts, p) do; Set{UUID}() end
                    push!(conflicts_p, fp)
                    # don't delete vn from depsp right away so as not to screw up iteration
                    push!(to_expunge, vn)
                end
            end
            for vn in to_expunge
                delete!(depsp, vn)
            end
            isempty(depsp) && push!(emptied, p)
        end
    end
    while !isempty(emptied)
        deleted_pkgs = UUID[]
        for p in emptied
            delete!(deps, p)
            push!(deleted_pkgs, p)
        end
        empty!(emptied)

        for dp in deleted_pkgs
            haskey(backdeps, dp) || continue
            for p in backdeps[dp]
                haskey(deps, p) || continue
                depsp = deps[p]
                empty!(to_expunge)
                for (vn,vdep) in depsp
                    haskey(vdep, dp) || continue
                    conflicts_p = get!(conflicts, p) do; Set{UUID}() end
                    union!(conflicts_p, conflicts[dp])
                    push!(to_expunge, vn)
                end
                for vn in to_expunge
                    delete!(depsp, vn)
                end
                isempty(depsp) && push!(emptied, p)
            end
        end
    end
    deps, conflicts
end

function check_requirements(reqs::Requires, deps::DepsGraph, fix::Dict{UUID,Fixed},
                            uuid_to_name::Dict{UUID,String})
    id(p) = pkgID(p, uuid_to_name)
    for (p,vs) in reqs
        any(vn->(vn ∈ vs), keys(deps[p])) && continue
        remaining_vs = VersionSpec()
        err_msg = "fixed packages introduce conflicting requirements for $(id(p)): \n"
        available_list = sort!(collect(keys(deps[p])))
        for (p1,f1) in fix
            f1r = f1.requires
            haskey(f1r, p) || continue
            err_msg *= "         $(id(p1)) requires versions $(f1r[p])"
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
        throw(PkgError(err_msg))
    end
end

# If there are explicitly required packages, dicards all versions outside
# the allowed range.
# It also propagates requirements: when all allowed versions of a required package
# require some other package, this creates a new implicit requirement.
# The propagation is tracked so that in case a contradiction is detected the error
# message allows to determine the cause.
# This is a pre-pruning step, so it also creates some structures which are later used by pruning
function filter_versions(reqs::Requires, deps::DepsGraph,
                         bktrc::ResolveBacktrace, uuid_to_name::Dict{UUID,String})
    id(p) = pkgID(p, uuid_to_name)
    allowed = Dict{UUID,Dict{VersionNumber,Bool}}()
    staged = copy(reqs)
    while !isempty(staged)
        staged_next = Requires()
        for (p,vs) in staged
            # Parse requirements and store allowed versions.
            depsp = deps[p]
            if !haskey(allowed, p)
                allowedp = Dict{VersionNumber,Bool}(vn=>true for vn in keys(depsp))
                allowed[p] = allowedp
                seen = false
            else
                allowedp = allowed[p]
                oldallowedp = copy(allowedp)
                seen = true
            end
            for vn in keys(depsp)
                allowedp[vn] &= vn ∈ vs
            end
            @assert !isempty(allowedp)
            if !any(values(allowedp))
                err_msg = "Unsatisfiable requirements detected for package $(id(p)):\n"
                err_msg *= sprint(showitem, bktrc, p)
                err_msg *= """The intersection of the requirements is $(bktrc[p].versionreq).
                              None of the available versions can satisfy this requirement."""
                throw(PkgError(err_msg))
            end

            # If we've seen this package already and nothing has changed since
            # the last time, we stop here.
            seen && allowedp == oldallowedp && continue

            # Propagate requirements:
            # if all allowed versions of a required package require some other package,
            # then compute the union of the allowed versions for that other package, and
            # treat that as a new requirement.
            # Start by filtering out the non-allowed versions
            fdepsp = Dict{VersionNumber,Requires}(vn=>depsp[vn] for vn in keys(depsp) if allowedp[vn])
            # Collect all required packages
            isreq = Dict{UUID,Bool}(rp=>true for vdep in values(fdepsp) for rp in keys(vdep))
            # Compute whether a required package appears in all requirements
            for rp in keys(isreq)
                isreq[rp] = all(haskey(vdep, rp) for vdep in values(fdepsp))
            end

            # Create a list of candidates for new implicit requirements
            staged_new = Set{UUID}()
            for vdep in values(fdepsp), (rp,rvs) in vdep
                # Skip packages that may not be required
                isreq[rp] || continue
                # Compute the union of the version sets
                if haskey(staged_next, rp)
                    snvs = staged_next[rp]
                    union!(snvs, rvs)
                else
                    snvs = copy(rvs)
                    staged_next[rp] = snvs
                end
                push!(staged_new, rp)
            end
            for rp in staged_new
                @assert isreq[rp]
                srvs = staged_next[rp]
                bktrcp = get!(bktrc, rp) do; ResolveBacktraceItem(); end
                push!(bktrcp, p=>bktrc[p], srvs)
                if isa(bktrcp.versionreq, VersionSpec) && isempty(bktrcp.versionreq)
                    err_msg = "Unsatisfiable requirements detected for package $(id(rp)):\n"
                    err_msg *= sprint(showitem, bktrc, rp)
                    err_msg *= "The intersection of the requirements is empty."
                    throw(PkgError(err_msg))
                end
            end
        end
        staged = staged_next
    end

    filtered_deps = DepsGraph()
    for (p,depsp) in deps
        filtered_deps[p] = Dict{VersionNumber,Requires}()
        allowedp = get(allowed, p) do; Dict{VersionNumber,Bool}() end
        fdepsp = filtered_deps[p]
        for (vn,vdep) in depsp
            get(allowedp, vn, true) || continue
            fdepsp[vn] = vdep
        end
    end

    return filtered_deps, allowed
end

# Reduce the number of versions by creating equivalence classes, and retaining
# only the highest version for each equivalence class.
# Two versions are equivalent if:
#   1) They appear together as dependecies of another package (i.e. for each
#      dependency relation, they are both required or both not required)
#   2) They have the same dependencies
# Preliminarily calls filter_versions.
function prune_versions(reqs::Requires, deps::DepsGraph, bktrc::ResolveBacktrace, uuid_to_name::Dict{UUID,String})
    filtered_deps, allowed = filter_versions(reqs, deps, bktrc, uuid_to_name)
    if !isempty(reqs)
        filtered_deps = dependencies_subset(filtered_deps, Set{UUID}(keys(reqs)))
    end

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = Dict{UUID,Dict{VersionNumber,BitVector}}()

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Dict{UUID,Set{VersionSpec}}()
    for (p,fdepsp) in filtered_deps
        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(values(fdepsp))

        # Store all dependencies seen so far for later use
        for r in uniqdepssets, (rp,rvs) in r
            get!(alldeps, rp) do; Set{VersionSpec}() end
            push!(alldeps[rp], rvs)
        end

        # If the package has just one version, it's uninteresting
        length(deps[p]) == 1 && continue

        # Grow the pattern by the number of classes
        luds = length(uniqdepssets)
        @assert !haskey(vmask, p)
        vmask[p] = Dict{VersionNumber,BitVector}()
        vmaskp = vmask[p]
        for vn in keys(fdepsp)
            vmaskp[vn] = falses(luds)
        end
        for (vn,vdep) in fdepsp
            vmind = findfirst(equalto(vdep), uniqdepssets)
            @assert vmind > 0
            vm = vmaskp[vn]
            vm[vmind] = true
        end
    end

    # Produce dependency patterns.
    for (p,vss) in alldeps, vs in vss
        # packages with just one version, or dependencies
        # which do not distiguish between versions, are not
        # interesting
        (length(deps[p]) == 1 || vs == VersionSpec()) && continue

        # Store the dependency info in the patterns
        @assert haskey(vmask, p)
        for (vn,vm) in vmask[p]
            push!(vm, vn in vs)
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers = Dict{UUID,Vector{VersionNumber}}()
    eq_classes = Dict{UUID,Dict{VersionNumber,Vector{VersionNumber}}}()
    for (p,vmaskp) in vmask
        vmask0_uniq = unique(values(vmaskp))
        nc = length(vmask0_uniq)
        classes = [VersionNumber[] for c0 = 1:nc]
        for (vn,vm) in vmaskp
            c0 = findfirst(equalto(vm), vmask0_uniq)
            push!(classes[c0], vn)
        end
        map(sort!, classes)

        # For each nonempty class, we store only the highest version)
        pruned_vers[p] = VersionNumber[]
        prunedp = pruned_vers[p]
        eq_classes[p] = Dict{VersionNumber,Vector{VersionNumber}}()
        eqclassp = eq_classes[p]
        for cl in classes
            isempty(cl) && continue
            vtop = maximum(cl)
            push!(prunedp, vtop)
            @assert !haskey(eqclassp, vtop)
            eqclassp[vtop] = cl
        end
        sort!(prunedp)
    end
    # Put non-allowed versions into eq_classes
    for (p,allowedp) in allowed
        haskey(eq_classes, p) || continue
        eqclassp = eq_classes[p]
        for (vn,a) in allowedp
            a && continue
            eqclassp[vn] = [vn]
        end
    end
    # Put all remaining packages into eq_classes
    for (p,depsp) in deps
        haskey(eq_classes, p) && continue
        eq_classes[p] = Dict{VersionNumber,Vector{VersionNumber}}()
        eqclassp = eq_classes[p]
        for vn in keys(depsp)
            eqclassp[vn] = [vn]
        end
    end


    # Recompute deps. We could simplify them, but it's not worth it
    new_deps = DepsGraph()

    for (p,depsp) in filtered_deps
        @assert !haskey(new_deps, p)
        if !haskey(pruned_vers, p)
            new_deps[p] = depsp
            continue
        end
        new_deps[p] = Dict{VersionNumber,Requires}()
        pruned_versp = pruned_vers[p]
        for (vn,vdep) in depsp
            vn ∈ pruned_versp || continue
            new_deps[p][vn] = vdep
        end
    end

    #println("pruning stats:")
    #numvers = 0
    #numdeps = 0
    #for (p,d) in deps, (vn,vdep) in d
    #    numvers += 1
    #    for r in vdep
    #        numdeps += 1
    #    end
    #end
    #numnewvers = 0
    #numnewdeps = 0
    #for (p,d) in new_deps, (vn,vdep) in d
    #    numnewvers += 1
    #    for r in vdep
    #        numnewdeps += 1
    #    end
    #end
    #println("  before: vers=$numvers deps=$numdeps")
    #println("  after: vers=$numnewvers deps=$numnewdeps")
    #println()

    return new_deps, eq_classes
end
prune_versions(deps::DepsGraph, uuid_to_name::Dict{UUID,String}) =
    prune_versions(Requires(), deps, ResolveBacktrace(uuid_to_name), uuid_to_name)

# Build a graph restricted to a subset of the packages
function subdeps(deps::DepsGraph, pkgs::Set{UUID})
    sub_deps = DepsGraph()
    for p in pkgs
        haskey(sub_deps, p) || (sub_deps[p] = Dict{VersionNumber,Requires}())
        sub_depsp = sub_deps[p]
        for (vn,vdep) in deps[p]
            sub_depsp[vn] = vdep
        end
    end
    return sub_deps
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package set
function dependencies_subset(deps::DepsGraph, pkgs::Set{UUID})
    staged::Set{UUID} = filter(p->p ∈ keys(deps), pkgs)
    allpkgs = copy(staged)
    while !isempty(staged)
        staged_next = Set{UUID}()
        for p in staged, vdep in values(get(deps, p, Dict{VersionNumber,Requires}())), rp in keys(vdep)
            rp ∉ allpkgs && rp ≠ julia_UUID && push!(staged_next, rp)
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

# Build a subgraph incuding only the (direct and indirect) dependencies and dependants
# of a given package set
function undirected_dependencies_subset(deps::DepsGraph, pkgs::Set{UUID})
    graph = Dict{UUID,Set{UUID}}()

    for (p,d) in deps
        haskey(graph, p) || (graph[p] = Set{UUID}())
        for vdep in values(d), rp in keys(vdep)
            push!(graph[p], rp)
            haskey(graph, rp) || (graph[rp] = Set{UUID}())
            push!(graph[rp], p)
        end
    end

    staged = pkgs
    allpkgs = copy(pkgs)
    while !isempty(staged)
        staged_next = Set{UUID}()
        for p in staged, rp in graph[p]
            rp ∉ allpkgs && push!(staged_next, rp)
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

function prune_dependencies(reqs::Requires, deps::DepsGraph, uuid_to_name::Dict{UUID,String},
                            bktrc::ResolveBacktrace = init_resolve_backtrace(uuid_to_name, reqs))
    deps, _ = prune_versions(reqs, deps, bktrc, uuid_to_name)
    return deps
end

end # module
