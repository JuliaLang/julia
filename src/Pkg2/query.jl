# This file is a part of Julia. License is MIT: https://julialang.org/license

module Query

import ..PkgError
using ..Types

# If there are explicitly required packages, dicards all versions outside
# the allowed range.
# It also propagates requirements: when all allowed versions of a required package
# require some other package, this creates a new implicit requirement.
# The propagation is tracked so that in case a contradiction is detected the error
# message allows to determine the cause.
# This is a pre-pruning step, so it also creates some structures which are later used by pruning
function filter_versions(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}},
                         bktrc::ResolveBacktrace, uuid_to_name::Dict{String, String})
    allowed = Dict{String,Dict{VersionNumber,Bool}}()
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
                name = haskey(uuid_to_name, p) ? uuid_to_name[p] : "UNKNOWN"
                uuid_short = p[1:8]
                err_msg = "Unsatisfiable requirements detected for package $name [$uuid_short]:\n"
                err_msg *= string(bktrc[p])
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
            fdepsp = Dict{VersionNumber,Available}(vn=>depsp[vn] for vn in keys(depsp) if allowedp[vn])
            # Collect all required packages
            isreq = Dict{String,Bool}(rp=>true for a in values(fdepsp) for rp in keys(a.requires))
            # Compute whether a required package appears in all requirements
            for a in values(fdepsp), rp in keys(isreq)
                haskey(a.requires, rp) || (isreq[rp] = false)
            end
            staged_new = Set{String}()
            for a in values(fdepsp)
                for (rp,rvs) in a.requires
                    # Skip packages that may not be required
                    isreq[rp] || continue
                    # Compute the union of the version sets
                    snvs = get!(staged_next, rp, copy(rvs))
                    union!(snvs, rvs)
                    push!(staged_new, rp)
                end
            end
            for rp in staged_new
                srvs = staged_next[rp]
                isreq[rp] || continue
                bktrcp = get!(bktrc, rp) do; ResolveBacktraceItem(); end
                push!(bktrcp, p=>bktrc[p], srvs)
                if isa(bktrcp.versionreq, VersionSet) && isempty(bktrcp.versionreq)
                    name = haskey(uuid_to_name, rp) ? uuid_to_name[rp] : "UNKNOWN"
                    uuid_short = rp[1:8]
                    err_msg = "Unsatisfiable requirements detected for package $name [$uuid_short]:\n"
                    err_msg *= string(bktrcp)
                    err_msg *= "The intersection of the requirements is empty."
                    throw(PkgError(err_msg))
                end
            end
        end
        staged = staged_next
    end

    filtered_deps = Dict{String,Dict{VersionNumber,Available}}()
    for (p,depsp) in deps
        filtered_deps[p] = Dict{VersionNumber,Available}()
        allowedp = get(allowed, p, Dict{VersionNumber,Bool}())
        fdepsp = filtered_deps[p]
        for (vn,a) in depsp
            get(allowedp, vn, true) || continue
            fdepsp[vn] = a
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
function prune_versions(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, bktrc::ResolveBacktrace, uuid_to_name::Dict{String, String})
    filtered_deps, allowed = filter_versions(reqs, deps, bktrc, uuid_to_name)

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = Dict{String,Dict{VersionNumber, BitVector}}()

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Dict{String,Set{VersionSet}}()
    for (p,fdepsp) in filtered_deps
        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(a.requires for a in values(fdepsp))

        # Store all dependencies seen so far for later use
        for r in uniqdepssets, (rp,rvs) in r
            get!(alldeps, rp) do; Set{VersionSet}() end
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
        for (vn,a) in fdepsp
            vmind = findfirst(uniqdepssets, a.requires)
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
        (length(deps[p]) == 1 || vs == VersionSet()) && continue

        # Store the dependency info in the patterns
        @assert haskey(vmask, p)
        for (vn,vm) in vmask[p]
            push!(vm, vn in vs)
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers = Dict{String,Vector{VersionNumber}}()
    eq_classes = Dict{String,Dict{VersionNumber,Vector{VersionNumber}}}()
    for (p, vmaskp) in vmask
        vmask0_uniq = unique(values(vmaskp))
        nc = length(vmask0_uniq)
        classes = [VersionNumber[] for c0 = 1:nc]
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
    new_deps = Dict{String,Dict{VersionNumber,Available}}()

    for (p,depsp) in deps
        @assert !haskey(new_deps, p)
        if !haskey(pruned_vers, p)
            new_deps[p] = depsp
            continue
        end
        new_deps[p] = Dict{VersionNumber,Available}()
        pruned_versp = pruned_vers[p]
        for (vn,a) in depsp
            vn ∈ pruned_versp || continue
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
prune_versions(env, deps::Dict{String,Dict{VersionNumber,Available}}) =
    prune_versions(env, Dict{String,VersionSet}(), deps, ResolveBacktrace())
prune_versions(env, deps::Dict{String,Dict{VersionNumber,Available}}, bktrc::ResolveBacktrace) =
    prune_versions(env, Dict{String,VersionSet}(), deps, bktrc)

# Build a graph restricted to a subset of the packages
function subdeps(deps::Dict{String,Dict{VersionNumber,Available}}, pkgs::Set{String})
    sub_deps = Dict{String,Dict{VersionNumber,Available}}()
    for p in pkgs
        haskey(sub_deps, p) || (sub_deps[p] = Dict{VersionNumber,Available}())
        sub_depsp = sub_deps[p]
        for (vn,a) in deps[p]
            sub_depsp[vn] = a
        end
    end

    return sub_deps
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package set
function dependencies_subset(deps::Dict{String,Dict{VersionNumber,Available}}, pkgs::Set{String})
    staged::Set{String} = filter(p->p in keys(deps), pkgs)
    allpkgs = copy(staged)
    while !isempty(staged)
        staged_next = Set{String}()
        for p in staged, a in values(get(deps, p, Dict{VersionNumber,Available}())), rp in keys(a.requires)
            rp ∉ allpkgs && rp ≠ "julia" && push!(staged_next, rp)
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

function prune_dependencies(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, uuid_to_name::Dict{String, String})
    bktrc = ResolveBacktrace()
    for (p,vs) in reqs
        bktrc[p] = ResolveBacktraceItem(:required, vs)
    end
    return prune_dependencies(reqs, deps, bktrc, uuid_to_name)
end

function prune_dependencies(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}},
                            bktrc::ResolveBacktrace, uuid_to_name::Dict{String, String})
    deps = dependencies_subset(deps, Set{String}(keys(reqs)))
    deps, _ = prune_versions(reqs, deps, bktrc, uuid_to_name)

    return deps
end

end # module
