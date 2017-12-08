# This file is a part of Julia. License is MIT: https://julialang.org/license

module Query

import ..PkgError
using ...Types
using ...Types.Pkg2Types
import Pkg3.equalto

function init_resolve_backtrace(reqs::Requires, fix::Dict{String,Fixed} = Dict{String,Fixed}())
    bktrc = ResolveBacktrace()
    for (p,f) in fix
        bktrc[p] = ResolveBacktraceItem(:fixed, f.version)
    end
    for (p,vs) in reqs
        bktrcp = get!(bktrc, p) do; ResolveBacktraceItem() end
        push!(bktrcp, :required, vs)
    end
    return bktrc
end

function check_fixed(reqs::Requires, fix::Dict{String,Fixed}, avail::Dict, uuid_to_name::Dict{String, String})
    for (p1,f1) in fix
        for p2 in keys(f1.requires)
            if !(haskey(avail, p2) || haskey(fix, p2))
                name1 = haskey(uuid_to_name, p1) ? uuid_to_name[p1] : "UNKNOWN"
                uuid_short1 = p1[1:8]
                name2 = haskey(uuid_to_name, p2) ? uuid_to_name[p2] : "UNKNOWN"
                uuid_short2 = p2[1:8]
                throw(PkgError("unknown package $name2 [$uuid_short2] required by $name1 [$uuid_short1]"))
            end
        end
        if !satisfies(p1, f1.version, reqs)
            name1 = haskey(uuid_to_name, p1) ? uuid_to_name[p1] : "UNKNOWN"
            uuid_short1 = p1[1:8]
            warn("$name1 [$uuid_short1] is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])")
        end
        for (p2,f2) in fix
            if !satisfies(p1, f1.version, f2.requires)
                name1 = haskey(uuid_to_name, p1) ? uuid_to_name[p1] : "UNKNOWN"
                uuid_short1 = p1[1:8]
                name2 = haskey(uuid_to_name, p2) ? uuid_to_name[p2] : "UNKNOWN"
                uuid_short2 = p2[1:8]
                warn("$name1 [$uuid_short1] is fixed at $(f1.version) conflicting with requirement for $name2 [$uuid_short2]: $(f2.requires[p1])")
            end
        end
    end
end

function propagate_fixed!(reqs::Requires, bktrc::ResolveBacktrace, fix::Dict{String,Fixed})
    for (p,f) in fix
        merge_requires!(reqs, f.requires)
        for (rp,rvs) in f.requires
            bktrcp = get!(bktrc, rp) do; ResolveBacktraceItem() end
            push!(bktrcp, p=>bktrc[p], rvs)
        end
    end
    for (p,f) in fix
        delete!(reqs, p)
    end
    reqs
end

# Specialized copy for the avail argument below because the deepcopy is slow
function availcopy(avail)
    new_avail = similar(avail)
    for (pkg, vers_avail) in avail
        new_vers_avail = similar(vers_avail)
        for (version, pkg_avail) in vers_avail
            new_vers_avail[version] = copy(pkg_avail)
        end
        new_avail[pkg] = new_vers_avail
    end
    return new_avail
end

# Generate a reverse dependency graph (package names only)
function gen_backdeps(avail::Dict)
    backdeps = Dict{String,Set{String}}()
    for (ap,av) in avail, (v,a) in av, rp in keys(a.requires)
        s = get!(backdeps, rp) do; Set{String}() end
        push!(s, ap)
    end
    return backdeps
end

function dependencies(avail::Dict, fix::Dict = Dict{String,Fixed}("julia"=>Fixed(VERSION)))
    avail = availcopy(avail)
    conflicts = Dict{String,Set{String}}()
    to_expunge = VersionNumber[]
    emptied = String[]
    backdeps = gen_backdeps(avail)

    for (fp,fx) in fix
        delete!(avail, fp)
        haskey(backdeps, fp) || continue
        # for (ap,av) in avail
        for ap in backdeps[fp]
            haskey(avail, ap) || continue
            av = avail[ap]
            empty!(to_expunge)
            for (v,a) in av
                if satisfies(fp, fx.version, a.requires)
                    delete!(a.requires, fp)
                else
                    conflicts_ap = get!(conflicts, ap) do; Set{String}() end
                    push!(conflicts_ap, fp)
                    # don't delete v from av right away so as not to screw up iteration
                    push!(to_expunge, v)
                end
            end
            for v in to_expunge
                delete!(av, v)
            end
            isempty(av) && push!(emptied, ap)
        end
    end
    while !isempty(emptied)
        deleted_pkgs = String[]
        for ap in emptied
            delete!(avail, ap)
            push!(deleted_pkgs, ap)
        end
        empty!(emptied)

        for dp in deleted_pkgs
            haskey(backdeps, dp) || continue
            for ap in backdeps[dp]
                haskey(avail, ap) || continue
                av = avail[ap]
                empty!(to_expunge)
                for (v,a) in av
                    haskey(a.requires, dp) || continue
                    conflicts_ap = get!(conflicts, ap) do; Set{String}() end
                    union!(conflicts_ap, conflicts[dp])
                    push!(to_expunge, v)
                end
                for v in to_expunge
                    delete!(av, v)
                end
                isempty(av) && push!(emptied, ap)
            end
        end
    end
    avail, conflicts
end
function check_requirements(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, fix::Dict,
                            uuid_to_name::Dict{String, String})
    for (p,vs) in reqs
        if !any(vn->(vn in vs), keys(deps[p]))
            remaining_vs = VersionSpec()
            name = haskey(uuid_to_name, p) ? uuid_to_name[p] : "UNKNOWN"
            uuid_short = p[1:8]
            err_msg = "fixed packages introduce conflicting requirements for $name [$uuid_short]: \n"
            available_list = sort!(collect(keys(deps[p])))
            for (p1,f1) in fix
                f1r = f1.requires
                haskey(f1r, p) || continue
                name1 = haskey(uuid_to_name, p1) ? uuid_to_name[p1] : "UNKNOWN"
                uuid_short1 = p1[1:8]
                err_msg *= "         $name1 [$uuid_short1] requires versions $(f1r[p])"
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
end

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
            for rp in keys(isreq)
                isreq[rp] = all(haskey(a.requires, rp) for a in values(fdepsp))
            end

            # Create a list of candidates for new implicit requirements
            staged_new = Set{String}()
            for a in values(fdepsp), (rp,rvs) in a.requires
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
                name = haskey(uuid_to_name, p) ? uuid_to_name[p] : "UNKNOWN"
                uuid_short = p[1:8]
                push!(bktrcp, "$name [$uuid_short]"=>bktrc[p], srvs)
                if isa(bktrcp.versionreq, VersionSpec) && isempty(bktrcp.versionreq)
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
        allowedp = get(allowed, p) do; Dict{VersionNumber,Bool}() end
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
    if !isempty(reqs)
        filtered_deps = dependencies_subset(filtered_deps, Set{String}(keys(reqs)))
    end

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = Dict{String,Dict{VersionNumber, BitVector}}()

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Dict{String,Set{VersionSpec}}()
    for (p,fdepsp) in filtered_deps
        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(a.requires for a in values(fdepsp))

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
        for (vn,a) in fdepsp
            vmind = findfirst(equalto(a.requires), uniqdepssets)
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
    pruned_vers = Dict{String,Vector{VersionNumber}}()
    eq_classes = Dict{String,Dict{VersionNumber,Vector{VersionNumber}}}()
    for (p, vmaskp) in vmask
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

    for (p,depsp) in filtered_deps
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
prune_versions(deps::Dict{String,Dict{VersionNumber,Available}}, uuid_to_name::Dict{String, String}) =
    prune_versions(Requires(), deps, ResolveBacktrace(), uuid_to_name)

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

# Build a subgraph incuding only the (direct and indirect) dependencies and dependants
# of a given package set
function undirected_dependencies_subset(deps::Dict{String,Dict{VersionNumber,Available}}, pkgs::Set{String})
    graph = Dict{String, Set{String}}()

    for (p,d) in deps
        haskey(graph, p) || (graph[p] = Set{String}())
        for a in values(d), rp in keys(a.requires)
            push!(graph[p], rp)
            haskey(graph, rp) || (graph[rp] = Set{String}())
            push!(graph[rp], p)
        end
    end

    staged = pkgs
    allpkgs = copy(pkgs)
    while !isempty(staged)
        staged_next = Set{String}()
        for p in staged, rp in graph[p]
            rp ∉ allpkgs && push!(staged_next, rp)
        end
        union!(allpkgs, staged_next)
        staged = staged_next
    end

    return subdeps(deps, allpkgs)
end

function prune_dependencies(reqs::Requires,
                            deps::Dict{String,Dict{VersionNumber,Available}},
                            uuid_to_name::Dict{String, String},
                            bktrc::ResolveBacktrace = init_resolve_backtrace(reqs))
    deps, _ = prune_versions(reqs, deps, bktrc, uuid_to_name)
    return deps
end

end # module
