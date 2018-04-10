# This file is a part of Julia. License is MIT: https://julialang.org/license

module Query

import Pkg
import ..PkgError
using ..Types

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

function check_fixed(reqs::Requires, fix::Dict{String,Fixed}, avail::Dict)
    for (p1,f1) in fix
        for p2 in keys(f1.requires)
            haskey(avail, p2) || haskey(fix, p2) || throw(PkgError("unknown package $p2 required by $p1"))
        end
        satisfies(p1, f1.version, reqs) ||
            @warn "$p1 is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])"
        for (p2,f2) in fix
            satisfies(p1, f1.version, f2.requires) ||
                @warn "$p1 is fixed at $(f1.version) conflicting with requirement for $p2: $(f2.requires[p1])"
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
    new_avail = empty(avail)
    for (pkg, vers_avail) in avail
        new_vers_avail = empty(vers_avail)
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

function partial_update_mask(instd::Dict{String,Tuple{VersionNumber,Bool}},
        avail::Dict{String,Dict{VersionNumber,Available}}, upkgs::Set{String})
    dont_update = Set{String}()
    isempty(upkgs) && return dont_update
    avail_new = deepcopy(avail)
    for p in upkgs
        haskey(instd, p) || throw(PkgError("Package $p is not installed"))
        v = instd[p][1]
        if haskey(avail, p)
            for vn in keys(avail[p])
                vn < v && delete!(avail_new[p], vn)
            end
        end
    end
    avail_new = dependencies_subset(avail_new, upkgs)

    for p in keys(avail)
        !haskey(avail_new, p) && push!(dont_update, p)
    end
    for p in keys(instd)
        !haskey(avail_new, p) && p ∉ upkgs && push!(dont_update, p)
    end
    return dont_update
end

# Try to produce some helpful message in case of a partial update which does not go all the way
# (Does not do a full analysis, it only checks requirements and direct dependents.)
function check_partial_updates(reqs::Requires,
                               deps::Dict{String,Dict{VersionNumber,Available}},
                               want::Dict{String,VersionNumber},
                               fixed::Dict{String,Fixed},
                               upkgs::Set{String})
    for p in upkgs
        if !haskey(want, p)
            if !haskey(fixed, p)
                @warn "Something went wrong with the update of package $p, please submit a bug report"
                continue
            end
            v = fixed[p].version
        else
            v = want[p]
            if haskey(fixed, p) && v != fixed[p].version
                @warn "Something went wrong with the update of package $p, please submit a bug report"
                continue
            end
        end
        haskey(deps, p) || continue
        vers = sort!(collect(keys(deps[p])))
        higher_vers = vers[vers .> v]
        isempty(higher_vers) && continue # package p has been set to the highest available version

        # Determine if there are packages which depend on `p` and somehow prevent its update to
        # the latest version
        blocking_parents = Set{String}()
        for (p1,d1) in deps
            p1 in upkgs && continue # package `p1` is among the ones to be updated, skip the check
            haskey(fixed, p1) || continue # if package `p1` is not fixed, it can't be blocking
            r1 = fixed[p1].requires # get `p1` requirements
            haskey(r1, p) || continue # check if package `p1` requires `p`
            vs1 = r1[p] # get the versions of `p` allowed by `p1` requirements
            any(hv in vs1 for hv in higher_vers) && continue # package `p1` would allow some of the higher versions,
                                                             # therefore it's not responsible for blocking `p`
            push!(blocking_parents, p1) # package `p1` is blocking the update of `p`
        end

        # Determine if the update of `p` is prevented by explicit user-provided requirements
        blocking_reqs = (haskey(reqs, p) && all(hv ∉ reqs[p] for hv in higher_vers))

        # Determine if the update of `p` is prevented by it being fixed (e.g. it's dirty, or pinned...)
        isfixed = haskey(fixed, p)

        @info begin
            "Package $p was set to version $v, but a higher version $(vers[end]) exists.\n" *
            if isfixed
                "The package is fixed. You can try using `Pkg.free(\"$p\")` to update it."
            elseif blocking_reqs
                "The update is prevented by explicit requirements constraints. Edit your REQUIRE file to change this."
            elseif !isempty(blocking_parents)
                string("To install the latest version, you could try updating these packages as well: ", join(blocking_parents, ", ", " and "), ".")
            else
                "To install the latest version, you could try doing a full update with `Pkg.update()`."
            end
        end
    end
end

const PackageState = Union{Nothing,VersionNumber}

function diff(have::Dict, want::Dict, avail::Dict, fixed::Dict)
    change = Vector{Tuple{String,Tuple{PackageState,PackageState}}}()
    remove = Vector{Tuple{String,Tuple{PackageState,PackageState}}}()

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

function check_requirements(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, fix::Dict)
    for (p,vs) in reqs
        if !any(vn->(vn in vs), keys(deps[p]))
            remaining_vs = VersionSet()
            err_msg = "fixed packages introduce conflicting requirements for $p: \n"
            available_list = sort!(collect(keys(deps[p])))
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
function filter_versions(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, bktrc::ResolveBacktrace)
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
                err_msg = "Unsatisfiable requirements detected for package $p:\n"
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
                push!(bktrcp, p=>bktrc[p], srvs)
                if isa(bktrcp.versionreq, VersionSet) && isempty(bktrcp.versionreq)
                    err_msg = "Unsatisfiable requirements detected for package $rp:\n"
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
function prune_versions(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}}, bktrc::ResolveBacktrace)
    filtered_deps, allowed = filter_versions(reqs, deps, bktrc)
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
            vmind = findfirst(isequal(a.requires), uniqdepssets)
            @assert vmind !== nothing
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
            c0 = findfirst(isequal(vm), vmask0_uniq)::Int
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
prune_versions(deps::Dict{String,Dict{VersionNumber,Available}}) =
    prune_versions(Dict{String,VersionSet}(), deps, ResolveBacktrace())
prune_versions(deps::Dict{String,Dict{VersionNumber,Available}}, bktrc::ResolveBacktrace) =
    prune_versions(Dict{String,VersionSet}(), deps, bktrc)

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
                            bktrc::ResolveBacktrace = init_resolve_backtrace(reqs))
    deps, _ = prune_versions(reqs, deps, bktrc)
    return deps
end

end # module
