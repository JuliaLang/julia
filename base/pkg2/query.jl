module Query

using ..Types

function requirements(reqs::Dict, fix::Dict)
    for (p1,f1) in fix
        satisfies(p1, f1.version, reqs) ||
            warn("$p1 is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])")
        for (p2,f2) in fix
            satisfies(p1, f1.version, f2.requires) ||
                warn("$p1 is fixed at $(f1.version) conflicting with requirement for $p2: $(f2.requires[p1])")
        end
    end
    reqs = copy(reqs)
    for (p1,f1) in fix
        merge_requires!(reqs, f1.requires)
    end
    for (p,f) in fix
        delete!(reqs, p, nothing)
    end
    reqs
end

function dependencies(avail::Dict, fix::Dict)
    avail = deepcopy(avail)
    for (fp,fx) in fix
        delete!(avail, fp, nothing)
        for (ap,av) in avail, (v,a) in copy(av)
            if satisfies(fp, fx.version, a.requires)
                delete!(a.requires, fp, nothing)
            else
                delete!(av, v)
            end
        end
    end
    for (ap,av) in avail
        isempty(av) && delete!(avail, ap)
    end
    avail
end

typealias PackageState Union(Nothing,VersionNumber) 

function diff(have::Dict, want::Dict, avail::Dict, fixed::Dict)
    changeslist  = Array((ByteString,(PackageState,PackageState)),0)

    for pkg in collect(union(keys(have),keys(want)))
        h, w = haskey(have,pkg), haskey(want,pkg)
        if h && w
            if have[pkg] != want[pkg]
                push!(changeslist, (pkg,(have[pkg], want[pkg])))
            end
        elseif h
            push!(changeslist, (pkg,(have[pkg],nothing)))
        elseif w
            push!(changeslist, (pkg,(nothing,want[pkg])))
        end
    end

    # Sort packages topologically
    sort!(changeslist) do a,b
        ((a,vera),(b,verb)) = (a,b)
        c = contains(Pkg2.Read.alldependencies(a,avail,want,fixed),b) 
        nonordered = (!c && !contains(Pkg2.Read.alldependencies(b,avail,want,fixed),a))
        if vera[2] == nothing
            if vera[2] == verb[2]
                return nonordered ? a < b : !c
            end
            return true
        end
        nonordered ? a < b : c
    end

    changeslist
end

# Reduce the number of versions by creating equivalence classes, and retaining
# only the highest version for each equivalence class.
# Two versions are equivalent if:
#   1) They appear together as dependecies of another package (i.e. for each
#      dependency relation, they are both required or both not required)
#   2) They have the same dependencies
# Also, if there are explicitly required packages, dicards all versions outside
# the allowed range (checking for impossible ranges while at it), and keeps only
# the subgraph or requirements dependencies.
prune_versions(deps::Dict{ByteString,Dict{VersionNumber,Available}}) = prune_versions((ByteString=>VersionSet)[], deps)
function prune_versions(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

    if !isempty(reqs)
        deps = dependencies_subset(deps, Set{ByteString}(keys(reqs)...))
    end

    np = length(deps)

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = (ByteString=>Dict{VersionNumber, BitVector})[]

    # Parse requirements and store allowed versions.
    allowed = (ByteString=>Dict{VersionNumber, Bool})[]
    for (p,vs) in reqs
        @assert !haskey(allowed, p)
        allowed[p] = (VersionNumber=>Bool)[]
        allowedp = allowed[p]
        for (vn,_) in deps[p]
            allowedp[vn] = contains(vs, vn)
        end
        @assert !isempty(allowedp)
        any([a for (_,a) in allowedp]) ||
            error("Invalid requirements: no version allowed for package $p")
    end

    filtered_deps = (ByteString=>Dict{VersionNumber,Available})[]
    for (p,depsp) in deps
        filtered_deps[p] = (VersionNumber=>Available)[]
        allowedp = get(allowed, p, (VersionNumber=>Bool)[])
        fdepsp = filtered_deps[p]
        for (vn,a) in depsp
            if !isempty(allowedp) && !allowedp[vn]
                continue
            end
            fdepsp[vn] = a
        end
    end

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = Set{(ByteString,VersionSet)}()
    for (p, fdepsp) in filtered_deps

        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique([a for (_,a) in fdepsp])

        # Store all dependencies seen so far for later use
        for a in uniqdepssets, r in a.requires
            add!(alldeps, r)
        end

        # If the package has just one version, it's uninteresting
        if length(deps[p]) == 1
            continue
        end

        # Grow the pattern by the number of classes
        luds = length(uniqdepssets)
        @assert !haskey(vmask, p)
        vmask[p] = (VersionNumber=>BitVector)[]
        vmaskp = vmask[p]
        for (vn,_) in fdepsp
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
    for (p,vs) in alldeps
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
            push!(vm, contains(vs, vn))
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers = (ByteString=>Vector{VersionNumber})[]
    eq_classes_map = (ByteString=>Dict{VersionNumber,Vector{VersionNumber}})[]
    for (p, vmaskp) in vmask
        vmask0_uniq = unique([vm for (_,vm) in vmaskp])
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
        eq_classes_map[p] = (VersionNumber=>Vector{VersionNumber})[]
        eqclassp = eq_classes_map[p]
        for cl in classes
            if !isempty(cl)
                vtop = max(cl)
                push!(prunedp, vtop)
                @assert !haskey(eqclassp, vtop)
                eqclassp[vtop] = cl
            end
        end
        sort!(prunedp)
    end
    # Put non-allowed versions into eq_classes_map
    for (p, allowedp) in allowed
        haskey(eq_classes_map, p) || continue
        eqclassp = eq_classes_map[p]
        for (vn, a) in allowedp
            a && continue
            eqclassp[vn] = [vn]
        end
    end
    # Put all remaining packages into eq_classes_map
    for (p, depsp) in deps
        haskey(eq_classes_map, p) && continue
        eq_classes_map[p] = (VersionNumber=>Vector{VersionNumber})[]
        eqclassp = eq_classes_map[p]
        for (vn,_) in depsp
            eqclassp[vn] = [vn]
        end
    end


    # Recompute deps. We could simplify them, but it's not worth it
    new_deps = (ByteString=>Dict{VersionNumber,Available})[]

    for (p,depsp) in deps
        @assert !haskey(new_deps, p)
        if !haskey(pruned_vers, p)
            new_deps[p] = depsp
            continue
        end
        new_deps[p] = (VersionNumber=>Available)[]
        pruned_versp = pruned_vers[p]
        for (vn,a) in depsp
            contains(pruned_versp, vn) || continue
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

    return new_deps, eq_classes_map
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package set
function dependencies_subset(deps::Dict{ByteString,Dict{VersionNumber,Available}}, pkgs::Set{ByteString})

    np = length(deps)

    staged = pkgs
    allpkgs = pkgs
    while !isempty(staged)
        staged_next = Set{ByteString}()
        for p in staged, (_,a) in deps[p], (rp,_) in a.requires
            if !contains(allpkgs, rp)
                add!(staged_next, rp)
            end
        end
        allpkgs = union(allpkgs, staged_next)
        staged = staged_next
    end

    sub_deps = Dict{ByteString,Dict{VersionNumber,Available}}()
    for p in allpkgs
        if !haskey(sub_deps, p)
            sub_depsp = (VersionNumber=>Available)[]
            sub_deps[p] = sub_depsp
        else
            sub_depsp = sub_deps[p]
        end
        for (vn, a) in deps[p]
            sub_deps[p][vn] = a
        end
    end

    return sub_deps
end

end # module
