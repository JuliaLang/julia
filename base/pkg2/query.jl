module Query

using ..Types

function requirements(reqs::Dict, fix::Dict)
    for (p1,f1) in fix
        satisfies(p1, f1.version, reqs) ||
            warn("$p1 is fixed at $(f1.version) conflicting with top-level requirement: $(reqs[p1])")
        for (p2,f2) in fix
            satisfies(p1, f1.version, f2.requires) ||
                warn("$p1 is fixed at $(f1.version) conflicting with requirement for $p2: $(f2.requires[p1])")
            merge_requires!(reqs, f2.requires)
        end
    end
    for (p,f) in fix
        delete!(reqs, p, nothing)
    end
    reqs
end

function dependencies(avail::Dict, fix::Dict)
    for (fp,fx) in fix
        if haskey(avail, fp)
            delete!(avail, fp)
            continue
        end
        for (ap,av) in avail, (v,a) in av
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

function diff(have::Dict, want::Dict)
    remove = Dict{ByteString,VersionNumber}()
    update = Dict{ByteString,(VersionNumber,VersionNumber)}()
    install = Dict{ByteString,VersionNumber}()

    for pkg in sort!(union(keys(have),keys(want)))
        h, w = haskey(have,pkg), haskey(want,pkg)
        if h && w
            if have[pkg] != want[pkg]
                update[pkg] = (free[pkg], want[pkg])
            end
        elseif h
            remove[pkg] = free[pkg]
        elseif w
            install[pkg] = want[pkg]
        end
    end

    remove, update, install
end

end # module
