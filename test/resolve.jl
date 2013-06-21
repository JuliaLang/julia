using Base.Pkg.Metadata

function deps_from_data(deps_data)
    deps = Array((Version, VersionSet),0)
    for d in deps_data
        if length(d) < 3
            continue
        end
        v = Version(d[1],VersionNumber(d[2]))
        vs = VersionSet(d[3], [VersionNumber(w) for w=d[4:end]])
        push!(deps, (v,vs))
    end
    deps
end
vers_from_data(deps_data) = unique([Version(d[1], VersionNumber(d[2])) for d in deps_data])
function reqs_from_data(reqs_data)
    reqs = VersionSet[]
    for r in reqs_data
        if length(r)==0
            push!(reqs, VersionSet(r[1]))
        else
            push!(reqs, VersionSet(r[1], [VersionNumber(v) for v=r[2:end]]))
        end
    end
    reqs
end
function sanity_tst(deps_data)
    deps = deps_from_data(deps_data)
    vers = vers_from_data(deps_data)
    #println()
    #println("deps="); showall(deps); println()
    #println("vers="); showall(vers); println()
    Pkg.Resolve.sanity_check(vers, deps)
    return true
end
function resolve_tst(deps_data, reqs_data)
    deps = deps_from_data(deps_data)
    vers = vers_from_data(deps_data)
    reqs = reqs_from_data(reqs_data)

    #println()
    #println("deps=$deps")
    #println("vers=$vers")
    #println("reqs=$reqs")
    want = Pkg.Resolve.resolve(reqs, vers, deps)
end

## DEPENDENCY SCHEME 1: TWO PACKAGES, DAG
deps_data = {
    {"A", 1, "B", 1},
    {"A", 2, "B", 2},
    {"B", 1, },
    {"B", 2, }
}

@test sanity_tst(deps_data)

# require just B
reqs_data = {
    {"B"}
}

want = resolve_tst(deps_data, reqs_data)
@test want == ["B"=>VersionNumber(2)]

# require just A: must bring in B
reqs_data = {
    {"A"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2)]


## DEPENDENCY SCHEME 2: TWO PACKAGES, CYCLIC
deps_data = {
    {"A", 1, "B", 2},
    {"A", 2, "B", 1},
    {"B", 1, "A", 2},
    {"B", 2, "A", 1}
}

@test sanity_tst(deps_data)

# require just A
reqs_data = {
    {"A"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2)]

# require just B, force lower version
reqs_data = {
    {"B", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(1)]

# require just A, force lower version
reqs_data = {
    {"A", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(2)]


## DEPENDENCY SCHEME 3: THREE PACKAGES, CYCLIC, TWO MUTUALLY EXCLUSIVE SOLUTIONS
deps_data = {
    {"A", 1, "B", 2},
    {"A", 2, "B", 1, 2},
    {"B", 1, "C", 2},
    {"B", 2, "C", 1, 2},
    {"C", 1, "A", 1, 2},
    {"C", 2, "A", 2}
}

@test sanity_tst(deps_data)

# require just A (must choose solution which has the highest version for A)
reqs_data = {
    {"A"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(1), "C"=>VersionNumber(2)]

# require just B (must choose solution which has the highest version for B)
reqs_data = {
    {"B"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(2), "C"=>VersionNumber(1)]

# require just A, force lower version
reqs_data = {
    {"A", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(2), "C"=>VersionNumber(1)]

# require A and C, incompatible versions
reqs_data = {
    {"A", 1, 2},
    {"C", 2}
}
@test_fails resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 4: TWO PACKAGES, DAG, WITH TRIVIAL INCONSISTENCY
deps_data = {
    {"A", 1, "B", 2},
    {"B", 1}
}

@test_fails sanity_tst(deps_data)

# require B (must not give errors)
reqs_data = {
    {"B"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["B"=>VersionNumber(1)]


## DEPENDENCY SCHEME 5: THREE PACKAGES, DAG, WITH IMPLICIT INCONSISTENCY
deps_data = {
    {"A", 1, "B", 2},
    {"A", 1, "C", 2},
    {"A", 2, "B", 1, 2},
    {"A", 2, "C", 1, 2},
    {"B", 1, "C", 2},
    {"B", 2, "C", 2},
    {"C", 1},
    {"C", 2}
}

@test_fails sanity_tst(deps_data)

# require A, any version (must use the highest non-inconsistent)
reqs_data = {
    {"A"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(2), "C"=>VersionNumber(2)]

# require A, force highest version (impossible)
reqs_data = {
    {"A", 2}
}
@test_fails resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 6: TWO PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = {
    {"A", 1, "B", 2},
    {"A", 2, "B", 1, 2},
    {"B", 1, "A", 1, 2},
    {"B", 2, "A", 2}
}

@test_fails sanity_tst(deps_data)

# require A (impossible)
reqs_data = {
    {"A"}
}
@test_fails resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = {
    {"B"}
}
@test_fails resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 7: THREE PACKAGES, CYCLIC, WITH INCONSISTENCY
deps_data = {
    {"A", 1, "B", 1, 2},
    {"A", 2, "B", 2},
    {"B", 1, "C", 1, 2},
    {"B", 2, "C", 2},
    {"C", 1, "A", 2},
    {"C", 2, "A", 2},
}

@test_fails sanity_tst(deps_data)

# require A
reqs_data = {
    {"A"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "C"=>VersionNumber(2)]

# require C
reqs_data = {
    {"C"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "C"=>VersionNumber(2)]

# require C, lowest version (impossible)
reqs_data = {
    {"C", 1, 2}
}
@test_fails resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 8: THREE PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = {
    {"A", 1, "B", 1, 2},
    {"A", 2, "B", 2},
    {"B", 1, "C", 1, 2},
    {"B", 2, "C", 2},
    {"C", 1, "A", 2},
    {"C", 2, "A", 1, 2},
}

@test_fails sanity_tst(deps_data)

# require A (impossible)
reqs_data = {
    {"A"}
}
@test_fails resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = {
    {"B"}
}
@test_fails resolve_tst(deps_data, reqs_data)

# require C (impossible)
reqs_data = {
    {"C"}
}
@test_fails resolve_tst(deps_data, reqs_data)

## DEPENDENCY SCHEME 9: SIX PACKAGES, DAG
deps_data = {
    {"A", 1},
    {"A", 2},
    {"A", 3},
    {"B", 1, "A", 1, 2},
    {"B", 2, "A"},
    {"C", 1, "A", 2, 3},
    {"C", 2, "A", 2},
    {"D", 1, "B", 1},
    {"D", 2, "B", 2},
    {"E", 1, "D"},
    {"F", 1, "A", 1, 3},
    {"F", 1, "E"},
    {"F", 2, "C", 2},
    {"F", 2, "E"},
}

@test sanity_tst(deps_data)

# require just F
reqs_data = {
    {"F"}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(3), "B"=>VersionNumber(2), "C"=>VersionNumber(2), "D"=>VersionNumber(2), "E"=>VersionNumber(1), "F"=>VersionNumber(2)]

# require just F, lower version
reqs_data = {
    {"F", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "D"=>VersionNumber(2), "E"=>VersionNumber(1), "F"=>VersionNumber(1)]

# require F and B; force lower B version -> must bring down F, A, and D versions too
reqs_data = {
    {"F"},
    {"B", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(1), "D"=>VersionNumber(1), "E"=>VersionNumber(1), "F"=>VersionNumber(1)]

# require F and D; force lower D version -> must not bring down F version
reqs_data = {
    {"F"},
    {"D", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(3), "B"=>VersionNumber(2), "C"=>VersionNumber(2), "D"=>VersionNumber(1), "E"=>VersionNumber(1), "F"=>VersionNumber(2)]

# require F and C; force lower C version -> must bring down F and A versions
reqs_data = {
    {"F"},
    {"C", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "C"=>VersionNumber(1), "D"=>VersionNumber(2), "E"=>VersionNumber(1), "F"=>VersionNumber(1)]
