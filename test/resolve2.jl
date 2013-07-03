using Base.Pkg2.Types
using Base.Pkg2.Resolve

function deps_from_data(deps_data)
    deps = Dict{ByteString,Dict{VersionNumber,Available}}()
    for d in deps_data
        p = d[1]; vnn = d[2]; r = d[3:end]
        if !haskey(deps, p)
            deps[p] = (VersionNumber=>Available)[]
        end
        vn = VersionNumber(vnn)
        if !haskey(deps[p], vn)
            deps[p][vn] = Available("$(p)_$(vn)_sha1", (ByteString=>VersionSet)[])
        end
        isempty(r) && continue
        rp = r[1]
        if length(r) > 1
            rvs = VersionSet([VersionNumber(w) for w=r[2:end]])
        else
            rvs = VersionSet()
        end
        deps[p][vn].requires[rp] = rvs
    end
    deps
end
function reqs_from_data(reqs_data)
    reqs = (ByteString=>VersionSet)[]
    for r in reqs_data
        p = r[1]; vns = r[2:end]
        reqs[p] = VersionSet([VersionNumber(vn) for vn in vns])
    end
    reqs
end
function sanity_tst(deps_data, expected_result)
    deps = deps_from_data(deps_data)
    #println("deps=$deps")
    #println()
    result = sanity_check(deps)
    length(result) == length(expected_result) || return false
    for (p, vn, pp) in result
        contains(expected_result, (p, vn)) || return  false
    end
    return true
end
sanity_tst(deps_data) = sanity_tst(deps_data, {})

function resolve_tst(deps_data, reqs_data)
    deps = deps_from_data(deps_data)
    reqs = reqs_from_data(reqs_data)

    #println()
    #println("deps=$deps")
    #println("reqs=$reqs")
    want = resolve(reqs, deps)
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
@test_throws resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 4: TWO PACKAGES, DAG, WITH TRIVIAL INCONSISTENCY
deps_data = {
    {"A", 1, "B", 2},
    {"B", 1}
}

@test sanity_tst(deps_data, {("A", VersionNumber(1))})

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

@test sanity_tst(deps_data, {("A", VersionNumber(2))})

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
@test_throws resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 6: TWO PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = {
    {"A", 1, "B", 2},
    {"A", 2, "B", 1, 2},
    {"B", 1, "A", 1, 2},
    {"B", 2, "A", 2}
}

@test sanity_tst(deps_data, {("A", VersionNumber(1)), ("A", VersionNumber(2)),
                             ("B", VersionNumber(1)), ("B", VersionNumber(2))})

# require A (impossible)
reqs_data = {
    {"A"}
}
@test_throws resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = {
    {"B"}
}
@test_throws resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 7: THREE PACKAGES, CYCLIC, WITH INCONSISTENCY
deps_data = {
    {"A", 1, "B", 1, 2},
    {"A", 2, "B", 2},
    {"B", 1, "C", 1, 2},
    {"B", 2, "C", 2},
    {"C", 1, "A", 2},
    {"C", 2, "A", 2},
}

@test sanity_tst(deps_data, {("A", VersionNumber(1)), ("B", VersionNumber(1)),
                             ("C", VersionNumber(1))})

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
@test_throws resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 8: THREE PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = {
    {"A", 1, "B", 1, 2},
    {"A", 2, "B", 2},
    {"B", 1, "C", 1, 2},
    {"B", 2, "C", 2},
    {"C", 1, "A", 2},
    {"C", 2, "A", 1, 2},
}

@test sanity_tst(deps_data, {("A", VersionNumber(1)), ("A", VersionNumber(2)),
                             ("B", VersionNumber(1)), ("B", VersionNumber(2)),
                             ("C", VersionNumber(1)), ("C", VersionNumber(2))})

# require A (impossible)
reqs_data = {
    {"A"}
}
@test_throws resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = {
    {"B"}
}
@test_throws resolve_tst(deps_data, reqs_data)

# require C (impossible)
reqs_data = {
    {"C"}
}
@test_throws resolve_tst(deps_data, reqs_data)

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
@test want == ["A"=>VersionNumber(3), "B"=>VersionNumber(2), "C"=>VersionNumber(2),
               "D"=>VersionNumber(2), "E"=>VersionNumber(1), "F"=>VersionNumber(2)]

# require just F, lower version
reqs_data = {
    {"F", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "D"=>VersionNumber(2),
               "E"=>VersionNumber(1), "F"=>VersionNumber(1)]

# require F and B; force lower B version -> must bring down F, A, and D versions too
reqs_data = {
    {"F"},
    {"B", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(1), "B"=>VersionNumber(1), "D"=>VersionNumber(1),
               "E"=>VersionNumber(1), "F"=>VersionNumber(1)]

# require F and D; force lower D version -> must not bring down F version
reqs_data = {
    {"F"},
    {"D", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(3), "B"=>VersionNumber(2), "C"=>VersionNumber(2),
               "D"=>VersionNumber(1), "E"=>VersionNumber(1), "F"=>VersionNumber(2)]

# require F and C; force lower C version -> must bring down F and A versions
reqs_data = {
    {"F"},
    {"C", 1, 2}
}
want = resolve_tst(deps_data, reqs_data)
@test want == ["A"=>VersionNumber(2), "B"=>VersionNumber(2), "C"=>VersionNumber(1),
               "D"=>VersionNumber(2), "E"=>VersionNumber(1), "F"=>VersionNumber(1)]
