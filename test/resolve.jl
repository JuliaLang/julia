# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Pkg.Types
using Base.Pkg.Query
using Base.Pkg.Resolve
using Base.Pkg.Resolve.VersionWeights
import Base.Pkg.PkgError

# Check that VersionWeight keeps the same ordering as VersionNumber

vlst = [
    v"0.0.0",
    v"0.0.1",
    v"0.1.0",
    v"0.1.1",
    v"1.0.0",
    v"1.0.1",
    v"1.1.0",
    v"1.1.1",
    v"1.0.0-pre",
    v"1.0.0-pre1",
    v"1.0.1-pre",
    v"1.0.0-0.pre.2",
    v"1.0.0-0.pre.3",
    v"1.0.0-0.pre1.tst",
    v"1.0.0-pre.1+0.1",
    v"1.0.0-pre.1+0.1plus",
    v"1.0.0-pre.1-+0.1plus",
    v"1.0.0-pre.1-+0.1Plus",
    v"1.0.0-pre.1-+0.1pLUs",
    v"1.0.0-pre.1-+0.1pluS",
    v"1.0.0+0.1plus",
    v"1.0.0+0.1plus-",
    v"1.0.0+-",
    v"1.0.0-",
    v"1.0.0+",
    v"1.0.0--",
    v"1.0.0---",
    v"1.0.0--+-",
    v"1.0.0+--",
    v"1.0.0+-.-",
    v"1.0.0+0.-",
    v"1.0.0+-.0",
    v"1.0.0-a+--",
    v"1.0.0-a+-.-",
    v"1.0.0-a+0.-",
    v"1.0.0-a+-.0"
    ]

for v1 in vlst, v2 in vlst
    vw1 = VersionWeight(v1)
    vw2 = VersionWeight(v2)
    clt = v1 < v2
    @test clt == (vw1 < vw2)
    ceq = v1 == v2
    @test ceq == (vw1 == vw2)
end

# auxiliary functions

function deps_from_data(deps_data)
    deps = Dict{String,Dict{VersionNumber,Available}}()
    for d in deps_data
        p = d[1]; vn = d[2]; r = d[3:end]
        if !haskey(deps, p)
            deps[p] = Dict{VersionNumber,Available}()
        end
        if !haskey(deps[p], vn)
            deps[p][vn] = Available("$(p)_$(vn)_sha1", Dict{String,VersionSet}())
        end
        isempty(r) && continue
        rp = r[1]
        if length(r) > 1
            rvs = VersionSet(VersionNumber[r[2:end]...])
        else
            rvs = VersionSet()
        end
        deps[p][vn].requires[rp] = rvs
    end
    deps
end
function reqs_from_data(reqs_data)
    reqs = Dict{String,VersionSet}()
    for r in reqs_data
        p = r[1]
        reqs[p] = VersionSet(VersionNumber[r[2:end]...])
    end
    reqs
end
function sanity_tst(deps_data, expected_result; pkgs=[])
    deps = deps_from_data(deps_data)
    #println("deps=$deps")
    #println()
    result = sanity_check(deps, Set(String[pkgs...]))
    length(result) == length(expected_result) || return false
    for (p, vn, pp) in result
        in((p, vn), expected_result) || return  false
    end
    return true
end
sanity_tst(deps_data; kw...) = sanity_tst(deps_data, []; kw...)

function resolve_tst(deps_data, reqs_data)
    deps = deps_from_data(deps_data)
    reqs = reqs_from_data(reqs_data)

    #println()
    #println("deps=$deps")
    #println("reqs=$reqs")
    deps = Query.prune_dependencies(reqs, deps)
    want = resolve(reqs, deps)
end

## DEPENDENCY SCHEME 1: TWO PACKAGES, DAG
deps_data = Any[
    ["A", v"1", "B", v"1"],
    ["A", v"2", "B", v"2"],
    ["B", v"1"],
    ["B", v"2"]
]

@test sanity_tst(deps_data)
@test sanity_tst(deps_data, pkgs=["A", "B"])
@test sanity_tst(deps_data, pkgs=["B"])
@test sanity_tst(deps_data, pkgs=["A"])

# require just B
reqs_data = Any[
    ["B"]
]

want = resolve_tst(deps_data, reqs_data)
@test want == Dict("B"=>v"2")

# require just A: must bring in B
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2")


## DEPENDENCY SCHEME 2: TWO PACKAGES, CYCLIC
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["A", v"2", "B", v"1"],
    ["B", v"1", "A", v"2"],
    ["B", v"2", "A", v"1"]
]

@test sanity_tst(deps_data)

# require just A
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2")

# require just B, force lower version
reqs_data = Any[
    ["B", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"1")

# require just A, force lower version
reqs_data = Any[
    ["A", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2")


## DEPENDENCY SCHEME 3: THREE PACKAGES, CYCLIC, TWO MUTUALLY EXCLUSIVE SOLUTIONS
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["A", v"2", "B", v"1", v"2"],
    ["B", v"1", "C", v"2"],
    ["B", v"2", "C", v"1", v"2"],
    ["C", v"1", "A", v"1", v"2"],
    ["C", v"2", "A", v"2"]
]

@test sanity_tst(deps_data)

# require just A (must choose solution which has the highest version for A)
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"1", "C"=>v"2")

# require just B (must choose solution which has the highest version for B)
reqs_data = Any[
    ["B"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2", "C"=>v"1")

# require just A, force lower version
reqs_data = Any[
    ["A", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2", "C"=>v"1")

# require A and C, incompatible versions
reqs_data = Any[
    ["A", v"1", v"2"],
    ["C", v"2"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 4: TWO PACKAGES, DAG, WITH TRIVIAL INCONSISTENCY
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["B", v"1"]
]

@test sanity_tst(deps_data, [("A", v"1")])
@test sanity_tst(deps_data, [("A", v"1")], pkgs=["B"])

# require B (must not give errors)
reqs_data = Any[
    ["B"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("B"=>v"1")


## DEPENDENCY SCHEME 5: THREE PACKAGES, DAG, WITH IMPLICIT INCONSISTENCY
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["A", v"1", "C", v"2"],
    ["A", v"2", "B", v"1", v"2"],
    ["A", v"2", "C", v"1", v"2"],
    ["B", v"1", "C", v"2"],
    ["B", v"2", "C", v"2"],
    ["C", v"1"],
    ["C", v"2"]
]

@test sanity_tst(deps_data, [("A", v"2")])
@test sanity_tst(deps_data, [("A", v"2")], pkgs=["B"])
@test sanity_tst(deps_data, [("A", v"2")], pkgs=["C"])

# require A, any version (must use the highest non-inconsistent)
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2", "C"=>v"2")

# require A, force highest version (impossible)
reqs_data = Any[
    ["A", v"2"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 6: TWO PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["A", v"2", "B", v"1", v"2"],
    ["B", v"1", "A", v"1", v"2"],
    ["B", v"2", "A", v"2"]
]

@test sanity_tst(deps_data, [("A", v"1"), ("A", v"2"),
                             ("B", v"1"), ("B", v"2")])

# require A (impossible)
reqs_data = Any[
    ["A"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = Any[
    ["B"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 7: THREE PACKAGES, CYCLIC, WITH INCONSISTENCY
deps_data = Any[
    ["A", v"1", "B", v"1", v"2"],
    ["A", v"2", "B", v"2"],
    ["B", v"1", "C", v"1", v"2"],
    ["B", v"2", "C", v"2"],
    ["C", v"1", "A", v"2"],
    ["C", v"2", "A", v"2"],
]

@test sanity_tst(deps_data, [("A", v"1"), ("B", v"1"),
                             ("C", v"1")])

# require A
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2", "C"=>v"2")

# require C
reqs_data = Any[
    ["C"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2", "C"=>v"2")

# require C, lowest version (impossible)
reqs_data = Any[
    ["C", v"1", v"2"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)


## DEPENDENCY SCHEME 8: THREE PACKAGES, CYCLIC, TOTALLY INCONSISTENT
deps_data = Any[
    ["A", v"1", "B", v"1", v"2"],
    ["A", v"2", "B", v"2"],
    ["B", v"1", "C", v"1", v"2"],
    ["B", v"2", "C", v"2"],
    ["C", v"1", "A", v"2"],
    ["C", v"2", "A", v"1", v"2"],
]

@test sanity_tst(deps_data, [("A", v"1"), ("A", v"2"),
                             ("B", v"1"), ("B", v"2"),
                             ("C", v"1"), ("C", v"2")])

# require A (impossible)
reqs_data = Any[
    ["A"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)

# require B (impossible)
reqs_data = Any[
    ["B"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)

# require C (impossible)
reqs_data = Any[
    ["C"]
]
@test_throws PkgError resolve_tst(deps_data, reqs_data)

## DEPENDENCY SCHEME 9: SIX PACKAGES, DAG
deps_data = Any[
    ["A", v"1"],
    ["A", v"2"],
    ["A", v"3"],
    ["B", v"1", "A", v"1", v"2"],
    ["B", v"2", "A"],
    ["C", v"1", "A", v"2", v"3"],
    ["C", v"2", "A", v"2"],
    ["D", v"1", "B", v"1"],
    ["D", v"2", "B", v"2"],
    ["E", v"1", "D"],
    ["F", v"1", "A", v"1", v"3"],
    ["F", v"1", "E"],
    ["F", v"2", "C", v"2"],
    ["F", v"2", "E"],
]

@test sanity_tst(deps_data)

# require just F
reqs_data = Any[
    ["F"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"3", "B"=>v"2", "C"=>v"2",
                   "D"=>v"2", "E"=>v"1", "F"=>v"2")

# require just F, lower version
reqs_data = Any[
    ["F", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2", "D"=>v"2",
                   "E"=>v"1", "F"=>v"1")

# require F and B; force lower B version -> must bring down F, A, and D versions too
reqs_data = Any[
    ["F"],
    ["B", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"1", "D"=>v"1",
                   "E"=>v"1", "F"=>v"1")

# require F and D; force lower D version -> must not bring down F version
reqs_data = Any[
    ["F"],
    ["D", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"3", "B"=>v"2", "C"=>v"2",
                   "D"=>v"1", "E"=>v"1", "F"=>v"2")

# require F and C; force lower C version -> must bring down F and A versions
reqs_data = Any[
    ["F"],
    ["C", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"2", "C"=>v"1",
                   "D"=>v"2", "E"=>v"1", "F"=>v"1")

## DEPENDENCY SCHEME 10: SIX PACKAGES, DAG, WITH PRERELEASE/BUILD VERSIONS
deps_data = Any[
    ["A", v"1"],
    ["A", v"2-rc.1"],
    ["A", v"2-rc.1+bld"],
    ["A", v"2"],
    ["A", v"2.1.0"],
    ["B", v"1", "A", v"1", v"2-"],
    ["B", v"1.0.1-beta", "A", v"2-rc"],
    ["B", v"1.0.1", "A"],
    ["C", v"1", "A", v"2-", v"2.1"],
    ["C", v"1+BLD", "A", v"2-rc.1", v"2.1"],
    ["C", v"2", "A", v"2-rc.1"],
    ["D", v"1", "B", v"1"],
    ["D", v"2", "B", v"1.0.1-"],
    ["E", v"1-plztst", "D"],
    ["E", v"1", "D"],
    ["F", v"1.1", "A", v"1", v"2.1"],
    ["F", v"1.1", "E", v"1"],
    ["F", v"2-rc.1", "A", v"2-", v"2.1"],
    ["F", v"2-rc.1", "C", v"1"],
    ["F", v"2-rc.1", "E"],
    ["F", v"2-rc.2", "A", v"2-", v"2.1"],
    ["F", v"2-rc.2", "C", v"2"],
    ["F", v"2-rc.2", "E"],
    ["F", v"2", "C", v"2"],
    ["F", v"2", "E"],
]

@test sanity_tst(deps_data)

# require just F
reqs_data = Any[
    ["F"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2.1", "B"=>v"1.0.1", "C"=>v"2",
                   "D"=>v"2", "E"=>v"1", "F"=>v"2")

# require just F, lower version
reqs_data = Any[
    ["F", v"1", v"2-"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"1.0.1", "D"=>v"2",
                   "E"=>v"1", "F"=>v"1.1")

# require F and B; force lower B version -> must bring down F, A, and D versions too
reqs_data = Any[
    ["F"],
    ["B", v"1", v"1.0.1-"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"1", "D"=>v"1",
                   "E"=>v"1", "F"=>v"1.1")

# require F and D; force lower D version -> must not bring down F version
reqs_data = Any[
    ["F"],
    ["D", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2.1", "B"=>v"1.0.1", "C"=>v"2",
                   "D"=>v"1", "E"=>v"1", "F"=>v"2")

# require F and C; force lower C version -> must bring down F and A versions
reqs_data = Any[
    ["F"],
    ["C", v"1", v"2"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"2", "B"=>v"1.0.1", "C"=>v"1+BLD",
                   "D"=>v"2", "E"=>v"1", "F"=>v"2-rc.1")

## DEPENDENCY SCHEME 11: FIVE PACKAGES, SAME AS SCHEMES 5 + 1, UNCONNECTED
deps_data = Any[
    ["A", v"1", "B", v"2"],
    ["A", v"1", "C", v"2"],
    ["A", v"2", "B", v"1", v"2"],
    ["A", v"2", "C", v"1", v"2"],
    ["B", v"1", "C", v"2"],
    ["B", v"2", "C", v"2"],
    ["C", v"1"],
    ["C", v"2"],
    ["D", v"1", "E", v"1"],
    ["D", v"2", "E", v"2"],
    ["E", v"1"],
    ["E", v"2"]
]

@test sanity_tst(deps_data, [("A", v"2")])
@test sanity_tst(deps_data, [("A", v"2")], pkgs=["B"])
@test sanity_tst(deps_data, pkgs=["D"])
@test sanity_tst(deps_data, pkgs=["E"])
@test sanity_tst(deps_data, [("A", v"2")], pkgs=["B", "D"])

# require A, any version (must use the highest non-inconsistent)
reqs_data = Any[
    ["A"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2", "C"=>v"2")

# require just D: must bring in E
reqs_data = Any[
    ["D"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("D"=>v"2", "E"=>v"2")


# require A and D, must be the merge of the previous two cases
reqs_data = Any[
    ["A"],
    ["D"]
]
want = resolve_tst(deps_data, reqs_data)
@test want == Dict("A"=>v"1", "B"=>v"2", "C"=>v"2", "D"=>v"2", "E"=>v"2")
