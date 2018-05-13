module NastyGenerator

using Random

pn(i) = "P$i"
randvers(k::Int) = VersionNumber(rand(0:k), rand(0:k), rand(0:k))

function randvspec(k::Int)
    lb, ub = randvers(k), randvers(k)
    if lb > ub
        lb, ub = ub, lb
    end

    slb = rand() < 0.1 ? "0" : string(lb)
    sub = rand() < 0.1 ? "*" : string(ub)

    return "$slb-$sub"
end

"""
Generates a random graph with 2 planted solutions (or quasi-solutions if sat==false).
With the right parameters, this is quite hard to solve for large sizes.
For added fun, the two planted solutions are cycles.
We want the solver to find the best of the two, of course.
This is an extremely unrealistic scenario and it's only intended to stress-test the solver.
Note that the "problematic" output assumes that all non-planted versions will be
uninstallable, which is only the case for some regimes of the parameters (e.g. large
enough d).
"""
function generate_nasty(n::Int,             # size of planted solutions
                        m::Int;             # size of the graph
                        k::Int = 10,        # version number limit
                        q::Int = 10,        # versions per package (upper bound)
                        d::Int = 10,        # neighbors per package
                        seed::Integer = 32524,
                        sat::Bool = true    # create a satisfiable problem?
                       )
    @assert m ≥ n
    d ≤ m-1 || @warn "d=$d, should be ≤ m-1=$(m-1)"

    srand(seed)

    allvers = [sort(unique(randvers(k) for j = 1:q)) for i = 1:m]

    planted1 = [rand(2:length(allvers[i])) for i = 1:n]

    planted2 = [rand(1:(planted1[i]-1)) for i = 1:n]

    deps = []
    problematic = []

    # random dependencies
    for i = 1:m, j = 1:length(allvers[i])
        if i ≤ n && (planted1[i] == j || planted2[i] == j)
            if j == planted1[i]
                if i < n
                    push!(deps, [pn(i), allvers[i][j], pn(i+1), "$(allvers[i+1][planted1[i+1]])-*"])
                else
                    if !sat
                        push!(deps, [pn(i), allvers[i][j], pn(1), "0-$(allvers[1][planted2[1]])"])
                    else
                        push!(deps, [pn(i), allvers[i][j], pn(1), "0-*"])
                    end
                end
            else # j == planted2[i]
                if i < n
                    push!(deps, [pn(i), allvers[i][j], pn(i+1), "0-$(allvers[i+1][planted2[i+1]])"])
                else
                    if !sat
                        push!(deps, [pn(i), allvers[i][j], pn(1), "$(allvers[1][planted1[1]])-*"])
                    else
                        push!(deps, [pn(i), allvers[i][j], pn(1), "0-*"])
                    end
                end
            end
            sat || push!(problematic, [pn(i), allvers[i][j]])
            continue
        end

        s = shuffle([1:(i-1); (i+1):m])[1:min(d,m-1)]
        for a in s
            push!(deps, [pn(i), allvers[i][j], pn(a), randvspec(k)])
        end
        push!(problematic, [pn(i), allvers[i][j]])
    end

    reqs = [[pn(1), "*"]]
    # reqs = [[pn(1), string(allvers[1][planted1[1]])]]

    # info("SOLUTION: $([(i,planted1[i]) for i = 1:n])")
    # info("REST: $([(i,length(allvers[i])+1) for i = (n+1):m])")

    want = Dict(pn(i) => allvers[i][planted1[i]] for i = 1:n)

    return deps, reqs, want, problematic
end

end # module
