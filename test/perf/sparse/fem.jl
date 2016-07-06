# This file is a part of Julia. License is MIT: http://julialang.org/license

## FEM benchmark from https://github.com/JuliaLang/julia/issues/9668

# assemble the finite-difference laplacian
function fdlaplacian(N)
    # create a 1D laplacian and a sparse identity
    fdl1 = spdiagm((ones(N-1),-2*ones(N),ones(N-1)), [-1,0,1])
    # laplace operator on the full grid
    return kron(speye(N), fdl1) + kron(fdl1, speye(N))
end

# get the list of boundary dof-indices
function get_free(N)
    L = zeros(Int, N, N)
    L[2:N-1, 2:N-1] = 1
    return find(L)
end

# timing of assembly, slice and solve
function run_fem(N)
    Ifree = get_free(N)
    # assembly
    A = fdlaplacian(N)
    # boundary condition
    B = A[Ifree, Ifree]
    # solver
    lufact(B)
end

function fem_perf()
    # run tests once to compile
    run_fem(10)

    # run
    @timeit run_fem(256) "finite_elements" ""

    # runs the tests
    #NN = 2.^(3:8)
    #TT = zeros(3, length(NN))
    #@printf("(All times are seconds)\n")
    #@printf("     N   |   assembly |   slice   |   lufact  | slice / N^4 \n")
    #@printf("---------|------------|-----------|-----------|-------------\n")
    #for n = 1:length(NN)
    #    t1, t2, t3 = timings(NN[n])
    #    @printf(" %4.1e |  %4.2e  | %4.2e  |  %4.2e |   %4.2e \n",
    #            NN[n], t1, t2, t3, t2 / NN[n]^4)
    #end
end
