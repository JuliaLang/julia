using Iterators, HDF5, JLD, NLopt

const LAPACK = Base.LinAlg.LAPACK
const BLAS = Base.LinAlg.BLAS

# T[n, i1, i2, ...] is the execution time of an algorithm
# with nthreads[n] threads and parameter setting p1[i1], p2[i2], etc.

function perf_run(func::Function, nthreads, P::Tuple, constructor::Function)
    l = map(length, P)
    T = Array(Float64, length(nthreads), l...)
    t = zeros(3)
    pindex = map(x->1:length(x), P)
    for i in product(pindex...)
        cp = [P[j][i[j]] for j = 1:length(P)]
        args = constructor(cp...)
        argscopy = ntuple(length(args), i->copy(args[i]))
        func(argscopy...)  # run once to force a compile
        gc()
        gc_disable()
        try
            for n = 1:length(nthreads)
                blas_set_num_threads(nthreads[n])
                for iter = 1:length(t)
                    for k = 1:length(args); copy!(argscopy[k], args[k]); end
                    t[iter] = @elapsed func(argscopy...)
                    if t[iter] < 1e-3
                        nrep = min(10, iceil(1e-3/t[iter]))
                        for r = 2:nrep
                            for k = 1:length(args); copy!(argscopy[k], args[k]); end
                            t[iter] += @elapsed func(argscopy...)
                        end
                        t[iter] = t[iter]/nrep
                    end
                end
                T[n,i...] = median(t)
            end
        finally
            gc_enable()
        end
    end
    T
end

function perf_measure1(f::Function, fid, name, nprocs, P, constructor::Function)
    println("Measuring ", name)
    g_create(fid, name) do g
        write(g, "nprocs", nprocs)
        write(g, "P", P)
        T = perf_run(f, nprocs, P, constructor)
        write(g, "T", T)
    end
end

function perf_measure()
    len = int(2.^([2:20]/2))
    nprocs = 1:4
    jldopen("blas_calibrate.jld", "w") do fid
        perf_measure1(A->LAPACK.geevx!('B','N','N','N',A), fid, "geevx!", nprocs, (len,), n->(rand(n,n),))
        perf_measure1((C,A,B)->BLAS.gemm!('N','N',1.0,A,B,0.0,C), fid, "gemm!", nprocs, (len,len,len), (m,k,n)->(Array(Float64,m,n),rand(m,k),rand(k,n)))
    end
    nothing
end

### Write the tuning file
# Optimize a model
#     nthreads = c0 + c1*log(p1) + c2*log(p2) + ...
# to yield the best overall performance, and then
# record the paramaters as a callable function to
# base/linalg/blas_tune.jl
function tune()
    open(joinpath(JULIA_HOME, "../../base/linalg/blas_tune.jl"),"w") do fileout
        write(fileout, """
function blas_nthreads(coefs, p...)
    np = coefs[1]
    for i = 1:length(p)
        np += coefs[i+1]*log(p[i])
    end
    min(CPU_CORES, max(1, iround(np)))
end
""")
        jldopen("blas_calibrate.jld") do filein
            tune1(fileout, filein, "geevx!")
            tune1(fileout, filein, "gemm!")
        end
    end
    nothing
end

function tune1(fileout, filein, name)
    println("Optimizing ", name)
    g = filein[name]
    try
        T = read(g, "T")
        np = read(g, "nprocs")
        P = read(g, "P")
        minf, minx, ret = perf_optimize1(T, np, P)
        if ret == :FTOL_REACHED || ret == :XTOL_REACHED
            write(fileout, """
$(name)_nthreads(p) = blas_nthreads([""")
            for i = 1:length(minx)
                print(fileout, minx[i])
                if i < length(minx)
                    write(fileout, ',')
                end
            end
            write(fileout, """], p...)
""")
        else
            error("Failed to converge")
        end
    finally
        close(g)
    end
end

function perf_optimize1(T, nprocs, P)
    Tnorm = T ./ minimum(T, 1)
    nd = length(P)+1
    opt = Opt(:GN_DIRECT_L, nd)
    mnp = maximum(nprocs)
    bounds = vcat(float(mnp),Float64[mnp/log(maximum(p)) for p in P])
    lower_bounds!(opt, -bounds)
    upper_bounds!(opt, bounds)
    min_objective!(opt, (x,g)->perf_objective(x, Tnorm, nprocs, P))
    xtol_abs!(opt, bounds/100)
    ftol_rel!(opt, 1e-5)
    stopval!(opt, 0)
    minf, minx, ret = optimize(opt, 2*bounds.*(rand(nd)-0.5))
end

function perf_objective(x::Vector, Tnorm, nprocs, P)
    if length(x) != length(P)+1
        error("x has $(length(x)) parameters, but given the parameter list it should be $(length(P)+1)")
    end
    logP = {reshape(log(P[d]), ntuple(d, i->i==d?length(P[d]):1)) for d = 1:length(P)}
    np = x[1]
    for i = 1:length(x)-1
        np = np .+ x[i+1]*logP[i]
    end
    # since np won't be inferrable, call a separate function
    # but bias parameters to zero if they don't matter
    val = perf_val(np, Tnorm, nprocs) + 0.001*(abs(x[1]-1) + sum(abs(x[2:end])))
    return val
end

function perf_val(np, Tnorm, nprocs)
    # Find the nearest entry in nprocs for each entry of np
    dist = zeros(length(nprocs))
    Tindex = similar(np, Int)
    for i = 1:length(np)
        thisnp = np[i]
        for j = 1:length(dist)
            dist[j] = abs(nprocs[j]-thisnp)
        end
        Tindex[i] = indmin(dist)
    end
    # Look up the execution time
    val = zero(eltype(Tnorm))
    for i = 1:div(length(Tnorm), size(Tnorm,1))
        val += Tnorm[Tindex[i],i]
    end
    val - length(np) + 1
end
