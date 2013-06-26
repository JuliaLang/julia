testnames = ["core", "keywordargs", "numbers", "strings", "unicode",
             "collections", "hashing", "remote", "iostring", "arrayops",
             "linalg", "blas", "fft", "dsp", "sparse", "bitarray",
             "random", "math", "functional", "bigint", "sorting",
             "statistics", "spawn", "parallel", "priorityqueue",
             "arpack", "file", "perf", "suitesparse", "version",
             "resolve", "pollfd", "mpfr", "broadcast", "complex",
             "socket", "floatapprox", "readdlm"]

tests = ARGS==["all"] ? testnames : ARGS
n = min(8, CPU_CORES, length(tests))
@unix_only n > 1 && addprocs(n)

ENV["OPENBLAS_NUM_THREADS"] = 1

@everywhere include("testdefs.jl")

reduce(propagate_errors, nothing, pmap(runtests, tests))

@unix_only begin
# Some tests for multiple process sets can only be executed from pid 1, since
# addprocs and rmprocs are only valid on 1.
    
    function testws(ws) 
        pmap_resp = pmap_procs(ws.idset, x -> myid(), 1:length(ws.idset))
        
        @test sum(ws.idset) == sum(pmap_resp)
        
        parallel_resp = @parallel ws.idset (+) for i=1:(length(ws.idset)*5)
            myid()
        end
        
        @test parallel_resp == (sum(ws.idset) * 5)
        
        v1 = remotecall_fetch(ws, myid)
        @test contains(ws.idset, v1)
        v2 = remotecall_fetch(ws, myid)
        @test contains(ws.idset, v2)
        @test v1 != v2
    end
    
    # remove all workers
    rmprocs(workers())

    np1 = nprocs()
    
    addprocs(5)
    w = workers()
    
    ws1 = WorkerSet(w[1:2])
    
    testws(ws1)
    
    ws2 = WorkerSet(w[3:5])
    testws(ws2)

    np2 = nprocs()
    @test np2 == (np1 + 5)
    

    # test regular pmap and @parallel with all processes.
    pmap_resp = pmap(x -> myid(), 1:nprocs())
    defprocs = procs()
    for i in pmap_resp
        @test contains(defprocs, i)
    end
    
    rmprocs(workers())
    np3 = nprocs()
    
    @test np1 == np3
end

@unix_only n > 1 && rmprocs(workers())
println("    \033[32;1mSUCCESS\033[0m")
