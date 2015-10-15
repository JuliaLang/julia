# This file is a part of Julia. License is MIT: http://julialang.org/license

include("choosetests.jl")
tests, net_on = choosetests(ARGS)
tests = unique(tests)

# Base.compile only works from node 1, so compile test is handled specially
compile_test = "compile" in tests
if compile_test
    splice!(tests, findfirst(tests, "compile"))
end

cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(8, CPU_CORES, length(tests))
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes --depwarn=error`)
        blas_set_num_threads(1)
    end

    @everywhere include("testdefs.jl")

    results=[]
    if haskey(ENV, "JULIA_TEST_MAXRSS_MB")
        max_worker_rss = parse(Int, ENV["JULIA_TEST_MAXRSS_MB"]) * 2^20
    else
        max_worker_rss = typemax(Csize_t)
    end
    println(max_worker_rss)
    @sync begin
        for p in workers()
            @async begin
                while length(tests) > 0
                    test = shift!(tests)
                    local resp
                    try
                        resp = remotecall_fetch(t -> runtests(t), p, test)
                    catch e
                        resp = e
                    end
                    push!(results, (test, resp))

                    if (isa(resp, Integer) && (resp > max_worker_rss)) || isa(resp, Exception)
                        if n > 1
                            rmprocs(p, waitfor=0.5)
                            p = addprocs(1; exeflags=`--check-bounds=yes --depwarn=error`)[1]
                            remotecall_fetch(()->include("testdefs.jl"), p)
                        else
                            # single process testing, bail if mem limit reached, or, on an exception.
                            isa(resp, Exception) ? rethrow(resp) : error("Halting tests. Memory limit reached : $resp > $max_worker_rss")
                        end
                    end
                end
            end
        end
    end

    errors = filter(x->isa(x[2], Exception), results)
    if length(errors) > 0
        for err in errors
            println("Exception running test $(err[1]) :")
            showerror(STDERR, err[2])
            println()
        end
        error("Some tests exited with errors.")
    end

    if compile_test
        n > 1 && print("\tFrom worker 1:\t")
        runtests("compile")
    end

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
    println("    \033[32;1mSUCCESS\033[0m")
end
