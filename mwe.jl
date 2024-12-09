using StatsBase
function Simulate()
    Simulations=Int(1e7)
    Size=1000
    result = Array{Float64}(undef, Simulations, 1)
    Threads.@threads for i = 1:Simulations
         x = randn(Size)
         s = sort(x)
        result[i, 1] = s[1]
    end
    println(median(result))
end
# GC.enable_logging(true)
for i in 1:1000
    println(i)
    Simulate()
    GC.gc(true)
    @ccall jl_print_mallocd_arrays_stats()::Cvoid
    @ccall jl_print_big_obj_list_stats()::Cvoid
    @ccall jl_print_pages_stats()::Cvoid
    # Print live_bytes
    println("live_bytes in MB: ", Base.gc_live_bytes() / 1024^2)
    sleep(10) # sleep for 10 seconds
end