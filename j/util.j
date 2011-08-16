# shell

ls() = system("ls")

catfile(file::String) = system(strcat("cat ", file))

# timing

clock() = ccall(:clock_now, Float64, ())

function tic()
    t0 = clock()
    tls(:TIMERS, (t0, get(tls(), :TIMERS, ())))
    return t0
end

function toc(noisy::Bool)
    t1 = clock()
    timers = get(tls(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]
    tls(:TIMERS, timers[2])
    t = t1-t0
    if noisy
        print("elapsed time: ", t, " sec\n")
    end
    t
end

toc() = toc(true)
toq() = toc(false)

# prints nothing
# returns elapsed time
macro elapsed(ex)
    x = gensym()
    :(local $x; tic(); $x = $ex; toq())
end

# prints elapsed time
# returns expression value
macro time(ex)
    x = gensym()
    :(local $x; tic(); $x = $ex; toc(); $x)
end

function peakflops()
    a = rand(2000,2000)
    t = @elapsed a*a
    t = @elapsed a*a
    floprate = (2*2000.0^3/t)
    println("The peak flop rate is ", floprate*1e-9, " gigaflops")
    floprate
end

macro benchmark(n,ex,T)
    s = gensym()
    quote
        local $s
        @time for i=1:long($n)
            x = convert($T,i)
            $s = $ex
        end
        $s
    end
end
