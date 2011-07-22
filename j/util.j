# shell

ls() = system("ls")

catfile(file::String) = system(strcat("cat ", file))

# timing

clock() = ccall(:clock_now, Float64, ())

_TIMERS = ()

function tic()
    t0 = clock()
    global _TIMERS = (t0, _TIMERS)
    return t0
end

function toc(noisy::Bool)
    t1 = clock()
    global _TIMERS
    if is(_TIMERS,())
        error("toc() without tic()")
    end
    t0 = _TIMERS[1]
    _TIMERS = _TIMERS[2]
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
