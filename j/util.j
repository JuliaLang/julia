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

function _toc(noisy)
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

qtoc() = _toc(false)
toc()  = _toc(true)

macro qtime(ex); :(tic(); $ex; qtoc()); end
macro time(ex); :(tic(); $ex; toc()); end

function peakflops()

    a = rand(2000,2000)
    tic(); a*a; qtoc();
    tic(); a*a; t=qtoc();

    floprate = (2 * 2000. ^ 3 / t)

    println("The peak flop rate is ", floprate*1e-9, " gigaflops")

    return floprate

end
