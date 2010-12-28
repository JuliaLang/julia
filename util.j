# shell

ls() = system("ls")

catfile(file::String) = system(strcat("cat ", file))

# timing

clock() = ccall(dlsym(JuliaDLHandle,"clock_now"), Float64, ())

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
