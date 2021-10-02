# This file is a part of Julia. License is MIT: https://julialang.org/license

const pthread_t = Culong
const uv_thread_t = pthread_t

function uv_thread_getaffinity()
    masksize = ccall(:uv_cpumask_size, Cint, ())
    self = ccall(:uv_thread_self, uv_thread_t, ())
    selfref = Ref(self)
    cpumask = zeros(Cchar, masksize)
    err = ccall(
        :uv_thread_getaffinity,
        Cint,
        (Ptr{uv_thread_t}, Ptr{Cchar}, Cssize_t),
        selfref,
        cpumask,
        masksize,
    )
    @assert err == 0
    n = findlast(isone, cpumask)
    resize!(cpumask, n)
    return cpumask
end

function print_process_affinity()
    isfirst = true
    for (i, m) in enumerate(uv_thread_getaffinity())
        if m != 0
            if isfirst
                isfirst = false
            else
                print(",")
            end
            print(i)
        end
    end
    println()
end

if Base.Filesystem.samefile(PROGRAM_FILE, @__FILE__)
    print_process_affinity()
end
