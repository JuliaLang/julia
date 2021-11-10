# This file is a part of Julia. License is MIT: https://julialang.org/license

const pthread_t = Culong # TODO: this is wrong, but usually tolerable by the ABI
const uv_thread_t = pthread_t

function uv_thread_getaffinity()
    masksize = ccall(:uv_cpumask_size, Cint, ())
    self = ccall(:uv_thread_self, uv_thread_t, ())
    ref = Ref(self)
    cpumask = zeros(Bool, masksize)
    err = ccall(
        :uv_thread_getaffinity,
        Cint,
        (Ref{uv_thread_t}, Ptr{Bool}, Cssize_t),
        ref,
        cpumask,
        masksize,
    )
    Base.uv_error("getaffinity", err)
    n = something(findlast(cpumask)) # we must have at least one active core
    resize!(cpumask, n)
    return cpumask
end

function print_process_affinity()
    join(stdout, findall(uv_thread_getaffinity()), ",")
    println()
end

if Base.Filesystem.samefile(PROGRAM_FILE, @__FILE__)
    print_process_affinity()
end
