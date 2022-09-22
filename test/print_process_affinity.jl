# This file is a part of Julia. License is MIT: https://julialang.org/license

const uv_thread_t = UInt # TODO: this is usually correct (or tolerated by the API), but not guaranteed

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
