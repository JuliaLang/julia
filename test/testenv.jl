# This file is a part of Julia. License is MIT: https://julialang.org/license

# This includes a few helper variables and functions that provide information about the
# test environment (command line flags, current module, etc).
# This file can be included multiple times in the same module if necessary,
# which can happen with unisolated test runs.

if !@isdefined(testenv_defined)
    const testenv_defined = true
    if haskey(ENV, "JULIA_TEST_EXEFLAGS")
        const test_exeflags = `$(Base.shell_split(ENV["JULIA_TEST_EXEFLAGS"]))`
    else
        const test_exeflags = Base.julia_cmd()
        filter!(test_exeflags.exec) do c
            return !(startswith(c, "--depwarn") || startswith(c, "--check-bounds"))
        end
        push!(test_exeflags.exec, "--check-bounds=yes")
        push!(test_exeflags.exec, "--startup-file=no")
        push!(test_exeflags.exec, "--depwarn=error")
    end
    if haskey(ENV, "JULIA_TEST_EXTRA_EXEFLAGS")
        append!(test_exeflags.exec, Base.shell_split(ENV["JULIA_TEST_EXTRA_EXEFLAGS"]))
    end

    if haskey(ENV, "JULIA_TEST_EXENAME")
        popfirst!(test_exeflags.exec)
        const test_exename = `$(Base.shell_split(ENV["JULIA_TEST_EXENAME"]))`
    else
        const test_exename = popfirst!(test_exeflags.exec)
    end

    if haskey(ENV, "JULIA_RR")
        const rr_exename = `$(Base.shell_split(ENV["JULIA_RR"]))`
    else
        const rr_exename = ``
    end

    const test_relocated_depot = haskey(ENV, "RELOCATEDEPOT")

    # Each worker gets an equal share of the machine's memory as its heap size hint, sized
    # for a pool of `pool_size` workers. A worker that replaces a recycled one is started
    # on its own, so it must pass the size of the pool it joins: sized for a pool of one,
    # it would get half the machine's memory.
    function addprocs_with_testenv(X; rr_allowed=true, pool_size=X, kwargs...)
        exename = rr_allowed ? `$rr_exename $test_exename` : test_exename
        exeflags = test_exeflags
        if pool_size isa Integer
            heap_size = round(Int, Sys.total_memory() / 2^20 / (pool_size + 1))
            exeflags = `$exeflags --heap-size-hint=$(heap_size)M`
        end
        addprocs(X; exename=exename, exeflags=exeflags, kwargs...)
    end

    const curmod = @__MODULE__
    const curmod_name = fullname(curmod)
    const curmod_str = curmod === Main ? "Main" : join(curmod_name, ".")
    const curmod_prefix = curmod === Main ? "" : "$(["$m." for m in curmod_name]...)"

    # platforms that support cfunction with closures
    # (requires LLVM back-end support for trampoline intrinsics)
    const cfunction_closure = Sys.ARCH === :x86_64 || Sys.ARCH === :i686

    macro async_logerr(expr)
        :(@async try
            $(esc(expr))
        catch err
            @error("An async task failed", exception=(err, catch_backtrace()))
        end)
    end
end
