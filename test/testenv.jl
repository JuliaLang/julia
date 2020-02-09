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

    if haskey(ENV, "JULIA_TEST_EXENAME")
        popfirst!(test_exeflags.exec)
        const test_exename = `$(Base.shell_split(ENV["JULIA_TEST_EXENAME"]))`
    else
        const test_exename = popfirst!(test_exeflags.exec)
    end

    addprocs_with_testenv(X; kwargs...) = addprocs(X; exename=test_exename, exeflags=test_exeflags, kwargs...)

    const curmod = @__MODULE__
    const curmod_name = fullname(curmod)
    const curmod_str = curmod === Main ? "Main" : join(curmod_name, ".")
    const curmod_prefix = "$(["$m." for m in curmod_name]...)"

    # platforms that support cfunction with closures
    # (requires LLVM back-end support for trampoline intrinsics)
    const cfunction_closure = Sys.ARCH === :x86_64 || Sys.ARCH === :i686
end
