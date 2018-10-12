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
        inline_flag = Base.JLOptions().can_inline == 1 ? `` : `--inline=no`
        cov_flag = ``
        if Base.JLOptions().code_coverage == 1
            cov_flag = `--code-coverage=user`
        elseif Base.JLOptions().code_coverage == 2
            cov_flag = `--code-coverage=all`
        end
        const test_exeflags = `$cov_flag $inline_flag --check-bounds=yes --startup-file=no --depwarn=error`
    end

    if haskey(ENV, "JULIA_TEST_EXENAME")
        const test_exename = `$(Base.shell_split(ENV["JULIA_TEST_EXENAME"]))`
    else
        const test_exename = `$(joinpath(Sys.BINDIR, Base.julia_exename()))`
    end

    addprocs_with_testenv(X; kwargs...) = addprocs(X; exename=test_exename, exeflags=test_exeflags, kwargs...)

    const curmod = @__MODULE__
    const curmod_name = fullname(curmod)
    const curmod_str = curmod === Main ? "Main" : join(curmod_name, ".")
    const curmod_prefix = "$(["$m." for m in curmod_name]...)"
end
