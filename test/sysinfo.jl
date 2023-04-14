# This file is a part of Julia. License is MIT: https://julialang.org/license

# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()

@test Base.libllvm_path() isa Symbol
@test contains(String(Base.libllvm_path()), "LLVM")

if Sys.isunix()
    mktempdir() do tempdir
        firstdir = joinpath(tempdir, "first")
        seconddir = joinpath(tempdir, "second")

        mkpath(firstdir)
        mkpath(seconddir)

        touch(joinpath(firstdir, "foo"))
        touch(joinpath(seconddir, "foo"))

        chmod(joinpath(firstdir, "foo"), 0o777)
        chmod(joinpath(seconddir, "foo"), 0o777)

        # zero permissions on first directory
        chmod(firstdir, 0o000)

        original_path = ENV["PATH"]
        ENV["PATH"] = string(firstdir, ":", seconddir, ":", original_path)
        try
            @test abspath(Base.Sys.which("foo")) == abspath(joinpath(seconddir, "foo"))
        finally
            # clean up
            chmod(firstdir, 0o777)
            ENV["PATH"] = original_path
        end
    end
end
