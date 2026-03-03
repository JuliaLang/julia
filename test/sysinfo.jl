# This file is a part of Julia. License is MIT: https://julialang.org/license

# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()

@test Base.libllvm_path() isa Symbol
@test contains(String(Base.libllvm_path()), "LLVM")

@test length(ccall(:jl_get_cpu_name, String, ())) != 0
@test length(ccall(:jl_get_cpu_features, String, ())) >= 0
foo_fma() = Core.Intrinsics.have_fma(Int64)
@test ccall(:jl_cpu_has_fma, Bool, (Cint,), 64) == foo_fma()

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
            if Libc.geteuid() == 0
                # Root bypasses permission checks
                @test abspath(Base.Sys.which("foo")) == abspath(joinpath(firstdir, "foo"))
            else
                @test abspath(Base.Sys.which("foo")) == abspath(joinpath(seconddir, "foo"))
            end
        finally
            # clean up
            chmod(firstdir, 0o777)
            ENV["PATH"] = original_path
        end
    end
end

@testset "username()" begin
    if Sys.isunix()
        passwd = Libc.getpwuid(Libc.getuid())
        @test Sys.username() == passwd.username
    elseif Sys.iswindows()
        @test Sys.username() == ENV["USERNAME"]
    else
        @test !isempty(Sys.username())
    end
end

@testset "Base.Sys docstrings" begin
    @test isempty(Docs.undocumented_names(Sys))
end

@testset "show" begin
    example_cpus = [Base.Sys.CPUinfo("Apple M1 Pro", 2400, 0x000000000d913b08, 0x0000000000000000, 0x0000000005f4243c, 0x00000000352a550a, 0x0000000000000000)
    Base.Sys.CPUinfo("Apple M1 Pro", 2400, 0x000000000d9040c2, 0x0000000000000000, 0x0000000005d4768c, 0x00000000356b3d22, 0x0000000000000000)
    Base.Sys.CPUinfo("Apple M1 Pro", 2400, 0x00000000026784da, 0x0000000000000000, 0x0000000000fda30e, 0x0000000046a731ea, 0x0000000000000000)
    Base.Sys.CPUinfo("Apple M1 Pro", 2400, 0x00000000017726c0, 0x0000000000000000, 0x00000000009491de, 0x0000000048134f1e, 0x0000000000000000)]

    @test repr(example_cpus[1]) == "Base.Sys.CPUinfo(\"Apple M1 Pro\", 2400, 0x000000000d913b08, 0x0000000000000000, 0x0000000005f4243c, 0x00000000352a550a, 0x0000000000000000)"
    @test repr("text/plain", example_cpus[1]) == "Apple M1 Pro: \n        speed         user         nice          sys         idle          irq\n     2400 MHz     227622 s          0 s      99886 s     891967 s          0 s  "
    @test sprint(Sys.cpu_summary, example_cpus) == "Apple M1 Pro: \n       speed         user         nice          sys         idle          irq\n#1  2400 MHz     227622 s          0 s      99886 s     891967 s          0 s  \n#2  2400 MHz     227558 s          0 s      97810 s     896220 s          0 s  \n#3  2400 MHz      40339 s          0 s      16622 s    1185362 s          0 s  \n#4  2400 MHz      24586 s          0 s       9737 s    1209225 s          0 s  \n"
end
