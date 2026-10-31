# Test that a second *non-privatized* library loaded into the same process
# throws an informative error.
using Test

outdir = ARGS[1]

exe_suffix = splitext(Base.julia_exename())[2]
dlext = Base.BinaryPlatforms.platform_dlext()
driver = joinpath(outdir, "bin", "twoimages" * exe_suffix)
liba = joinpath(outdir, "twoimages_a." * dlext)
libb = joinpath(outdir, "twoimages_b." * dlext)

const SIGABRT = 6
const MESSAGE = "a Julia runtime is already initialized in this process with image"

function run_driver(mode)
    cmd = `$driver $mode $liba $libb`
    if Sys.iswindows()
        # Windows resolves libjulia through PATH rather than a run path.
        cmd = addenv(cmd, "PATH" => string(Sys.BINDIR, ';', get(ENV, "PATH", "")))
    end
    errfile = tempname()
    proc = run(pipeline(ignorestatus(cmd), stdout=devnull, stderr=errfile))
    err = read(errfile, String)
    rm(errfile, force=true)
    return proc, err
end

@testset "TwoImages" begin
    # One library on its own initializes the runtime and returns normally.
    proc, err = run_driver("solo")
    @test success(proc)
    @test isempty(err)

    # Entering the second image aborts, whether or not the calling thread has
    # already been adopted by the runtime.
    @testset "$mode" for mode in ("samethread", "newthread")
        proc, err = run_driver(mode)
        @test !success(proc)
        if !Sys.iswindows()
            @test Base.process_signaled(proc) && proc.termsignal == SIGABRT
        end
        # The message names the image that owns the runtime first, then the one
        # that cannot be initialized.
        i = findfirst(MESSAGE, err)
        @test i !== nothing
        if i !== nothing
            j = findnext("twoimages_a." * dlext, err, last(i))
            @test j !== nothing
            @test j !== nothing && findnext("twoimages_b." * dlext, err, last(j)) !== nothing
        end
    end
end
