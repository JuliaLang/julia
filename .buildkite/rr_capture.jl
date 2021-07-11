using Dates
using Pkg
using Tar

if Base.VERSION < v"1.6"
    throw(ErrorException("The `rr_capture.jl` script requires Julia 1.6 or greater"))
end

if length(ARGS) < 1
    throw(ErrorException("Usage: rr_capture.jl [command...]"))
end

const TIMEOUT = 2 * 60 * 60 # timeout in seconds

# We only use `rr` on the `tester_linux64` builder
const use_rr_if_builder_is = "tester_linux64"

const run_id = get(ENV, "BUILDKITE_JOB_ID", "unknown")
const shortcommit = get(ENV, "BUILDKITE_COMMIT", "unknown")
const builder = get(ENV, "BUILDKITE_STEP_KEY", use_rr_if_builder_is)
const use_rr = builder == use_rr_if_builder_is

@info "" run_id shortcommit builder use_rr
@info "" ARGS

# if !use_rr # TODO: uncomment this line
if true # TODO: delete this line
    @info "We will not run the tests under rr"
    p = run(`$ARGS`)
    exit(p.exitcode)
end

@info "We will run the tests under rr"

const num_cores = min(Sys.CPU_THREADS, 8, parse(Int, get(ENV, "JULIA_TEST_NUM_CORES", "8")) + 1)
@info "" num_cores

proc = nothing

new_env = copy(ENV)
mktempdir() do dir
    Pkg.activate(dir)
    Pkg.add("rr_jll")
    Pkg.add("Zstd_jll")

    rr_jll = Base.require(Base.PkgId(Base.UUID((0xe86bdf43_55f7_5ea2_9fd0_e7daa2c0f2b4)), "rr_jll"))
    zstd_jll = Base.require(Base.PkgId(Base.UUID((0x3161d3a3_bdf6_5164_811a_617609db77b4)), "Zstd_jll"))
    rr(func) = Base.invokelatest(rr_jll.rr, func; adjust_LIBPATH=false)
    rr() do rr_path
        capture_script_path = joinpath(dir, "capture_output.sh")
        loader = Sys.WORD_SIZE == 64 ? "/lib64/ld-linux-x86-64.so.2" : "/lib/ld-linux.so.2"
        open(capture_script_path, "w") do io
            write(io, """
            #!/bin/bash

            $(rr_path) record --nested=detach "\$@" > >(tee -a $(dir)/stdout.log) 2> >(tee -a $(dir)/stderr.log >&2)
            """)
        end
        chmod(capture_script_path, 0o755)

        new_env = copy(ENV)
        new_env["_RR_TRACE_DIR"] = joinpath(dir, "rr_traces")
        new_env["RR_LOG"]="all:debug"
        new_env["RR_LOG_BUFFER"]="100000"
        new_env["JULIA_RR"] = capture_script_path
        t_start = time()
        global proc = run(setenv(`$(rr_path) record --num-cores=$(num_cores) $ARGS`, new_env), (stdin, stdout, stderr); wait=false)

        # Start asynchronous timer that will kill `rr`
        @async begin
            sleep(TIMEOUT)

            # If we've exceeded the timeout and `rr` is still running, kill it.
            if isopen(proc)
                println(stderr, "\n\nProcess timed out. Signalling `rr` for force-cleanup!")
                kill(proc, Base.SIGTERM)

                # Give `rr` a chance to cleanup
                sleep(60)

                if isopen(proc)
                    println(stderr, "\n\n`rr` failed to cleanup within one minute, killing and exiting immediately!")
                    kill(proc, Base.SIGKILL)
                    exit(1)
                end
            end
        end

        # Wait for `rr` to finish, either through naturally finishing its run, or `SIGTERM`.
        # If we have to `SIGKILL`
        wait(proc)

        # On a non-zero exit code, upload the `rr` trace
        if !success(proc)
            println(stderr, "`rr` returned $(proc.exitcode), packing and uploading traces...")

            if !isdir(joinpath(dir, "rr_traces"))
                println(stderr, "No `rr_traces` directory!  Did `rr` itself fail?")
                exit(1)
            end

            # Clean up non-traces
            rm(joinpath(dir, "rr_traces", "latest-trace"))
            rm(joinpath(dir, "rr_traces", "cpu_lock"))

            # Create a directory for the pack files to go
            pack_dir = joinpath(dir, "pack")
            mkdir(pack_dir)

            # Pack all traces
            trace_dirs = [joinpath(dir, "rr_traces", f) for f in readdir(joinpath(dir, "rr_traces"))]
            filter!(isdir, trace_dirs)
            run(ignorestatus(`$(rr_path) pack --pack-dir=$pack_dir $(trace_dirs)`))

            # Tar it up
            mkpath("dumps")
            datestr = Dates.format(now(), dateformat"yyyy-mm-dd_HH_MM_SS")
            dst_path = "dumps/rr-run_$(run_id)-gitsha_$(shortcommit)-$(datestr).tar.zst"
            zstd_jll.zstdmt() do zstdp
                tarproc = open(`$zstdp -o $dst_path`, "w")
                Tar.create(dir, tarproc)
                close(tarproc.in)
            end
        end
    end
end

# Pass the exit code back up to Buildkite
if proc.termsignal != 0
    ccall(:raise, Cvoid, (Cint,), proc.termsignal)
    exit(1) # Just in case the signal did not cause an exit
else
    exit(proc.exitcode)
end
