using Pkg, Dates

if length(ARGS) < 3
    println(stderr, "Usage: rr_capture.jl [buildnumber] [shortcommit] [command...]")
    exit(2)
end

const TIMEOUT = 2*60*60 # seconds

run_id = popfirst!(ARGS)
shortcommit = popfirst!(ARGS)
num_cores = min(Sys.CPU_THREADS, 8, parse(Int, get(ENV, "JULIA_TEST_NUM_CORES", "8")))

new_env = copy(ENV)
mktempdir() do dir
    Pkg.activate(dir)
    Pkg.add("rr_jll")

    rr_jll = Base.require(Base.PkgId(Base.UUID((0xe86bdf43_55f7_5ea2_9fd0_e7daa2c0f2b4)), "rr_jll"))
    rr(func) = Base.invokelatest(rr_jll.rr, func; adjust_LIBPATH=false)
    rr() do rr_path
        capture_script_path = joinpath(dir, "capture_output.sh")
        loader = Sys.WORD_SIZE == 64 ? "/lib64/ld-linux-x86-64.so.2" : "/lib/ld-linux.so.2"
        open(capture_script_path, "w") do io
            write(io, """
            #!/bin/bash

            $(loader) --library-path "$(rr_jll.LIBPATH)" $(rr_path) record --nested=detach \$* > >(tee -a $(dir)/stdout.log) 2> >(tee -a $(dir)/stderr.log >&2)
            """)
        end
        chmod(capture_script_path, 0o755)

        new_env = copy(ENV)
        new_env["_RR_TRACE_DIR"] = joinpath(dir, "rr_traces")
        new_env["JULIA_RR"] = capture_script_path
        t_start = time()
        proc = run(setenv(`$(loader) --library-path "$(rr_jll.LIBPATH)" $(rr_path) record --num-cores=$(num_cores + 1) $ARGS`, new_env), (stdin, stdout, stderr); wait=false)

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
        if proc.exitcode != 0
            println(stderr, "`rr` returned $(proc.exitcode), packing and uploading traces...")

            if !isdir(joinpath(dir, "rr_traces"))
                println(stderr, "No `rr_traces` directory!  Did `rr` itself fail?")
                exit(1)
            end

            # Pack all traces
            trace_dirs = [joinpath(dir, "rr_traces", f) for f in readdir(joinpath(dir, "rr_traces"))]
            filter!(isdir, trace_dirs)
            for trace_dir in trace_dirs
                println(stderr, " -> packing $(basename(trace_dir))")
                run(ignorestatus(`$(loader) --library-path "$(rr_jll.LIBPATH)" $(rr_path) pack $(trace_dir)`))
            end

            # Tar it up
            mkpath("dumps")
            datestr = Dates.format(now(), dateformat"yyyy-mm-dd_HH_MM_SS")
            Pkg.PlatformEngines.package(dir, "dumps/rr-run_$(run_id)-gitsha_$(shortcommit)-$(datestr).tar.gz")
        end

        # Pass the exit code back up to buildbot
        exit(proc.exitcode)
    end
end
