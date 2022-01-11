import Dates
import Pkg
import Tar

function get_bool_from_env(name::AbstractString, default_value::Bool)
    value = get(ENV, name, "$(default_value)") |> strip |> lowercase
    result = parse(Bool, value)::Bool
    return result
end

const is_buildkite         = get_bool_from_env("BUILDKITE",                  false)
const always_save_rr_trace = get_bool_from_env("JULIA_ALWAYS_SAVE_RR_TRACE", false)

function get_from_env(name::AbstractString)
    if is_buildkite
        value = ENV[name]
    else
        value = get(ENV, name, "")
    end
    result = convert(String, strip(value))::String
    return result
end

function my_exit(process::Base.Process)
    wait(process)

    @info(
        "",
        process.exitcode,
        process.termsignal,
    )

    # Pass the exit code back up
    if process.termsignal != 0
        ccall(:raise, Cvoid, (Cint,), process.termsignal)

        # If for some reason the signal did not cause an exit, we'll exit manually.
        # We need to make sure that we exit with a non-zero exit code.
        if process.exitcode != 0
            exit(process.exitcode)
        else
            exit(1)
        end
    end
    exit(process.exitcode)
end

if Base.VERSION < v"1.6"
    throw(ErrorException("The `$(basename(@__FILE__))` script requires Julia 1.6 or greater"))
end

if length(ARGS) < 1
    throw(ErrorException("Usage: julia $(basename(@__FILE__)) [command...]"))
end

@info "We will run the command under rr"

const build_number                      = get_from_env("BUILDKITE_BUILD_NUMBER")
const job_name                          = get_from_env("BUILDKITE_STEP_KEY")
const commit_full                       = get_from_env("BUILDKITE_COMMIT")
const commit_short                      = first(commit_full, 10)
const JULIA_TEST_RR_TIMEOUT             = get(ENV,  "JULIA_TEST_RR_TIMEOUT", "120")
const timeout_minutes                   = parse(Int, JULIA_TEST_RR_TIMEOUT)
const JULIA_TEST_NUM_CORES              = get(ENV,  "JULIA_TEST_NUM_CORES", "8")
const julia_test_num_cores_int          = parse(Int, JULIA_TEST_NUM_CORES)
const num_cores = min(
    8,
    Sys.CPU_THREADS,
    julia_test_num_cores_int + 1,
)

ENV["JULIA_RRCAPTURE_NUM_CORES"] = "$(num_cores)"

@info(
    "",
    build_number,
    job_name,
    commit_full,
    commit_short,
    timeout_minutes,
    num_cores,
)

const dumps_dir       = joinpath(pwd(), "dumps")
const temp_parent_dir = joinpath(pwd(), "temp_for_rr")

mkpath(dumps_dir)
mkpath(temp_parent_dir)

proc = nothing

mktempdir(temp_parent_dir) do dir
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
            sleep(timeout_minutes * 60)

            # If we've exceeded the timeout and `rr` is still running, kill it.
            if isopen(proc)
                println(stderr, "\n\nProcess timed out (with a timeout of $(timeout_minutes) minutes). Signalling `rr` for force-cleanup!")
                kill(proc, Base.SIGTERM)

                # Give `rr` a chance to cleanup and upload.
                # Note: this time period includes the time to upload the `rr` trace files
                # as Buildkite artifacts, so make sure it is long enough to allow the
                # uploads to finish.
                cleanup_minutes = 30
                sleep(cleanup_minutes * 60)

                if isopen(proc)
                    println(stderr, "\n\n`rr` failed to cleanup and upload within $(cleanup_minutes) minutes, killing and exiting immediately!")
                    kill(proc, Base.SIGKILL)
                    exit(1)
                end
            end
        end

        # Wait for `rr` to finish, either through naturally finishing its run, or `SIGTERM`.
        wait(proc)
        process_failed = !success(proc)

        if process_failed || always_save_rr_trace || is_buildkite
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
            mkpath(dumps_dir)
            date_str = Dates.format(Dates.now(), Dates.dateformat"yyyy_mm_dd_HH_MM_SS")
            dst_file_name = string(
                "rr",
                "--build_$(build_number)",
                "--$(job_name)",
                "--commit_$(commit_short)",
                "--$(date_str)",
                ".tar.zst",
            )
            dst_full_path = joinpath(dumps_dir, dst_file_name)
            zstd_jll.zstdmt() do zstdp
                tarproc = open(`$(zstdp) -o $(dst_full_path)`, "w")
                Tar.create(dir, tarproc)
                close(tarproc.in)
            end

            @info "The `rr` trace file has been saved to: $(dst_full_path)"
            if is_buildkite
                @info "Since this is a Buildkite run, we will upload the `rr` trace file."
                cd(dumps_dir) do
                    run(`buildkite-agent artifact upload $(dst_file_name)`)
                end
            end
        end

    end
end

@info "Finished running the command under rr"
my_exit(proc)
