import Dates
import Pkg
import Tar

function my_exit(process::Base.Process)
    wait(process)

    @info(
        "",
        process.exitcode,
        process.termsignal,
        success(process),
        Base.process_signaled(process),
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

function directory_has_files(dir::AbstractString)
    if !isabspath(dir)
        throw(ArgumentError("The directory path must be an absolute path"))
    end
    if !isdir(dir)
        throw(ArgumentError("`$(dir)` is not a directory"))
    end

    for (root, dirs, files) in walkdir(dir)
        for file in files
            return true
        end
    end
    return false
end

if length(ARGS) < 1
    throw(ErrorException("Usage: julia $(basename(@__FILE__)) [command...]"))
end
if Base.VERSION < v"1.6"
    throw(ErrorException("The `$(basename(@__FILE__))` script requires Julia 1.6 or greater"))
end

@info "We will run the tests under rr"

const JULIA_RRCAPTURE_BUILD_NUMBER    = get(ENV, "JULIA_RRCAPTURE_BUILD_NUMBER",    "unknownbuildnumber")
const JULIA_RRCAPTURE_IS_BUILDKITE    = get(ENV, "JULIA_RRCAPTURE_IS_BUILDKITE",    "false")
const JULIA_RRCAPTURE_JOB_NAME        = get(ENV, "JULIA_RRCAPTURE_JOB_NAME",        "unknownjobname")
const JULIA_RRCAPTURE_TIMEOUT_MINUTES = get(ENV, "JULIA_RRCAPTURE_TIMEOUT_MINUTES", "120")
const JULIA_TEST_NUM_CORES            = get(ENV, "JULIA_TEST_NUM_CORES", "8")

if haskey(ENV, "JULIA_RRCAPTURE_COMMIT")
    const JULIA_RRCAPTURE_COMMIT       = ENV["JULIA_RRCAPTURE_COMMIT"]
    const julia_rrcapture_commit_short = first(JULIA_RRCAPTURE_COMMIT, 10)
else
    const JULIA_RRCAPTURE_COMMIT       = "unknowncommit"
    const julia_rrcapture_commit_short = "unknowncommit"
end

@info "ENV[\"JULIA_RRCAPTURE_BUILD_NUMBER\"]    = \"$(JULIA_RRCAPTURE_BUILD_NUMBER)\""
@info "ENV[\"JULIA_RRCAPTURE_COMMIT\"]          = \"$(JULIA_RRCAPTURE_COMMIT)\""
@info "ENV[\"JULIA_RRCAPTURE_IS_BUILDKITE\"]    = \"$(JULIA_RRCAPTURE_IS_BUILDKITE)\""
@info "ENV[\"JULIA_RRCAPTURE_JOB_NAME\"]        = \"$(JULIA_RRCAPTURE_JOB_NAME)\""
@info "ENV[\"JULIA_RRCAPTURE_TIMEOUT_MINUTES\"] = \"$(JULIA_RRCAPTURE_TIMEOUT_MINUTES)\""
@info "ENV[\"JULIA_TEST_NUM_CORES\"]            = \"$(JULIA_TEST_NUM_CORES)\""

const build_number             = JULIA_RRCAPTURE_BUILD_NUMBER
const commit_full              = JULIA_RRCAPTURE_COMMIT
const commit_short             = julia_rrcapture_commit_short
const is_buildkite             = parse(Bool, JULIA_RRCAPTURE_IS_BUILDKITE)
const job_name                 = JULIA_RRCAPTURE_JOB_NAME
const julia_test_num_cores_int = parse(Int, JULIA_TEST_NUM_CORES)
const num_cores = min(
    8,
    Sys.CPU_THREADS,
    julia_test_num_cores_int + 1,
)
const timeout_minutes = parse(Int, JULIA_RRCAPTURE_TIMEOUT_MINUTES)

ENV["JULIA_RRCAPTURE_NUM_CORES"] = "$(num_cores)"

@info(
    "",
    build_number,
    commit_full,
    commit_short,
    is_buildkite,
    job_name,
    num_cores,
    timeout_minutes,
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

        # On a non-zero exit code, upload the `rr` trace
        # if !success(proc)
        if true
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
            dst_path = joinpath(
                dumps_dir,
                "rr--build_$(build_number)--gitsha_$(commit_short)--$(job_name)--$(date_str).tar.zst",
            )
            zstd_jll.zstdmt() do zstdp
                tarproc = open(`$zstdp -o $dst_path`, "w")
                Tar.create(dir, tarproc)
                close(tarproc.in)
            end
        end
    end
end

if directory_has_files(dumps_dir)
    @info "There are one or more `rr` trace files"
    for (root, dirs, files) in walkdir(dumps_dir)
        for file in files
            full_path_to_file = joinpath(root, file)
            if is_buildkite
                cd(root) do
                    run(`buildkite-agent artifact upload $(file)`)
                end
            else
                @info "Found `rr` trace file: $(full_path_to_file)"
            end
        end
    end
else
    @info "There are no `rr` trace files"
end

@info "Finished running the tests under rr"
my_exit(proc)
