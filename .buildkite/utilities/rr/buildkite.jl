if length(ARGS) < 1
    throw(ErrorException("Usage: julia $(basename(@__FILE__)) [command...]"))
end

const is_buildkite = tryparse(Bool, lowercase(strip(get(ENV, "BUILDKITE", "")))) === true
if !is_buildkite
    msg = string(
        "The `$(basename(@__FILE__))` script only works on Buildkite. ",
        "If you are running locally, you should use the `rr_capture.jl` script instead.",
    )
    throw(ErrorException(msg))
end

# We only use `rr` on certain Buildkite jobs
const rr_job_list = String[
    # "tester_linux64",
    # "tester_linux64_mt",
    # "tester_linux64_st",
    "tester_linux64_rr",
    "tester_linux64_rr_mt",
    "tester_linux64_rr_st",
]
const this_job = ENV["BUILDKITE_STEP_KEY"]

if !(this_job in rr_job_list)
    @info "We will not run the tests under rr" this_job rr_job_list
    run(`$ARGS`)
    @info "Finished running the tests (not under rr)"
    exit(0)
end

@info "This Buildkite job is in the list of `rr` jobs" this_job rr_job_list

const env_mappings = Dict{String, String}()
env_mappings["JULIA_RRCAPTURE_BUILD_NUMBER"]    = ENV["BUILDKITE_BUILD_NUMBER"]
env_mappings["JULIA_RRCAPTURE_COMMIT"]          = ENV["BUILDKITE_COMMIT"]
env_mappings["JULIA_RRCAPTURE_JOB_NAME"]        = this_job
env_mappings["JULIA_RRCAPTURE_IS_BUILDKITE"]    = "true"

rr_capture = joinpath(@__DIR__, "rr_capture.jl")

cmd = addenv(
    `$(Base.julia_cmd().exec[1]) $(rr_capture) $ARGS`,
    env_mappings,
)

run(cmd)
