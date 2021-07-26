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

const env_mappings = Dict{String, String}()
env_mappings["JULIA_RRCAPTURE_BUILD_NUMBER"]    = ENV["BUILDKITE_BUILD_NUMBER"]
env_mappings["JULIA_RRCAPTURE_COMMIT"]          = ENV["BUILDKITE_COMMIT"]
env_mappings["JULIA_RRCAPTURE_JOB_NAME"]        = ENV["BUILDKITE_STEP_KEY"]
env_mappings["JULIA_RRCAPTURE_IS_BUILDKITE"]    = "true"

rr_capture = joinpath(@__DIR__, "rr_capture.jl")

cmd = addenv(
    `$(Base.julia_cmd().exec[1]) $(rr_capture) $ARGS`,
    env_mappings,
)

run(cmd)
