module ScratchUsage
using Pkg, Scratch

# TODO: Replace these with Pkg.API.get_uuid() and Pkg.API.get_version()
# from https://github.com/JuliaLang/Pkg.jl/pull/1947
const my_uuid = Base.UUID("93485645-17f1-6f3b-45bc-419db53815ea")
function get_version()
    project_toml = Pkg.TOML.parsefile(joinpath(dirname(@__DIR__), "Project.toml"))
    return VersionNumber(project_toml["version"])
end
const my_version = get_version()

# This function will create a bevy of spaces here
function touch_scratch()
    # Create an explicitly version-specific space
    private_space = get_scratch!(
        string(my_version.major, ".", my_version.minor, ".", my_version.patch),
        my_uuid,
    )
    touch(joinpath(private_space, string("ScratchUsage-", my_version)))

    # Create a space shared between all instances of the same major version,
    # using the `@get_scratch!` macro which automatically looks up the UUID
    major_space = @get_scratch!(string(my_version.major))
    touch(joinpath(major_space, string("ScratchUsage-", my_version)))

    # Create a global space that is not locked to this package at all
    global_space = get_scratch!("GlobalSpace")
    touch(joinpath(global_space, string("ScratchUsage-", my_version)))
end

end # module ScratchUsage