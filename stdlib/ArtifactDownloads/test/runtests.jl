# This file is a part of Julia. License is MIT: https://julialang.org/license

module ArtifactDownloadsTests

using Test
using ArtifactDownloads
using Tar

# Everything below installs artifacts and writes usage logs, so it runs against a depot of
# its own rather than the user's.
const TEST_DEPOT = mktempdir()
pushfirst!(Base.DEPOT_PATH, TEST_DEPOT)

function list_tarball_files(tarball_path::AbstractString)
    names = String[]
    Tar.list(`$(ArtifactDownloads.PlatformEngines.exe7z()) x $tarball_path -so`) do hdr
        push!(names, hdr.path)
    end
    return names
end

try
    @testset "ArtifactDownloads" begin
        include("platformengines.jl")
        include("artifacts.jl")
    end
finally
    popfirst!(Base.DEPOT_PATH)
    rm(TEST_DEPOT; recursive = true, force = true)
end

end # module
