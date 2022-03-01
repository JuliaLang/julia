# This file is a part of Julia. License is MIT: https://julialang.org/license

using LazyArtifacts
using Test

mktempdir() do tempdir
    LazyArtifacts.Artifacts.with_artifacts_directory(tempdir) do
        socrates_dir = artifact"socrates"
        @test isdir(socrates_dir)
        ex = @test_throws ErrorException artifact"HelloWorldC"
        @test startswith(ex.value.msg, "Artifact \"HelloWorldC\" was not installed correctly. ")
    end
end

# Need to set depwarn flag before testing deprecations
@test success(run(setenv(`$(Base.julia_cmd()) --depwarn=no --startup-file=no -e '
    using Artifacts, Pkg
    using Test
    mktempdir() do tempdir
        Artifacts.with_artifacts_directory(tempdir) do
            socrates_dir = @test_logs(
                    (:warn, "using Pkg instead of using LazyArtifacts is deprecated"),
                    artifact"socrates")
            @test isdir(socrates_dir)
        end
    end'`,
    dir=@__DIR__)))
