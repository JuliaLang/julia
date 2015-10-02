# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libgit2-online" begin

#########
# Setup #
#########

function temp_dir(fn::Function, remove_tmp_dir::Bool=true)
    tmpdir = joinpath(tempdir(),randstring())
    @test !isdir(tmpdir)
    try
        mkdir(tmpdir)
        @test isdir(tmpdir)
        fn(tmpdir)
    finally
        remove_tmp_dir && rm(tmpdir, recursive=true)
    end
end

#########
# TESTS #
#########
# init & clone
temp_dir() do dir
    repo_url = "github.com/JuliaLang/Example.jl"
    https_prefix = "https://"
    ssh_prefix = "git@"
    @testset "Cloning repository" begin
        @testset "with 'https' protocol" begin
            repo_path = joinpath(dir, "Example1")
            repo = LibGit2.clone(https_prefix*repo_url, repo_path)
            try
                @test isdir(repo_path)
                @test isdir(joinpath(repo_path, ".git"))
            finally
                finalize(repo)
            end
        end
        @testset "with incorrect url" begin
            repo_path = joinpath(dir, "Example2")
            # credential are required because github try authenticate on uknown repo
            x = Nullable(LibGit2.UserPasswordCredentials("X","X"))
            @test_throws LibGit2.Error.GitError LibGit2.clone(https_prefix*repo_url*randstring(10), repo_path, payload=x)
        end

        try
            @testset "with 'ssh' protocol (by default is not supported)" begin
                repo_path = joinpath(dir, "Example3")
                @test_throws LibGit2.Error.GitError LibGit2.clone(ssh_prefix*repo_url, repo_path)
            end
        catch ex
            # but we cloned succesfully, so check that repo was created
            ex.fail == 1 && @test isdir(joinpath(path, ".git"))
        end
    end
end

end
