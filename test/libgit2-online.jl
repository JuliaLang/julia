# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libgit2-online" begin

#########
# TESTS #
#########
# init & clone
mktempdir() do dir
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
            try
                repo_path = joinpath(dir, "Example2")
                # credentials are required because github tries to authenticate on unknown repo
                cred = LibGit2.UserPasswordCredentials("","") # empty credentials cause authentication error
                LibGit2.clone(https_prefix*repo_url*randstring(10), repo_path, payload=Nullable(cred))
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                @test ex.code == LibGit2.Error.EAUTH
            end
        end

        #TODO: remove or condition on libgit2 features this test when ssh protocol will be supported
        @testset "with 'ssh' protocol (by default is not supported)" begin
            try
                repo_path = joinpath(dir, "Example3")
                @test_throws LibGit2.Error.GitError LibGit2.clone(ssh_prefix*repo_url, repo_path)
            catch ex
                # but we cloned succesfully, so check that repo was created
                ex.fail == 1 && @test isdir(joinpath(path, ".git"))
            end
        end
    end
end

end
