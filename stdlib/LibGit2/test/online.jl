# This file is a part of Julia. License is MIT: https://julialang.org/license

module LibGit2OnlineTests

using Test
import LibGit2
using Random

#########
# TESTS #
#########
# init & clone
mktempdir() do dir
    repo_url = "https://github.com/JuliaLang/Example.jl"
    @testset "Cloning repository" begin
        @testset "with 'https' protocol" begin
            repo_path = joinpath(dir, "Example1")
            payload = LibGit2.CredentialPayload(allow_prompt=false, allow_git_helpers=false)
            repo = LibGit2.clone(repo_url, repo_path, payload=payload)
            try
                @test isdir(repo_path)
                @test isdir(joinpath(repo_path, ".git"))
            finally
                close(repo)
            end
        end

        @testset "with incorrect url" begin
            try
                repo_path = joinpath(dir, "Example2")
                # credentials are required because github tries to authenticate on unknown repo
                cred = LibGit2.UserPasswordCredential("JeffBezanson", "hunter2") # make sure Jeff is using a good password :)
                payload = LibGit2.CredentialPayload(cred, allow_prompt=false, allow_git_helpers=false)
                LibGit2.clone(repo_url*randstring(10), repo_path, payload=payload)
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                @test ex.code == LibGit2.Error.EAUTH
            end
        end

        @testset "with empty credentials" begin
            try
                repo_path = joinpath(dir, "Example3")
                # credentials are required because github tries to authenticate on unknown repo
                cred = LibGit2.UserPasswordCredential("","") # empty credentials cause authentication error
                payload = LibGit2.CredentialPayload(cred, allow_prompt=false, allow_git_helpers=false)
                LibGit2.clone(repo_url*randstring(10), repo_path, payload=payload)
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                @test ex.code == LibGit2.Error.EAUTH
            end
        end
    end
end

end # module