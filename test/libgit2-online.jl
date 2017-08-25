# This file is a part of Julia. License is MIT: https://julialang.org/license

#########
# TESTS #
#########
# init & clone
mktempdir() do dir
    repo_url = "https://github.com/JuliaLang/Example.jl"
    @testset "Cloning repository" begin
        @testset "with 'https' protocol" begin
            repo_path = joinpath(dir, "Example1")
            repo = LibGit2.clone(repo_url, repo_path)
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
                cred = LibGit2.UserPasswordCredentials("JeffBezanson", "hunter2") # make sure Jeff is using a good password :)
                payload = LibGit2.CredentialPayload(cred)
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
                cred = LibGit2.UserPasswordCredentials("","") # empty credentials cause authentication error
                payload = LibGit2.CredentialPayload(cred)
                LibGit2.clone(repo_url*randstring(10), repo_path, payload=payload)
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                if Sys.iswindows() && LibGit2.version() >= v"0.26.0"
                    # see #22681 and https://github.com/libgit2/libgit2/pull/4055
                    @test_broken ex.code == LibGit2.Error.EAUTH
                    @test ex.code == LibGit2.Error.ERROR
                else
                    @test ex.code == LibGit2.Error.EAUTH
                end
            end
        end
    end
end
