# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libgit2-online" begin

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
                cred = LibGit2.UserPasswordCredentials("","") # empty credentials cause authentication error
                LibGit2.clone(repo_url*randstring(10), repo_path, payload=Nullable(cred))
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                @test ex.code == LibGit2.Error.EAUTH
            end
        end
    end
end

end
