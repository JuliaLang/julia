# This file is a part of Julia. License is MIT: https://julialang.org/license

module LibGit2OnlineTests

using Test
import LibGit2
using Random

function transfer_progress(progress::Ptr{LibGit2.TransferProgress}, payload::Dict)
    status = payload[:transfer_progress]
    progress = unsafe_load(progress)

    status[] = (current=progress.received_objects, total=progress.total_objects)

    return Cint(0)
end

#########
# TESTS #
#########
# init & clone
mktempdir() do dir
    repo_url = "https://github.com/JuliaLang/Example.jl"

    @testset "Cloning repository" begin
        @testset "HTTPS protocol" begin
            repo_path = joinpath(dir, "Example.HTTPS")
            c = LibGit2.CredentialPayload(allow_prompt=false, allow_git_helpers=false)
            repo = LibGit2.clone(repo_url, repo_path, credentials=c)
            try
                @test isdir(repo_path)
                @test isdir(joinpath(repo_path, ".git"))
            finally
                close(repo)
            end
        end

        @testset "Transfer progress callbacks" begin
            status = Ref((current=0, total=-1))
            callbacks = LibGit2.Callbacks(
                :transfer_progress => (
                    @cfunction(transfer_progress, Cint, (Ptr{LibGit2.TransferProgress}, Any)),
                    status,
                )
            )

            repo_path = joinpath(dir, "Example.TransferProgress")
            c = LibGit2.CredentialPayload(allow_prompt=false, allow_git_helpers=false)
            repo = LibGit2.clone(repo_url, repo_path, credentials=c, callbacks=callbacks)
            try
                @test isdir(repo_path)
                @test isdir(joinpath(repo_path, ".git"))

                @test status[].total >= 0
                @test status[].current == status[].total
            finally
                close(repo)
            end
        end

        @testset "Incorrect URL" begin
            repo_path = joinpath(dir, "Example.IncorrectURL")
            # credentials are required because github tries to authenticate on unknown repo
            cred = LibGit2.UserPasswordCredential("JeffBezanson", "hunter2") # make sure Jeff is using a good password :)
            c = LibGit2.CredentialPayload(cred, allow_prompt=false, allow_git_helpers=false)
            try
                LibGit2.clone(repo_url*randstring(10), repo_path, credentials=c)
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                # Return code seems to vary, see #32186, #32219
                @test ex.code ∈ (LibGit2.Error.EAUTH, LibGit2.Error.ERROR)
            end
            Base.shred!(cred)
        end

        @testset "Empty Credentials" begin
            repo_path = joinpath(dir, "Example.EmptyCredentials")
            # credentials are required because github tries to authenticate on unknown repo
            cred = LibGit2.UserPasswordCredential("","") # empty credentials cause authentication error
            c = LibGit2.CredentialPayload(cred, allow_prompt=false, allow_git_helpers=false)
            try
                LibGit2.clone(repo_url*randstring(10), repo_path, credentials=c)
                error("unexpected")
            catch ex
                @test isa(ex, LibGit2.Error.GitError)
                @test ex.code == LibGit2.Error.EAUTH
            end
        end
    end
end

end # module
