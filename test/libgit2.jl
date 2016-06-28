# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libgit2" begin

const LIBGIT2_VER = v"0.23.0"

#########
# TESTS #
#########

@testset "Check library verison" begin
    v = LibGit2.version()
    @test  v.major == LIBGIT2_VER.major && v.minor >= LIBGIT2_VER.minor
end

@testset "OID" begin
    z = LibGit2.Oid()
    @test LibGit2.iszero(z)
    @test z == zero(LibGit2.Oid)
    @test z == LibGit2.Oid(z)
    rs = string(z)
    rr = LibGit2.raw(z)
    @test z == LibGit2.Oid(rr)
    @test z == LibGit2.Oid(rs)
    @test z == LibGit2.Oid(pointer(rr))
    for i in 11:length(rr); rr[i] = 0; end
    @test LibGit2.Oid(rr) == LibGit2.Oid(rs[1:20])
    @test_throws ArgumentError LibGit2.Oid(Ptr{UInt8}(C_NULL))
end

@testset "StrArrayStruct" begin
    p1 = "XXX"
    p2 = "YYY"
    sa1 = LibGit2.StrArrayStruct(p1)
    try
        arr = convert(Vector{AbstractString}, sa1)
        @test arr[1] == p1
    finally
        finalize(sa1)
    end

    sa2 = LibGit2.StrArrayStruct(p1, p2)
    try
        arr1 = convert(Vector{AbstractString}, sa2)
        @test arr1[1] == p1
        @test arr1[2] == p2
        sa3 = copy(sa2)
        arr2 = convert(Vector{AbstractString}, sa3)
        @test arr1[1] == arr2[1]
        @test arr1[2] == arr2[2]
        finalize(sa3)
    finally
        finalize(sa2)
    end
end

@testset "Signature" begin
    sig = LibGit2.Signature("AAA", "AAA@BBB.COM", round(time(), 0), 0)
    git_sig = convert(LibGit2.GitSignature, sig)
    sig2 = LibGit2.Signature(git_sig)
    finalize(git_sig)
    @test sig.name == sig2.name
    @test sig.email == sig2.email
    @test sig.time == sig2.time
    sig3 = LibGit2.Signature("AAA","AAA@BBB.COM")
    @test sig3.name == sig.name
    @test sig3.email == sig.email
end

mktempdir() do dir
    # test parameters
    repo_url = "https://github.com/JuliaLang/Example.jl"
    ssh_prefix = "git@"
    cache_repo = joinpath(dir, "Example")
    test_repo = joinpath(dir, "Example.Test")
    test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
    test_file = "testfile"
    config_file = "testconfig"
    commit_msg1 = randstring(10)
    commit_msg2 = randstring(10)
    commit_oid1 = LibGit2.Oid()
    commit_oid2 = LibGit2.Oid()
    commit_oid3 = LibGit2.Oid()
    master_branch = "master"
    test_branch = "test_branch"
    tag1 = "tag1"
    tag2 = "tag2"

    @testset "Configuration" begin
        cfg = LibGit2.GitConfig(joinpath(dir, config_file), LibGit2.Consts.CONFIG_LEVEL_APP)
        try
            @test_throws LibGit2.Error.GitError LibGit2.get(AbstractString, cfg, "tmp.str")
            @test isempty(LibGit2.get(cfg, "tmp.str", "")) == true

            LibGit2.set!(cfg, "tmp.str", "AAAA")
            LibGit2.set!(cfg, "tmp.int32", Int32(1))
            LibGit2.set!(cfg, "tmp.int64", Int64(1))
            LibGit2.set!(cfg, "tmp.bool", true)

            @test LibGit2.get(cfg, "tmp.str", "") == "AAAA"
            @test LibGit2.get(cfg, "tmp.int32", Int32(0)) == Int32(1)
            @test LibGit2.get(cfg, "tmp.int64", Int64(0)) == Int64(1)
            @test LibGit2.get(cfg, "tmp.bool", false) == true
        finally
            finalize(cfg)
        end
    end

    @testset "Initializing repository" begin
        @testset "with remote branch" begin
            repo = LibGit2.init(cache_repo)
            try
                @test isdir(cache_repo)
                @test isdir(joinpath(cache_repo, ".git"))

                # set a remote branch
                branch = "upstream"
                LibGit2.GitRemote(repo, branch, repo_url) |> finalize

                config = joinpath(cache_repo, ".git", "config")
                lines = split(open(readstring, config, "r"), "\n")
                @test any(map(x->x == "[remote \"upstream\"]", lines))

                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                @test LibGit2.url(remote) == repo_url
                @test LibGit2.isattached(repo)
                finalize(remote)
            finally
                finalize(repo)
            end
        end

        @testset "bare" begin
            path = joinpath(dir, "Example.Bare")
            repo = LibGit2.init(path, true)
            try
                @test isdir(path)
                @test isfile(joinpath(path, LibGit2.Consts.HEAD_FILE))
                @test LibGit2.isattached(repo)
            finally
                finalize(repo)
            end

            path = joinpath("garbagefakery", "Example.Bare")
            try
                LibGit2.GitRepo(path)
                error("unexpected")
            catch e
                @test typeof(e) == LibGit2.GitError
                @test startswith(sprint(show,e),"GitError(Code:ENOTFOUND, Class:OS, Failed to resolve path")
            end
        end
    end

    @testset "Cloning repository" begin

        @testset "bare" begin
            repo_path = joinpath(dir, "Example.Bare1")
            repo = LibGit2.clone(cache_repo, repo_path, isbare = true)
            try
                @test isdir(repo_path)
                @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
                @test LibGit2.isattached(repo)
                @test LibGit2.remotes(repo) == ["origin"]
            finally
                finalize(repo)
            end
        end
        @testset "bare with remote callback" begin
            repo_path = joinpath(dir, "Example.Bare2")
            repo = LibGit2.clone(cache_repo, repo_path, isbare = true, remote_cb = LibGit2.mirror_cb())
            try
                @test isdir(repo_path)
                @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
                rmt = LibGit2.get(LibGit2.GitRemote, repo, "origin")
                try
                    @test LibGit2.fetch_refspecs(rmt)[1] == "+refs/*:refs/*"
                    @test LibGit2.isattached(repo)
                    @test LibGit2.remotes(repo) == ["origin"]
                finally
                    finalize(rmt)
                end
            finally
                finalize(repo)
            end
        end
        @testset "normal" begin
            repo = LibGit2.clone(cache_repo, test_repo, remote_cb = LibGit2.mirror_cb())
            try
                @test isdir(test_repo)
                @test isdir(joinpath(test_repo, ".git"))
                @test LibGit2.isattached(repo)
            finally
                finalize(repo)
            end
        end
    end

    @testset "Update cache repository" begin

        @testset "with commits" begin
            repo = LibGit2.GitRepo(cache_repo)
            repo_file = open(joinpath(cache_repo,test_file), "a")
            try
                # create commits
                println(repo_file, commit_msg1)
                flush(repo_file)
                LibGit2.add!(repo, test_file)
                @test LibGit2.iszero(commit_oid1)
                commit_oid1 = LibGit2.commit(repo, commit_msg1; author=test_sig, committer=test_sig)
                @test !LibGit2.iszero(commit_oid1)

                println(repo_file, randstring(10))
                flush(repo_file)
                LibGit2.add!(repo, test_file)
                commit_oid3 = LibGit2.commit(repo, randstring(10); author=test_sig, committer=test_sig)

                println(repo_file, commit_msg2)
                flush(repo_file)
                LibGit2.add!(repo, test_file)
                @test LibGit2.iszero(commit_oid2)
                commit_oid2 = LibGit2.commit(repo, commit_msg2; author=test_sig, committer=test_sig)
                @test !LibGit2.iszero(commit_oid2)
                auths = LibGit2.authors(repo)
                @test length(auths) == 3
                for auth in auths
                    @test auth.name == test_sig.name
                    @test auth.time == test_sig.time
                    @test auth.email == test_sig.email
                end

                # lookup commits
                cmt = LibGit2.get(LibGit2.GitCommit, repo, commit_oid1)
                try
                    @test commit_oid1 == LibGit2.Oid(cmt)
                    auth = LibGit2.author(cmt)
                    @test isa(auth, LibGit2.Signature)
                    @test auth.name == test_sig.name
                    @test auth.time == test_sig.time
                    @test auth.email == test_sig.email
                    cmtr = LibGit2.committer(cmt)
                    @test isa(cmtr, LibGit2.Signature)
                    @test cmtr.name == test_sig.name
                    @test cmtr.time == test_sig.time
                    @test cmtr.email == test_sig.email
                    @test LibGit2.message(cmt) == commit_msg1
                finally
                    finalize(cmt)
                end
            finally
                finalize(repo)
                close(repo_file)
            end
        end

        @testset "with branch" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                brnch = LibGit2.branch(repo)
                brref = LibGit2.head(repo)
                try
                    @test LibGit2.isbranch(brref)
                    @test !LibGit2.isremote(brref)
                    @test LibGit2.name(brref) == "refs/heads/master"
                    @test LibGit2.shortname(brref) == master_branch
                    @test LibGit2.ishead(brref)
                    @test LibGit2.upstream(brref) === nothing
                    @test repo.ptr == LibGit2.owner(brref).ptr
                    @test brnch == master_branch
                    @test LibGit2.headname(repo) == master_branch
                    LibGit2.branch!(repo, test_branch, string(commit_oid1), set_head=false)

                    @test LibGit2.lookup_branch(repo, test_branch, true) === nothing
                    tbref = LibGit2.lookup_branch(repo, test_branch, false)
                    try
                        @test LibGit2.shortname(tbref) == test_branch
                        @test LibGit2.upstream(tbref) === nothing
                    finally
                        finalize(tbref)
                    end
                finally
                    finalize(brref)
                end

                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test master_branch in branches
                @test test_branch in branches
            finally
                finalize(repo)
            end
        end

        @testset "with default configuration" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                try
                    LibGit2.Signature(repo)
                catch ex
                    # these test configure repo with new signature
                    # in case when global one does not exsist
                    @test isa(ex, LibGit2.Error.GitError) == true

                    cfg = LibGit2.GitConfig(repo)
                    LibGit2.set!(cfg, "user.name", "AAAA")
                    LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")
                    sig = LibGit2.Signature(repo)
                    @test sig.name == "AAAA"
                    @test sig.email == "BBBB@BBBB.COM"
                end
            finally
                finalize(repo)
            end
        end

        @testset "with tags" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 0

                tag_oid1 = LibGit2.tag_create(repo, tag1, commit_oid1, sig=test_sig)
                @test !LibGit2.iszero(tag_oid1)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag1 in tags
                tag1ref = LibGit2.GitReference(repo, "refs/tags/$tag1")
                @test isempty(LibGit2.fullname(tag1ref)) #because this is a reference to an OID
                tag1tag = LibGit2.peel(LibGit2.GitTag,tag1ref)
                @test LibGit2.name(tag1tag) == tag1
                @test LibGit2.target(tag1tag) == commit_oid1

                tag_oid2 = LibGit2.tag_create(repo, tag2, commit_oid2)
                @test !LibGit2.iszero(tag_oid2)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 2
                @test tag2 in tags

                refs = LibGit2.ref_list(repo)
                @test refs == ["refs/heads/master","refs/heads/test_branch","refs/tags/tag1","refs/tags/tag2"]

                LibGit2.tag_delete(repo, tag1)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag2 ∈ tags
                @test tag1 ∉ tags
            finally
                finalize(repo)
            end
        end

        @testset "status" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                status = LibGit2.GitStatus(repo)
                @test length(status) == 0
                @test_throws BoundsError status[1]
                repo_file = open(joinpath(cache_repo,"statusfile"), "a")

                # create commits
                println(repo_file, commit_msg1)
                flush(repo_file)
                LibGit2.add!(repo, test_file)
                status = LibGit2.GitStatus(repo)
                @test length(status) != 0
                @test_throws BoundsError status[0]
                @test_throws BoundsError status[length(status)+1]
                #we've added a file - show that it is new
                @test status[1].status == LibGit2.Consts.STATUS_WT_NEW
            finally
                finalize(repo)
                close(repo_file)
            end
    end

    @testset "Fetch from cache repository" begin
        repo = LibGit2.GitRepo(test_repo)
        try
            # fetch changes
            @test LibGit2.fetch(repo) == 0
            @test !isfile(joinpath(test_repo, test_file))

            # ff merge them
            @test LibGit2.merge!(repo, fastforward=true)

            # because there was not any file we need to reset branch
            head_oid = LibGit2.head_oid(repo)
            LibGit2.reset!(repo, head_oid, LibGit2.Consts.RESET_HARD)
            @test isfile(joinpath(test_repo, test_file))

            # Detach HEAD - no merge
            LibGit2.checkout!(repo, string(commit_oid3))
            @test_throws LibGit2.Error.GitError LibGit2.merge!(repo, fastforward=true)

            # Switch to a branch without remote - no merge
            LibGit2.branch!(repo, test_branch)
            @test_throws LibGit2.Error.GitError LibGit2.merge!(repo, fastforward=true)

            # Set the username and email for the test_repo (needed for rebase)
            cfg = LibGit2.GitConfig(repo)
            LibGit2.set!(cfg, "user.name", "AAAA")
            LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")

            # Try rebasing on master instead
            LibGit2.rebase!(repo, master_branch)

            # Switch to the master branch
            LibGit2.branch!(repo, master_branch)

        finally
            finalize(repo)
        end
    end

    @testset "Examine test repository" begin
        @testset "files" begin
            @test readstring(joinpath(test_repo, test_file)) == readstring(joinpath(cache_repo, test_file))
        end

        @testset "tags & branches" begin
            repo = LibGit2.GitRepo(test_repo)
            try
                # all tag in place
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag2 in tags

                # all tag in place
                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test master_branch in branches
                @test test_branch in branches

                # issue #16337
                tag2ref = LibGit2.GitReference(repo, "refs/tags/$tag2")
                try
                    @test_throws LibGit2.Error.GitError LibGit2.upstream(tag2ref)
                finally
                    finalize(tag2ref)
                end

            finally
                finalize(repo)
            end
        end

        @testset "commits with revwalk" begin
            repo = LibGit2.GitRepo(test_repo)
            cache = LibGit2.GitRepo(cache_repo)
            try
                oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    LibGit2.map((oid,repo)->(oid,repo), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME)
                end
                @test length(oids) == 1

                test_oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    LibGit2.map((oid,repo)->string(oid), walker, by = LibGit2.Consts.SORT_TIME)
                end
                cache_oids = LibGit2.with(LibGit2.GitRevWalker(cache)) do walker
                    LibGit2.map((oid,repo)->string(oid), walker, by = LibGit2.Consts.SORT_TIME)
                end
                for i in eachindex(oids)
                    @test cache_oids[i] == test_oids[i]
                end
            finally
                finalize(repo)
                finalize(cache)
            end
        end
    end

    @testset "Transact test repository" begin
        repo = LibGit2.GitRepo(test_repo)
        try
            cp(joinpath(test_repo, test_file), joinpath(test_repo, "CCC"))
            cp(joinpath(test_repo, test_file), joinpath(test_repo, "AAA"))
            LibGit2.add!(repo, "AAA")
            @test_throws ErrorException LibGit2.transact(repo) do trepo
                mv(joinpath(test_repo, test_file), joinpath(test_repo, "BBB"))
                LibGit2.add!(trepo, "BBB")
                oid = LibGit2.commit(trepo, "test commit"; author=test_sig, committer=test_sig)
                error("Force recovery")
            end
            @test isfile(joinpath(test_repo, "AAA"))
            @test isfile(joinpath(test_repo, "CCC"))
            @test !isfile(joinpath(test_repo, "BBB"))
            @test isfile(joinpath(test_repo, test_file))
        finally
            finalize(repo)
        end
    end

    @testset "Credentials" begin
        creds = LibGit2.EmptyCredentials()
        @test LibGit2.checkused!(creds)
        @test LibGit2.reset!(creds) === nothing
        @test creds[:user] === nothing
        @test creds[:pass] === nothing
        @test creds[:pubkey, "localhost"] === nothing

        creds_user = "USER"
        creds_pass = "PASS"
        creds = LibGit2.UserPasswordCredentials(creds_user, creds_pass)
        @test !LibGit2.checkused!(creds)
        @test !LibGit2.checkused!(creds)
        @test !LibGit2.checkused!(creds)
        @test LibGit2.checkused!(creds)
        @test LibGit2.reset!(creds) == 3
        @test !LibGit2.checkused!(creds)
        @test creds.count == 2
        @test creds[:user] == creds_user
        @test creds[:pass] == creds_pass
        @test creds[:pubkey] === nothing
        @test creds[:user, "localhost"] == creds_user
        @test creds[:pubkey, "localhost"] === nothing
        @test creds[:usesshagent, "localhost"] == "Y"
        creds[:usesshagent, "localhost"] = "E"
        @test creds[:usesshagent, "localhost"] == "E"

        creds = LibGit2.CachedCredentials()
        @test !LibGit2.checkused!(creds)
        @test !LibGit2.checkused!(creds)
        @test !LibGit2.checkused!(creds)
        @test LibGit2.checkused!(creds)
        @test LibGit2.reset!(creds) == 3
        @test !LibGit2.checkused!(creds)
        @test creds.count == 2
        @test creds[:user, "localhost"] === nothing
        @test creds[:pass, "localhost"] === nothing
        @test creds[:pubkey, "localhost"] === nothing
        @test creds[:prvkey, "localhost"] === nothing
        @test creds[:usesshagent, "localhost"] === nothing
        creds[:user, "localhost"] = creds_user
        creds[:pass, "localhost"] = creds_pass
        creds[:usesshagent, "localhost"] = "Y"
        @test creds[:user] === nothing
        @test creds[:user, "localhost2"] === nothing
        @test creds[:user, "localhost"] == creds_user
        @test creds[:pass, "localhost"] == creds_pass
        @test creds[:pubkey, "localhost"] === nothing
        @test creds[:usesshagent, "localhost"] == "Y"
    end
end

end
