# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "libgit2" begin

const LIBGIT2_VER = v"0.23.0"

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

@testset "Check library verison" begin
    v = LibGit2.version()
    @test  v.major == LIBGIT2_VER.major && v.minor >= LIBGIT2_VER.minor
end

@testset "OID" begin
    z = LibGit2.Oid()
    r = rand(LibGit2.Oid)
    @test LibGit2.iszero(z)
    @test z == zero(LibGit2.Oid)
    rs = string(r)
    rr = LibGit2.raw(r)
    @test r == LibGit2.Oid(rr)
    @test r == LibGit2.Oid(rs)
    @test r == LibGit2.Oid(pointer(rr))
    for i in 11:length(rr); rr[i] = 0; end
    @test LibGit2.Oid(rr) == LibGit2.Oid(rs[1:20])
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
end

temp_dir() do dir

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
                lines = split(open(readall, config, "r"), "\n")
                @test any(map(x->x == "[remote \"upstream\"]", lines))

                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                @test LibGit2.url(remote) == repo_url
                finalize(remote)
            finally
                finalize(repo)
            end
        end

        @testset "bare" begin
            path = joinpath(dir, "Example.Bare")
            repo = LibGit2.init(path, Cuint(1))
            try
                @test isdir(path)
                @test isfile(joinpath(path, LibGit2.Consts.HEAD_FILE))
            finally
                finalize(repo)
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
                LibGit2.commit(repo, randstring(10); author=test_sig, committer=test_sig)

                println(repo_file, commit_msg2)
                flush(repo_file)
                LibGit2.add!(repo, test_file)
                @test LibGit2.iszero(commit_oid2)
                commit_oid2 = LibGit2.commit(repo, commit_msg2; author=test_sig, committer=test_sig)
                @test !LibGit2.iszero(commit_oid2)

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
                @test brnch == "master"

                LibGit2.branch!(repo, test_branch, string(commit_oid1), set_head=false)

                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test "master" in branches
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

                tag_oid2 = LibGit2.tag_create(repo, tag2, commit_oid2)
                @test !LibGit2.iszero(tag_oid2)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 2
                @test tag2 in tags

                LibGit2.tag_delete(repo, tag1)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag2 ∈ tags
                @test tag1 ∉ tags
            finally
                finalize(repo)
            end
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
        finally
            finalize(repo)
        end
    end

    @testset "Examine test repository" begin
        @testset "files" begin
            @test readall(joinpath(test_repo, test_file)) == readall(joinpath(cache_repo, test_file))
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
                @test "master" in branches
                @test test_branch in branches
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
                @testloop for i in eachindex(oids)
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

end

end
