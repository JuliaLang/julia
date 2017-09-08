# This file is a part of Julia. License is MIT: https://julialang.org/license

isdefined(Main, :TestHelpers) || @eval Main include(joinpath(@__DIR__, "TestHelpers.jl"))
import TestHelpers: challenge_prompt

const LIBGIT2_MIN_VER = v"0.23.0"
const LIBGIT2_HELPER_PATH = joinpath(@__DIR__, "libgit2-helpers.jl")

const KEY_DIR = joinpath(@__DIR__, "libgit2")

function get_global_dir()
    buf = Ref(LibGit2.Buffer())
    LibGit2.@check ccall((:git_libgit2_opts, :libgit2), Cint,
                         (Cint, Cint, Ptr{LibGit2.Buffer}),
                         LibGit2.Consts.GET_SEARCH_PATH, LibGit2.Consts.CONFIG_LEVEL_GLOBAL, buf)
    path = unsafe_string(buf[].ptr)
    LibGit2.free(buf)
    return path
end

function set_global_dir(dir)
    LibGit2.@check ccall((:git_libgit2_opts, :libgit2), Cint,
                         (Cint, Cint, Cstring),
                         LibGit2.Consts.SET_SEARCH_PATH, LibGit2.Consts.CONFIG_LEVEL_GLOBAL, dir)
    return
end

function with_libgit2_temp_home(f)
    mktempdir() do tmphome
        oldpath = get_global_dir()
        set_global_dir(tmphome)
        try
            @test get_global_dir() == tmphome
            f(tmphome)
        finally
            set_global_dir(oldpath)
        end
        return
    end
end

#########
# TESTS #
#########

@testset "Check library version" begin
    v = LibGit2.version()
    @test v.major == LIBGIT2_MIN_VER.major && v.minor >= LIBGIT2_MIN_VER.minor
end

@testset "Check library features" begin
    f = LibGit2.features()
    @test findfirst(f, LibGit2.Consts.FEATURE_SSH) > 0
    @test findfirst(f, LibGit2.Consts.FEATURE_HTTPS) > 0
end

@testset "OID" begin
    z = LibGit2.GitHash()
    @test LibGit2.iszero(z)
    @test z == zero(LibGit2.GitHash)
    @test z == LibGit2.GitHash(z)
    rs = string(z)
    rr = LibGit2.raw(z)
    @test z == LibGit2.GitHash(rr)
    @test z == LibGit2.GitHash(rs)
    @test z == LibGit2.GitHash(pointer(rr))

    @test LibGit2.GitShortHash(z, 20) == LibGit2.GitShortHash(rs[1:20])
    @test_throws ArgumentError LibGit2.GitHash(Ptr{UInt8}(C_NULL))
    @test_throws ArgumentError LibGit2.GitHash(rand(UInt8, 2*LibGit2.OID_RAWSZ))
    @test_throws ArgumentError LibGit2.GitHash("a")
end

@testset "StrArrayStruct" begin
    p = ["XXX","YYY"]
    a = Base.cconvert(Ptr{LibGit2.StrArrayStruct}, p)
    b = Base.unsafe_convert(Ptr{LibGit2.StrArrayStruct}, a)
    @test p == convert(Vector{String}, unsafe_load(b))
    @noinline gcuse(a) = a
    gcuse(a)
end

@testset "Signature" begin
    sig = LibGit2.Signature("AAA", "AAA@BBB.COM", round(time(), 0), 0)
    git_sig = convert(LibGit2.GitSignature, sig)
    sig2 = LibGit2.Signature(git_sig)
    close(git_sig)
    @test sig.name == sig2.name
    @test sig.email == sig2.email
    @test sig.time == sig2.time
    sig3 = LibGit2.Signature("AAA","AAA@BBB.COM")
    @test sig3.name == sig.name
    @test sig3.email == sig.email
end

@testset "Default config" begin
    with_libgit2_temp_home() do tmphome
        cfg = LibGit2.GitConfig()
        @test isa(cfg, LibGit2.GitConfig)
        @test LibGit2.getconfig("fake.property", "") == ""
        LibGit2.set!(cfg, "fake.property", "AAAA")
        @test LibGit2.getconfig("fake.property", "") == "AAAA"
    end
end

# See #21872 and #21636
LibGit2.version() >= v"0.26.0" && Sys.isunix() && @testset "Default config with symlink" begin
    with_libgit2_temp_home() do tmphome
        write(joinpath(tmphome, "real_gitconfig"), "[fake]\n\tproperty = BBB")
        symlink(joinpath(tmphome, "real_gitconfig"),
                joinpath(tmphome, ".gitconfig"))
        cfg = LibGit2.GitConfig()
        @test isa(cfg, LibGit2.GitConfig)
        LibGit2.getconfig("fake.property", "") == "BBB"
        LibGit2.set!(cfg, "fake.property", "AAAA")
        LibGit2.getconfig("fake.property", "") == "AAAA"
    end
end

@testset "Git URL parsing" begin
    @testset "HTTPS URL" begin
        m = match(LibGit2.URL_REGEX, "https://user:pass@server.com:80/org/project.git")
        @test m[:scheme] == "https"
        @test m[:user] == "user"
        @test m[:password] == "pass"
        @test m[:host] == "server.com"
        @test m[:port] == "80"
        @test m[:path] == "org/project.git"
    end

    @testset "SSH URL" begin
        m = match(LibGit2.URL_REGEX, "ssh://user:pass@server:22/project.git")
        @test m[:scheme] == "ssh"
        @test m[:user] == "user"
        @test m[:password] == "pass"
        @test m[:host] == "server"
        @test m[:port] == "22"
        @test m[:path] == "project.git"
    end

    @testset "SSH URL, scp-like syntax" begin
        m = match(LibGit2.URL_REGEX, "user@server:project.git")
        @test m[:scheme] === nothing
        @test m[:user] == "user"
        @test m[:password] === nothing
        @test m[:host] == "server"
        @test m[:port] === nothing
        @test m[:path] == "project.git"
    end

    # scp-like syntax corner case. The SCP syntax does not support port so everything after
    # the colon is part of the path.
    @testset "scp-like syntax, no port" begin
        m = match(LibGit2.URL_REGEX, "server:1234/repo")
        @test m[:scheme] === nothing
        @test m[:user] === nothing
        @test m[:password] === nothing
        @test m[:host] == "server"
        @test m[:port] === nothing
        @test m[:path] == "1234/repo"
    end

    @testset "HTTPS URL, realistic" begin
        m = match(LibGit2.URL_REGEX, "https://github.com/JuliaLang/Example.jl.git")
        @test m[:scheme] == "https"
        @test m[:user] === nothing
        @test m[:password] === nothing
        @test m[:host] == "github.com"
        @test m[:port] === nothing
        @test m[:path] == "JuliaLang/Example.jl.git"
    end

    @testset "SSH URL, realistic" begin
        m = match(LibGit2.URL_REGEX, "git@github.com:JuliaLang/Example.jl.git")
        @test m[:scheme] === nothing
        @test m[:user] == "git"
        @test m[:password] === nothing
        @test m[:host] == "github.com"
        @test m[:port] === nothing
        @test m[:path] == "JuliaLang/Example.jl.git"
    end

    @testset "usernames with special characters" begin
        m = match(LibGit2.URL_REGEX, "user-name@hostname.com")
        @test m[:user] == "user-name"
    end

    @testset "HTTPS URL, no path" begin
        m = match(LibGit2.URL_REGEX, "https://user:pass@server.com:80")
        @test m[:path] === nothing
    end

    @testset "scp-like syntax, no path" begin
        m = match(LibGit2.URL_REGEX, "user@server:")
        @test m[:path] == ""

        m = match(LibGit2.URL_REGEX, "user@server")
        @test m[:path] === nothing
    end

    @testset "HTTPS URL, invalid path" begin
        m = match(LibGit2.URL_REGEX, "https://git@server:repo")
        @test m === nothing
    end

    # scp-like syntax should have a colon separating the hostname from the path
    @testset "scp-like syntax, invalid path" begin
        m = match(LibGit2.URL_REGEX, "git@server/repo")
        @test m === nothing
    end
end

@testset "Git URL formatting" begin
    @testset "HTTPS URL" begin
        url = LibGit2.git_url(
            scheme="https",
            username="user",
            password="pass",
            host="server.com",
            port=80,
            path="org/project.git")
        @test url == "https://user:pass@server.com:80/org/project.git"
    end

    @testset "SSH URL" begin
        url = LibGit2.git_url(
            scheme="ssh",
            username="user",
            password="pass",
            host="server",
            port="22",
            path="project.git")
        @test url == "ssh://user:pass@server:22/project.git"
    end

    @testset "SSH URL, scp-like syntax" begin
        url = LibGit2.git_url(
            scheme="",
            username="user",
            host="server",
            path="project.git")
        @test url == "user@server:project.git"
    end

    @testset "HTTPS URL, realistic" begin
        url = LibGit2.git_url(
            scheme="https",
            host="github.com",
            path="JuliaLang/Example.jl.git")
        @test url == "https://github.com/JuliaLang/Example.jl.git"
    end

    @testset "SSH URL, realistic" begin
        url = LibGit2.git_url(
            username="git",
            host="github.com",
            path="JuliaLang/Example.jl.git")
        @test url == "git@github.com:JuliaLang/Example.jl.git"
    end

    @testset "HTTPS URL, no path" begin
        url = LibGit2.git_url(
            scheme="https",
            username="user",
            password="pass",
            host="server.com",
            port="80")
        @test url == "https://user:pass@server.com:80"
    end

    @testset "scp-like syntax, no path" begin
        url = LibGit2.git_url(
            username="user",
            host="server.com")
        @test url == "user@server.com"
    end

    @testset "HTTP URL, path includes slash prefix" begin
        url = LibGit2.git_url(
            scheme="http",
            host="server.com",
            path="/path")
        @test url == "http://server.com/path"
    end

    @testset "empty" begin
        @test_throws ArgumentError LibGit2.git_url()

        @test LibGit2.git_url(host="server.com") == "server.com"
        url = LibGit2.git_url(
            scheme="",
            username="",
            password="",
            host="server.com",
            port="",
            path="")
        @test url == "server.com"
    end
end

@testset "Passphrase Required" begin
    @testset "missing file" begin
        @test !LibGit2.is_passphrase_required("")

        file = joinpath(KEY_DIR, "foobar")
        @test !isfile(file)
        @test !LibGit2.is_passphrase_required(file)
    end

    @testset "not private key" begin
        @test !LibGit2.is_passphrase_required(joinpath(KEY_DIR, "invalid.pub"))
    end

    @testset "private key, with passphrase" begin
        @test LibGit2.is_passphrase_required(joinpath(KEY_DIR, "valid-passphrase"))
    end

    @testset "private key, no passphrase" begin
        @test !LibGit2.is_passphrase_required(joinpath(KEY_DIR, "valid"))
    end
end

mktempdir() do dir
    # test parameters
    repo_url = "https://github.com/JuliaLang/Example.jl"
    cache_repo = joinpath(dir, "Example")
    test_repo = joinpath(dir, "Example.Test")
    test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
    test_file = "testfile"
    config_file = "testconfig"
    commit_msg1 = randstring(10)
    commit_msg2 = randstring(10)
    commit_oid1 = LibGit2.GitHash()
    commit_oid2 = LibGit2.GitHash()
    commit_oid3 = LibGit2.GitHash()
    master_branch = "master"
    test_branch = "test_branch"
    test_branch2 = "test_branch_two"
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

            # Ordering of entries appears random when using `LibGit2.set!`
            count = 0
            for entry in LibGit2.GitConfigIter(cfg, r"tmp.*")
                count += 1
                name, value = unsafe_string(entry.name), unsafe_string(entry.value)
                if name == "tmp.str"
                    @test value == "AAAA"
                elseif name == "tmp.int32"
                    @test value == "1"
                elseif name == "tmp.int64"
                    @test value == "1"
                elseif name == "tmp.bool"
                    @test value == "true"
                else
                    error("Found unexpected entry: $name")
                end
                show_str = sprint(show, entry)
                @test show_str == string("ConfigEntry(\"", name, "\", \"", value, "\")")
            end
            @test count == 4
        finally
            close(cfg)
        end
    end

    @testset "Configuration Iteration" begin
        config_path = joinpath(dir, config_file)

        # Write config entries with duplicate names
        open(config_path, "a") do fp
            write(fp, """
            [credential]
            \thelper = store
            [credential]
            \thelper = cache
            """)
        end

        cfg = LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)
        try
            # Will only see the last entry
            @test LibGit2.get(cfg, "credential.helper", "") == "cache"

            count = 0
            for entry in LibGit2.GitConfigIter(cfg, "credential.helper")
                count += 1
                name, value = unsafe_string(entry.name), unsafe_string(entry.value)
                @test name == "credential.helper"
                @test value == (count == 1 ? "store" : "cache")
            end
            @test count == 2
        finally
            close(cfg)
        end
    end

    @testset "Initializing repository" begin
        @testset "with remote branch" begin
            repo = LibGit2.init(cache_repo)
            try
                @test isdir(cache_repo)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(cache_repo))
                @test isdir(joinpath(cache_repo, ".git"))
                # set a remote branch
                branch = "upstream"
                LibGit2.GitRemote(repo, branch, repo_url) |> close

                # test remote's representation in the repo's config
                config = joinpath(cache_repo, ".git", "config")
                lines = split(open(x->read(x, String), config, "r"), "\n")
                @test any(map(x->x == "[remote \"upstream\"]", lines))

                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                # test various remote properties
                @test LibGit2.url(remote) == repo_url
                @test LibGit2.push_url(remote) == ""
                @test LibGit2.name(remote) == "upstream"
                @test isa(remote, LibGit2.GitRemote)

                # test showing a GitRemote object
                @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: $repo_url"

                # test setting and getting the remote's URL
                @test LibGit2.isattached(repo)
                LibGit2.set_remote_url(repo, "upstream", "unknown")
                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                @test LibGit2.url(remote) == "unknown"
                @test LibGit2.push_url(remote) == "unknown"
                @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: unknown"
                close(remote)
                LibGit2.set_remote_url(cache_repo, "upstream", repo_url)
                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                @test LibGit2.url(remote) == repo_url
                @test LibGit2.push_url(remote) == repo_url
                @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: $repo_url"
                LibGit2.add_fetch!(repo, remote, "upstream")

                # test setting fetch and push refspecs
                @test LibGit2.fetch_refspecs(remote) == String["+refs/heads/*:refs/remotes/upstream/*"]
                LibGit2.add_push!(repo, remote, "refs/heads/master")
                close(remote)
                remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
                @test LibGit2.push_refspecs(remote) == String["refs/heads/master"]
                close(remote)
                # constructor with a refspec
                remote = LibGit2.GitRemote(repo, "upstream2", repo_url, "upstream")
                @test sprint(show, remote) == "GitRemote:\nRemote name: upstream2 url: $repo_url"
                @test LibGit2.fetch_refspecs(remote) == String["upstream"]
                close(remote)

                remote = LibGit2.GitRemoteAnon(repo, repo_url)
                @test LibGit2.url(remote) == repo_url
                @test LibGit2.push_url(remote) == ""
                @test LibGit2.name(remote) == ""
                @test isa(remote, LibGit2.GitRemote)
                close(remote)
            finally
                close(repo)
            end
        end

        @testset "bare" begin
            path = joinpath(dir, "Example.Bare")
            repo = LibGit2.init(path, true)
            try
                @test isdir(path)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(path))
                @test isfile(joinpath(path, LibGit2.Consts.HEAD_FILE))
                @test LibGit2.isattached(repo)
            finally
                close(repo)
            end

            path = joinpath("garbagefakery", "Example.Bare")
            try
                LibGit2.GitRepo(path)
                error("unexpected")
            catch e
                @test typeof(e) == LibGit2.GitError
                @test startswith(
                    lowercase(sprint(show, e)),
                    lowercase("GitError(Code:ENOTFOUND, Class:OS, failed to resolve path"))
            end
            path = joinpath(dir, "Example.BareTwo")
            repo = LibGit2.init(path, true)
            try
                #just to see if this works
                LibGit2.cleanup(repo)
            finally
                close(repo)
            end
        end
    end

    @testset "Cloning repository" begin
        @testset "bare" begin
            repo_path = joinpath(dir, "Example.Bare1")
            repo = LibGit2.clone(cache_repo, repo_path, isbare = true)
            try
                @test isdir(repo_path)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(repo_path))
                @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
                @test LibGit2.isattached(repo)
                @test LibGit2.remotes(repo) == ["origin"]
            finally
                close(repo)
            end
        end
        @testset "bare with remote callback" begin
            repo_path = joinpath(dir, "Example.Bare2")
            repo = LibGit2.clone(cache_repo, repo_path, isbare = true, remote_cb = LibGit2.mirror_cb())
            try
                @test isdir(repo_path)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(repo_path))
                @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
                rmt = LibGit2.get(LibGit2.GitRemote, repo, "origin")
                try
                    @test LibGit2.fetch_refspecs(rmt)[1] == "+refs/*:refs/*"
                    @test LibGit2.isattached(repo)
                    @test LibGit2.remotes(repo) == ["origin"]
                finally
                    close(rmt)
                end
            finally
                close(repo)
            end
        end
        @testset "normal" begin
            repo = LibGit2.clone(cache_repo, test_repo)
            try
                @test isdir(test_repo)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(test_repo))
                @test isdir(joinpath(test_repo, ".git"))
                @test LibGit2.workdir(repo) == LibGit2.path(repo)*"/"
                @test LibGit2.isattached(repo)
                @test LibGit2.isorphan(repo)
                repo_str = sprint(show, repo)
                @test repo_str == "LibGit2.GitRepo($(sprint(show,LibGit2.path(repo))))"
            finally
                close(repo)
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
                @test LibGit2.GitHash(LibGit2.head(cache_repo)) == commit_oid1

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

                # test getting list of commit authors
                auths = LibGit2.authors(repo)
                @test length(auths) == 3
                for auth in auths
                    @test auth.name == test_sig.name
                    @test auth.time == test_sig.time
                    @test auth.email == test_sig.email
                end

                # check various commit properties - commit_oid1 happened before
                # commit_oid2, so it *is* an ancestor of commit_oid2
                @test LibGit2.is_ancestor_of(string(commit_oid1), string(commit_oid2), repo)
                @test LibGit2.iscommit(string(commit_oid1), repo)
                @test !LibGit2.iscommit(string(commit_oid1)*"fake", repo)
                @test LibGit2.iscommit(string(commit_oid2), repo)

                # lookup commits
                cmt = LibGit2.GitCommit(repo, commit_oid1)
                try
                    @test LibGit2.Consts.OBJECT(typeof(cmt)) == LibGit2.Consts.OBJ_COMMIT
                    @test commit_oid1 == LibGit2.GitHash(cmt)
                    short_oid1 = LibGit2.GitShortHash(string(commit_oid1))
                    @test hex(commit_oid1) == hex(short_oid1)
                    @test cmp(commit_oid1, short_oid1) == 0
                    @test cmp(short_oid1, commit_oid1) == 0
                    @test !(short_oid1 < commit_oid1)

                    # test showing ShortHash
                    short_str = sprint(show, short_oid1)
                    @test short_str == "GitShortHash(\"$(string(short_oid1))\")"
                    short_oid2 = LibGit2.GitShortHash(cmt)
                    @test startswith(hex(commit_oid1), hex(short_oid2))

                    cmt2 = LibGit2.GitCommit(repo, short_oid2)
                    try
                        @test commit_oid1 == LibGit2.GitHash(cmt2)
                    finally
                        close(cmt2)
                    end
                    # check that the author and committer signatures are correct
                    auth = LibGit2.author(cmt)
                    @test isa(auth, LibGit2.Signature)
                    @test auth.name == test_sig.name
                    @test auth.time == test_sig.time
                    @test auth.email == test_sig.email
                    short_auth = LibGit2.author(LibGit2.GitCommit(repo, short_oid1))
                    @test short_auth.name == test_sig.name
                    @test short_auth.time == test_sig.time
                    @test short_auth.email == test_sig.email
                    cmtr = LibGit2.committer(cmt)
                    @test isa(cmtr, LibGit2.Signature)
                    @test cmtr.name == test_sig.name
                    @test cmtr.time == test_sig.time
                    @test cmtr.email == test_sig.email
                    @test LibGit2.message(cmt) == commit_msg1

                    # test showing the commit
                    showstr = split(sprint(show, cmt), "\n")
                    # the time of the commit will vary so just test the first two parts
                    @test contains(showstr[1], "Git Commit:")
                    @test contains(showstr[2], "Commit Author: Name: TEST, Email: TEST@TEST.COM, Time:")
                    @test contains(showstr[3], "Committer: Name: TEST, Email: TEST@TEST.COM, Time:")
                    @test contains(showstr[4], "SHA:")
                    @test showstr[5] == "Message:"
                    @test showstr[6] == commit_msg1
                    @test LibGit2.revcount(repo, string(commit_oid1), string(commit_oid3)) == (-1,0)

                    blame = LibGit2.GitBlame(repo, test_file)
                    @test LibGit2.counthunks(blame) == 3
                    @test_throws BoundsError getindex(blame, LibGit2.counthunks(blame)+1)
                    @test_throws BoundsError getindex(blame, 0)
                    sig = LibGit2.Signature(blame[1].orig_signature)
                    @test sig.name == cmtr.name
                    @test sig.email == cmtr.email
                    show_strs = split(sprint(show, blame[1]), "\n")
                    @test show_strs[1] == "GitBlameHunk:"
                    @test show_strs[2] == "Original path: $test_file"
                    @test show_strs[3] == "Lines in hunk: 1"
                    @test show_strs[4] == "Final commit oid: $commit_oid1"
                    @test show_strs[6] == "Original commit oid: $commit_oid1"
                    @test length(show_strs) == 7
                finally
                    close(cmt)
                end
            finally
                close(repo)
                close(repo_file)
            end
        end

        @testset "with branch" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                brnch = LibGit2.branch(repo)
                brref = LibGit2.head(repo)
                try
                    # various branch properties
                    @test LibGit2.isbranch(brref)
                    @test !LibGit2.isremote(brref)
                    @test LibGit2.name(brref) == "refs/heads/master"
                    @test LibGit2.shortname(brref) == master_branch
                    @test LibGit2.ishead(brref)
                    @test isnull(LibGit2.upstream(brref))

                    # showing the GitReference to this branch
                    show_strs = split(sprint(show, brref), "\n")
                    @test show_strs[1] == "GitReference:"
                    @test show_strs[2] == "Branch with name refs/heads/master"
                    @test show_strs[3] == "Branch is HEAD."
                    @test repo.ptr == LibGit2.repository(brref).ptr
                    @test brnch == master_branch
                    @test LibGit2.headname(repo) == master_branch

                    # create a branch *without* setting its tip as HEAD
                    LibGit2.branch!(repo, test_branch, string(commit_oid1), set_head=false)
                    # null because we are looking for a REMOTE branch
                    @test isnull(LibGit2.lookup_branch(repo, test_branch, true))
                    # not null because we are now looking for a LOCAL branch
                    tbref = Base.get(LibGit2.lookup_branch(repo, test_branch, false))
                    try
                        @test LibGit2.shortname(tbref) == test_branch
                        @test isnull(LibGit2.upstream(tbref))
                    finally
                        close(tbref)
                    end
                    @test isnull(LibGit2.lookup_branch(repo, test_branch2, true))
                    # test deleting the branch
                    LibGit2.branch!(repo, test_branch2; set_head=false)
                    tbref = Base.get(LibGit2.lookup_branch(repo, test_branch2, false))
                    try
                        @test LibGit2.shortname(tbref) == test_branch2
                        LibGit2.delete_branch(tbref)
                        @test isnull(LibGit2.lookup_branch(repo, test_branch2, true))
                    finally
                        close(tbref)
                    end
                finally
                    close(brref)
                end
                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test master_branch in branches
                @test test_branch in branches
            finally
                close(repo)
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
                    @test LibGit2.getconfig(repo, "user.name", "") == "AAAA"
                    @test LibGit2.getconfig(cache_repo, "user.name", "") == "AAAA"
                end
            finally
                close(repo)
            end
        end

        @testset "with tags" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 0

                # create tag and extract it from a GitReference
                tag_oid1 = LibGit2.tag_create(repo, tag1, commit_oid1, sig=test_sig)
                @test !LibGit2.iszero(tag_oid1)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag1 in tags
                tag1ref = LibGit2.GitReference(repo, "refs/tags/$tag1")
                # because this is a reference to an OID
                @test isempty(LibGit2.fullname(tag1ref))

                # test showing a GitReference to a GitTag, and the GitTag itself
                show_strs = split(sprint(show, tag1ref), "\n")
                @test show_strs[1] == "GitReference:"
                @test show_strs[2] == "Tag with name refs/tags/$tag1"
                tag1tag = LibGit2.peel(LibGit2.GitTag,tag1ref)
                @test LibGit2.name(tag1tag) == tag1
                @test LibGit2.target(tag1tag) == commit_oid1
                @test sprint(show, tag1tag) == "GitTag:\nTag name: $tag1 target: $commit_oid1"
                tag_oid2 = LibGit2.tag_create(repo, tag2, commit_oid2)
                @test !LibGit2.iszero(tag_oid2)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 2
                @test tag2 in tags

                refs = LibGit2.ref_list(repo)
                @test refs == ["refs/heads/master", "refs/heads/test_branch", "refs/tags/tag1", "refs/tags/tag2"]
                # test deleting a tag
                LibGit2.tag_delete(repo, tag1)
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag2 ∈ tags
                @test tag1 ∉ tags

                # test git describe functions applied to these GitTags
                description = LibGit2.GitDescribeResult(repo)
                fmtted_description = LibGit2.format(description)
                @test sprint(show, description) == "GitDescribeResult:\n$fmtted_description\n"
                @test fmtted_description == "tag2"
                description = LibGit2.GitDescribeResult(LibGit2.GitObject(repo, "HEAD"))
                fmtted_description = LibGit2.format(description)
                @test sprint(show, description) == "GitDescribeResult:\n$fmtted_description\n"
                @test fmtted_description == "tag2"
            finally
                close(repo)
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
                # we've added a file - show that it is new
                @test status[1].status == LibGit2.Consts.STATUS_WT_NEW
                close(repo_file)
            finally
                close(repo)
            end
        end

        @testset "blobs" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                # this is slightly dubious, as it assumes the object has not been packed
                # could be replaced by another binary format
                hash_string = hex(commit_oid1)
                blob_file   = joinpath(cache_repo,".git/objects", hash_string[1:2], hash_string[3:end])

                id = LibGit2.addblob!(repo, blob_file)
                blob = LibGit2.GitBlob(repo, id)
                @test LibGit2.isbinary(blob)
                len1 = length(blob)

                # test showing a GitBlob
                blob_show_strs = split(sprint(show, blob), "\n")
                @test blob_show_strs[1] == "GitBlob:"
                @test contains(blob_show_strs[2], "Blob id:")
                @test blob_show_strs[3] == "Contents are binary."

                blob2 = LibGit2.GitBlob(repo, LibGit2.GitHash(blob))
                @test LibGit2.isbinary(blob2)
                @test length(blob2) == len1
            finally
                close(repo)
            end
        end
        @testset "trees" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                @test_throws LibGit2.Error.GitError LibGit2.GitTree(repo, "HEAD")
                tree = LibGit2.GitTree(repo, "HEAD^{tree}")
                @test isa(tree, LibGit2.GitTree)
                @test isa(LibGit2.GitObject(repo, "HEAD^{tree}"), LibGit2.GitTree)
                @test LibGit2.Consts.OBJECT(typeof(tree)) == LibGit2.Consts.OBJ_TREE
                @test count(tree) == 1

                # test showing the GitTree and its entries
                tree_str = sprint(show, tree)
                @test tree_str == "GitTree:\nOwner: $(LibGit2.repository(tree))\nNumber of entries: 1\n"
                @test_throws BoundsError tree[0]
                @test_throws BoundsError tree[2]
                tree_entry = tree[1]
                @test LibGit2.filemode(tree_entry) == 33188
                te_str = sprint(show, tree_entry)
                ref_te_str = "GitTreeEntry:\nEntry name: testfile\nEntry type: Base.LibGit2.GitBlob\nEntry OID: "
                ref_te_str *= "$(LibGit2.entryid(tree_entry))\n"
                @test te_str == ref_te_str
                blob = LibGit2.GitBlob(tree_entry)
                blob_str = sprint(show, blob)
                @test blob_str == "GitBlob:\nBlob id: $(LibGit2.GitHash(blob))\nContents:\n$(LibGit2.content(blob))\n"
            finally
                close(repo)
            end
        end

        @testset "diff" begin
            repo = LibGit2.GitRepo(cache_repo)
            try
                @test !LibGit2.isdirty(repo)
                @test !LibGit2.isdirty(repo, test_file)
                @test !LibGit2.isdirty(repo, "nonexistent")
                @test !LibGit2.isdiff(repo, "HEAD")
                @test !LibGit2.isdirty(repo, cached=true)
                @test !LibGit2.isdirty(repo, test_file, cached=true)
                @test !LibGit2.isdirty(repo, "nonexistent", cached=true)
                @test !LibGit2.isdiff(repo, "HEAD", cached=true)
                open(joinpath(cache_repo,test_file), "a") do f
                    println(f, "zzzz")
                end
                @test LibGit2.isdirty(repo)
                @test LibGit2.isdirty(repo, test_file)
                @test !LibGit2.isdirty(repo, "nonexistent")
                @test LibGit2.isdiff(repo, "HEAD")
                @test !LibGit2.isdirty(repo, cached=true)
                @test !LibGit2.isdiff(repo, "HEAD", cached=true)
                LibGit2.add!(repo, test_file)
                @test LibGit2.isdirty(repo)
                @test LibGit2.isdiff(repo, "HEAD")
                @test LibGit2.isdirty(repo, cached=true)
                @test LibGit2.isdiff(repo, "HEAD", cached=true)
                tree = LibGit2.GitTree(repo, "HEAD^{tree}")

                # test properties of the diff_tree
                diff = LibGit2.diff_tree(repo, tree, "", cached=true)
                @test count(diff) == 1
                @test_throws BoundsError diff[0]
                @test_throws BoundsError diff[2]
                @test LibGit2.Consts.DELTA_STATUS(diff[1].status) == LibGit2.Consts.DELTA_MODIFIED
                @test diff[1].nfiles == 2

                # test showing a DiffDelta
                diff_strs = split(sprint(show, diff[1]), '\n')
                @test diff_strs[1] == "DiffDelta:"
                @test diff_strs[2] == "Status: DELTA_MODIFIED"
                @test diff_strs[3] == "Number of files: 2"
                @test diff_strs[4] == "Old file:"
                @test diff_strs[5] == "DiffFile:"
                @test contains(diff_strs[6], "Oid:")
                @test contains(diff_strs[7], "Path:")
                @test contains(diff_strs[8], "Size:")
                @test isempty(diff_strs[9])
                @test diff_strs[10] == "New file:"

                # test showing a GitDiff
                diff_strs = split(sprint(show, diff), '\n')
                @test diff_strs[1] == "GitDiff:"
                @test diff_strs[2] == "Number of deltas: 1"
                @test diff_strs[3] == "GitDiffStats:"
                @test diff_strs[4] == "Files changed: 1"
                @test diff_strs[5] == "Insertions: 1"
                @test diff_strs[6] == "Deletions: 0"

                LibGit2.commit(repo, "zzz")
                @test !LibGit2.isdirty(repo)
                @test !LibGit2.isdiff(repo, "HEAD")
                @test !LibGit2.isdirty(repo, cached=true)
                @test !LibGit2.isdiff(repo, "HEAD", cached=true)
            finally
                close(repo)
            end
        end
    end

    # TO DO: add more tests for various merge
    # preference options
    @testset "Fastforward merges" begin
        path = joinpath(dir, "Example.FF")
        repo = LibGit2.clone(cache_repo, path)
        # need to set this for merges to succeed
        cfg = LibGit2.GitConfig(repo)
        LibGit2.set!(cfg, "user.name", "AAAA")
        LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")
        try
            # Sets up a branch "branch/ff_a" which will be two commits ahead
            # of "master". It's possible to fast-forward merge "branch/ff_a"
            # into "master", which is the default behavior.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/ff_a")
            open(joinpath(LibGit2.path(repo),"ff_file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(repo, "ff_file1")
            LibGit2.commit(repo, "add ff_file1")

            open(joinpath(LibGit2.path(repo),"ff_file2"),"w") do f
                write(f, "222\n")
            end
            LibGit2.add!(repo, "ff_file2")
            LibGit2.commit(repo, "add ff_file2")
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/a
            # set up the merge using GitAnnotated objects
            upst_ann = LibGit2.GitAnnotated(repo, "branch/ff_a")
            head_ann = LibGit2.GitAnnotated(repo, "master")

            # ff merge them
            @test LibGit2.merge!(repo, [upst_ann], true)
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            # Repeat the process, but specifying a commit to merge in as opposed
            # to a branch name or GitAnnotated.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/ff_b")
            open(joinpath(LibGit2.path(repo),"ff_file3"),"w") do f
                write(f, "333\n")
            end
            LibGit2.add!(repo, "ff_file3")
            LibGit2.commit(repo, "add ff_file3")

            open(joinpath(LibGit2.path(repo),"ff_file4"),"w") do f
                write(f, "444\n")
            end
            LibGit2.add!(repo, "ff_file4")
            LibGit2.commit(repo, "add ff_file4")
            branchhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/a using committish
            @test LibGit2.merge!(repo, committish=string(branchhead))
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            # Repeat the process, but specifying a branch name to merge in as opposed
            # to a commit or GitAnnotated.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/ff_c")
            open(joinpath(LibGit2.path(repo),"ff_file5"),"w") do f
                write(f, "555\n")
            end
            LibGit2.add!(repo, "ff_file5")
            LibGit2.commit(repo, "add ff_file5")

            open(joinpath(LibGit2.path(repo),"ff_file6"),"w") do f
                write(f, "666\n")
            end
            LibGit2.add!(repo, "ff_file6")
            LibGit2.commit(repo, "add ff_file6")
            branchhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/ff_c using branch name
            @test LibGit2.merge!(repo, branch="refs/heads/branch/ff_c")
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)
        finally
            close(repo)
        end
    end

    @testset "Cherrypick" begin
        path = joinpath(dir, "Example.Cherrypick")
        repo = LibGit2.clone(cache_repo, path)
        # need to set this for merges to succeed
        cfg = LibGit2.GitConfig(repo)
        LibGit2.set!(cfg, "user.name", "AAAA")
        LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")
        try
            # Create a commit on the new branch and cherry-pick it over to
            # master. Since the cherry-pick does *not* make a new commit on
            # master, we have to create our own commit of the dirty state.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/cherry_a")
            open(joinpath(LibGit2.path(repo),"file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(repo, "file1")
            cmt_oid = LibGit2.commit(repo, "add file1")
            cmt = LibGit2.GitCommit(repo, cmt_oid)
            # switch back, try to cherrypick
            # from branch/cherry_a
            LibGit2.branch!(repo, "master")
            LibGit2.cherrypick(repo, cmt, options=Base.LibGit2.CherrypickOptions())
            cmt_oid2 = LibGit2.commit(repo, "add file1")
            @test isempty(LibGit2.diff_files(repo, "master", "branch/cherry_a"))
        finally
            close(repo)
        end
    end

    @testset "Merges" begin
        path = joinpath(dir, "Example.Merge")
        repo = LibGit2.clone(cache_repo, path)
        # need to set this for merges to succeed
        cfg = LibGit2.GitConfig(repo)
        LibGit2.set!(cfg, "user.name", "AAAA")
        LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")
        try
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/merge_a")
            open(joinpath(LibGit2.path(repo),"file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(repo, "file1")
            LibGit2.commit(repo, "add file1")

            # switch back, add a commit, try to merge
            # from branch/merge_a
            LibGit2.branch!(repo, "master")

            # test for showing a Reference to a non-HEAD branch
            brref = LibGit2.GitReference(repo, "refs/heads/branch/merge_a")
            @test LibGit2.name(brref) == "refs/heads/branch/merge_a"
            @test !LibGit2.ishead(brref)
            show_strs = split(sprint(show, brref), "\n")
            @test show_strs[1] == "GitReference:"
            @test show_strs[2] == "Branch with name refs/heads/branch/merge_a"
            @test show_strs[3] == "Branch is not HEAD."

            open(joinpath(LibGit2.path(repo), "file2"), "w") do f
                write(f, "222\n")
            end
            LibGit2.add!(repo, "file2")
            LibGit2.commit(repo, "add file2")

            upst_ann = LibGit2.GitAnnotated(repo, "branch/merge_a")
            head_ann = LibGit2.GitAnnotated(repo, "master")

            # (fail to) merge them because we can't fastforward
            @test_warn "WARNING: Cannot perform fast-forward merge." !LibGit2.merge!(repo, [upst_ann], true)
            # merge them now that we allow non-ff
            @test_warn "INFO: Review and commit merged changes." LibGit2.merge!(repo, [upst_ann], false)
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            # go back to merge_a and rename a file
            LibGit2.branch!(repo, "branch/merge_b")
            mv(joinpath(LibGit2.path(repo),"file1"),joinpath(LibGit2.path(repo),"mvfile1"))
            LibGit2.add!(repo, "mvfile1")
            LibGit2.commit(repo, "move file1")
            LibGit2.branch!(repo, "master")
            upst_ann = LibGit2.GitAnnotated(repo, "branch/merge_b")
            rename_flag = 0
            rename_flag = LibGit2.toggle(rename_flag, 0) # turns on the find renames opt
            mos = LibGit2.MergeOptions(flags=rename_flag)
            @test_warn "INFO: Review and commit merged changes." LibGit2.merge!(repo, [upst_ann], merge_opts=mos)
        finally
            close(repo)
        end
    end

    @testset "push" begin
        up_path = joinpath(dir, "Example.PushUp")
        up_repo = LibGit2.clone(cache_repo, up_path)
        our_path = joinpath(dir, "Example.Push")
        our_repo = LibGit2.clone(cache_repo, our_path)
        # need to set this for merges to succeed
        our_cfg = LibGit2.GitConfig(our_repo)
        LibGit2.set!(our_cfg, "user.name", "AAAA")
        LibGit2.set!(our_cfg, "user.email", "BBBB@BBBB.COM")
        up_cfg = LibGit2.GitConfig(up_repo)
        LibGit2.set!(up_cfg, "user.name", "AAAA")
        LibGit2.set!(up_cfg, "user.email", "BBBB@BBBB.COM")
        try
            open(joinpath(LibGit2.path(our_repo),"file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(our_repo, "file1")
            LibGit2.commit(our_repo, "add file1")
            if LibGit2.version() >= v"0.26.0" # See #21872, #21639 and #21597
                # we cannot yet locally push to non-bare repos
                @test_throws LibGit2.GitError LibGit2.push(our_repo, remoteurl=up_path)
            end
        finally
            close(our_repo)
            close(up_repo)
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
            new_head = LibGit2.reset!(repo, head_oid, LibGit2.Consts.RESET_HARD)
            @test isfile(joinpath(test_repo, test_file))
            @test new_head == head_oid

            # GitAnnotated for a fetchhead
            fh_ann = LibGit2.GitAnnotated(repo, LibGit2.Consts.FETCH_HEAD)
            @test LibGit2.GitHash(fh_ann) == head_oid

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

            # If upstream argument is empty, libgit2 will look for tracking
            # information. If the current branch isn't tracking any upstream
            # the rebase should fail.
            @test_throws LibGit2.GitError LibGit2.rebase!(repo)
            # Try rebasing on master instead
            newhead = LibGit2.rebase!(repo, master_branch)
            @test newhead == head_oid

            # Switch to the master branch
            LibGit2.branch!(repo, master_branch)

            fetch_heads = LibGit2.fetchheads(repo)
            @test fetch_heads[1].name == "refs/heads/master"
            @test fetch_heads[1].ismerge == true # we just merged master
            @test fetch_heads[2].name == "refs/heads/test_branch"
            @test fetch_heads[2].ismerge == false
            @test fetch_heads[3].name == "refs/tags/tag2"
            @test fetch_heads[3].ismerge == false
            for fh in fetch_heads
                @test fh.url == cache_repo
                fh_strs = split(sprint(show, fh), '\n')
                @test fh_strs[1] == "FetchHead:"
                @test fh_strs[2] == "Name: $(fh.name)"
                @test fh_strs[3] == "URL: $(fh.url)"
                @test fh_strs[5] == "Merged: $(fh.ismerge)"
            end
        finally
            close(repo)
        end
    end

    @testset "Examine test repository" begin
        @testset "files" begin
            @test read(joinpath(test_repo, test_file), String) == read(joinpath(cache_repo, test_file), String)
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
                    close(tag2ref)
                end
            finally
                close(repo)
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
                LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    @test count((oid,repo)->(oid == commit_oid1), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME) == 1
                end
            finally
                close(repo)
                close(cache)
            end
        end
    end

    @testset "Modify and reset repository" begin
        repo = LibGit2.GitRepo(test_repo)
        try
            # check index for file
            LibGit2.with(LibGit2.GitIndex(repo)) do idx
                i = find(test_file, idx)
                @test !isnull(i)
                idx_entry = idx[get(i)]
                @test idx_entry !== nothing
                idx_entry_str = sprint(show, idx_entry)
                @test idx_entry_str == "IndexEntry($(string(idx_entry.id)))"
                @test LibGit2.stage(idx_entry) == 0

                i = find("zzz", idx)
                @test isnull(i)
                idx_str = sprint(show, idx)
                @test idx_str == "GitIndex:\nRepository: $(LibGit2.repository(idx))\nNumber of elements: 1\n"

                LibGit2.remove!(repo, test_file)
                LibGit2.read!(repo)
                @test count(idx) == 0
                LibGit2.add!(repo, test_file)
                LibGit2.update!(repo, test_file)
                @test count(idx) == 1
            end

            # check non-existent file status
            st = LibGit2.status(repo, "XYZ")
            @test isnull(st)

            # check file status
            st = LibGit2.status(repo, test_file)
            @test !isnull(st)
            @test LibGit2.isset(get(st), LibGit2.Consts.STATUS_CURRENT)

            # modify file
            open(joinpath(test_repo, test_file), "a") do io
                write(io, 0x41)
            end

            # file modified but not staged
            st_mod = LibGit2.status(repo, test_file)
            @test !LibGit2.isset(get(st_mod), LibGit2.Consts.STATUS_INDEX_MODIFIED)
            @test LibGit2.isset(get(st_mod), LibGit2.Consts.STATUS_WT_MODIFIED)

            # stage file
            LibGit2.add!(repo, test_file)

            # modified file staged
            st_stg = LibGit2.status(repo, test_file)
            @test LibGit2.isset(get(st_stg), LibGit2.Consts.STATUS_INDEX_MODIFIED)
            @test !LibGit2.isset(get(st_stg), LibGit2.Consts.STATUS_WT_MODIFIED)

            # try to unstage to unknown commit
            @test_throws LibGit2.Error.GitError LibGit2.reset!(repo, "XYZ", test_file)

            # status should not change
            st_new = LibGit2.status(repo, test_file)
            @test get(st_new) == get(st_stg)

            # try to unstage to HEAD
            new_head = LibGit2.reset!(repo, LibGit2.Consts.HEAD_FILE, test_file)
            st_uns = LibGit2.status(repo, test_file)
            @test get(st_uns) == get(st_mod)

            # reset repo
            @test_throws LibGit2.Error.GitError LibGit2.reset!(repo, LibGit2.GitHash(), LibGit2.Consts.RESET_HARD)

            new_head = LibGit2.reset!(repo, LibGit2.head_oid(repo), LibGit2.Consts.RESET_HARD)
            open(joinpath(test_repo, test_file), "r") do io
                @test read(io)[end] != 0x41
            end
        finally
            close(repo)
        end
    end

    @testset "Modify remote" begin
        path = test_repo
        repo = LibGit2.GitRepo(path)
        try
            remote_name = "test"
            url = "https://test.com/repo"

            @test isnull(LibGit2.lookup_remote(repo, remote_name))

            for r in (repo, path)
                # Set just the fetch URL
                LibGit2.set_remote_fetch_url(r, remote_name, url)
                remote = get(LibGit2.lookup_remote(repo, remote_name))
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) == url
                @test LibGit2.push_url(remote) == ""

                LibGit2.remote_delete(repo, remote_name)
                @test isnull(LibGit2.lookup_remote(repo, remote_name))

                # Set just the push URL
                LibGit2.set_remote_push_url(r, remote_name, url)
                remote = get(LibGit2.lookup_remote(repo, remote_name))
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) == ""
                @test LibGit2.push_url(remote) == url

                LibGit2.remote_delete(repo, remote_name)
                @test isnull(LibGit2.lookup_remote(repo, remote_name))

                # Set the fetch and push URL
                LibGit2.set_remote_url(r, remote_name, url)
                remote = get(LibGit2.lookup_remote(repo, remote_name))
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) ==  url
                @test LibGit2.push_url(remote) == url

                LibGit2.remote_delete(repo, remote_name)
                @test isnull(LibGit2.lookup_remote(repo, remote_name))
            end
            # Invalid remote name
            @test_throws LibGit2.GitError LibGit2.set_remote_url(repo, "", url)
            @test_throws LibGit2.GitError LibGit2.set_remote_url(repo, remote_name, "")
        finally
            close(repo)
        end
    end

    @testset "rebase" begin
        repo = LibGit2.GitRepo(test_repo)
        try
            LibGit2.branch!(repo, "branch/a")

            oldhead = LibGit2.head_oid(repo)
            open(joinpath(LibGit2.path(repo),"file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(repo, "file1")
            LibGit2.commit(repo, "add file1")

            open(joinpath(LibGit2.path(repo),"file2"),"w") do f
                write(f, "222\n")
            end
            LibGit2.add!(repo, "file2")
            LibGit2.commit(repo, "add file2")

            LibGit2.branch!(repo, "branch/b")

            # squash last 2 commits
            new_head = LibGit2.reset!(repo, oldhead, LibGit2.Consts.RESET_SOFT)
            @test new_head == oldhead
            LibGit2.commit(repo, "squash file1 and file2")

            # add another file
            open(joinpath(LibGit2.path(repo),"file3"),"w") do f
                write(f, "333\n")
            end
            LibGit2.add!(repo, "file3")
            LibGit2.commit(repo, "add file3")

            newhead = LibGit2.head_oid(repo)

            files = LibGit2.diff_files(repo, "branch/a", "branch/b", filter=Set([LibGit2.Consts.DELTA_ADDED]))
            @test files == ["file3"]
            files = LibGit2.diff_files(repo, "branch/a", "branch/b", filter=Set([LibGit2.Consts.DELTA_MODIFIED]))
            @test files == []
            # switch back and rebase
            LibGit2.branch!(repo, "branch/a")
            newnewhead = LibGit2.rebase!(repo, "branch/b")

            # issue #19624
            @test newnewhead == newhead

            # add yet another file
            open(joinpath(LibGit2.path(repo),"file4"),"w") do f
                write(f, "444\n")
            end
            LibGit2.add!(repo, "file4")
            LibGit2.commit(repo, "add file4")

            # rebase with onto
            newhead = LibGit2.rebase!(repo, "branch/a", "master")

            newerhead = LibGit2.head_oid(repo)
            @test newerhead == newhead

            # add yet more files
            open(joinpath(LibGit2.path(repo),"file5"),"w") do f
                write(f, "555\n")
            end
            LibGit2.add!(repo, "file5")
            LibGit2.commit(repo, "add file5")
            open(joinpath(LibGit2.path(repo),"file6"),"w") do f
                write(f, "666\n")
            end
            LibGit2.add!(repo, "file6")
            LibGit2.commit(repo, "add file6")

            pre_abort_head = LibGit2.head_oid(repo)
            # Rebase type
            head_ann = LibGit2.GitAnnotated(repo, "branch/a")
            upst_ann = LibGit2.GitAnnotated(repo, "master")
            rb = LibGit2.GitRebase(repo, head_ann, upst_ann)
            @test_throws BoundsError rb[3]
            @test_throws BoundsError rb[0]
            rbo = next(rb)
            rbo_str = sprint(show, rbo)
            @test rbo_str == "RebaseOperation($(string(rbo.id)))\nOperation type: REBASE_OPERATION_PICK\n"
            rb_str = sprint(show, rb)
            @test rb_str == "GitRebase:\nNumber: 2\nCurrently performing operation: 1\n"
            rbo = rb[2]
            rbo_str = sprint(show, rbo)
            @test rbo_str == "RebaseOperation($(string(rbo.id)))\nOperation type: REBASE_OPERATION_PICK\n"

            # test rebase abort
            LibGit2.abort(rb)
            @test LibGit2.head_oid(repo) == pre_abort_head
        finally
            close(repo)
        end
    end

    @testset "merge" begin
        path = joinpath(dir, "Example.simple_merge")
        repo = LibGit2.clone(cache_repo, path)
        cfg = LibGit2.GitConfig(repo)
        LibGit2.set!(cfg, "user.name", "AAAA")
        LibGit2.set!(cfg, "user.email", "BBBB@BBBB.COM")
        try
            LibGit2.branch!(repo, "branch/merge_a")

            a_head = LibGit2.head_oid(repo)
            open(joinpath(LibGit2.path(repo),"merge_file1"),"w") do f
                write(f, "111\n")
            end
            LibGit2.add!(repo, "merge_file1")
            LibGit2.commit(repo, "add merge_file1")
            LibGit2.branch!(repo, "master")
            a_head_ann = LibGit2.GitAnnotated(repo, "branch/merge_a")
            @test_warn "INFO: Review and commit merged changes." LibGit2.merge!(repo, [a_head_ann]) #merge returns true if successful
        finally
            close(repo)
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
            close(repo)
        end
    end

    @testset "checkout/headname" begin
        repo = LibGit2.GitRepo(cache_repo)
        try
            LibGit2.checkout!(repo, string(commit_oid1))
            @test !LibGit2.isattached(repo)
            @test LibGit2.headname(repo) == "(detached from $(string(commit_oid1)[1:7]))"
        finally
            close(repo)
        end
    end


    if Sys.isunix()
        @testset "checkout/proptest" begin
            repo = LibGit2.GitRepo(test_repo)
            try
                cp(joinpath(test_repo, test_file), joinpath(test_repo, "proptest"))
                LibGit2.add!(repo, "proptest")
                id1 = LibGit2.commit(repo, "test property change 1")
                # change in file permissions (#17610)
                chmod(joinpath(test_repo, "proptest"),0o744)
                LibGit2.add!(repo, "proptest")
                id2 = LibGit2.commit(repo, "test property change 2")
                LibGit2.checkout!(repo, string(id1))
                @test !LibGit2.isdirty(repo)
                # change file to symlink (#18420)
                mv(joinpath(test_repo, "proptest"), joinpath(test_repo, "proptest2"))
                symlink(joinpath(test_repo, "proptest2"), joinpath(test_repo, "proptest"))
                LibGit2.add!(repo, "proptest", "proptest2")
                id3 = LibGit2.commit(repo, "test symlink change")
                LibGit2.checkout!(repo, string(id1))
                @test !LibGit2.isdirty(repo)
            finally
                close(repo)
            end
        end
    end


    @testset "Credentials" begin
        creds_user = "USER"
        creds_pass = "PASS"
        creds = LibGit2.UserPasswordCredentials(creds_user, creds_pass)
        @test creds.user == creds_user
        @test creds.pass == creds_pass
        creds2 = LibGit2.UserPasswordCredentials(creds_user, creds_pass)
        @test creds == creds2
        sshcreds = LibGit2.SSHCredentials(creds_user, creds_pass)
        @test sshcreds.user == creds_user
        @test sshcreds.pass == creds_pass
        @test isempty(sshcreds.prvkey)
        @test isempty(sshcreds.pubkey)
        sshcreds2 = LibGit2.SSHCredentials(creds_user, creds_pass)
        @test sshcreds == sshcreds2
    end

    # The following tests require that we can fake a TTY so that we can provide passwords
    # which use the `getpass` function. At the moment we can only fake this on UNIX based
    # systems.
    if Sys.isunix()
        git_ok = LibGit2.GitError(
            LibGit2.Error.None, LibGit2.Error.GIT_OK,
            "No errors")

        abort_prompt = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EAUTH,
            "Aborting, user cancelled credential request.")

        incompatible_error = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EAUTH,
            "The explicitly provided credential is incompatible with the requested " *
            "authentication methods.")

        eauth_error = LibGit2.GitError(
            LibGit2.Error.None, LibGit2.Error.EAUTH,
            "No errors")

        @testset "SSH credential prompt" begin
            url = "git@github.com:test/package.jl"

            valid_key = joinpath(KEY_DIR, "valid")
            invalid_key = joinpath(KEY_DIR, "invalid")
            valid_p_key = joinpath(KEY_DIR, "valid-passphrase")
            username = "git"
            passphrase = "secret"

            ssh_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = SSHCredentials($username, "", $valid_key, $(valid_key * ".pub"))
                credential_loop(valid_cred, $url, $username)
            end

            ssh_p_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = SSHCredentials($username, $passphrase, $valid_p_key, $(valid_p_key * ".pub"))
                credential_loop(valid_cred, $url, $username)
            end

            # SSH requires username
            url_no_username = "github.com:test/package.jl"
            ssh_u_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = SSHCredentials($username, "", $valid_key, $(valid_key * ".pub"))
                credential_loop(valid_cred, $url_no_username)
            end

            # Note: We cannot use the default ~/.ssh/id_rsa for tests since we cannot be
            # sure a users will actually have these files. Instead we will use the ENV
            # variables to set the default values.

            # Default credentials are valid
            withenv("SSH_KEY_PATH" => valid_key) do
                err, auth_attempts = challenge_prompt(ssh_ex, [])
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Default credentials are valid but requires a passphrase
            withenv("SSH_KEY_PATH" => valid_p_key) do
                challenges = [
                    "Passphrase for $valid_p_key:" => "$passphrase\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User mistypes passphrase.
                # Note: In reality LibGit2 will raise an error upon using the invalid SSH
                # credentials. Since we don't control the internals of LibGit2 though they
                # could also just re-call the credential callback like they do for HTTP.
                challenges = [
                    "Passphrase for $valid_p_key:" => "foo\n",
                    # "Private key location for 'git@github.com' [$valid_p_key]:" => "\n",
                    "Passphrase for $valid_p_key:" => "$passphrase\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2

                # User sends EOF in passphrase prompt which aborts the credential request
                challenges = [
                    "Passphrase for $valid_p_key:" => "\x04",
                ]
                err, auth_attempts = challenge_prompt(ssh_p_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty passphrase
                challenges = [
                    "Passphrase for $valid_p_key:" => "\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_p_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1
            end

            withenv("SSH_KEY_PATH" => valid_p_key, "SSH_KEY_PASS" => passphrase) do
                err, auth_attempts = challenge_prompt(ssh_p_ex, [])
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Missing username
            withenv("SSH_KEY_PATH" => valid_key) do
                # User provides a valid username
                challenges = [
                    "Username for 'github.com':" => "$username\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_u_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User sends EOF in username prompt which aborts the credential request
                challenges = [
                    "Username for 'github.com':" => "\x04",
                ]
                err, auth_attempts = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty username
                challenges = [
                    "Username for 'github.com':" => "\n",
                    "Username for 'github.com':" => "\x04",
                ]
                err, auth_attempts = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 2

                # User repeatedly chooses an invalid username
                challenges = [
                    "Username for 'github.com':" => "foo\n",
                    "Username for 'github.com' [foo]:" => "\n",
                    "Username for 'github.com' [foo]:" => "\x04",  # Need to manually abort
                ]
                err, auth_attempts = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 3

                # Credential callback is given an empty string in the `username_ptr`
                # instead of the typical C_NULL.
                ssh_user_empty_ex = quote
                    include($LIBGIT2_HELPER_PATH)
                    valid_cred = LibGit2.SSHCredentials($username, "", $valid_key, $(valid_key * ".pub"))
                    credential_loop(valid_cred, $url_no_username, "")
                end

                challenges = [
                    "Username for 'github.com':" => "$username\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_user_empty_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Explicitly setting these env variables to be empty means the user will be
            # given a prompt with no defaults set.
            withenv("SSH_KEY_PATH" => nothing,
                    "SSH_PUB_KEY_PATH" => nothing,
                    "SSH_KEY_PASS" => nothing,
                    (Sys.iswindows() ? "USERPROFILE" : "HOME") => tempdir()) do

                # Set the USERPROFILE / HOME above to be a directory that does not contain
                # the "~/.ssh/id_rsa" file. If this file exists the credential callback
                # will default to use this private key instead of triggering a prompt.
                @test !isfile(joinpath(homedir(), ".ssh", "id_rsa"))

                # User provides valid credentials
                challenges = [
                    "Private key location for 'git@github.com':" => "$valid_key\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User provides valid credentials that requires a passphrase
                challenges = [
                    "Private key location for 'git@github.com':" => "$valid_p_key\n",
                    "Passphrase for $valid_p_key:" => "$passphrase\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User sends EOF in private key prompt which aborts the credential request
                challenges = [
                    "Private key location for 'git@github.com':" => "\x04",
                ]
                err, auth_attempts = challenge_prompt(ssh_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty private key which triggers a re-prompt
                challenges = [
                    "Private key location for 'git@github.com':" => "\n",
                    "Public key location for 'git@github.com' [.pub]:" => "\n",
                    "Private key location for 'git@github.com':" => "\x04",
                ]
                err, auth_attempts = challenge_prompt(ssh_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 2
            end

            # TODO: Tests are currently broken. Credential callback currently infinite loops
            # and never prompts user to change private keys.
            #=
            # Explicitly setting these env variables to an existing but invalid key pair
            # means the user will be given a prompt with that defaults to the given values.
            withenv("SSH_KEY_PATH" => invalid_key, "SSH_PUB_KEY_PATH" => invalid_key * ".pub") do
                challenges = [
                    "Private key location for 'git@github.com' [$invalid_key]:" => "$valid_key\n",
                ]
                err, auth_attempts = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2
            end
            =#

            # TODO: Tests are currently broken. Credential callback currently infinite loops
            # and never prompts user to change private keys.
            #=
            withenv("SSH_KEY_PATH" => valid_key, "SSH_PUB_KEY_PATH" => valid_key * ".public") do
                @test !isfile(ENV["SSH_PUB_KEY_PATH"])

                # User explicitly sets the SSH_PUB_KEY_PATH incorrectly.
                challenges = [
                    "Private key location for 'git@github.com' [$valid_key]:" => "\n"
                    "Public key location for 'git@github.com':" => "$valid_key.pub\n"
                ]
                err, auth_attempts = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2
            end
            =#
        end

        @testset "HTTPS credential prompt" begin
            url = "https://github.com/test/package.jl"

            valid_username = "julia"
            valid_password = randstring(16)

            https_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                credential_loop(valid_cred, $url)
            end

            # User provides a valid username and password
            challenges = [
                "Username for 'https://github.com':" => "$valid_username\n",
                "Password for 'https://$valid_username@github.com':" => "$valid_password\n",
            ]
            err, auth_attempts = challenge_prompt(https_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 1

            # User sends EOF in username prompt which aborts the credential request
            challenges = [
                "Username for 'https://github.com':" => "\x04",
            ]
            err, auth_attempts = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User sends EOF in password prompt which aborts the credential request
            challenges = [
                "Username for 'https://github.com':" => "foo\n",
                "Password for 'https://foo@github.com':" => "\x04",
            ]
            err, auth_attempts = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User provides an empty password which aborts the credential request since we
            # cannot tell it apart from an EOF.
            challenges = [
                "Username for 'https://github.com':" => "foo\n",
                "Password for 'https://foo@github.com':" => "\n",
            ]
            err, auth_attempts = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User repeatedly chooses invalid username/password until the prompt limit is
            # reached
            challenges = [
                "Username for 'https://github.com':" => "foo\n",
                "Password for 'https://foo@github.com':" => "bar\n",
                "Username for 'https://github.com' [foo]:" => "$valid_username\n",
                "Password for 'https://$valid_username@github.com':" => "$valid_password\n",
            ]
            err, auth_attempts = challenge_prompt(https_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 2
        end

        @testset "SSH agent username" begin
            url = "github.com:test/package.jl"

            valid_key = joinpath(KEY_DIR, "valid")
            username = "git"

            ssh_empty_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.SSHCredentials($username, "", $valid_key, $(valid_key * ".pub"))
                credential_loop(valid_cred, $url, Nullable(""), use_ssh_agent=true)
            end

            ssh_null_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.SSHCredentials($username, "", $valid_key, $(valid_key * ".pub"))
                credential_loop(valid_cred, $url, Nullable(), use_ssh_agent=true)
            end

            challenges = [
                "Username for 'github.com':" => "\x04",
            ]

            err, auth_attempts = challenge_prompt(ssh_empty_ex, challenges)
            @test err == abort_prompt  # TODO: `eauth_error` when we can disable prompting
            @test auth_attempts == 2

            # A null username_ptr passed into `git_cred_ssh_key_from_agent` can cause a
            # segfault.
            err, auth_attempts = challenge_prompt(ssh_null_ex, challenges)
            @test err == abort_prompt  # TODO: `eauth_error` when we can disable prompting
            @test auth_attempts == 1
        end

        @testset "SSH explicit credentials" begin
            url = "git@github.com:test/package.jl"

            invalid_key = joinpath(KEY_DIR, "invalid")
            valid_p_key = joinpath(KEY_DIR, "valid-passphrase")
            username = "git"
            passphrase = "secret"

            valid_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.SSHCredentials($username, $passphrase, $valid_p_key, $(valid_p_key * ".pub"))
                payload = CredentialPayload(valid_cred)
                credential_loop(valid_cred, $url, $username, payload)
            end

            invalid_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.SSHCredentials($username, $passphrase, $valid_p_key, $(valid_p_key * ".pub"))
                invalid_cred = LibGit2.SSHCredentials($username, "", $invalid_key, $(invalid_key * ".pub"))
                payload = CredentialPayload(invalid_cred)
                credential_loop(valid_cred, $url, $username, payload, use_ssh_agent=false)
            end

            # Explicitly provided credential is correct
            err, auth_attempts = challenge_prompt(valid_ex, [])
            @test err == git_ok
            @test auth_attempts == 1

            # Explicitly provided credential is incorrect
            # TODO: Unless the SSH agent is disabled we may get caught in an infinite loop
            err, auth_attempts = challenge_prompt(invalid_ex, [])
            @test err == eauth_error
            @test auth_attempts == 2
        end

        @testset "HTTPS explicit credentials" begin
            url = "https://github.com/test/package.jl"

            valid_username = "julia"
            valid_password = randstring(16)
            invalid_username = "alice"
            invalid_password = randstring(15)

            valid_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                payload = CredentialPayload(valid_cred)
                credential_loop(valid_cred, $url, "", payload)
            end

            invalid_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                invalid_cred = LibGit2.UserPasswordCredentials($invalid_username, $invalid_password)
                payload = CredentialPayload(invalid_cred)
                credential_loop(valid_cred, $url, "", payload)
            end

            # Explicitly provided credential is correct
            err, auth_attempts = challenge_prompt(valid_ex, [])
            @test err == git_ok
            @test auth_attempts == 1

            # Explicitly provided credential is incorrect
            err, auth_attempts = challenge_prompt(invalid_ex, [])
            @test err == eauth_error
            @test auth_attempts == 2
        end

        @testset "Cached credentials" begin
            url = "https://github.com/test/package.jl"
            cred_id = "https://github.com"

            valid_username = "julia"
            valid_password = randstring(16)
            invalid_username = "alice"
            invalid_password = randstring(15)

            valid_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                cache = CachedCredentials()
                LibGit2.get_creds!(cache, $cred_id, valid_cred)
                payload = CredentialPayload(cache)
                credential_loop(valid_cred, $url, "", payload)
            end

            add_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                cache = CachedCredentials()
                payload = CredentialPayload(cache)
                err, auth_attempts = credential_loop(valid_cred, $url, "", payload)
                (err, auth_attempts, cache)
            end

            replace_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials($valid_username, $valid_password)
                invalid_cred = LibGit2.UserPasswordCredentials($invalid_username, $invalid_password, true)
                cache = CachedCredentials()
                LibGit2.get_creds!(cache, $cred_id, invalid_cred)
                payload = CredentialPayload(cache)
                err, auth_attempts = credential_loop(valid_cred, $url, "", payload)
                (err, auth_attempts, cache)
            end

            # Cache contains a correct credential
            err, auth_attempts = challenge_prompt(valid_ex, [])
            @test err == git_ok
            @test auth_attempts == 1

            # Add a credential into the cache
            challenges = [
                "Username for 'https://github.com':" => "$valid_username\n",
                "Password for 'https://$valid_username@github.com':" => "$valid_password\n",
            ]
            expected_cred = LibGit2.UserPasswordCredentials(valid_username, valid_password)
            err, auth_attempts, cache = challenge_prompt(add_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 1
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict(cred_id => expected_cred)

            # Replace a credential in the cache
            challenges = [
                "Username for 'https://github.com' [alice]:" => "$valid_username\n",
                "Password for 'https://$valid_username@github.com':" => "$valid_password\n",
            ]
            expected_cred = LibGit2.UserPasswordCredentials(valid_username, valid_password)
            err, auth_attempts, cache = challenge_prompt(replace_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 2
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict(cred_id => expected_cred)
        end

        @testset "Incompatible explicit credentials" begin
            # User provides a user/password credential where a SSH credential is required.
            expect_ssh_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials("foo", "bar")
                payload = CredentialPayload(valid_cred)
                credential_loop(valid_cred, "ssh://github.com/repo", Nullable(""),
                    Cuint(LibGit2.Consts.CREDTYPE_SSH_KEY), payload)
            end

            # User provides a SSH credential where a user/password credential is required.
            expect_https_ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.SSHCredentials("foo", "", "", "")
                payload = CredentialPayload(valid_cred)
                credential_loop(valid_cred, "https://github.com/repo", Nullable(""),
                    Cuint(LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT), payload)
            end

            err, auth_attempts = challenge_prompt(expect_ssh_ex, [])
            @test err == incompatible_error
            @test auth_attempts == 1

            err, auth_attempts = challenge_prompt(expect_https_ex, [])
            @test err == incompatible_error
            @test auth_attempts == 1
        end

        # A hypothetical scenario where the the allowed authentication can either be
        # SSH or username/password.
        @testset "SSH & HTTPS authentication" begin
            allowed_types = Cuint(LibGit2.Consts.CREDTYPE_SSH_KEY) |
                Cuint(LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT)

            # User provides a user/password credential where a SSH credential is required.
            ex = quote
                include($LIBGIT2_HELPER_PATH)
                valid_cred = LibGit2.UserPasswordCredentials("foo", "bar")
                payload = CredentialPayload(valid_cred)
                credential_loop(valid_cred, "foo://github.com/repo", Nullable(""),
                    $allowed_types, payload)
            end

            err, auth_attempts = challenge_prompt(ex, [])
            @test err == git_ok
            @test auth_attempts == 1
        end
    end

    #= temporarily disabled until working on the buildbots, ref https://github.com/JuliaLang/julia/pull/17651#issuecomment-238211150
    @testset "SSH" begin
        sshd_command = ""
        ssh_repo = joinpath(dir, "Example.SSH")
        if !Sys.iswindows()
            try
                # SSHD needs to be executed by its full absolute path
                sshd_command = strip(read(`which sshd`, String))
            catch
                warn("Skipping SSH tests (Are `which` and `sshd` installed?)")
            end
        end
        if !isempty(sshd_command)
            mktempdir() do fakehomedir
                mkdir(joinpath(fakehomedir,".ssh"))
                # Unsetting the SSH agent serves two purposes. First, we make
                # sure that we don't accidentally pick up an existing agent,
                # and second we test that we fall back to using a key file
                # if the agent isn't present.
                withenv("HOME"=>fakehomedir,"SSH_AUTH_SOCK"=>nothing) do
                    # Generate user file, first an unencrypted one
                    wait(spawn(`ssh-keygen -N "" -C juliatest@localhost -f $fakehomedir/.ssh/id_rsa`))

                    # Generate host keys
                    wait(spawn(`ssh-keygen -f $fakehomedir/ssh_host_rsa_key -N '' -t rsa`))
                    wait(spawn(`ssh-keygen -f $fakehomedir/ssh_host_dsa_key -N '' -t dsa`))

                    our_ssh_port = rand(13000:14000) # Chosen arbitrarily

                    key_option = "AuthorizedKeysFile $fakehomedir/.ssh/id_rsa.pub"
                    pidfile_option = "PidFile $fakehomedir/sshd.pid"
                    sshp = agentp = nothing
                    logfile = tempname()
                    ssh_debug = false
                    function spawn_sshd()
                        debug_flags = ssh_debug ? `-d -d` : ``
                        _p = open(logfile, "a") do logfilestream
                            spawn(pipeline(pipeline(`$sshd_command
                            -e -f /dev/null $debug_flags
                            -h $fakehomedir/ssh_host_rsa_key
                            -h $fakehomedir/ssh_host_dsa_key -p $our_ssh_port
                            -o $pidfile_option
                            -o 'Protocol 2'
                            -o $key_option
                            -o 'UsePrivilegeSeparation no'
                            -o 'StrictModes no'`,STDOUT),stderr=logfilestream))
                        end
                        # Give the SSH server 5 seconds to start up
                        yield(); sleep(5)
                        _p
                    end
                    sshp = spawn_sshd()

                    TIOCSCTTY_str = "ccall(:ioctl, Void, (Cint, Cint, Int64), 0,
                        (Sys.isbsd() || Sys.isapple()) ? 0x20007461 : Sys.islinux() ? 0x540E :
                        error(\"Fill in TIOCSCTTY for this OS here\"), 0)"

                    # To fail rather than hang
                    function killer_task(p, master)
                        @async begin
                            sleep(10)
                            kill(p)
                            if isopen(master)
                                nb_available(master) > 0 &&
                                    write(logfile,
                                        readavailable(master))
                                close(master)
                            end
                        end
                    end

                    try
                        function try_clone(challenges = [])
                            cmd = """
                            repo = nothing
                            try
                                $TIOCSCTTY_str
                                reponame = "ssh://$(ENV["USER"])@localhost:$our_ssh_port$cache_repo"
                                repo = LibGit2.clone(reponame, "$ssh_repo")
                            catch err
                                open("$logfile","a") do f
                                    println(f,"HOME: ",ENV["HOME"])
                                    println(f, err)
                                end
                            finally
                                close(repo)
                            end
                            """
                            # We try to be helpful by desperately looking for
                            # a way to prompt the password interactively. Pretend
                            # to be a TTY to suppress those shenanigans. Further, we
                            # need to detach and change the controlling terminal with
                            # TIOCSCTTY, since getpass opens the controlling terminal
                            TestHelpers.with_fake_pty() do slave, master
                                err = Base.Pipe()
                                let p = spawn(detach(
                                    `$(Base.julia_cmd()) --startup-file=no -e $cmd`),slave,slave,STDERR)
                                    killer_task(p, master)
                                    for (challenge, response) in challenges
                                        readuntil(master, challenge)
                                        sleep(1)
                                        print(master, response)
                                    end
                                    sleep(2)
                                    wait(p)
                                    close(master)
                                end
                            end
                            @test isfile(joinpath(ssh_repo,"testfile"))
                            rm(ssh_repo, recursive = true)
                        end

                        # Should use the default files, no interaction required.
                        try_clone()
                        ssh_debug && (kill(sshp); sshp = spawn_sshd())

                        # Ok, now encrypt the file and test with that (this also
                        # makes sure that we don't accidentally fall back to the
                        # unencrypted version)
                        wait(spawn(`ssh-keygen -p -N "xxxxx" -f $fakehomedir/.ssh/id_rsa`))

                        # Try with the encrypted file. Needs a password.
                        try_clone(["Passphrase"=>"xxxxx\r\n"])
                        ssh_debug && (kill(sshp); sshp = spawn_sshd())

                        # Move the file. It should now ask for the location and
                        # then the passphrase
                        mv("$fakehomedir/.ssh/id_rsa","$fakehomedir/.ssh/id_rsa2")
                        cp("$fakehomedir/.ssh/id_rsa.pub","$fakehomedir/.ssh/id_rsa2.pub")
                        try_clone(["location"=>"$fakehomedir/.ssh/id_rsa2\n",
                                   "Passphrase"=>"xxxxx\n"])
                        mv("$fakehomedir/.ssh/id_rsa2","$fakehomedir/.ssh/id_rsa")
                        rm("$fakehomedir/.ssh/id_rsa2.pub")

                        # Ok, now start an agent
                        agent_sock = tempname()
                        agentp = spawn(`ssh-agent -a $agent_sock -d`)
                        while stat(agent_sock).mode == 0 # Wait until the agent is started
                            sleep(1)
                        end

                        # fake pty is required for the same reason as in try_clone
                        # above
                        withenv("SSH_AUTH_SOCK" => agent_sock) do
                            TestHelpers.with_fake_pty() do slave, master
                                cmd = """
                                    $TIOCSCTTY_str
                                    run(pipeline(`ssh-add $fakehomedir/.ssh/id_rsa`,
                                        stderr = DevNull))
                                """
                                addp = spawn(detach(`$(Base.julia_cmd()) --startup-file=no -e $cmd`),
                                        slave, slave, STDERR)
                                killer_task(addp, master)
                                sleep(2)
                                write(master, "xxxxx\n")
                                wait(addp)
                            end

                            # Should now use the agent
                            try_clone()
                        end
                    catch err
                        println("SSHD logfile contents follows:")
                        println(read(logfile, String))
                        rethrow(err)
                    finally
                        rm(logfile)
                        sshp !== nothing && kill(sshp)
                        agentp !== nothing && kill(agentp)
                    end
                end
            end
        end
    end
    =#

    # Note: Tests only work on linux as SSL_CERT_FILE is only respected on linux systems.
    @testset "Hostname verification" begin
        openssl_installed = false
        common_name = ""
        if Sys.islinux()
            try
                # OpenSSL needs to be on the path
                openssl_installed = !isempty(read(`openssl version`, String))
            catch
                warn("Skipping hostname verification tests. Is `openssl` on the path?")
            end

            # Find a hostname that maps to the loopback address
            hostnames = ["localhost"]

            # In minimal environments a hostname might not be available (issue #20758)
            try
                # In some environments, namely Macs, the hostname "macbook.local" is bound
                # to the external address while "macbook" is bound to the loopback address.
                unshift!(hostnames, replace(gethostname(), r"\..*$", ""))
            end

            loopback = ip"127.0.0.1"
            for hostname in hostnames
                local addr
                try
                    addr = getaddrinfo(hostname)
                catch
                    continue
                end

                if addr == loopback
                    common_name = hostname
                    break
                end
            end

            if isempty(common_name)
                warn("Skipping hostname verification tests. Unable to determine a hostname which maps to the loopback address")
            end
        end
        if openssl_installed && !isempty(common_name)
            mktempdir() do root
                key = joinpath(root, common_name * ".key")
                cert = joinpath(root, common_name * ".crt")
                pem = joinpath(root, common_name * ".pem")

                # Generated a certificate which has the CN set correctly but no subjectAltName
                run(pipeline(`openssl req -new -x509 -newkey rsa:2048 -nodes -keyout $key -out $cert -days 1 -subj "/CN=$common_name"`, stderr=DevNull))
                run(`openssl x509 -in $cert -out $pem -outform PEM`)

                # Find an available port by listening
                port, server = listenany(49152)
                close(server)

                # Make a fake Julia package and minimal HTTPS server with our generated
                # certificate. The minimal server can't actually serve a Git repository.
                mkdir(joinpath(root, "Example.jl"))
                pobj = cd(root) do
                    spawn(`openssl s_server -key $key -cert $cert -WWW -accept $port`)
                end

                errfile = joinpath(root, "error")
                repo_url = "https://$common_name:$port/Example.jl"
                repo_dir = joinpath(root, "dest")
                code = """
                    dest_dir = "$repo_dir"
                    open("$errfile", "w+") do f
                        try
                            repo = LibGit2.clone("$repo_url", dest_dir)
                        catch err
                            serialize(f, err)
                        finally
                            isdir(dest_dir) && rm(dest_dir, recursive=true)
                        end
                    end
                """
                cmd = `$(Base.julia_cmd()) --startup-file=no -e $code`

                try
                    # The generated certificate is normally invalid
                    run(cmd)
                    err = open(errfile, "r") do f
                        deserialize(f)
                    end
                    @test err.code == LibGit2.Error.ECERTIFICATE
                    @test startswith(lowercase(err.msg),
                                     lowercase("The SSL certificate is invalid"))

                    rm(errfile)

                    # Specify that Julia use only the custom certificate. Note: we need to
                    # spawn a new Julia process in order for this ENV variable to take effect.
                    withenv("SSL_CERT_FILE" => pem) do
                        run(cmd)
                        err = open(errfile, "r") do f
                            deserialize(f)
                        end
                        @test err.code == LibGit2.Error.ERROR
                        @test lowercase(err.msg) == lowercase("invalid Content-Type: text/plain")
                    end

                    # OpenSSL s_server should still be running
                    @test process_running(pobj)
                finally
                    kill(pobj)
                end
            end
        end
    end
end
