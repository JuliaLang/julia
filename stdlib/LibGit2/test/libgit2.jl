# This file is a part of Julia. License is MIT: https://julialang.org/license

module LibGit2Tests

import LibGit2
using Test
using Random, Serialization, Sockets

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :FakePTYs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FakePTYs.jl"))
import .Main.FakePTYs: with_fake_pty

function challenge_prompt(code::Expr, challenges; timeout::Integer=60, debug::Bool=true)
    input_code = tempname()
    open(input_code, "w") do fp
        serialize(fp, code)
    end
    output_file = tempname()
    wrapped_code = quote
        using Serialization
        result = open($input_code) do fp
            eval(deserialize(fp))
        end
        open($output_file, "w") do fp
            serialize(fp, result)
        end
    end
    torun = "import LibGit2; $wrapped_code"
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $torun`
    try
        challenge_prompt(cmd, challenges, timeout=timeout, debug=debug)
        return open(output_file, "r") do fp
            deserialize(fp)
        end
    finally
        isfile(output_file) && rm(output_file)
        isfile(input_code) && rm(input_code)
    end
    return nothing
end

function challenge_prompt(cmd::Cmd, challenges; timeout::Integer=60, debug::Bool=true)
    function format_output(output)
        !debug && return ""
        str = read(seekstart(output), String)
        isempty(str) && return ""
        return "Process output found:\n\"\"\"\n$str\n\"\"\""
    end
    out = IOBuffer()
    with_fake_pty() do pty_slave, pty_master
        p = run(detach(cmd), pty_slave, pty_slave, pty_slave, wait=false)
        Base.close_stdio(pty_slave)

        # Kill the process if it takes too long. Typically occurs when process is waiting
        # for input.
        timer = Channel{Symbol}(1)
        watcher = @async begin
            waited = 0
            while waited < timeout && process_running(p)
                sleep(1)
                waited += 1
            end

            if process_running(p)
                kill(p)
                put!(timer, :timeout)
            elseif success(p)
                put!(timer, :success)
            else
                put!(timer, :failure)
            end

            # SIGKILL stubborn processes
            if process_running(p)
                sleep(3)
                process_running(p) && kill(p, Base.SIGKILL)
            end
            wait(p)
        end

        for (challenge, response) in challenges
            write(out, readuntil(pty_master, challenge, keep=true))
            if !isopen(pty_master)
                error("Could not locate challenge: \"$challenge\". ",
                      format_output(out))
            end
            write(pty_master, response)
        end

        # Capture output from process until `pty_slave` is closed
        try
            write(out, pty_master)
        catch ex
            if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
                rethrow() # ignore EIO from master after slave dies
            end
        end

        status = fetch(timer)
        close(pty_master)
        if status != :success
            if status == :timeout
                error("Process timed out possibly waiting for a response. ",
                      format_output(out))
            else
                error("Failed process. ", format_output(out), "\n", p)
            end
        end
        wait(watcher)
    end
    nothing
end

const LIBGIT2_MIN_VER = v"0.23.0"
const LIBGIT2_HELPER_PATH = joinpath(@__DIR__, "libgit2-helpers.jl")

const KEY_DIR = joinpath(@__DIR__, "keys")
const HOME = Sys.iswindows() ? "USERPROFILE" : "HOME"  # Environment variable name for home
const GIT_INSTALLED = try
    success(`git --version`)
catch
    false
end

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
    @test findfirst(isequal(LibGit2.Consts.FEATURE_SSH), f) !== nothing
    @test findfirst(isequal(LibGit2.Consts.FEATURE_HTTPS), f) !== nothing
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
    sig = LibGit2.Signature("AAA", "AAA@BBB.COM", round(time(); digits=0), 0)
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
            host="server.com",
            port=80,
            path="org/project.git")
        @test url == "https://user@server.com:80/org/project.git"
    end

    @testset "SSH URL" begin
        url = LibGit2.git_url(
            scheme="ssh",
            username="user",
            host="server",
            port="22",
            path="project.git")
        @test url == "ssh://user@server:22/project.git"
    end

    @testset "SSH URL, scp-like syntax" begin
        url = LibGit2.git_url(
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
            host="server.com",
            port="80")
        @test url == "https://user@server.com:80"
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

@testset "GitCredential" begin
    @testset "missing" begin
        str = ""
        cred = read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == LibGit2.GitCredential()
        @test sprint(write, cred) == str
        Base.shred!(cred)
    end

    @testset "empty" begin
        str = """
            protocol=
            host=
            path=
            username=
            password=
            """
        cred = read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == LibGit2.GitCredential("", "", "", "", "")
        @test sprint(write, cred) == str
        Base.shred!(cred)
    end

    @testset "input/output" begin
        str = """
            protocol=https
            host=example.com
            username=alice
            password=*****
            """
        expected_cred = LibGit2.GitCredential("https", "example.com", nothing, "alice", "*****")

        cred = read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == expected_cred
        @test sprint(write, cred) == str
        Base.shred!(cred)
        Base.shred!(expected_cred)
    end

    @testset "extra newline" begin
        # The "Git for Windows" installer will also install the "Git Credential Manager for
        # Windows" (https://github.com/Microsoft/Git-Credential-Manager-for-Windows) (also
        # known as "manager" in the .gitconfig files). This credential manager returns an
        # additional newline when returning the results.
        str = """
            protocol=https
            host=example.com
            path=
            username=bob
            password=*****

            """
        expected_cred = LibGit2.GitCredential("https", "example.com", "", "bob", "*****")

        cred = read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == expected_cred
        @test sprint(write, cred) * "\n" == str
        Base.shred!(cred)
        Base.shred!(expected_cred)
    end

    @testset "unknown attribute" begin
        str = """
            protocol=https
            host=example.com
            attribute=value
            username=bob
            password=*****
            """
        expected_cred = LibGit2.GitCredential("https", "example.com", nothing, "bob", "*****")
        expected_log = (:warn, "Unknown git credential attribute found: \"attribute\"")

        cred = @test_logs expected_log read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == expected_cred
        Base.shred!(cred)
        Base.shred!(expected_cred)
    end

    @testset "use http path" begin
        cred = LibGit2.GitCredential("https", "example.com", "dir/file", "alice", "*****")
        expected = """
            protocol=https
            host=example.com
            username=alice
            password=*****
            """

        @test cred.use_http_path
        cred.use_http_path = false

        @test cred.path == "dir/file"
        @test sprint(write, cred) == expected
        Base.shred!(cred)
    end

    @testset "URL input/output" begin
        str = """
            host=example.com
            password=bar
            url=https://a@b/c
            username=foo
            """
        expected_str = """
            protocol=https
            host=b
            path=c
            username=foo
            """
        expected_cred = LibGit2.GitCredential("https", "b", "c", "foo", nothing)

        cred = read!(IOBuffer(str), LibGit2.GitCredential())
        @test cred == expected_cred
        @test sprint(write, cred) == expected_str
        Base.shred!(cred)
        Base.shred!(expected_cred)
    end

    @testset "ismatch" begin
        # Equal
        cred = LibGit2.GitCredential("https", "github.com")
        @test LibGit2.ismatch("https://github.com", cred)
        Base.shred!(cred)

        # Credential hostname is different
        cred = LibGit2.GitCredential("https", "github.com")
        @test !LibGit2.ismatch("https://myhost", cred)
        Base.shred!(cred)

        # Credential is less specific than URL
        cred = LibGit2.GitCredential("https")
        @test !LibGit2.ismatch("https://github.com", cred)
        Base.shred!(cred)

        # Credential is more specific than URL
        cred = LibGit2.GitCredential("https", "github.com", "path", "user", "pass")
        @test LibGit2.ismatch("https://github.com", cred)
        Base.shred!(cred)

        # Credential needs to have an "" username to match
        cred = LibGit2.GitCredential("https", "github.com", nothing, "")
        @test LibGit2.ismatch("https://@github.com", cred)
        Base.shred!(cred)

        cred = LibGit2.GitCredential("https", "github.com", nothing, nothing)
        @test !LibGit2.ismatch("https://@github.com", cred)
        Base.shred!(cred)
    end

    @testset "GITHUB_REGEX" begin
        github_regex_test = function(url, user, repo)
            m = match(LibGit2.GITHUB_REGEX, url)
            @test m !== nothing
            @test m[1] == "$user/$repo"
            @test m[2] == user
            @test m[3] == repo
        end
        user = "User"
        repo = "Repo"
        github_regex_test("git@github.com/$user/$repo.git", user, repo)
        github_regex_test("https://github.com/$user/$repo.git", user, repo)
        github_regex_test("https://username@github.com/$user/$repo.git", user, repo)
        github_regex_test("ssh://git@github.com/$user/$repo.git", user, repo)
        github_regex_test("git@github.com/$user/$repo", user, repo)
        github_regex_test("https://github.com/$user/$repo", user, repo)
        github_regex_test("https://username@github.com/$user/$repo", user, repo)
        github_regex_test("ssh://git@github.com/$user/$repo", user, repo)
        @test !occursin(LibGit2.GITHUB_REGEX, "git@notgithub.com/$user/$repo.git")
    end
end

mktempdir() do dir
    dir = realpath(dir)
    # test parameters
    repo_url = "https://github.com/JuliaLang/Example.jl"
    cache_repo = joinpath(dir, "Example")
    test_repo = joinpath(dir, "Example.Test")
    test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(); digits=0), 0)
    test_dir = "testdir"
    test_file = "$(test_dir)/testfile"
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
        LibGit2.with(LibGit2.GitConfig(joinpath(dir, config_file), LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
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
        end
    end

    @testset "Configuration Iteration" begin
        config_path = joinpath(dir, config_file)

        # Write config entries with duplicate names
        open(config_path, "a") do fp
            write(fp, """
                [credential]
                    helper = store
                    username = julia
                [credential]
                    helper = cache
                """)
        end

        LibGit2.with(LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
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
        end
    end

    @testset "Initializing repository" begin
        @testset "with remote branch" begin
            LibGit2.with(LibGit2.init(cache_repo)) do repo
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

                LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, branch)) do remote
                    # test various remote properties
                    @test LibGit2.url(remote) == repo_url
                    @test LibGit2.push_url(remote) == ""
                    @test LibGit2.name(remote) == "upstream"
                    @test isa(remote, LibGit2.GitRemote)

                    # test showing a GitRemote object
                    @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: $repo_url"
                end
                # test setting and getting the remote's URL
                @test LibGit2.isattached(repo)
                LibGit2.set_remote_url(repo, "upstream", "unknown")
                LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, branch)) do remote
                    @test LibGit2.url(remote) == "unknown"
                    @test LibGit2.push_url(remote) == "unknown"
                    @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: unknown"
                end
                LibGit2.set_remote_url(cache_repo, "upstream", repo_url)
                LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, branch)) do remote
                    @test LibGit2.url(remote) == repo_url
                    @test LibGit2.push_url(remote) == repo_url
                    @test sprint(show, remote) == "GitRemote:\nRemote name: upstream url: $repo_url"
                    LibGit2.add_fetch!(repo, remote, "upstream")

                    # test setting fetch and push refspecs
                    @test LibGit2.fetch_refspecs(remote) == String["+refs/heads/*:refs/remotes/upstream/*"]
                    LibGit2.add_push!(repo, remote, "refs/heads/master")
                end
                LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, branch)) do remote
                    @test LibGit2.push_refspecs(remote) == String["refs/heads/master"]
                end
                # constructor with a refspec
                LibGit2.with(LibGit2.GitRemote(repo, "upstream2", repo_url, "upstream")) do remote
                    @test sprint(show, remote) == "GitRemote:\nRemote name: upstream2 url: $repo_url"
                    @test LibGit2.fetch_refspecs(remote) == String["upstream"]
                end

                LibGit2.with(LibGit2.GitRemoteAnon(repo, repo_url)) do remote
                    @test LibGit2.url(remote) == repo_url
                    @test LibGit2.push_url(remote) == ""
                    @test LibGit2.name(remote) == ""
                    @test isa(remote, LibGit2.GitRemote)
                end
            end
        end

        @testset "bare" begin
            path = joinpath(dir, "Example.Bare")
            LibGit2.with(LibGit2.init(path, true)) do repo
                @test isdir(path)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(path))
                @test isfile(joinpath(path, LibGit2.Consts.HEAD_FILE))
                @test LibGit2.isattached(repo)
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
            LibGit2.with(LibGit2.init(path, true)) do repo
                #just to see if this works
                LibGit2.cleanup(repo)
            end
        end
    end

    @testset "Cloning repository" begin
        function bare_repo_tests(repo, repo_path)
            @test isdir(repo_path)
            @test LibGit2.path(repo) == LibGit2.posixpath(realpath(repo_path))
            @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
            @test LibGit2.isattached(repo)
            @test LibGit2.remotes(repo) == ["origin"]
        end
        @testset "bare" begin
            repo_path = joinpath(dir, "Example.Bare1")
            LibGit2.with(LibGit2.clone(cache_repo, repo_path, isbare = true)) do repo
                bare_repo_tests(repo, repo_path)
            end
        end
        @testset "bare with remote callback" begin
            repo_path = joinpath(dir, "Example.Bare2")
            LibGit2.with(LibGit2.clone(cache_repo, repo_path, isbare = true, remote_cb = LibGit2.mirror_cb())) do repo
                bare_repo_tests(repo, repo_path)
                LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, "origin")) do rmt
                    @test LibGit2.fetch_refspecs(rmt)[1] == "+refs/*:refs/*"
                end
            end
        end
        @testset "normal" begin
            LibGit2.with(LibGit2.clone(cache_repo, test_repo)) do repo
                @test isdir(test_repo)
                @test LibGit2.path(repo) == LibGit2.posixpath(realpath(test_repo))
                @test isdir(joinpath(test_repo, ".git"))
                @test LibGit2.workdir(repo) == LibGit2.path(repo)*"/"
                @test LibGit2.isattached(repo)
                @test LibGit2.isorphan(repo)
                repo_str = sprint(show, repo)
                @test repo_str == "LibGit2.GitRepo($(sprint(show,LibGit2.path(repo))))"
            end
        end
        @testset "credentials callback conflict" begin
            callbacks = LibGit2.Callbacks(:credentials => (C_NULL, 0))
            cred_payload = LibGit2.CredentialPayload()
            @test_throws ArgumentError LibGit2.clone(cache_repo, test_repo, callbacks=callbacks, credentials=cred_payload)
        end
    end

    @testset "Update cache repository" begin

        @testset "with commits" begin
            repo = LibGit2.GitRepo(cache_repo)
            repo_dir = joinpath(cache_repo,test_dir)
            mkdir(repo_dir)
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
                LibGit2.with(LibGit2.GitCommit(repo, commit_oid1)) do cmt
                    @test LibGit2.Consts.OBJECT(typeof(cmt)) == LibGit2.Consts.OBJ_COMMIT
                    @test commit_oid1 == LibGit2.GitHash(cmt)
                    short_oid1 = LibGit2.GitShortHash(string(commit_oid1))
                    @test string(commit_oid1) == string(short_oid1)
                    @test cmp(commit_oid1, short_oid1) == 0
                    @test cmp(short_oid1, commit_oid1) == 0
                    @test !(short_oid1 < commit_oid1)

                    # test showing ShortHash
                    short_str = sprint(show, short_oid1)
                    @test short_str == "GitShortHash(\"$(string(short_oid1))\")"
                    short_oid2 = LibGit2.GitShortHash(cmt)
                    @test startswith(string(commit_oid1), string(short_oid2))

                    LibGit2.with(LibGit2.GitCommit(repo, short_oid2)) do cmt2
                        @test commit_oid1 == LibGit2.GitHash(cmt2)
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
                    @test occursin("Git Commit:", showstr[1])
                    @test occursin("Commit Author: Name: TEST, Email: TEST@TEST.COM, Time:", showstr[2])
                    @test occursin("Committer: Name: TEST, Email: TEST@TEST.COM, Time:", showstr[3])
                    @test occursin("SHA:", showstr[4])
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
                end
            finally
                close(repo)
                close(repo_file)
            end
        end

        @testset "with branch" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
                brnch = LibGit2.branch(repo)
                LibGit2.with(LibGit2.head(repo)) do brref
                    # various branch properties
                    @test LibGit2.isbranch(brref)
                    @test !LibGit2.isremote(brref)
                    @test LibGit2.name(brref) == "refs/heads/master"
                    @test LibGit2.shortname(brref) == master_branch
                    @test LibGit2.ishead(brref)
                    @test LibGit2.upstream(brref) === nothing

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
                    @test LibGit2.lookup_branch(repo, test_branch, true) === nothing
                    # not nothing because we are now looking for a LOCAL branch
                    LibGit2.with(LibGit2.lookup_branch(repo, test_branch, false)) do tbref
                        @test LibGit2.shortname(tbref) == test_branch
                        @test LibGit2.upstream(tbref) === nothing
                    end
                    @test LibGit2.lookup_branch(repo, test_branch2, true) === nothing
                    # test deleting the branch
                    LibGit2.branch!(repo, test_branch2; set_head=false)
                    LibGit2.with(LibGit2.lookup_branch(repo, test_branch2, false)) do tbref
                        @test LibGit2.shortname(tbref) == test_branch2
                        LibGit2.delete_branch(tbref)
                        @test LibGit2.lookup_branch(repo, test_branch2, true) === nothing
                    end
                end
                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test master_branch in branches
                @test test_branch in branches
            end
        end

        @testset "with default configuration" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
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
            end
        end

        @testset "with tags" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
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
                tag1tag = LibGit2.peel(LibGit2.GitTag, tag1ref)
                @test LibGit2.name(tag1tag) == tag1
                @test LibGit2.target(tag1tag) == commit_oid1
                @test sprint(show, tag1tag) == "GitTag:\nTag name: $tag1 target: $commit_oid1"
                # peels to the commit the tag points to
                tag1cmt = LibGit2.peel(tag1ref)
                @test LibGit2.GitHash(tag1cmt) == commit_oid1
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
            end
        end

        @testset "status" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
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
            end
        end

        @testset "blobs" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
                # this is slightly dubious, as it assumes the object has not been packed
                # could be replaced by another binary format
                hash_string = string(commit_oid1)
                blob_file   = joinpath(cache_repo,".git/objects", hash_string[1:2], hash_string[3:end])

                id = LibGit2.addblob!(repo, blob_file)
                blob = LibGit2.GitBlob(repo, id)
                @test LibGit2.isbinary(blob)
                len1 = length(blob)

                # test showing a GitBlob
                blob_show_strs = split(sprint(show, blob), "\n")
                @test blob_show_strs[1] == "GitBlob:"
                @test occursin("Blob id:", blob_show_strs[2])
                @test blob_show_strs[3] == "Contents are binary."

                blob2 = LibGit2.GitBlob(repo, LibGit2.GitHash(blob))
                @test LibGit2.isbinary(blob2)
                @test length(blob2) == len1
                @test blob  == blob2
                @test blob !== blob2
            end
        end
        @testset "trees" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
                @test_throws LibGit2.Error.GitError LibGit2.GitTree(repo, "HEAD")
                tree = LibGit2.GitTree(repo, "HEAD^{tree}")
                @test isa(tree, LibGit2.GitTree)
                @test isa(LibGit2.GitObject(repo, "HEAD^{tree}"), LibGit2.GitTree)
                @test LibGit2.Consts.OBJECT(typeof(tree)) == LibGit2.Consts.OBJ_TREE
                @test LibGit2.count(tree) == 1

                # test showing the GitTree and its entries
                tree_str = sprint(show, tree)
                @test tree_str == "GitTree:\nOwner: $(LibGit2.repository(tree))\nNumber of entries: 1\n"
                @test_throws BoundsError tree[0]
                @test_throws BoundsError tree[2]
                tree_entry = tree[1]
                subtree = LibGit2.GitTree(tree_entry)
                @test_throws BoundsError subtree[0]
                @test_throws BoundsError subtree[2]
                tree_entry = subtree[1]
                @test LibGit2.filemode(tree_entry) == 33188
                te_str = sprint(show, tree_entry)
                ref_te_str = "GitTreeEntry:\nEntry name: testfile\nEntry type: LibGit2.GitBlob\nEntry OID: "
                ref_te_str *= "$(LibGit2.entryid(tree_entry))\n"
                @test te_str == ref_te_str
                blob = LibGit2.GitBlob(tree_entry)
                blob_str = sprint(show, blob)
                @test blob_str == "GitBlob:\nBlob id: $(LibGit2.GitHash(blob))\nContents:\n$(LibGit2.content(blob))\n"

                # tests for walking the tree and accessing objects
                @test tree[""] == tree
                @test tree["/"] == tree
                @test isa(tree[test_dir], LibGit2.GitTree)
                @test tree["$test_dir/"] == tree[test_dir]
                @test isa(tree[test_file], LibGit2.GitBlob)
                @test_throws KeyError tree["nonexistent"]

                # test workaround for git_tree_walk issue
                # https://github.com/libgit2/libgit2/issues/4693
                ccall((:giterr_set_str, :libgit2), Cvoid, (Cint, Cstring),
                      Cint(LibGit2.Error.Invalid), "previous error")
                try
                    # file needs to exist in tree in order to trigger the stop walk condition
                    tree[test_file]
                catch err
                    if isa(err, LibGit2.Error.GitError) && err.class == LibGit2.Error.Invalid
                        @test false
                    else
                        rethrow()
                    end
                end
            end
        end

        @testset "diff" begin
            LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
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
                @test LibGit2.count(diff) == 1
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
                @test occursin("Oid:", diff_strs[6])
                @test occursin("Path:", diff_strs[7])
                @test occursin("Size:", diff_strs[8])
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
            end
        end
    end

    function setup_clone_repo(cache_repo::AbstractString, path::AbstractString; name="AAAA", email="BBBB@BBBB.COM")
        repo = LibGit2.clone(cache_repo, path)
        # need to set this for merges to succeed
        cfg = LibGit2.GitConfig(repo)
        LibGit2.set!(cfg, "user.name", name)
        LibGit2.set!(cfg, "user.email", email)
        return repo
    end
    # TO DO: add more tests for various merge
    # preference options
    function add_and_commit_file(repo, filenm, filecontent)
        open(joinpath(LibGit2.path(repo), filenm),"w") do f
            write(f, filecontent)
        end
        LibGit2.add!(repo, filenm)
        return LibGit2.commit(repo, "add $filenm")
    end
    @testset "Fastforward merges" begin
        LibGit2.with(setup_clone_repo(cache_repo, joinpath(dir, "Example.FF"))) do repo
            # Sets up a branch "branch/ff_a" which will be two commits ahead
            # of "master". It's possible to fast-forward merge "branch/ff_a"
            # into "master", which is the default behavior.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/ff_a")
            add_and_commit_file(repo, "ff_file1", "111\n")
            add_and_commit_file(repo, "ff_file2", "222\n")
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
            add_and_commit_file(repo, "ff_file3", "333\n")
            branchhead = add_and_commit_file(repo, "ff_file4", "444\n")
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/a using committish
            @test LibGit2.merge!(repo, committish=string(branchhead))
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            # Repeat the process, but specifying a branch name to merge in as opposed
            # to a commit or GitAnnotated.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/ff_c")
            add_and_commit_file(repo, "ff_file5", "555\n")
            branchhead = add_and_commit_file(repo, "ff_file6", "666\n")
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/ff_c using branch name
            @test LibGit2.merge!(repo, branch="refs/heads/branch/ff_c")
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            LibGit2.branch!(repo, "branch/ff_d")
            branchhead = add_and_commit_file(repo, "ff_file7", "777\n")
            LibGit2.branch!(repo, "master")
            # switch back, now try to ff-merge the changes
            # from branch/a
            # set up the merge using GitAnnotated objects
            # from a fetchhead
            fh = LibGit2.fetchheads(repo)
            upst_ann = LibGit2.GitAnnotated(repo, fh[1])
            @test LibGit2.merge!(repo, [upst_ann], true)
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)
        end
    end

    @testset "Cherrypick" begin
        LibGit2.with(setup_clone_repo(cache_repo, joinpath(dir, "Example.Cherrypick"))) do repo
            # Create a commit on the new branch and cherry-pick it over to
            # master. Since the cherry-pick does *not* make a new commit on
            # master, we have to create our own commit of the dirty state.
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/cherry_a")
            cmt_oid = add_and_commit_file(repo, "file1", "111\n")
            cmt = LibGit2.GitCommit(repo, cmt_oid)
            # switch back, try to cherrypick
            # from branch/cherry_a
            LibGit2.branch!(repo, "master")
            LibGit2.cherrypick(repo, cmt, options=LibGit2.CherrypickOptions())
            cmt_oid2 = LibGit2.commit(repo, "add file1")
            @test isempty(LibGit2.diff_files(repo, "master", "branch/cherry_a"))
        end
    end

    @testset "Merges" begin
        LibGit2.with(setup_clone_repo(cache_repo, joinpath(dir, "Example.Merge"))) do repo
            oldhead = LibGit2.head_oid(repo)
            LibGit2.branch!(repo, "branch/merge_a")
            add_and_commit_file(repo, "file1", "111\n")
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

            add_and_commit_file(repo, "file2", "222\n")
            upst_ann = LibGit2.GitAnnotated(repo, "branch/merge_a")
            head_ann = LibGit2.GitAnnotated(repo, "master")

            # (fail to) merge them because we can't fastforward
            @test_logs (:warn,"Cannot perform fast-forward merge") !LibGit2.merge!(repo, [upst_ann], true)
            # merge them now that we allow non-ff
            @test_logs (:info,"Review and commit merged changes") LibGit2.merge!(repo, [upst_ann], false)
            @test LibGit2.is_ancestor_of(string(oldhead), string(LibGit2.head_oid(repo)), repo)

            # go back to merge_a and rename a file
            LibGit2.branch!(repo, "branch/merge_b")
            mv(joinpath(LibGit2.path(repo),"file1"),joinpath(LibGit2.path(repo),"mvfile1"))
            LibGit2.add!(repo, "mvfile1")
            LibGit2.commit(repo, "move file1")
            LibGit2.branch!(repo, "master")
            upst_ann = LibGit2.GitAnnotated(repo, "branch/merge_b")
            rename_flag = Cint(0)
            rename_flag = LibGit2.toggle(rename_flag, Cint(0)) # turns on the find renames opt
            mos = LibGit2.MergeOptions(flags=rename_flag)
            @test_logs (:info,"Review and commit merged changes") LibGit2.merge!(repo, [upst_ann], merge_opts=mos)
        end
    end

    @testset "push" begin
        up_path = joinpath(dir, "Example.PushUp")
        up_repo = setup_clone_repo(cache_repo, up_path)
        our_repo = setup_clone_repo(cache_repo, joinpath(dir, "Example.Push"))
        try
            add_and_commit_file(our_repo, "file1", "111\n")
            if LibGit2.version() >= v"0.26.0" # See #21872, #21639 and #21597
                # we cannot yet locally push to non-bare repos
                @test_throws LibGit2.GitError LibGit2.push(our_repo, remoteurl=up_path)
            end
        finally
            close(our_repo)
            close(up_repo)
        end

        @testset "credentials callback conflict" begin
            callbacks = LibGit2.Callbacks(:credentials => (C_NULL, 0))
            cred_payload = LibGit2.CredentialPayload()

            LibGit2.with(LibGit2.GitRepo(joinpath(dir, "Example.Push"))) do repo
                @test_throws ArgumentError LibGit2.push(repo, callbacks=callbacks, credentials=cred_payload)
            end
        end
    end

    @testset "Show closed repo" begin
        # Make sure this doesn't crash
        buf = IOBuffer()
        Base.show(buf, LibGit2.with(identity, LibGit2.GitRepo(test_repo)))
        @test String(take!(buf)) == "LibGit2.GitRepo(<closed>)"
    end

    @testset "Fetch from cache repository" begin
        LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
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
        end

        @testset "credentials callback conflict" begin
            callbacks = LibGit2.Callbacks(:credentials => (C_NULL, 0))
            cred_payload = LibGit2.CredentialPayload()

            LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
                @test_throws ArgumentError LibGit2.fetch(repo, callbacks=callbacks, credentials=cred_payload)
            end
        end
    end

    @testset "Examine test repository" begin
        @testset "files" begin
            @test read(joinpath(test_repo, test_file), String) == read(joinpath(cache_repo, test_file), String)
        end

        @testset "tags & branches" begin
            LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
                # all tag in place
                tags = LibGit2.tag_list(repo)
                @test length(tags) == 1
                @test tag2 in tags

                # all tag in place
                branches = map(b->LibGit2.shortname(b[1]), LibGit2.GitBranchIter(repo))
                @test master_branch in branches
                @test test_branch in branches

                # issue #16337
                LibGit2.with(LibGit2.GitReference(repo, "refs/tags/$tag2")) do tag2ref
                    @test_throws LibGit2.Error.GitError LibGit2.upstream(tag2ref)
                end
            end
        end

        @testset "commits with revwalk" begin
            repo = LibGit2.GitRepo(test_repo)
            cache = LibGit2.GitRepo(cache_repo)
            try
                # test map with oid
                oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    LibGit2.map((oid,repo)->(oid,repo), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME)
                end
                @test length(oids) == 1
                # test map with range
                str_1 = string(commit_oid1)
                str_3 = string(commit_oid3)
                oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    LibGit2.map((oid,repo)->(oid,repo), walker, range="$str_1..$str_3", by=LibGit2.Consts.SORT_TIME)
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
                # test with specified oid
                LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    @test LibGit2.count((oid,repo)->(oid == commit_oid1), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME) == 1
                end
                # test without specified oid
                LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                    @test LibGit2.count((oid,repo)->(oid == commit_oid1), walker, by=LibGit2.Consts.SORT_TIME) == 1
                end
            finally
                close(repo)
                close(cache)
            end
        end
    end

    @testset "Modify and reset repository" begin
        LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
            # check index for file
            LibGit2.with(LibGit2.GitIndex(repo)) do idx
                i = findall(test_file, idx)
                @test i !== nothing
                idx_entry = idx[i]
                @test idx_entry !== nothing
                idx_entry_str = sprint(show, idx_entry)
                @test idx_entry_str == "IndexEntry($(string(idx_entry.id)))"
                @test LibGit2.stage(idx_entry) == 0

                i = findall("zzz", idx)
                @test i === nothing
                idx_str = sprint(show, idx)
                @test idx_str == "GitIndex:\nRepository: $(LibGit2.repository(idx))\nNumber of elements: 1\n"

                LibGit2.remove!(repo, test_file)
                LibGit2.read!(repo)
                @test LibGit2.count(idx) == 0
                LibGit2.add!(repo, test_file)
                LibGit2.update!(repo, test_file)
                @test LibGit2.count(idx) == 1
            end

            # check non-existent file status
            st = LibGit2.status(repo, "XYZ")
            @test st === nothing

            # check file status
            st = LibGit2.status(repo, test_file)
            @test st !== nothing
            @test LibGit2.isset(st, LibGit2.Consts.STATUS_CURRENT)

            # modify file
            open(joinpath(test_repo, test_file), "a") do io
                write(io, 0x41)
            end

            # file modified but not staged
            st_mod = LibGit2.status(repo, test_file)
            @test !LibGit2.isset(st_mod, LibGit2.Consts.STATUS_INDEX_MODIFIED)
            @test LibGit2.isset(st_mod, LibGit2.Consts.STATUS_WT_MODIFIED)

            # stage file
            LibGit2.add!(repo, test_file)

            # modified file staged
            st_stg = LibGit2.status(repo, test_file)
            @test LibGit2.isset(st_stg, LibGit2.Consts.STATUS_INDEX_MODIFIED)
            @test !LibGit2.isset(st_stg, LibGit2.Consts.STATUS_WT_MODIFIED)

            # try to unstage to unknown commit
            @test_throws LibGit2.Error.GitError LibGit2.reset!(repo, "XYZ", test_file)

            # status should not change
            st_new = LibGit2.status(repo, test_file)
            @test st_new == st_stg

            # try to unstage to HEAD
            new_head = LibGit2.reset!(repo, LibGit2.Consts.HEAD_FILE, test_file)
            st_uns = LibGit2.status(repo, test_file)
            @test st_uns == st_mod

            # reset repo
            @test_throws LibGit2.Error.GitError LibGit2.reset!(repo, LibGit2.GitHash(), LibGit2.Consts.RESET_HARD)

            new_head = LibGit2.reset!(repo, LibGit2.head_oid(repo), LibGit2.Consts.RESET_HARD)
            open(joinpath(test_repo, test_file), "r") do io
                @test read(io)[end] != 0x41
            end
        end
    end

    @testset "Modify remote" begin
        path = test_repo
        LibGit2.with(LibGit2.GitRepo(path)) do repo
            remote_name = "test"
            url = "https://test.com/repo"

            @test LibGit2.lookup_remote(repo, remote_name) === nothing

            for r in (repo, path)
                # Set just the fetch URL
                LibGit2.set_remote_fetch_url(r, remote_name, url)
                remote = LibGit2.lookup_remote(repo, remote_name)
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) == url
                @test LibGit2.push_url(remote) == ""

                LibGit2.remote_delete(repo, remote_name)
                @test LibGit2.lookup_remote(repo, remote_name) === nothing

                # Set just the push URL
                LibGit2.set_remote_push_url(r, remote_name, url)
                remote = LibGit2.lookup_remote(repo, remote_name)
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) == ""
                @test LibGit2.push_url(remote) == url

                LibGit2.remote_delete(repo, remote_name)
                @test LibGit2.lookup_remote(repo, remote_name) === nothing

                # Set the fetch and push URL
                LibGit2.set_remote_url(r, remote_name, url)
                remote = LibGit2.lookup_remote(repo, remote_name)
                @test LibGit2.name(remote) == remote_name
                @test LibGit2.url(remote) ==  url
                @test LibGit2.push_url(remote) == url

                LibGit2.remote_delete(repo, remote_name)
                @test LibGit2.lookup_remote(repo, remote_name) === nothing
            end
            # Invalid remote name
            @test_throws LibGit2.GitError LibGit2.set_remote_url(repo, "", url)
            @test_throws LibGit2.GitError LibGit2.set_remote_url(repo, remote_name, "")
        end
    end

    @testset "rebase" begin
        LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
            LibGit2.branch!(repo, "branch/a")

            oldhead = LibGit2.head_oid(repo)
            add_and_commit_file(repo, "file1", "111\n")
            add_and_commit_file(repo, "file2", "222\n")
            LibGit2.branch!(repo, "branch/b")

            # squash last 2 commits
            new_head = LibGit2.reset!(repo, oldhead, LibGit2.Consts.RESET_SOFT)
            @test new_head == oldhead
            LibGit2.commit(repo, "squash file1 and file2")

            # add another file
            newhead = add_and_commit_file(repo, "file3", "333\n")
            @test LibGit2.diff_files(repo, "branch/a", "branch/b", filter=Set([LibGit2.Consts.DELTA_ADDED])) == ["file3"]
            @test LibGit2.diff_files(repo, "branch/a", "branch/b", filter=Set([LibGit2.Consts.DELTA_MODIFIED])) == []
            # switch back and rebase
            LibGit2.branch!(repo, "branch/a")
            newnewhead = LibGit2.rebase!(repo, "branch/b")

            # issue #19624
            @test newnewhead == newhead

            # add yet another file
            add_and_commit_file(repo, "file4", "444\n")
            # rebase with onto
            newhead = LibGit2.rebase!(repo, "branch/a", "master")

            newerhead = LibGit2.head_oid(repo)
            @test newerhead == newhead

            # add yet more files
            add_and_commit_file(repo, "file5", "555\n")
            pre_abort_head = add_and_commit_file(repo, "file6", "666\n")
            # Rebase type
            head_ann = LibGit2.GitAnnotated(repo, "branch/a")
            upst_ann = LibGit2.GitAnnotated(repo, "master")
            rb = LibGit2.GitRebase(repo, head_ann, upst_ann)
            @test_throws BoundsError rb[3]
            @test_throws BoundsError rb[0]
            rbo, _ = iterate(rb)
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
        end
    end

    @testset "merge" begin
        LibGit2.with(setup_clone_repo(cache_repo, joinpath(dir, "Example.simple_merge"))) do repo
            LibGit2.branch!(repo, "branch/merge_a")

            a_head = LibGit2.head_oid(repo)
            add_and_commit_file(repo, "merge_file1", "111\n")
            LibGit2.branch!(repo, "master")
            a_head_ann = LibGit2.GitAnnotated(repo, "branch/merge_a")
            # merge returns true if successful
            @test_logs (:info,"Review and commit merged changes") LibGit2.merge!(repo, [a_head_ann])
        end
    end

    @testset "Transact test repository" begin
        LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
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
        end
    end

    @testset "checkout/headname" begin
        LibGit2.with(LibGit2.GitRepo(cache_repo)) do repo
            LibGit2.checkout!(repo, string(commit_oid1))
            @test !LibGit2.isattached(repo)
            @test LibGit2.headname(repo) == "(detached from $(string(commit_oid1)[1:7]))"
        end
    end


    if Sys.isunix()
        @testset "checkout/proptest" begin
            LibGit2.with(LibGit2.GitRepo(test_repo)) do repo
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
            end
        end
    end


    @testset "Credentials" begin
        creds_user = "USER"
        creds_pass = Base.SecretBuffer("PASS")
        creds = LibGit2.UserPasswordCredential(creds_user, creds_pass)
        @test creds.user == creds_user
        @test creds.pass == creds_pass
        creds2 = LibGit2.UserPasswordCredential(creds_user, creds_pass)
        @test creds == creds2

        sshcreds = LibGit2.SSHCredential(creds_user, creds_pass)
        @test sshcreds.user == creds_user
        @test sshcreds.pass == creds_pass
        @test sshcreds.prvkey == ""
        @test sshcreds.pubkey == ""
        sshcreds2 = LibGit2.SSHCredential(creds_user, creds_pass)
        @test sshcreds == sshcreds2

        Base.shred!(creds)
        Base.shred!(creds2)
        Base.shred!(sshcreds)
        Base.shred!(sshcreds2)
        Base.shred!(creds_pass)
    end

    @testset "CachedCredentials" begin
        cache = LibGit2.CachedCredentials()

        url = "https://github.com/JuliaLang/Example.jl"
        cred_id = LibGit2.credential_identifier(url)
        cred = LibGit2.UserPasswordCredential("julia", "password")

        @test !haskey(cache, cred_id)
        password = Base.SecretBuffer("password")

        # Attempt to reject a credential which wasn't stored
        LibGit2.reject(cache, cred, url)
        @test !haskey(cache, cred_id)
        @test cred.user == "julia"
        @test cred.pass == password

        # Approve a credential which causes it to be stored
        LibGit2.approve(cache, cred, url)
        @test haskey(cache, cred_id)
        @test cache[cred_id] === cred

        # Approve the same credential again which does not overwrite
        LibGit2.approve(cache, cred, url)
        @test haskey(cache, cred_id)
        @test cache[cred_id] === cred

        # Overwrite an already cached credential
        dup_cred = deepcopy(cred)
        LibGit2.approve(cache, dup_cred, url)  # Shreds overwritten `cred`
        @test haskey(cache, cred_id)
        @test cache[cred_id] === dup_cred
        @test cred.user != "julia"
        @test cred.pass != password
        @test dup_cred.user == "julia"
        @test dup_cred.pass == password

        cred = dup_cred

        # Reject an approved credential
        @test cache[cred_id] === cred
        LibGit2.reject(cache, cred, url)  # Avoids shredding the credential passed in
        @test !haskey(cache, cred_id)
        @test cred.user == "julia"
        @test cred.pass == password

        # Reject and shred an approved credential
        dup_cred = deepcopy(cred)
        LibGit2.approve(cache, cred, url)

        LibGit2.reject(cache, dup_cred, url)  # Shred `cred` but not passed in `dup_cred`
        @test !haskey(cache, cred_id)
        @test cred.user != "julia"
        @test cred.pass != password
        @test dup_cred.user == "julia"
        @test dup_cred.pass == password

        Base.shred!(dup_cred)
        Base.shred!(cache)
        Base.shred!(password)
    end

    @testset "Git credential username" begin
        @testset "fill username" begin
            config_path = joinpath(dir, config_file)
            isfile(config_path) && rm(config_path)

            LibGit2.with(LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                # No credential settings should be set for these tests
                @test isempty(collect(LibGit2.GitConfigIter(cfg, r"credential.*")))

                github_cred = LibGit2.GitCredential("https", "github.com")
                mygit_cred = LibGit2.GitCredential("https", "mygithost")

                # No credential settings in configuration.
                username = LibGit2.default_username(cfg, github_cred)
                @test username === nothing

                # Add a credential setting for a specific for a URL
                LibGit2.set!(cfg, "credential.https://github.com.username", "foo")

                username = LibGit2.default_username(cfg, github_cred)
                @test username == "foo"

                username = LibGit2.default_username(cfg, mygit_cred)
                @test username === nothing

                # Add a global credential setting after the URL specific setting. The first
                # setting to match will be the one that is used.
                LibGit2.set!(cfg, "credential.username", "bar")

                username = LibGit2.default_username(cfg, github_cred)
                @test username == "foo"

                username = LibGit2.default_username(cfg, mygit_cred)
                @test username == "bar"

                Base.shred!(github_cred)
                Base.shred!(mygit_cred)
            end
        end

        @testset "empty username" begin
            config_path = joinpath(dir, config_file)
            isfile(config_path) && rm(config_path)

            LibGit2.with(LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                # No credential settings should be set for these tests
                @test isempty(collect(LibGit2.GitConfigIter(cfg, r"credential.*")))

                # An empty username should count as being set
                LibGit2.set!(cfg, "credential.https://github.com.username", "")
                LibGit2.set!(cfg, "credential.username", "name")

                github_cred = LibGit2.GitCredential("https", "github.com")
                mygit_cred = LibGit2.GitCredential("https", "mygithost", "path")

                username = LibGit2.default_username(cfg, github_cred)
                @test username == ""

                username = LibGit2.default_username(cfg, mygit_cred)
                @test username == "name"

                Base.shred!(github_cred)
                Base.shred!(mygit_cred)
            end
        end
    end

    @testset "Git helpers useHttpPath" begin
        @testset "use_http_path" begin
            config_path = joinpath(dir, config_file)
            isfile(config_path) && rm(config_path)

            LibGit2.with(LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                # No credential settings should be set for these tests
                @test isempty(collect(LibGit2.GitConfigIter(cfg, r"credential.*")))

                github_cred = LibGit2.GitCredential("https", "github.com")
                mygit_cred = LibGit2.GitCredential("https", "mygithost")

                # No credential settings in configuration.
                @test !LibGit2.use_http_path(cfg, github_cred)
                @test !LibGit2.use_http_path(cfg, mygit_cred)

                # Add a credential setting for a specific for a URL
                LibGit2.set!(cfg, "credential.https://github.com.useHttpPath", "true")

                @test LibGit2.use_http_path(cfg, github_cred)
                @test !LibGit2.use_http_path(cfg, mygit_cred)

                # Invert the current settings.
                LibGit2.set!(cfg, "credential.useHttpPath", "true")
                LibGit2.set!(cfg, "credential.https://github.com.useHttpPath", "false")

                @test !LibGit2.use_http_path(cfg, github_cred)
                @test LibGit2.use_http_path(cfg, mygit_cred)

                Base.shred!(github_cred)
                Base.shred!(mygit_cred)
            end
        end
    end

    @testset "GitCredentialHelper" begin
        GitCredentialHelper = LibGit2.GitCredentialHelper
        GitCredential = LibGit2.GitCredential

        @testset "parse" begin
            @test parse(GitCredentialHelper, "!echo hello") == GitCredentialHelper(`echo hello`)
            @test parse(GitCredentialHelper, "/bin/bash") == GitCredentialHelper(`/bin/bash`)
            @test parse(GitCredentialHelper, "store") == GitCredentialHelper(`git credential-store`)
        end

        @testset "empty helper" begin
            config_path = joinpath(dir, config_file)

            # Note: LibGit2.set! doesn't allow us to set duplicates or ordering
            open(config_path, "w+") do fp
                write(fp, """
                    [credential]
                        helper = !echo first
                    [credential "https://mygithost"]
                        helper = ""
                    [credential]
                        helper = !echo second
                    """)
            end

            LibGit2.with(LibGit2.GitConfig(config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                @test length(collect(LibGit2.GitConfigIter(cfg, r"credential.*"))) == 3

                expected = [
                    GitCredentialHelper(`echo first`),
                    GitCredentialHelper(`echo second`),
                ]

                github_cred = GitCredential("https", "github.com")
                mygit_cred = GitCredential("https", "mygithost")

                @test LibGit2.credential_helpers(cfg, github_cred) == expected

                println(stderr, "The following 'Resetting the helper list...' warning is expected:")
                @test_broken LibGit2.credential_helpers(cfg, mygit_cred) == expected[2]

                Base.shred!(github_cred)
                Base.shred!(mygit_cred)
            end
        end

        @testset "approve/reject" begin
            # In order to use the "store" credential helper `git` needs to be installed and
            # on the path.
            if GIT_INSTALLED
                credential_path = joinpath(dir, ".git-credentials")
                isfile(credential_path) && rm(credential_path)

                # Requires `git` to be installed and available on the path.
                helper = parse(LibGit2.GitCredentialHelper, "store")

                # Set HOME to control where the .git-credentials file is written.
                # Note: In Cygwin environments `git` will use HOME instead of USERPROFILE.
                # Setting both environment variables ensures home was overridden.
                withenv("HOME" => dir, "USERPROFILE" => dir) do
                    query = LibGit2.GitCredential("https", "mygithost")
                    filled = LibGit2.GitCredential("https", "mygithost", nothing, "bob", "s3cre7")

                    @test !isfile(credential_path)

                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == query
                    end

                    LibGit2.approve(helper, filled)
                    @test isfile(credential_path)
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == filled
                    end

                    LibGit2.reject(helper, filled)
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == query
                    end

                    Base.shred!(query)
                    Base.shred!(filled)
                end
            end
        end

        @testset "approve/reject with path" begin
            # In order to use the "store" credential helper `git` needs to be installed and
            # on the path.
            if GIT_INSTALLED
                credential_path = joinpath(dir, ".git-credentials")
                isfile(credential_path) && rm(credential_path)

                # Requires `git` to be installed and available on the path.
                helper = parse(LibGit2.GitCredentialHelper, "store")

                # Set HOME to control where the .git-credentials file is written.
                # Note: In Cygwin environments `git` will use HOME instead of USERPROFILE.
                # Setting both environment variables ensures home was overridden.
                withenv("HOME" => dir, "USERPROFILE" => dir) do
                    query = LibGit2.GitCredential("https", "mygithost")
                    query_a = LibGit2.GitCredential("https", "mygithost", "a")
                    query_b = LibGit2.GitCredential("https", "mygithost", "b")

                    filled_a = LibGit2.GitCredential("https", "mygithost", "a", "alice", "1234")
                    filled_b = LibGit2.GitCredential("https", "mygithost", "b", "bob", "s3cre7")

                    function without_path(cred)
                        c = deepcopy(cred)
                        c.path = nothing
                        c
                    end

                    filled_without_path_a = without_path(filled_a)
                    filled_without_path_b = without_path(filled_b)

                    @test !isfile(credential_path)

                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == query
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_a))) do result
                        @test result == query_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_b))) do result
                        @test result == query_b
                    end

                    LibGit2.approve(helper, filled_a)
                    @test isfile(credential_path)
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == filled_without_path_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_a))) do result
                        @test result == filled_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_b))) do result
                        @test result == query_b
                    end

                    LibGit2.approve(helper, filled_b)
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == filled_without_path_b
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_a))) do result
                        @test result == filled_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_b))) do result
                        @test result == filled_b
                    end

                    LibGit2.reject(helper, filled_b)
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query))) do result
                        @test result == filled_without_path_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_a))) do result
                        @test result == filled_a
                    end
                    Base.shred!(LibGit2.fill!(helper, deepcopy(query_b))) do result
                        @test result == query_b
                    end

                    Base.shred!(query)
                    Base.shred!(query_a)
                    Base.shred!(query_b)
                    Base.shred!(filled_a)
                    Base.shred!(filled_b)
                    Base.shred!(filled_without_path_a)
                    Base.shred!(filled_without_path_b)
                end
            end
        end
    end

    # The following tests require that we can fake a TTY so that we can provide passwords
    # which use the `getpass` function. At the moment we can only fake this on UNIX based
    # systems.
    if Sys.isunix()
        git_ok = LibGit2.GitError(
            LibGit2.Error.None, LibGit2.Error.GIT_OK,
            "No errors")

        abort_prompt = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EUSER,
            "Aborting, user cancelled credential request.")

        prompt_limit = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EAUTH,
            "Aborting, maximum number of prompts reached.")

        incompatible_error = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EAUTH,
            "The explicitly provided credential is incompatible with the requested " *
            "authentication methods.")

        exhausted_error = LibGit2.GitError(
            LibGit2.Error.Callback, LibGit2.Error.EAUTH,
            "All authentication methods have failed.")

        @testset "SSH credential prompt" begin
            url = "git@github.com:test/package.jl"
            username = "git"

            valid_key = joinpath(KEY_DIR, "valid")
            valid_cred = LibGit2.SSHCredential(username, "", valid_key, valid_key * ".pub")

            valid_p_key = joinpath(KEY_DIR, "valid-passphrase")
            passphrase = "secret"
            valid_p_cred = LibGit2.SSHCredential(username, passphrase, valid_p_key, valid_p_key * ".pub")

            invalid_key = joinpath(KEY_DIR, "invalid")

            function gen_ex(cred; username="git")
                url = username !== nothing && !isempty(username) ? "$username@" : ""
                url *= "github.com:test/package.jl"
                quote
                    include($LIBGIT2_HELPER_PATH)
                    credential_loop($cred, $url, $username)
                end
            end

            ssh_ex = gen_ex(valid_cred)
            ssh_p_ex = gen_ex(valid_p_cred)
            ssh_u_ex = gen_ex(valid_cred, username=nothing)

            # Note: We cannot use the default ~/.ssh/id_rsa for tests since we cannot be
            # sure a users will actually have these files. Instead we will use the ENV
            # variables to set the default values.

            # ENV credentials are valid
            withenv("SSH_KEY_PATH" => valid_key) do
                err, auth_attempts, p = challenge_prompt(ssh_ex, [])
                @test err == git_ok
                @test auth_attempts == 1
            end

            # ENV credentials are valid but requires a passphrase
            withenv("SSH_KEY_PATH" => valid_p_key) do
                challenges = [
                    "Passphrase for $valid_p_key: " => "$passphrase\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User mistypes passphrase.
                # Note: In reality LibGit2 will raise an error upon using the invalid SSH
                # credentials. Since we don't control the internals of LibGit2 though they
                # could also just re-call the credential callback like they do for HTTP.
                challenges = [
                    "Passphrase for $valid_p_key: " => "foo\n",
                    "Private key location for 'git@github.com' [$valid_p_key]: " => "\n",
                    "Passphrase for $valid_p_key: " => "$passphrase\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2

                # User sends EOF in passphrase prompt which aborts the credential request
                challenges = [
                    "Passphrase for $valid_p_key: " => "\x04",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty passphrase
                challenges = [
                    "Passphrase for $valid_p_key: " => "\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1
            end

            # ENV credential requiring passphrase
            withenv("SSH_KEY_PATH" => valid_p_key, "SSH_KEY_PASS" => passphrase) do
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, [])
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Missing username
            withenv("SSH_KEY_PATH" => valid_key) do
                # User provides a valid username
                challenges = [
                    "Username for 'github.com': " => "$username\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_u_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User sends EOF in username prompt which aborts the credential request
                challenges = [
                    "Username for 'github.com': " => "\x04",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty username
                challenges = [
                    "Username for 'github.com': " => "\n",
                    "Username for 'github.com': " => "\x04",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 2

                # User repeatedly chooses an invalid username
                challenges = [
                    "Username for 'github.com': " => "foo\n",
                    "Username for 'github.com' [foo]: " => "\n",
                    "Private key location for 'foo@github.com' [$valid_key]: " => "\n",
                    "Username for 'github.com' [foo]: " => "\x04",  # Need to manually abort
                ]
                err, auth_attempts, p = challenge_prompt(ssh_u_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 3

                # Credential callback is given an empty string in the `username_ptr`
                # instead of the C_NULL in the other missing username tests.
                ssh_user_empty_ex = gen_ex(valid_cred, username="")
                challenges = [
                    "Username for 'github.com': " => "$username\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_user_empty_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Explicitly setting these env variables to be empty means the user will be
            # given a prompt with no defaults set.
            withenv("SSH_KEY_PATH" => nothing,
                    "SSH_PUB_KEY_PATH" => nothing,
                    "SSH_KEY_PASS" => nothing,
                    HOME => dir) do

                # Set the USERPROFILE / HOME above to be a directory that does not contain
                # the "~/.ssh/id_rsa" file. If this file exists the credential callback
                # will default to use this private key instead of triggering a prompt.
                @test !isfile(joinpath(homedir(), ".ssh", "id_rsa"))

                # User provides valid credentials
                challenges = [
                    "Private key location for 'git@github.com': " => "$valid_key\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User provides valid credentials that requires a passphrase
                challenges = [
                    "Private key location for 'git@github.com': " => "$valid_p_key\n",
                    "Passphrase for $valid_p_key: " => "$passphrase\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_p_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # User sends EOF in private key prompt which aborts the credential request
                challenges = [
                    "Private key location for 'git@github.com': " => "\x04",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 1

                # User provides an empty private key which triggers a re-prompt
                challenges = [
                    "Private key location for 'git@github.com': " => "\n",
                    "Private key location for 'git@github.com': " => "\x04",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == abort_prompt
                @test auth_attempts == 2

                # User provides an invalid private key until prompt limit reached.
                # Note: the prompt should not supply an invalid default.
                challenges = [
                    "Private key location for 'git@github.com': " => "foo\n",
                    "Private key location for 'git@github.com' [foo]: " => "foo\n",
                    "Private key location for 'git@github.com' [foo]: " => "foo\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == prompt_limit
                @test auth_attempts == 3
            end

            # Explicitly setting these env variables to an existing but invalid key pair
            # means the user will be given a prompt with that defaults to the given values.
            withenv("SSH_KEY_PATH" => invalid_key,
                    "SSH_PUB_KEY_PATH" => invalid_key * ".pub") do
                challenges = [
                    "Private key location for 'git@github.com' [$invalid_key]: " => "$valid_key\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2

                # User repeatedly chooses the default invalid private key until prompt limit reached
                challenges = [
                    "Private key location for 'git@github.com' [$invalid_key]: " => "\n",
                    "Private key location for 'git@github.com' [$invalid_key]: " => "\n",
                    "Private key location for 'git@github.com' [$invalid_key]: " => "\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == prompt_limit
                @test auth_attempts == 4
            end

            # Explicitly set the public key ENV variable to a non-existent file.
            withenv("SSH_KEY_PATH" => valid_key,
                    "SSH_PUB_KEY_PATH" => valid_key * ".public") do
                @test !isfile(ENV["SSH_PUB_KEY_PATH"])

                challenges = [
                    # "Private key location for 'git@github.com' [$valid_key]: " => "\n"
                    "Public key location for 'git@github.com' [$valid_key.public]: " => "$valid_key.pub\n"
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1
            end

            # Explicitly set the public key ENV variable to a public key that doesn't match
            # the private key.
            withenv("SSH_KEY_PATH" => valid_key,
                    "SSH_PUB_KEY_PATH" => invalid_key * ".pub") do
                @test isfile(ENV["SSH_PUB_KEY_PATH"])

                challenges = [
                    "Private key location for 'git@github.com' [$valid_key]: " => "\n"
                    "Public key location for 'git@github.com' [$invalid_key.pub]: " => "$valid_key.pub\n"
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2
            end

            Base.shred!(valid_cred)
            Base.shred!(valid_p_cred)
        end

        @testset "HTTPS credential prompt" begin
            url = "https://github.com/test/package.jl"

            valid_username = "julia"
            valid_password = randstring(16)
            valid_cred = LibGit2.UserPasswordCredential(valid_username, valid_password)

            https_ex = quote
                include($LIBGIT2_HELPER_PATH)
                credential_loop($valid_cred, $url)
            end

            # User provides a valid username and password
            challenges = [
                "Username for 'https://github.com': " => "$valid_username\n",
                "Password for 'https://$valid_username@github.com': " => "$valid_password\n",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 1

            # User sends EOF in username prompt which aborts the credential request
            challenges = [
                "Username for 'https://github.com': " => "\x04",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User sends EOF in password prompt which aborts the credential request
            challenges = [
                "Username for 'https://github.com': " => "foo\n",
                "Password for 'https://foo@github.com': " => "\x04",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User provides an empty password which aborts the credential request since we
            # cannot tell it apart from an EOF.
            challenges = [
                "Username for 'https://github.com': " => "foo\n",
                "Password for 'https://foo@github.com': " => "\n",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == abort_prompt
            @test auth_attempts == 1

            # User repeatedly chooses invalid username/password until the prompt limit is
            # reached
            challenges = [
                "Username for 'https://github.com': " => "foo\n",
                "Password for 'https://foo@github.com': " => "bar\n",
                "Username for 'https://github.com' [foo]: " => "foo\n",
                "Password for 'https://foo@github.com': " => "bar\n",
                "Username for 'https://github.com' [foo]: " => "foo\n",
                "Password for 'https://foo@github.com': " => "bar\n",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == prompt_limit
            @test auth_attempts == 3

            Base.shred!(valid_cred)
        end

        @testset "SSH agent username" begin
            url = "github.com:test/package.jl"

            valid_key = joinpath(KEY_DIR, "valid")
            valid_cred = LibGit2.SSHCredential("git", "", valid_key, valid_key * ".pub")

            function gen_ex(; username="git")
                quote
                    include($LIBGIT2_HELPER_PATH)
                    payload = CredentialPayload(allow_prompt=false, allow_ssh_agent=true,
                                                allow_git_helpers=false)
                    credential_loop($valid_cred, $url, $username, payload)
                end
            end

            # An empty string username_ptr
            ex = gen_ex(username="")
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == exhausted_error
            @test auth_attempts == 3

            # A null username_ptr passed into `git_cred_ssh_key_from_agent` can cause a
            # segfault.
            ex = gen_ex(username=nothing)
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == exhausted_error
            @test auth_attempts == 2

            Base.shred!(valid_cred)
        end

        @testset "SSH default" begin
            mktempdir() do home_dir
                url = "github.com:test/package.jl"

                default_key = joinpath(home_dir, ".ssh", "id_rsa")
                mkdir(dirname(default_key))

                valid_key = joinpath(KEY_DIR, "valid")
                valid_cred = LibGit2.SSHCredential("git", "", valid_key, valid_key * ".pub")

                valid_p_key = joinpath(KEY_DIR, "valid-passphrase")
                passphrase = "secret"
                valid_p_cred = LibGit2.SSHCredential("git", passphrase, valid_p_key, valid_p_key * ".pub")

                function gen_ex(cred)
                    quote
                        valid_cred = $cred

                        default_cred = deepcopy(valid_cred)
                        default_cred.prvkey = $default_key
                        default_cred.pubkey = $default_key * ".pub"

                        cp(valid_cred.prvkey, default_cred.prvkey)
                        cp(valid_cred.pubkey, default_cred.pubkey)

                        try
                            include($LIBGIT2_HELPER_PATH)
                            credential_loop(default_cred, $url, "git", shred=false)
                        finally
                            rm(default_cred.prvkey)
                            rm(default_cred.pubkey)
                        end
                    end
                end

                withenv("SSH_KEY_PATH" => nothing,
                        "SSH_PUB_KEY_PATH" => nothing,
                        "SSH_KEY_PASS" => nothing,
                        HOME => home_dir) do

                    # Automatically use the default key
                    ex = gen_ex(valid_cred)
                    err, auth_attempts, p = challenge_prompt(ex, [])
                    @test err == git_ok
                    @test auth_attempts == 1
                    @test p.credential.prvkey == default_key
                    @test p.credential.pubkey == default_key * ".pub"

                    # Confirm the private key if any other prompting is required
                    ex = gen_ex(valid_p_cred)
                    challenges = [
                        "Private key location for 'git@github.com' [$default_key]: " => "\n",
                        "Passphrase for $default_key: " => "$passphrase\n",
                    ]
                    err, auth_attempts, p = challenge_prompt(ex, challenges)
                    @test err == git_ok
                    @test auth_attempts == 1
                end

                Base.shred!(valid_cred)
                Base.shred!(valid_p_cred)
            end
        end

        @testset "SSH expand tilde" begin
            url = "git@github.com:test/package.jl"

            valid_key = joinpath(KEY_DIR, "valid")
            valid_cred = LibGit2.SSHCredential("git", "", valid_key, valid_key * ".pub")

            invalid_key = joinpath(KEY_DIR, "invalid")

            ssh_ex = quote
                include($LIBGIT2_HELPER_PATH)
                payload = CredentialPayload(allow_prompt=true, allow_ssh_agent=false,
                                            allow_git_helpers=false)
                credential_loop($valid_cred, $url, "git", payload, shred=false)
            end

            withenv("SSH_KEY_PATH" => nothing,
                    "SSH_PUB_KEY_PATH" => nothing,
                    "SSH_KEY_PASS" => nothing,
                    HOME => KEY_DIR) do

                # Expand tilde during the private key prompt
                challenges = [
                    "Private key location for 'git@github.com': " => "~/valid\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1
                @test p.credential.prvkey == abspath(valid_key)
            end

            withenv("SSH_KEY_PATH" => valid_key,
                    "SSH_PUB_KEY_PATH" => invalid_key * ".pub",
                    "SSH_KEY_PASS" => nothing,
                    HOME => KEY_DIR) do

                # Expand tilde during the public key prompt
                challenges = [
                    "Private key location for 'git@github.com' [$valid_key]: " => "\n",
                    "Public key location for 'git@github.com' [$invalid_key.pub]: " => "~/valid.pub\n",
                ]
                err, auth_attempts, p = challenge_prompt(ssh_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 2
                @test p.credential.pubkey == abspath(valid_key * ".pub")
            end

            Base.shred!(valid_cred)
        end

        @testset "SSH explicit credentials" begin
            url = "git@github.com:test/package.jl"
            username = "git"

            valid_p_key = joinpath(KEY_DIR, "valid-passphrase")
            passphrase = "secret"
            valid_cred = LibGit2.SSHCredential(username, passphrase, valid_p_key, valid_p_key * ".pub")

            invalid_key = joinpath(KEY_DIR, "invalid")
            invalid_cred = LibGit2.SSHCredential(username, "", invalid_key, invalid_key * ".pub")

            function gen_ex(cred; allow_prompt=true, allow_ssh_agent=false)
                quote
                    include($LIBGIT2_HELPER_PATH)
                    payload = CredentialPayload($cred, allow_prompt=$allow_prompt,
                                                allow_ssh_agent=$allow_ssh_agent,
                                                allow_git_helpers=false)
                    credential_loop($valid_cred, $url, $username, payload)
                end
            end

            # Explicitly provided credential is correct. Note: allowing prompting and
            # SSH agent to ensure they are skipped.
            ex = gen_ex(valid_cred, allow_prompt=true, allow_ssh_agent=true)
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == git_ok
            @test auth_attempts == 1
            @test p.explicit == valid_cred
            @test p.credential != valid_cred

            # Explicitly provided credential is incorrect
            ex = gen_ex(invalid_cred, allow_prompt=false, allow_ssh_agent=false)
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == exhausted_error
            @test auth_attempts == 3
            @test p.explicit == invalid_cred
            @test p.credential != invalid_cred

            Base.shred!(valid_cred)
            Base.shred!(invalid_cred)
        end

        @testset "HTTPS explicit credentials" begin
            url = "https://github.com/test/package.jl"

            valid_cred = LibGit2.UserPasswordCredential("julia", randstring(16))
            invalid_cred = LibGit2.UserPasswordCredential("alice", randstring(15))

            function gen_ex(cred; allow_prompt=true)
                quote
                    include($LIBGIT2_HELPER_PATH)
                    payload = CredentialPayload($cred, allow_prompt=$allow_prompt,
                                                allow_git_helpers=false)
                    credential_loop($valid_cred, $url, "", payload)
                end
            end

            # Explicitly provided credential is correct
            ex = gen_ex(valid_cred, allow_prompt=true)
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == git_ok
            @test auth_attempts == 1
            @test p.explicit == valid_cred
            @test p.credential != valid_cred

            # Explicitly provided credential is incorrect
            ex = gen_ex(invalid_cred, allow_prompt=false)
            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == exhausted_error
            @test auth_attempts == 2
            @test p.explicit == invalid_cred
            @test p.credential != invalid_cred

            Base.shred!(valid_cred)
            Base.shred!(invalid_cred)
        end

        @testset "Cached credentials" begin
            url = "https://github.com/test/package.jl"
            cred_id = "https://github.com"

            valid_username = "julia"
            valid_password = randstring(16)
            valid_cred = LibGit2.UserPasswordCredential(valid_username, valid_password)

            invalid_username = "alice"
            invalid_password = randstring(15)
            invalid_cred = LibGit2.UserPasswordCredential(invalid_username, invalid_password)

            function gen_ex(; cached_cred=nothing, allow_prompt=true)
                quote
                    include($LIBGIT2_HELPER_PATH)
                    cache = CachedCredentials()
                    $(cached_cred !== nothing && :(LibGit2.approve(cache, $cached_cred, $url)))
                    payload = CredentialPayload(cache, allow_prompt=$allow_prompt,
                                                allow_git_helpers=false)
                    credential_loop($valid_cred, $url, "", payload)
                end
            end

            # Cache contains a correct credential
            err, auth_attempts, p = challenge_prompt(gen_ex(cached_cred=valid_cred), [])
            @test err == git_ok
            @test auth_attempts == 1

            # Note: Approved cached credentials are not shredded

            # Add a credential into the cache
            ex = gen_ex()
            challenges = [
                "Username for 'https://github.com': " => "$valid_username\n",
                "Password for 'https://$valid_username@github.com': " => "$valid_password\n",
            ]
            err, auth_attempts, p = challenge_prompt(ex, challenges)
            cache = p.cache
            @test err == git_ok
            @test auth_attempts == 1
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict(cred_id => valid_cred)
            @test p.credential == valid_cred

            # Replace a credential in the cache
            ex = gen_ex(cached_cred=invalid_cred)
            challenges = [
                "Username for 'https://github.com' [alice]: " => "$valid_username\n",
                "Password for 'https://$valid_username@github.com': " => "$valid_password\n",
            ]
            err, auth_attempts, p = challenge_prompt(ex, challenges)
            cache = p.cache
            @test err == git_ok
            @test auth_attempts == 2
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict(cred_id => valid_cred)
            @test p.credential == valid_cred

            # Canceling a credential request should leave the cache unmodified
            ex = gen_ex(cached_cred=invalid_cred)
            challenges = [
                "Username for 'https://github.com' [alice]: " => "foo\n",
                "Password for 'https://foo@github.com': " => "bar\n",
                "Username for 'https://github.com' [foo]: " => "\x04",
            ]
            err, auth_attempts, p = challenge_prompt(ex, challenges)
            cache = p.cache
            @test err == abort_prompt
            @test auth_attempts == 3
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict(cred_id => invalid_cred)
            @test p.credential != invalid_cred

            # An EAUTH error should remove credentials from the cache
            ex = gen_ex(cached_cred=invalid_cred, allow_prompt=false)
            err, auth_attempts, p = challenge_prompt(ex, [])
            cache = p.cache
            @test err == exhausted_error
            @test auth_attempts == 2
            @test typeof(cache) == LibGit2.CachedCredentials
            @test cache.cred == Dict()
            @test p.credential != invalid_cred

            Base.shred!(valid_cred)
            Base.shred!(invalid_cred)
        end

        @testset "HTTPS git helper username" begin
            url = "https://github.com/test/package.jl"

            valid_username = "julia"
            valid_password = randstring(16)
            valid_cred = LibGit2.UserPasswordCredential(valid_username, valid_password)

            config_path = joinpath(dir, config_file)
            write(config_path, """
                [credential]
                    username = $valid_username
                """)

            https_ex = quote
                include($LIBGIT2_HELPER_PATH)
                LibGit2.with(LibGit2.GitConfig($config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                    payload = CredentialPayload(nothing,
                                                nothing, cfg,
                                                allow_git_helpers=true)
                    credential_loop($valid_cred, $url, nothing, payload, shred=false)
                end
            end

            # Username is supplied from the git configuration file
            challenges = [
                "Username for 'https://github.com' [$valid_username]: " => "\n",
                "Password for 'https://$valid_username@github.com': " => "$valid_password\n",
            ]
            err, auth_attempts, p = challenge_prompt(https_ex, challenges)
            @test err == git_ok
            @test auth_attempts == 1

            # Verify credential wasn't accidentally zeroed (#24731)
            @test p.credential == valid_cred

            Base.shred!(valid_cred)
        end

        @testset "HTTPS git helper password" begin
            if GIT_INSTALLED
                url = "https://github.com/test/package.jl"

                valid_username = "julia"
                valid_password = randstring(16)
                valid_cred = LibGit2.UserPasswordCredential(valid_username, valid_password)

                cred_file = joinpath(dir, "test-credentials")
                config_path = joinpath(dir, config_file)
                write(config_path, """
                    [credential]
                        helper = store --file $cred_file
                    """)

                # Directly write to the cleartext credential store. Note: we are not using
                # the LibGit2.approve message to avoid any possibility of the tests
                # accidentally writing to a user's global store.
                write(cred_file, "https://$valid_username:$valid_password@github.com")

                https_ex = quote
                    include($LIBGIT2_HELPER_PATH)
                    LibGit2.with(LibGit2.GitConfig($config_path, LibGit2.Consts.CONFIG_LEVEL_APP)) do cfg
                        payload = CredentialPayload(nothing,
                                                    nothing, cfg,
                                                    allow_git_helpers=true)
                        credential_loop($valid_cred, $url, nothing, payload, shred=false)
                    end
                end

                # Username will be provided by the credential helper
                challenges = []
                err, auth_attempts, p = challenge_prompt(https_ex, challenges)
                @test err == git_ok
                @test auth_attempts == 1

                # Verify credential wasn't accidentally zeroed (#24731)
                @test p.credential == valid_cred

                Base.shred!(valid_cred)
            end
        end

        @testset "Incompatible explicit credentials" begin
            # User provides a user/password credential where a SSH credential is required.
            valid_cred = LibGit2.UserPasswordCredential("foo", "bar")
            expect_ssh_ex = quote
                include($LIBGIT2_HELPER_PATH)
                payload = CredentialPayload($valid_cred, allow_ssh_agent=false,
                                            allow_git_helpers=false)
                credential_loop($valid_cred, "ssh://github.com/repo", "",
                                Cuint(LibGit2.Consts.CREDTYPE_SSH_KEY), payload)
            end

            err, auth_attempts, p = challenge_prompt(expect_ssh_ex, [])
            @test err == incompatible_error
            @test auth_attempts == 1
            @test p.explicit == valid_cred
            @test p.credential != valid_cred

            Base.shred!(valid_cred)

            # User provides a SSH credential where a user/password credential is required.
            valid_cred = LibGit2.SSHCredential("foo", "", "", "")
            expect_https_ex = quote
                include($LIBGIT2_HELPER_PATH)
                payload = CredentialPayload($valid_cred, allow_ssh_agent=false,
                                            allow_git_helpers=false)
                credential_loop($valid_cred, "https://github.com/repo", "",
                                Cuint(LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT), payload)
            end

            err, auth_attempts, p = challenge_prompt(expect_https_ex, [])
            @test err == incompatible_error
            @test auth_attempts == 1
            @test p.explicit == valid_cred
            @test p.credential != valid_cred

            Base.shred!(valid_cred)
        end

        # A hypothetical scenario where the the allowed authentication can either be
        # SSH or username/password.
        @testset "SSH & HTTPS authentication" begin
            allowed_types = Cuint(LibGit2.Consts.CREDTYPE_SSH_KEY) |
                Cuint(LibGit2.Consts.CREDTYPE_USERPASS_PLAINTEXT)

            # User provides a user/password credential where a SSH credential is required.
            valid_cred = LibGit2.UserPasswordCredential("foo", "bar")
            ex = quote
                include($LIBGIT2_HELPER_PATH)
                payload = CredentialPayload($valid_cred, allow_ssh_agent=false,
                                            allow_git_helpers=false)
                credential_loop($valid_cred, "foo://github.com/repo", "",
                                $allowed_types, payload)
            end

            err, auth_attempts, p = challenge_prompt(ex, [])
            @test err == git_ok
            @test auth_attempts == 1

            Base.shred!(valid_cred)
        end

        @testset "CredentialPayload reset" begin
            urls = [
                "https://github.com/test/package.jl"
                "https://myhost.com/demo.jl"
            ]

            valid_username = "julia"
            valid_password = randstring(16)
            valid_cred = LibGit2.UserPasswordCredential(valid_username, valid_password)

            # Users should be able to re-use the same payload if the state is reset
            ex = quote
                include($LIBGIT2_HELPER_PATH)
                user = nothing
                payload = CredentialPayload(allow_git_helpers=false)
                first_result = credential_loop($valid_cred, $(urls[1]), user, payload)
                LibGit2.reset!(payload)
                second_result = credential_loop($valid_cred, $(urls[2]), user, payload)
                (first_result, second_result)
            end

            challenges = [
                "Username for 'https://github.com': " => "$valid_username\n",
                "Password for 'https://$valid_username@github.com': " => "$valid_password\n",
                "Username for 'https://myhost.com': " => "$valid_username\n",
                "Password for 'https://$valid_username@myhost.com': " => "$valid_password\n",
            ]
            first_result, second_result = challenge_prompt(ex, challenges)

            err, auth_attempts, p = first_result
            @test err == git_ok
            @test auth_attempts == 1

            err, auth_attempts, p = second_result
            @test err == git_ok
            @test auth_attempts == 1

            Base.shred!(valid_cred)
        end
    end

    # Note: Tests only work on linux as SSL_CERT_FILE is only respected on linux systems.
    @testset "Hostname verification" begin
        openssl_installed = false
        common_name = ""
        if Sys.islinux()
            try
                # OpenSSL needs to be on the path
                openssl_installed = !isempty(read(`openssl version`, String))
            catch ex
                @warn "Skipping hostname verification tests. Is `openssl` on the path?" exception=ex
            end

            # Find a hostname that maps to the loopback address
            hostnames = ["localhost"]

            # In minimal environments a hostname might not be available (issue #20758)
            try
                # In some environments, namely Macs, the hostname "macbook.local" is bound
                # to the external address while "macbook" is bound to the loopback address.
                pushfirst!(hostnames, replace(gethostname(), r"\..*$" => ""))
            catch
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
                @warn "Skipping hostname verification tests. Unable to determine a hostname which maps to the loopback address"
            end
        end
        if openssl_installed && !isempty(common_name)
            mktempdir() do root
                key = joinpath(root, common_name * ".key")
                cert = joinpath(root, common_name * ".crt")
                pem = joinpath(root, common_name * ".pem")

                # Generated a certificate which has the CN set correctly but no subjectAltName
                run(pipeline(`openssl req -new -x509 -newkey rsa:2048 -sha256 -nodes -keyout $key -out $cert -days 1 -subj "/CN=$common_name"`, stderr=devnull))
                run(`openssl x509 -in $cert -out $pem -outform PEM`)

                # Find an available port by listening
                port, server = listenany(49152)
                close(server)

                # Make a fake Julia package and minimal HTTPS server with our generated
                # certificate. The minimal server can't actually serve a Git repository.
                mkdir(joinpath(root, "Example.jl"))
                pobj = cd(root) do
                    run(`openssl s_server -key $key -cert $cert -WWW -accept $port`, wait=false)
                end

                errfile = joinpath(root, "error")
                repo_url = "https://$common_name:$port/Example.jl"
                repo_dir = joinpath(root, "dest")
                code = """
                    using Serialization
                    import LibGit2
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

let cache = LibGit2.CachedCredentials()
    get!(cache, "foo", LibGit2.SSHCredential("", "bar"))
    Base.shred!(cache)
    @test all(cache["foo"].pass.data .== UInt(0))
end

end # module
