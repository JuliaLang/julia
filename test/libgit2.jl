# This file is a part of Julia. License is MIT: http://julialang.org/license

# check that libgit2 has been installed correctly

const LIBGIT2_VER = v"0.23.0"

function check_version()
    v = LibGit2.version()
    if v.major == LIBGIT2_VER.major && v.minor >= LIBGIT2_VER.minor
        return true
    else
        return false
    end
end
@test check_version()

# strarray
begin
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

function credentials!(cfg::LibGit2.GitConfig, usr="Test User", usr_email="Test@User.com")
    git_user = LibGit2.get(cfg, "user.name", usr)
    usr==git_user && LibGit2.set!(cfg, "user.name", usr)
    git_user_email = LibGit2.get(cfg, "user.email", usr_email)
    usr_email==git_user_email && LibGit2.set!(cfg, "user.email", usr_email)
end

#TODO: tests need 'user.name' & 'user.email' in config ???
LibGit2.with(LibGit2.GitConfig) do cfg
    credentials!(cfg)
end

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

# clone bare
temp_dir() do dir
    repo_url = "https://github.com/JuliaLang/Example.jl"
    repo_path = joinpath(dir, "Example.Bare")
    repo = LibGit2.clone(repo_url, repo_path, isbare = true, remote_cb = LibGit2.mirror_cb())
    finalize(repo)
    @test isdir(repo_path)
    @test isfile(joinpath(repo_path, LibGit2.Consts.HEAD_FILE))
end

# clone
temp_dir() do dir
    url = "https://github.com/JuliaLang/Example.jl"
    path = joinpath(dir, "Example")
    repo = LibGit2.clone(url, path)
    finalize(repo)
    @test isdir(path)
    @test isdir(joinpath(path, ".git"))
end

# init
temp_dir() do dir
    path = joinpath(dir, "Example")
    repo = LibGit2.init(path)

    @test isdir(path)
    @test isdir(joinpath(path, ".git"))

    branch = "upstream"
    url = "https://github.com/JuliaLang/julia.git"
    remote = LibGit2.GitRemote(repo, branch, url)
    finalize(remote)

    config = joinpath(path, ".git", "config")
    lines = split(open(readall, config, "r"), "\n")
    @test any(map(x->x == "[remote \"upstream\"]", lines))

    remote = LibGit2.get(LibGit2.GitRemote, repo, branch)
    @test LibGit2.url(remote) == url

    finalize(repo)
end

# fetch
temp_dir() do dir_cache
    # create cache
    url = "https://github.com/JuliaLang/Example.jl"
    path_cache = joinpath(dir_cache, "Example.Bare")
    repo = LibGit2.clone(url, path_cache, isbare = true, remote_cb = LibGit2.mirror_cb())
    LibGit2.with(LibGit2.GitConfig, repo) do cfg
        credentials!(cfg)
    end
    finalize(repo)

    # fetch
    temp_dir() do dir
        # clone repo
        path = joinpath(dir, "Example")
        repo = LibGit2.clone(path_cache, path)
        LibGit2.with(LibGit2.GitConfig, repo) do cfg
            credentials!(cfg)
        end

        LibGit2.fetch(repo)
        refs1 = parse(Int, readchomp(pipeline(`find $(joinpath(path, ".git/refs"))`,`wc -l`)))

        LibGit2.fetch(repo, remoteurl=path_cache, refspecs =["+refs/*:refs/remotes/cache/*"])
        refs2 = parse(Int, readchomp(pipeline(`find $(joinpath(path, ".git/refs"))`,`wc -l`)))

        finalize(repo)
        @test refs1 > 0
        @test refs2 > 0
        @test refs2 > refs1
    end

    # signature
    temp_dir() do dir
        path = joinpath(dir, "Example")
        LibGit2.with(LibGit2.clone(path_cache, path)) do repo
            oid = "129eb39c8e0817c616296d1ac5f2cd1cf4f8b312"
            c = LibGit2.get(LibGit2.GitCommit, repo, oid)
            auth = LibGit2.author(c)
            cmtr = LibGit2.committer(c)
            cmsg = LibGit2.message(c)
            @test isa(auth, LibGit2.Signature)
            @test length(auth.name) > 0
            @test isa(cmtr, LibGit2.Signature)
            @test length(cmtr.email) > 0
            @test length(cmsg) > 0

            sig = LibGit2.Signature("AAA", "AAA@BBB.COM", round(time(), 0), 0)
            git_sig = convert(LibGit2.GitSignature, sig)
            sig2 = LibGit2.Signature(git_sig)
            finalize(git_sig)
            @test sig.name == sig2.name
            @test sig.email == sig2.email
            @test sig.time == sig2.time
        end
    end

    # revwalk
    temp_dir() do dir
        path = joinpath(dir, "Example")
        repo = LibGit2.clone(path_cache, path)
        LibGit2.with(LibGit2.GitConfig, repo) do cfg
            credentials!(cfg)
        end
        oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
            LibGit2.map((oid,repo)->string(oid), walker, by = LibGit2.Consts.SORT_TIME)
        end
        @test length(oids) > 0
        finalize(repo)

        LibGit2.with(LibGit2.GitRepo, path) do repo
            oid = LibGit2.Oid(oids[end])
            oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
                LibGit2.map((oid,repo)->(oid,repo), walker, oid=oid, by=LibGit2.Consts.SORT_TIME)
            end
            @test length(oids) > 0
        end
    end

    # transact
    temp_dir() do dir
        # clone repo
        path = joinpath(dir, "Example")
        LibGit2.with(LibGit2.clone(path_cache, path)) do repo
            LibGit2.with(LibGit2.GitConfig, repo) do cfg
                credentials!(cfg)
            end
            cp(joinpath(path, "REQUIRE"), joinpath(path, "CCC"))
            cp(joinpath(path, "REQUIRE"), joinpath(path, "AAA"))
            LibGit2.add!(repo, "AAA")
            @test_throws ErrorException LibGit2.transact(repo) do repo
                mv(joinpath(path, "REQUIRE"), joinpath(path, "BBB"))
                LibGit2.add!(repo, "BBB")
                oid = LibGit2.commit(repo, "test commit")
                error("Force recovery")
            end
            @test isfile(joinpath(path, "AAA"))
            @test isfile(joinpath(path, "CCC"))
            @test !isfile(joinpath(path, "BBB"))
            @test isfile(joinpath(path, "REQUIRE"))
        end
    end

    # branch
    temp_dir() do dir
        path = joinpath(dir, "Example")
        LibGit2.with(LibGit2.clone(path_cache, path)) do repo
            LibGit2.with(LibGit2.GitConfig, repo) do cfg
                credentials!(cfg)
            end
            brnch = LibGit2.branch(repo)
            @test brnch == "master"

            brnch_name = "test"
            LibGit2.branch!(repo, brnch_name)
            brnch = LibGit2.branch(repo)
            @test brnch == brnch_name
        end
    end
end

# Oid
begin
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

