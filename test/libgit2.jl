# This file is a part of Julia. License is MIT: http://julialang.org/license

# check that libgit2 has been installed correctly

const LIBGIT2_VER = v"0.22.2+"

function check_version()
    major, minor, patch = Cint[0], Cint[0], Cint[0]
    ccall((:git_libgit2_version, :libgit2), Void,
          (Ptr{Cint}, Ptr{Cint}, Ptr{Cint}), major, minor, patch)
    v = VersionNumber(major[1], minor[1], patch[1])
    if v.major == LIBGIT2_VER.major && v.minor >= LIBGIT2_VER.minor
        return true
    else
        return false
    end
end

@test check_version()

function credentials!(cfg::Pkg.LibGit2.GitConfig, usr="Test User", usr_email="Test@User.com")
    git_user = Pkg.LibGit2.get(cfg, "user.name", usr)
    usr==git_user && Pkg.LibGit2.set!(cfg, "user.name", usr)
    git_user_email = Pkg.LibGit2.get(cfg, "user.email", usr_email)
    usr_email==git_user_email && Pkg.LibGit2.set!(cfg, "user.email", usr_email)
end

#TODO: tests need 'user.name' & 'user.email' in config ???
Pkg.LibGit2.with(Pkg.LibGit2.GitConfig) do cfg
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
    url = "https://github.com/JuliaLang/Example.jl"
    path = joinpath(dir, "Example.Bare")
    repo = Pkg.LibGit2.clone(url, path, bare = true, remote_cb = Pkg.LibGit2.mirror_cb)
    Pkg.LibGit2.finalize(repo)
    @test isdir(path)
    @test isfile(joinpath(path, Pkg.LibGit2.GitConst.HEAD_FILE))
end

# clone
temp_dir() do dir
    url = "https://github.com/JuliaLang/Example.jl"
    path = joinpath(dir, "Example")
    repo = Pkg.LibGit2.clone(url, path)
    Pkg.LibGit2.finalize(repo)
    @test isdir(path)
    @test isdir(joinpath(path, ".git"))
end

# init
temp_dir() do dir
    path = joinpath(dir, "Example")
    repo = Pkg.LibGit2.init(path)

    @test isdir(path)
    @test isdir(joinpath(path, ".git"))

    branch = "upstream"
    url = "https://github.com/JuliaLang/julia.git"
    remote = Pkg.LibGit2.GitRemote(repo, branch, url)
    Pkg.LibGit2.save(remote)
    Pkg.LibGit2.finalize(remote)

    config = joinpath(path, ".git", "config")
    lines = split(open(readall, config, "r"), "\n")
    @test any(map(x->x == "[remote \"upstream\"]", lines))

    remote = Pkg.LibGit2.get(Pkg.LibGit2.GitRemote, repo, branch)
    @test Pkg.LibGit2.url(remote) == url

    Pkg.LibGit2.finalize(repo)
end

# fetch
temp_dir() do dir_cache
    # create cache
    url = "https://github.com/JuliaLang/Example.jl"
    path_cache = joinpath(dir_cache, "Example.Bare")
    repo = Pkg.LibGit2.clone(url, path_cache, bare = true, remote_cb = Pkg.LibGit2.mirror_cb)
    Pkg.LibGit2.with(Pkg.LibGit2.GitConfig, repo) do cfg
        credentials!(cfg)
    end
    Pkg.LibGit2.finalize(repo)


    # fetch
    temp_dir() do dir
        # clone repo
        path = joinpath(dir, "Example")
        repo = Pkg.LibGit2.clone(path_cache, path)
        Pkg.LibGit2.with(Pkg.LibGit2.GitConfig, repo) do cfg
            credentials!(cfg)
        end

        Pkg.LibGit2.fetch(repo)
        refs1 = parse(Int, readchomp(pipe(`find $(joinpath(path, ".git/refs"))`,`wc -l`)))

        Pkg.LibGit2.fetch(repo, path_cache, refspecs = "+refs/*:refs/remotes/cache/*")
        refs2 = parse(Int, readchomp(pipe(`find $(joinpath(path, ".git/refs"))`,`wc -l`)))

        Pkg.LibGit2.finalize(repo)
        @test refs1 > 0
        @test refs2 > 0
        @test refs2 > refs1
    end

    # revwalk
    temp_dir() do dir
        path = joinpath(dir, "Example")
        repo = Pkg.LibGit2.clone(path_cache, path)
        Pkg.LibGit2.with(Pkg.LibGit2.GitConfig, repo) do cfg
            credentials!(cfg)
        end
        oids = Pkg.LibGit2.map((oid,repo)->string(oid), repo, by = Pkg.LibGit2.GitConst.SORT_TIME)
        @test length(oids) > 0
        Pkg.LibGit2.finalize(repo)

        Pkg.LibGit2.with(Pkg.LibGit2.GitRepo, path) do repo
            oid = Pkg.LibGit2.Oid(oids[end])
            oids = Pkg.LibGit2.map((oid,repo)->(oid,repo), repo, oid=oid, by=Pkg.LibGit2.GitConst.SORT_TIME)
            @test length(oids) > 0
        end
    end

    # signature
    temp_dir() do dir
        path = joinpath(dir, "Example")
        repo = Pkg.LibGit2.clone(path_cache, path)
        oid = "129eb39c8e0817c616296d1ac5f2cd1cf4f8b312"
        c = Pkg.LibGit2.get(Pkg.LibGit2.GitCommit, repo, oid)
        auth = Pkg.LibGit2.author(c)
        cmtr = Pkg.LibGit2.committer(c)
        cmsg = Pkg.LibGit2.message(c)
        @test isa(auth, Pkg.LibGit2.Signature)
        @test length(auth.name) > 0
        @test isa(cmtr, Pkg.LibGit2.Signature)
        @test length(cmtr.email) > 0
        @test length(cmsg) > 0
        Pkg.LibGit2.finalize(repo)

        sig = Pkg.LibGit2.Signature("AAA", "AAA@BBB.COM", round(time(), 0), 0)
        git_sig = convert(Pkg.LibGit2.GitSignature, sig)
        sig2 = Pkg.LibGit2.Signature(git_sig)
        Pkg.LibGit2.finalize(git_sig)
        @test sig.name == sig2.name
        @test sig.email == sig2.email
        @test sig.time == sig2.time
    end

    # transact
    temp_dir() do dir
        # clone repo
        path = joinpath(dir, "Example")
        repo = Pkg.LibGit2.clone(path_cache, path)
        Pkg.LibGit2.with(Pkg.LibGit2.GitConfig, repo) do cfg
            credentials!(cfg)
        end
        cp(joinpath(path, "REQUIRE"), joinpath(path, "CCC"))
        cp(joinpath(path, "REQUIRE"), joinpath(path, "AAA"))
        Pkg.LibGit2.add!(repo, "AAA")
        @test_throws ErrorException Pkg.LibGit2.transact(repo) do repo
            mv(joinpath(path, "REQUIRE"), joinpath(path, "BBB"))
            Pkg.LibGit2.add!(repo, "BBB")
            oid = Pkg.LibGit2.commit(repo, "test commit")
            error("Force recovery")
        end
        @test isfile(joinpath(path, "AAA"))
        @test isfile(joinpath(path, "CCC"))
        @test !isfile(joinpath(path, "BBB"))
        @test isfile(joinpath(path, "REQUIRE"))
    end

end

# strarray
begin
    p1 = "XXX"
    p2 = "YYY"
    sa1 = Pkg.LibGit2.StrArrayStruct(p1)
    try
        arr = convert(Vector{AbstractString}, sa1)
        @test arr[1] == p1
    finally
        Pkg.LibGit2.finalize(sa1)
    end

    sa2 = Pkg.LibGit2.StrArrayStruct(p1, p2)
    try
        arr = convert(Vector{AbstractString}, sa2)
        @test arr[1] == p1
        @test arr[2] == p2
    finally
        Pkg.LibGit2.finalize(sa2)
    end
end


