module Git
#
# some utility functions for working with git repos
#
import Base: shell_escape

function dir(d)
    g = joinpath(d,".git")
    isdir(g) && return g
    normpath(d, Base.readchomp(setenv(`git rev-parse --git-dir`, dir=d)))
end

function git(d)
    isempty(d) && return `git`
    work_tree = abspath(d)
    git_dir = joinpath(work_tree, dir(work_tree))
    normpath(work_tree, ".") == normpath(git_dir, ".") ? # is it a bare repo?
    `git --git-dir=$work_tree` : `git --work-tree=$work_tree --git-dir=$git_dir`
end

function get_repo(dir)
    return Base.LibGit2.repo_open(dir)
end

cmd(args::Cmd; dir="") = `$(git(dir)) $args`
run(args::Cmd; dir="", out=STDOUT) = Base.run(cmd(args,dir=dir) |> out)
readall(args::Cmd; dir="") = Base.readall(cmd(args,dir=dir))
readchomp(args::Cmd; dir="") = Base.readchomp(cmd(args,dir=dir))

function success(args::Cmd; dir="")
    g = git(dir)
    repo = get_repo(dir)
    if !Base.LibGit2.repo_isbare(repo)
        Base.LibGit2.update_all!(Base.LibGit2.repo_index(repo))
    end
    # Base.readchomp(`$g rev-parse --is-bare-repository`) == "false" &&
    #     Base.run(`$g update-index -q --really-refresh`)
    Base.success(`$g $args`)
end

macro libgit2_success(args)
    quote
        try
            $args
            return true
        except
            return false
        end
    end
end

modules(args::Cmd; dir="") = readchomp(`config -f .gitmodules $args`, dir=dir)
different(verA::String, verB::String, path::String; dir="") =
    !success(`diff-tree --quiet $verA $verB -- $path`, dir=dir)

#dirty(; dir="") = !success(`diff-index --quiet HEAD`, dir=dir)
function dirty(; dir="")
    repo = get_repo(dir)
    return length(Base.LibGit2.diff(repo, repo_index(repo))) > 0
end
staged(; dir="") = !success(`diff-index --quiet --cached HEAD`, dir=dir)
#unstaged(; dir="") = !success(`diff-files --quiet`, dir=dir)
function unstaged(; dir="")
    repo = get_repo(dir)
    return length(Base.LibGit2.diff(repo)) > 0
end
dirty(paths; dir="") = !success(`diff-index --quiet HEAD -- $paths`, dir=dir)
staged(paths; dir="") = !success(`diff-index --quiet --cached HEAD -- $paths`, dir=dir)
unstaged(paths; dir="") = !success(`diff-files --quiet -- $paths`, dir=dir)

#iscommit(name; dir="") = success(`cat-file commit $name`, dir=dir)
iscommit(name; dir="") = @libgit2_success Base.LibGit2.lookup(get_repo(dir), Base.LibGit2.Oid(name)) &&
                            @libgit2_success Base.LibGit2.lookup_ref(get_repo(dir), name)
#attached(; dir="") = success(`symbolic-ref -q HEAD`, dir=dir)
attached(; dir="") = @libgit2_success Base.LibGit2.name(Base.LibGit2.head(get_repo(dir)))
#branch(; dir="") = readchomp(`rev-parse --symbolic-full-name --abbrev-ref HEAD`, dir=dir)
branch(; dir="") = Base.LibGit2.shorthand(Base.LibGit2.head(get_repo(dir)))
#head(; dir="") = readchomp(`rev-parse HEAD`, dir=dir)
head(; dir="") = string(Base.LibGit2.target(Base.LibGit2.head(get_repo(dir))))

immutable State
    head::ASCIIString
    index::ASCIIString
    work::ASCIIString
end

function snapshot(; dir="")
    head = readchomp(`rev-parse HEAD`, dir=dir)
    index = readchomp(`write-tree`, dir=dir)
    work = try
        if length(readdir(abspath(dir))) > 1
            run(`add --all`, dir=dir)
            run(`add .`, dir=dir)
        end
        readchomp(`write-tree`, dir=dir)
    finally
        run(`read-tree $index`, dir=dir) # restore index
    end
    State(head, index, work)
end

function restore(s::State; dir="")
    run(`reset -q --`, dir=dir)               # unstage everything
    run(`read-tree $(s.work)`, dir=dir)       # move work tree to index
    run(`checkout-index -fa`, dir=dir)        # check the index out to work
    run(`clean -qdf`, dir=dir)                # remove everything else
    run(`read-tree $(s.index)`, dir=dir)      # restore index
    run(`reset -q --soft $(s.head)`, dir=dir) # restore head
end

function transact(f::Function; dir="")
    state = snapshot(dir=dir)
    try f() catch
        restore(state, dir=dir)
        rethrow()
    end
end

function is_ancestor_of(a::String, b::String; dir="")
    A = readchomp(`rev-parse $a`, dir=dir)
    readchomp(`merge-base $A $b`, dir=dir) == A
end

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

function set_remote_url(url::String; remote::String="origin", dir="")
    repo = Base.LibGit2.repo_open(dir)
    config = Base.LibGit2.config(repo)
    config["remote.$remote.url"] = url
    m = match(GITHUB_REGEX,url)
    m == nothing && return
    push = "git@github.com:$(m.captures[1]).git"
    if push != url
        config["remote.$remote.pushurl"] = push
    end
end

function normalize_url(url::String)
    m = match(GITHUB_REGEX,url)
    m == nothing ? url : "git://github.com/$(m.captures[1]).git"
end

end # module
