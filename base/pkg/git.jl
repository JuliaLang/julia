# This file is a part of Julia. License is MIT: http://julialang.org/license

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

cmd(args::Cmd; dir="") = `$(git(dir)) $args`
run(args::Cmd; dir="", out=STDOUT) = Base.run(pipeline(cmd(args,dir=dir), out))
readstring(args::Cmd; dir="") = Base.readstring(cmd(args,dir=dir))
readchomp(args::Cmd; dir="") = Base.readchomp(cmd(args,dir=dir))

function success(args::Cmd; dir="")
    g = git(dir)
    Base.readchomp(`$g rev-parse --is-bare-repository`) == "false" &&
        Base.run(`$g update-index -q --really-refresh`)
    Base.success(`$g $args`)
end

function version()
    vs = split(readchomp(`version`), ' ')[3]
    ns = split(vs, '.')
    if length(ns) > 3
        VersionNumber(join(ns[1:3], '.'))
    else
        VersionNumber(join(ns, '.'))
    end
end

modules(args::Cmd; dir="") = readchomp(`config -f .gitmodules $args`, dir=dir)
different(verA::AbstractString, verB::AbstractString, path::AbstractString; dir="") =
    !success(`diff-tree --quiet $verA $verB -- $path`, dir=dir)

dirty(; dir="") = !success(`diff-index --quiet HEAD`, dir=dir)
staged(; dir="") = !success(`diff-index --quiet --cached HEAD`, dir=dir)
unstaged(; dir="") = !success(`diff-files --quiet`, dir=dir)
dirty(paths; dir="") = !success(`diff-index --quiet HEAD -- $paths`, dir=dir)
staged(paths; dir="") = !success(`diff-index --quiet --cached HEAD -- $paths`, dir=dir)
unstaged(paths; dir="") = !success(`diff-files --quiet -- $paths`, dir=dir)

iscommit(name; dir="") = success(`cat-file commit $name`, dir=dir)
attached(; dir="") = success(`symbolic-ref -q HEAD`, dir=dir)
branch(; dir="") = readchomp(`rev-parse --symbolic-full-name --abbrev-ref HEAD`, dir=dir)
head(; dir="") = readchomp(`rev-parse HEAD`, dir=dir)

function iscommit(sha1s::Vector; dir="")
    indexin(sha1s,split(readchomp(`log --all --format=%H`, dir=dir),"\n")).!=0
end

immutable State
    head::String
    index::String
    work::String
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

function is_ancestor_of(a::AbstractString, b::AbstractString; dir="")
    A = readchomp(`rev-parse $a`, dir=dir)
    readchomp(`merge-base $A $b`, dir=dir) == A
end

const GITHUB_REGEX =
    r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](([^/].+)/(.+?))(?:\.git)?$"i

function set_remote_url(url::AbstractString; remote::AbstractString="origin", dir="")
    run(`config remote.$remote.url $url`, dir=dir)
    m = match(GITHUB_REGEX,url)
    m === nothing && return
    push = "git@github.com:$(m.captures[1]).git"
    push != url && run(`config remote.$remote.pushurl $push`, dir=dir)
end

function normalize_url(url::AbstractString)
    m = match(GITHUB_REGEX,url)
    m === nothing ? url : "git://github.com/$(m.captures[1]).git"
end

end # module
