module Git
#
# some utility functions for working with git repos
#
import Base: shell_escape

function dir(d)
    g = joinpath(d,".git")
    isdir(g) && return g
    c = `git rev-parse --git-dir`
    isempty(d) || (c = `sh -c "cd $(shell_escape(d)) && $(shell_escape(c))"`)
    normpath(d,Base.readchomp(c))
end

function git(d)
    isempty(d) && return `git`
    work_tree = abspath(d)
    git_dir = joinpath(work_tree, dir(work_tree))
    normpath(work_tree, ".") == normpath(git_dir, ".") ? # is it a bare repo?
    `git --git-dir=$work_tree` : `git --work-tree=$work_tree --git-dir=$git_dir`
end

cmd(args::Cmd; dir="") = `$(git(dir)) $args`
run(args::Cmd; dir="", out=STDOUT) = Base.run(cmd(args,dir=dir) |> out)
readall(args::Cmd; dir="") = Base.readall(cmd(args,dir=dir))
readchomp(args::Cmd; dir="") = Base.readchomp(cmd(args,dir=dir))
success(args::Cmd; dir="") = Base.success(cmd(args,dir=dir))

modules(args::Cmd; dir="") = readchomp(`config -f .gitmodules $args`, dir=dir)
different(verA::String, verB::String, path::String; dir="") =
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

immutable State
    head::ASCIIString
    index::ASCIIString
    work::ASCIIString
end

function snapshot(; dir="")
    head = readchomp(`rev-parse HEAD`, dir=dir)
    index = readchomp(`write-tree`, dir=dir)
    work = try
        run(`add --all`, dir=dir)
        run(`add .`, dir=dir)
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

const GITHUB_REGEX = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

function set_remote_url(url::String; remote::String="origin", dir="")
    run(`config remote.$remote.url $url`, dir=dir)
    m = match(GITHUB_REGEX,url)
    m == nothing && return
    push = "git@github.com:$(m.captures[1])"
    push != url && run(`config remote.$remote.pushurl $push`, dir=dir)
end

end # module
