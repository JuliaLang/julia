module Git
#
# some utility functions for working with git repos
#

dir(d) = cd(dir,d)
dir() = Base.readchomp(`git rev-parse --git-dir`)

function git(d)
    isempty(d) && return `git`
    work_tree = abspath(d)
    git_dir = joinpath(work_tree, dir(work_tree))
    normpath(work_tree, ".") == normpath(git_dir, ".") ? # is it a bare repo?
    `git --git-dir=$work_tree` : `git --work-tree=$work_tree --git-dir=$git_dir`
end

function gitenv(cmd)
    @windows_only begin
        env = Dict(zip(ENV...)...)
        env["HOME"] = ENV["USERPROFILE"]
        cmd = setenv(cmd, env)
    end
    cmd
end

run(args::Cmd; dir="", out=STDOUT) = Base.run(gitenv(`$(git(dir)) $args`) |> out)
success(args::Cmd; dir="") = Base.success(gitenv(`$(git(dir)) $args`))
readall(args::Cmd; dir="") = Base.readall(gitenv(`$(git(dir)) $args`))
readchomp(args::Cmd; dir="") = Base.readchomp(gitenv(`$(git(dir)) $args`))

modules(args::Cmd; dir="") = readchomp(`config -f .gitmodules $args`, dir=dir)
different(verA::String, verB::String, path::String; dir="") =
    !success(`diff --quiet $verA $verB -- $path`, dir=dir)

dirty(; dir="") = !success(`diff --quiet HEAD`, dir=dir)
staged(; dir="") = !success(`diff --quiet --cached`, dir=dir)
unstaged(; dir="") = !success(`diff --quiet`, dir=dir)
dirty(paths; dir="") = !success(`diff --quiet HEAD -- $paths`, dir=dir)
staged(paths; dir="") = !success(`diff --quiet --cached -- $paths`, dir=dir)
unstaged(paths; dir="") = !success(`diff --quiet -- $paths`, dir=dir)
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

## below here to be retired ##

function each_tagged_version()
    git_dir = abspath(dir())
    @task for line in eachline(`git --git-dir=$git_dir show-ref --tags`)
        m = match(r"^([0-9a-f]{40}) refs/tags/(v\S+)$", line)
        if m != nothing && ismatch(Base.VERSION_REGEX, m.captures[2])
            produce((convert(VersionNumber,m.captures[2]),m.captures[1]))
        end
    end
end
each_tagged_version(dir::String) = cd(each_tagged_version,dir)

function each_submodule(f::Function, recursive::Bool, dir::ByteString)
    cmd = `git submodule foreach --quiet 'echo "$name $path $sha1"'`
    for line in eachline(cmd)
        name, path, sha1 = match(r"^(.*) (.*) ([0-9a-f]{40})$", line).captures
        cd(dir) do
            f(name, path, sha1)
        end
        if recursive
            cd(path) do
                each_submodule(true, dir) do n,p,s
                    cd(dir) do
                        f(n,"$path/$p",s)
                    end
                end
            end
        end
    end
end
each_submodule(f::Function, r::Bool) = each_submodule(f, r, pwd())

function read_config(file::String)
    cfg = Dict()
    # TODO: use --null option for better handling of weird values.
    for line in eachline(`git config -f $file --get-regexp '.*'`)
        key, val = match(r"^(\S+)\s+(.*)$", line).captures
        cfg[key] = haskey(cfg,key) ? [cfg[key],val] : val
    end
    return cfg
end

# TODO: provide a clean way to avoid this disaster
function read_config_blob(blob::String)
    tmp, io = mktemp()
    try
        write(io, readall(`cat-file blob $blob`))
    finally
        close(io)
    end
    cfg = read_config(tmp)
    run(`rm -f tmp`)
    return cfg
end

function write_config(file::String, cfg::Dict)
    tmp = tempname()
    for key in sort!(collect(keys(cfg)))
        val = cfg[key]
        if isa(val,Array)
            for x in val
                run(`config -f $tmp --add $key $x`)
            end
        else
            run(`config -f $tmp $key $val`)
        end
    end
    if isfile(tmp)
        open(file,"w") do io
            print(io,readall(tmp))
        end
    end
end

canonicalize_config(file::String) = write_config(file, read_config(file))

function config_sections(cfg::Dict)
    sections = Set{ByteString}()
    for (key,_) in cfg
        m = match(r"^(.+)\.", key)
        if m != nothing add!(sections,m.captures[1]) end
    end
    sections
end

function merge_configs(Bc::Dict, Lc::Dict, Rc::Dict)
    # extract section names
    Bs = config_sections(Bc)
    Ls = config_sections(Lc)
    Rs = config_sections(Rc)
    # expunge removed submodules from left and right sides
    deleted = Set{ByteString}()
    for section in Bs - Ls & Rs
        filter!((k,v)->!beginswith(k,"$section."),Lc)
        filter!((k,v)->!beginswith(k,"$section."),Rc)
        add!(deleted, section)
    end
    # merge the remaining config key-value pairs
    cfg = Dict()
    conflicts = Dict()
    for (key,_) in merge(Lc,Rc)
        Lv = get(Lc,key,nothing)
        Rv = get(Rc,key,nothing)
        Bv = get(Bc,key,nothing)
        if Lv == Rv || Rv == Bv
            if Lv != nothing cfg[key] = Lv end
        elseif Lv == Bv
            if Rv != nothing cfg[key] = Rv end
        else # conflict!
            conflicts[key] = [Lv,Rv]
            cfg[key] = Bv
        end
    end
    return cfg, conflicts, deleted
end

const GITHUB_REGEX = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# setup a repo's push URL intelligently

function autoconfig_pushurl()
    url = readchomp(`config remote.origin.url`)
    m = match(GITHUB_REGEX,url)
    if m != nothing
        pushurl = "git@github.com:$(m.captures[1])"
        if pushurl != url
            run(`config remote.origin.pushurl $pushurl`)
        end
    end
end

end # module
