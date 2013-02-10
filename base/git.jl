module Git
#
# some utility functions for working with git repos
#

dir() = readchomp(`git rev-parse --git-dir`)
modules(args::Cmd) = readchomp(`git config -f .gitmodules $args`)
different(verA::String, verB::String, path::String) =
    !success(`git diff --quiet $verA $verB -- $path`)

dirty() = !success(`git diff --quiet HEAD`)
staged() = !success(`git diff --quiet --cached`)
unstaged() = !success(`git diff --quiet`)
dirty(paths) = !success(`git diff --quiet HEAD -- $paths`)
staged(paths) = !success(`git diff --quiet --cached -- $paths`)
unstaged(paths) = !success(`git diff --quiet -- $paths`)

attached() = success(`git symbolic-ref -q HEAD` > SpawnNullStream())
branch() = readchomp(`git rev-parse --symbolic-full-name --abbrev-ref HEAD`)
head() = readchomp(`git rev-parse HEAD`)

function each_tagged_version()
    git_dir = abspath(dir())
    @task for line in each_line(`git --git-dir=$git_dir show-ref --tags`)
        m = match(r"^([0-9a-f]{40}) refs/tags/(v\S+)$", line)
        if m != nothing && ismatch(Base.VERSION_REGEX, m.captures[2])
            produce((convert(VersionNumber,m.captures[2]),m.captures[1]))
        end
    end
end
each_tagged_version(dir::String) = cd(each_tagged_version,dir)

function each_submodule(f::Function, recursive::Bool, dir::ByteString)
    cmd = `git submodule foreach --quiet 'echo "$name $path $sha1"'`
    for line in each_line(cmd)
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
    for line in each_line(`git config -f $file --get-regexp '.*'`)
        key, val = match(r"^(\S+)\s+(.*)$", line).captures
        cfg[key] = has(cfg,key) ? [cfg[key],val] : val
    end
    return cfg
end

# TODO: provide a clean way to avoid this disaster
function read_config_blob(blob::String)
    tmp, io = mktemp()
    try
        write(io, readall(`git cat-file blob $blob`))
    finally
        close(io)
    end
    cfg = read_config(tmp)
    run(`rm -f tmp`)
    return cfg
end

function write_config(file::String, cfg::Dict)
    tmp = tempname()
    for key in sort!(keys(cfg))
        val = cfg[key]
        if isa(val,Array)
            for x in val
                run(`git config -f $tmp --add $key $x`)
            end
        else
            run(`git config -f $tmp $key $val`)
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
        filter!((k,v)->!begins_with(k,"$section."),Lc)
        filter!((k,v)->!begins_with(k,"$section."),Rc)
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
    url = readchomp(`git config remote.origin.url`)
    m = match(GITHUB_REGEX,url)
    if m != nothing
        pushurl = "git@github.com:$(m.captures[1])"
        if pushurl != url
            run(`git config remote.origin.pushurl $pushurl`)
        end
    end
end

end # module
