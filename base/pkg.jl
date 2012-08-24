load("$JULIA_HOME/../../base/git.jl")
load("$JULIA_HOME/../../extras/linprog.jl")

# module Pkg
#
# Julia's git-based declarative package manager
#
import Base.*
import Git

# default locations: local package repo, remote metadata repo

const DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const DEFAULT_META = "file:///Users/stefan/projects/pkg/METADATA"
const GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# generate versions metadata

function gen_versions(pkg::String)
    for (ver,sha1) in Git.each_version(pkg)
        dir = "METADATA/$pkg/versions/$ver"
        run(`mkdir -p $dir`)
        open("$dir/sha1","w") do io
            println(io,sha1)
        end
    end
end

each_package() = @task begin
    for line in each_line(`git --git-dir=METADATA/.git ls-tree HEAD`)
        m = match(r"\d{6} tree [0-9a-f]{40}\t(\S+)$", line)
        if m != nothing && isdir("METADATA/$(m.captures[1])/versions")
            produce(m.captures[1])
        end
    end
end

each_version(pkg::String) = @task begin
    for line in each_line(`git --git-dir=METADATA/.git ls-tree HEAD:$pkg/versions`)
        m = match(r"\d{6} tree [0-9a-f]{40}\t(\d\S*)$", line)
        if m != nothing && ismatch(Base.VERSION_REGEX,m.captures[1])
            ver = convert(VersionNumber,m.captures[1])
            dir = "METADATA/$pkg/versions/$(m.captures[1])"
            if isfile("$dir/sha1")
                produce((ver,dir))
            end
        end
    end
end

function packages()
    pkgs = String[]
    for pkg in each_package()
        push(pkgs,pkg)
    end
    sort!(pkgs)
end

type Version
    package::ByteString
    version::VersionNumber
end
isequal(a::Version, b::Version) =
    a.package == b.package && a.version == b.version
function isless(a::Version, b::Version)
    (a.package < b.package) && return true
    (a.package > b.package) && return false
    return a.version < b.version
end

function versions(pkgs)
    vers = Version[]
    for pkg in pkgs
        for (ver,dir) in each_version(pkg)
            push(vers,Version(pkg,ver))
        end
    end
    sort!(vers)
end

type VersionSet
    package::ByteString
    versions::Vector{VersionNumber}

    function VersionSet(pkg::ByteString, vers::Vector{VersionNumber})
        if !issorted(vers)
            error("version numbers must be sorted")
        end
        new(pkg,vers)
    end
end
VersionSet(pkg::ByteString) = VersionSet(pkg, VersionNumber[])
isless(a::VersionSet, b::VersionSet) = a.package < b.package

function contains(s::VersionSet, v::Version)
    (s.package != v.package) && return false
    for i in length(s.versions):-1:1
        (v.version >= s.versions[i]) && return isodd(i)
    end
    return isempty(s.versions)
end

function parse_requires(file::String)
    reqs = Dict{String,Vector{VersionNumber}}()
    open(file) do io
        for line in each_line(io)
            if ismatch(r"^\s*(?:#|$)", line) continue end
            line = replace(line, r"#.*$", "")
            fields = split(line)
            pkg = shift(fields)
            if has(reqs,pkg)
                error("duplicate requires entry for $pkg in $file")
            end
            vers = map(x->convert(VersionNumber,x),fields)
            if !issorted(vers)
                error("invalid requires entry for $pkg in $file: $vers")
            end
            reqs[pkg] = vers
        end
    end
    [ VersionSet(pkg,reqs[pkg]) for pkg in sort!(keys(reqs)) ]
end

function dependencies(pkgs,vers)
    deps = Array((Version,VersionSet),0)
    for pkg in each_package()
        for (ver,dir) in each_version(pkg)
            v = Version(pkg,ver)
            file = "$dir/requires"
            if isfile(file)
                for d in parse_requires("$dir/requires")
                    push(deps,(v,d))
                end
            end
        end
    end
    sort!(deps)
end

older(a::Version, b::Version) = a.package == b.package && a.version < b.version

function solve(reqs::Vector{VersionSet})
    pkgs = packages()
    vers = versions(pkgs)
    deps = dependencies(pkgs,vers)

    n = length(vers)
    z = zeros(Int,n)
    u = ones(Int,n)

    G  = [ v == d[1]        ? 1 : 0  for v=vers, d=deps ]
    G *= [ contains(d[2],v) ? 1 : 0  for d=deps, v=vers ]
    G += [ older(a,b)       ? 2 : 0  for a=vers, b=vers ]
    I = find(G)
    W = zeros(Int,length(I),n)
    for (i,r) in enumerate(I)
        W[r,rem(i-1,21)+1] = -1
        W[r,div(i-1,21)+1] = G[i]
    end
    w = iround(linprog_simplex(u,W,zeros(Int,length(I)),nothing,nothing,u,nothing)[2])

    V = [ p == v.package ? 1 : 0                     for p=pkgs, v=vers ]
    R = [ contains(r,v) ? -1 : 0                     for r=reqs, v=vers ]
    D = [ d[1] == v ? 1 : contains(d[2],v) ? -1 : 0  for d=deps, v=vers ]
    b = [  ones(Int,length(pkgs))
          -ones(Int,length(reqs))
          zeros(Int,length(deps)) ]

    x = linprog_simplex(w,[V;R;D],b,nothing,nothing,z,u)[2]
    vers[x .> 0.5]
end
solve() = Version[]
solve(want::VersionSet...) = solve([want...])
solve(want::String...) = solve([ VersionSet(x) for x in want ])

# create a new empty packge repository

function init(dir::String, meta::String)
    run(`mkdir $dir`)
    cd(dir) do
        run(`git init`)
        run(`git commit --allow-empty -m"[jul] empty package repo"`)
        install({"METADATA" => meta})
    end
end
init(dir::String) = init(dir, DEFAULT_META)
init() = init(DEFAULT_DIR)

# clone a new package repo from a URL

function clone(dir::String, url::String)
    run(`git clone $url $dir`)
    cd(dir) do
        checkout("HEAD")
    end
end
clone(url::String) = clone(DEFAULT_DIR, url)

# record all submodule commits as tags

function tag_submodules()
    Git.in_each_submodule(true) do name, path, sha1
        run(`git fetch-pack -q $path HEAD`)
        run(`git tag -f submodules/$path/$(sha1[1:10]) $sha1`)
        run(`git --git-dir=$path/.git gc -q`)
    end
end

# checkout a particular repo version

function checkout(rev::String)
    dir = cwd()
    run(`git checkout -fq $rev`)
    run(`git submodule update --init --reference $dir --recursive`)
    run(`git ls-files --other` | `xargs rm -rf`)
    Git.in_each_submodule(true) do name, path, sha1
        branch = try Git.modules(`submodule.$name.branch`) end
        if branch != nothing
            cd(path) do
                run(`git checkout -B $branch $sha1`)
            end
        end
    end
end
checkout() = checkout("HEAD")

# commit the current state of the repo with the given message

function commit(msg::String)
    tag_submodules()
    Git.canonicalize_config(".gitmodules")
    run(`git add .gitmodules`)
    run(`git commit -m $msg`)
end

# install packages by name and, optionally, git url

function install(urls::Associative)
    names = sort!(keys(urls))
    if isempty(names) return end
    dir = cwd()
    for pkg in names
        url = urls[pkg]
        run(`git submodule add --reference $dir $url $pkg`)
    end
    commit("[jul] install "*join(names, ", "))
    checkout()
end
function install(names::AbstractVector)
    urls = Dict()
    for pkg in names
        urls[pkg] = readchomp("METADATA/$pkg/url")
    end
    install(urls)
end
install(names::String...) = install([names...])

# remove packages by name

function remove(names::AbstractVector)
    if isempty(names) return end
    sort!(names)
    for pkg in names
        run(`git rm --cached -- $pkg`)
        Git.modules(`--remove-section submodule.$pkg`)
    end
    run(`git add .gitmodules`)
    commit("[jul] remove "*join(names, ", "))
    run(`rm -rf $names`)
end
remove(names::String...) = remove([names...])

# push & pull package repos to/from remotes

function push()
    tag_submodules()
    run(`git push --tags`)
    run(`git push`)
end

function pull()
    # get remote data
    run(`git fetch --tags`)
    run(`git fetch`)

    # see how far git gets with merging
    if success(`git merge -m "[jul] pull (simple merge)" FETCH_HEAD`) return end

    # get info about local, remote and base trees
    L = readchomp(`git rev-parse --verify HEAD`)
    R = readchomp(`git rev-parse --verify FETCH_HEAD`)
    B = readchomp(`git merge-base $L $R`)
    Rc = Git.read_config_blob("$R:.gitmodules")
    Rs = Git.config_sections(Rc)

    # intelligently 3-way merge .gitmodules versions
    if Git.different(L,R,".gitmodules")
        Bc = Git.read_config_blob("$B:.gitmodules")
        Lc = Git.read_config_blob("$L:.gitmodules")
        Cc, conflicts, deleted = Git.merge_configs(Bc,Lc,Rc)
        # warn about config conflicts
        for (key,vals) in conflicts
            print(stderr_stream,
                "\nModules config conflict for $key:\n",
                "  local value  = $(vals[1])\n",
                "  remote value = $(vals[2])\n",
                "\n",
                "Both values written to .gitmodules -- please edit and choose one.\n\n",
            )
            Cc[key] = vals
        end
        # remove submodules that were deleted
        for section in deleted
            if !begins_with(section,"submodule.") continue end
            path = get(Lc,"$section.path",nothing)
            if path == nothing continue end
            run(`git rm -qrf --cached --ignore-unmatch -- $path`)
            run(`rm -rf $path`)
        end
        # write the result (unconditionally) and stage it (if no conflicts)
        Git.write_config(".gitmodules", Cc)
        if isempty(conflicts) run(`git add .gitmodules`) end
    end

    # merge submodules
    Git.in_each_submodule(false) do name, path, sha1
        if has(Rs,"submodule.$name") && Git.different(L,R,path)
            alt = readchomp(`git rev-parse $R:$path`)
            cd(path) do
                run(`git merge --no-edit $alt`)
            end
            run(`git add -- $path`)
        end
    end

    # check for remaining merge conflicts
    if Git.unstaged()
        unmerged = readall(`git ls-files -m` | `sort` | `uniq`)
        unmerged = replace(unmerged, r"^", "    ")
        print(stderr_stream,
            "\n*** WARNING ***\n\n",
            "You have unresolved merge conflicts in the following files:\n\n",
            unmerged,
            "\nPlease resolve these conflicts, `git add` the files, and commit.\n"
        )
        error("pull: merge conflicts")
    end

    # try to commit -- fails if unresolved conflicts remain
    run(`git commit -m "[jul] pull (complex merge)"`)
    checkout("HEAD")
    tag_submodules()
end

# end # module
