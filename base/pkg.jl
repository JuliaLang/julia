load("$JULIA_HOME/../../base/git.jl")
load("$JULIA_HOME/../../base/pkgmetadata.jl")

# module Pkg
#
# Julia's git-based declarative package manager
#
import Base.*
import Git
import Metadata
import Metadata.*

# default locations: local package repo, remote metadata repo

const DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const DEFAULT_META = "file:///Users/stefan/projects/pkg/METADATA.git"
const GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# create a new empty packge repository

function init(dir::String, meta::String)
    run(`mkdir $dir`)
    cd(dir) do
        run(`git init`)
        run(`git submodule add $meta METADATA`)
        run(`git commit -m"[jul] METADATA"`)
    end
end
init(dir::String) = init(dir, DEFAULT_META)
init() = init(DEFAULT_DIR)

# list required packages

list() = run(`cat REQUIRES`)

# install & remove packages by name

function install(pkgs::String...)
    for pkg in pkgs
        if !contains(Metadata.packages(),pkg)
            error("invalid package: $pkg")
        end
        reqs = parse_requires("REQUIRES")
        if anyp(req->req.package==pkg,reqs)
            error("package already required: $pkg")
        end
        open("REQUIRES","a") do io
            println(io,pkg)
        end
    end
    run(`git add REQUIRES`)
    resolve()
end

function remove(pkgs::String...)
    for pkg in pkgs
        if !contains(Metadata.packages(),pkg)
            error("invalid package: $pkg")
        end
        reqs = parse_requires("REQUIRES")
        if !anyp(req->req.package==pkg,reqs)
            error("package not required: $pkg")
        end
        open("REQUIRES") do r
            open("REQUIRES.new","w") do w
                for line in each_line(r)
                    fields = split(line)
                    if isempty(fields) || fields[1]!=pkg
                        print(w,line)
                    end
                end
            end
        end
        run(`mv REQUIRES.new REQUIRES`)
    end
    run(`git add REQUIRES`)
    resolve()
end

# dealing with version metadata

# update packages from requirements

function resolve()
    vers = Metadata.resolve(parse_requires("REQUIRES"))
    # TODO
end

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
