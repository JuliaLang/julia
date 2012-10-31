load("git.jl")
load("pkgmetadata.jl")

module Pkg
#
# Julia's git-based declarative package manager
#
using Base
import Git
using Metadata

# default locations: local package repo, remote metadata repo

const DEFAULT_META = "https://github.com/JuliaLang/METADATA.jl.git"
const GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# get package repo directory

directory() = get(ENV,"JULIA_PACKAGES",string(ENV["HOME"],"/.julia"))

# create a new empty packge repository

function init(meta::String)
    dir = directory()
    run(`mkdir $dir`)
    cd(dir) do
        # create & configure
        run(`git init`)
        run(`git remote add origin .`)
        run(`git config --unset remote.origin.url`)
        run(`git config branch.master.remote origin`)
        run(`git config branch.master.merge refs/heads/master`)
        # initial content
        run(`touch REQUIRE`)
        run(`git add REQUIRE`)
        run(`git submodule add $meta METADATA`)
        run(`git commit -m"[jul] METADATA"`)
        Metadata.gen_hashes()
    end
end
init() = init(DEFAULT_META)

# get/set the origin url for package repo

origin() = cd(directory()) do
    try readchomp(`git config remote.origin.url`)
    catch
        return nothing
    end
end
origin(url::String) = cd(directory()) do
    run(`git config remote.origin.url $url`)
end

# add and remove packages by name

global add
add(pkgs::Vector{VersionSet}) = cd(directory()) do
    commit("add: $(join(sort!(map(x->x.package,pkgs)), ' '))") do
        for pkg in pkgs
            if !contains(Metadata.packages(),pkg.package)
                error("invalid package: $(pkg.package)")
            end
            reqs = parse_requires("REQUIRE")
            if anyp(req->req.package==pkg.package,reqs)
                error("package already required: $pkg")
            end
            open("REQUIRE","a") do io
                print(io,pkg.package)
                for ver in pkg.versions
                    print(io,"\t$ver")
                end
                println(io)
            end
        end
        run(`git add REQUIRE`)
        _resolve()
    end
end
function add(pkgs::Union(String,VersionSet)...)
    pkgs_ = VersionSet[]
    for pkg in pkgs
        push(pkgs_, isa(pkg,VersionSet) ? pkg : VersionSet(pkg))
    end
    add(pkgs_)
end

rm(pkgs::Vector{String}) = cd(directory()) do
    commit("rm: $(join(sort!(pkgs), ' '))") do
        for pkg in pkgs
            if !contains(Metadata.packages(),pkg)
                error("invalid package: $pkg")
            end
            reqs = parse_requires("REQUIRE")
            if !anyp(req->req.package==pkg,reqs)
                error("package not required: $pkg")
            end
            open("REQUIRE") do r
                open("REQUIRE.new","w") do w
                    for line in each_line(r)
                        fields = split(line)
                        if isempty(fields) || fields[1]!=pkg
                            print(w,line)
                        end
                    end
                end
            end
            run(`mv REQUIRE.new REQUIRE`)
        end
        run(`git add REQUIRE`)
        _resolve()
    end
end
rm(pkgs::String...) = rm(String[pkgs...])

# list available, required & installed packages

available() = cd(directory()) do
    for pkg in Metadata.each_package()
        println(pkg)
    end
end

required() = cd(directory()) do
    open("REQUIRE") do io
        for line in each_line(io)
            print(line)
        end
    end
end

installed() = cd(directory()) do
    Git.each_submodule(false) do name, path, sha1
        if name != "METADATA"
            try
                ver = Metadata.version(name,sha1)
                println("$name\tv$ver")
            catch
                println("$name\t$sha1")
            end
        end
    end
end

# update packages from requirements

function _resolve()
    reqs = parse_requires("REQUIRE")
    have = Dict{String,ASCIIString}()
    Git.each_submodule(false) do pkg, path, sha1
        if pkg != "METADATA"
            have[pkg] = sha1
            if cd(Git.attached,path) && isfile("$path/REQUIRE")
                append!(reqs,parse_requires("$path/REQUIRE"))
                if isfile("$path/VERSION")
                    ver = convert(VersionNumber,readchomp("$path/VERSION"))
                    push(reqs,VersionSet(pkg,[ver]))
                end
            end
        end
    end
    sort!(reqs)
    want = Metadata.resolve(reqs)
    pkgs = sort!(keys(merge(want,have)))
    for pkg in pkgs
        if has(have,pkg)
            if cd(Git.attached,pkg)
                # don't touch packages with attached heads
                continue
            end
            if has(want,pkg)
                if have[pkg] != want[pkg]
                    oldver = Metadata.version(pkg,have[pkg])
                    newver = Metadata.version(pkg,want[pkg])
                    up = oldver < newver ? "up" : "down"
                    println("$(up)grading $pkg v$oldver => v$newver")
                    cd(pkg) do
                        run(`git reset --soft $(want[pkg])`)
                    end
                    run(`git add -- $pkg`)
                end
            else
                ver = Metadata.version(pkg,have[pkg])
                println("removing $pkg v$ver")
                run(`git rm -qrf --cached -- $pkg`)
                Git.modules(`--remove-section submodule.$pkg`)
                run(`git add .gitmodules`)
            end
        else
            ver = Metadata.version(pkg,want[pkg])
            println("installing $pkg v$ver")
            url = readchomp("METADATA/$pkg/url")
            run(`git submodule add --reference . $url $pkg`)
            cd(pkg) do
                run(`git checkout -q --detach`)
                run(`git reset --soft $(want[pkg])`)
            end
            run(`git add -- $pkg`)
        end
    end
end
resolve() = cd(_resolve,directory())

# clone a new package repo from a URL

function clone(url::String)
    dir = directory()
    run(`git clone $url $dir`)
    cd(dir) do
        checkout("HEAD")
    end
end

# record all submodule commits as tags

function tag_submodules()
    Git.each_submodule(true) do name, path, sha1
        run(`git fetch-pack -q $path HEAD`)
        run(`git tag -f submodules/$path/$(sha1[1:10]) $sha1`)
        run(`git --git-dir=$path/.git gc -q`)
    end
end

# checkout a particular repo version

checkout(rev::String) = cd(directory()) do
    dir = cwd()
    run(`git checkout -fq $rev`)
    run(`git submodule update --init --reference $dir --recursive`)
    run(`git ls-files --other` | `xargs rm -rf`)
    Git.each_submodule(true) do name, path, sha1
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

assert_git_clean() = Git.dirty() &&
    error("The following contents must be committed:\n",
          readall(`git ls-files -d -m -s -u`))

function commit(msg::String)
    tag_submodules()
    Git.canonicalize_config(".gitmodules")
    run(`git add .gitmodules`)
    run(`git commit -m $msg`)
end

function commit(f::Function, msg::String)
    assert_git_clean()
    try f()
    catch err
        print(stderr_stream,
              "\n\n*** ERROR ENCOUNTERED ***\n\n",
              "Rolling back to HEAD...\n")
        checkout()
        throw(err)
    end
    if Git.staged() && !Git.unstaged()
        commit(msg)
    elseif !Git.dirty()
        println(stderr_stream, "Nothing to commit.")
    else
        error("There are both staged and unstaged changes to packages.")
    end
end

# push & pull package repos to/from remotes

push() = cd(directory()) do
    assert_git_clean()
    tag_submodules()
    run(`git push --tags`)
    run(`git push`)
end

pull() = cd(directory()) do
    assert_git_clean()

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
    Git.each_submodule(false) do name, path, sha1
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
            "\n\n*** WARNING ***\n\n",
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

# update system to latest and greatest

update() = cd(directory()) do
    commit("update") do
        Git.each_submodule(false) do name, path, sha1
            cd(path) do
                if Git.attached()
                    run(`git pull`)
                else
                    run(`git fetch -q --all --tags --prune --recurse-submodules=on-demand`)
                end
            end
        end
        cd("METADATA") do
            run(`git pull`)
        end
        run(`git add METADATA`)
        Metadata.gen_hashes()
        _resolve()
    end
end

end # module
