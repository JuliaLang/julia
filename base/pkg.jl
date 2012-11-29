require("git")
require("pkgmetadata")

module Pkg
#
# Julia's git-based declarative package manager
#

using Metadata

import Git

# default locations: local package repo, remote metadata repo

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl.git"

# create a new empty packge repository

function init(meta::String)
    dir = julia_pkgdir()
    run(`mkdir $dir`)
    cd(dir) do
        # create & configure
        run(`git init`)
        run(`git remote add origin .`)
        if success(`git config --global github.user` > "/dev/null")
            base = basename(julia_pkgdir())
            user = readchomp(`git config --global github.user`)
            run(`git config remote.origin.url git@github.com:$user/$base`)
        else
            run(`git config --unset remote.origin.url`)
        end
        run(`git config branch.master.remote origin`)
        run(`git config branch.master.merge refs/heads/master`)
        # initial content
        run(`touch REQUIRE`)
        run(`git add REQUIRE`)
        run(`git submodule add $meta METADATA`)
        run(`git commit -m "empty package repo"`)
        cd(Git.autoconfig_pushurl,"METADATA")
        Metadata.gen_hashes()
    end
end
init() = init(DEFAULT_META)

# get/set the origin url for package repo

origin() = cd(julia_pkgdir()) do
    try readchomp(`git config remote.origin.url`)
    catch
        return nothing
    end
end
origin(url::String) = cd(julia_pkgdir()) do
    run(`git config remote.origin.url $url`)
end

# add and remove packages by name

add(pkgs::Vector{VersionSet}) = cd(julia_pkgdir()) do
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
function add(pkgs::Union(String,VersionSet)...)
    pkgs_ = VersionSet[]
    for pkg in pkgs
        Base.push(pkgs_, isa(pkg,VersionSet) ? pkg : VersionSet(pkg))
    end
    add(pkgs_)
end

rm(pkgs::Vector{String}) = cd(julia_pkgdir()) do
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
rm(pkgs::String...) = rm(String[pkgs...])

# list available, required & installed packages

available() = cd(julia_pkgdir()) do
    [Metadata.each_package()...]
end

required() = cd(julia_pkgdir()) do
    parse_requires("REQUIRE")
end

installed() = cd(julia_pkgdir()) do
    h = Dict{String,Union(VersionNumber,String)}()
    Git.each_submodule(false) do name, path, sha1
        if name != "METADATA"
            try
                h[name] = Metadata.version(name,sha1)
            catch
                h[name] = sha1
            end
        end
    end
    return h
end

# update packages from requirements

function _resolve()
    reqs = parse_requires("REQUIRE")
    have = (String=>ASCIIString)[]
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
                    up = oldver <= newver ? "Up" : "Down"
                    println("$(up)grading $pkg: v$oldver => v$newver")
                    cd(pkg) do
                        run(`git checkout -q $(want[pkg])`)
                    end
                    run(`git add -- $pkg`)
                end
            else
                ver = Metadata.version(pkg,have[pkg])
                println("Removing $pkg v$ver")
                run(`git rm -qrf --cached -- $pkg`)
                Git.modules(`--remove-section submodule.$pkg`)
                run(`git add .gitmodules`)
                run(`rm -rf -- $pkg`)
            end
        else
            ver = Metadata.version(pkg,want[pkg])
            println("Installing $pkg: v$ver")
            url = Metadata.pkg_url(pkg)
            run(`git submodule add --reference . $url $pkg`)
            cd(pkg) do
                Git.autoconfig_pushurl()
                run(`git checkout -q $(want[pkg])`)
            end
            run(`git add -- $pkg`)
        end
    end
end
resolve() = cd(_resolve,julia_pkgdir())

# clone a new package repo from a URL

function clone(url::String)
    dir = julia_pkgdir()
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

checkout(rev::String) = cd(julia_pkgdir()) do
    run(`git checkout -fq $rev -- REQUIRE`)
    _resolve()
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
        run(`git diff --name-only --diff-filter=D HEAD^ HEAD` | `xargs rm -rf`)
        checkout()
    elseif !Git.dirty()
        println(stderr_stream, "Nothing to commit.")
    else
        error("There are both staged and unstaged changes to packages.")
    end
end

# push & pull package repos to/from remotes

push() = cd(julia_pkgdir()) do
    assert_git_clean()
    tag_submodules()
    run(`git push --tags`)
    run(`git push`)
end

pull() = cd(julia_pkgdir()) do
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

update() = cd(julia_pkgdir()) do
    Git.each_submodule(false) do name, path, sha1
        cd(path) do
            if Git.attached()
                run(`git pull`)
            else
                run(`git fetch -q --all --tags --prune --recurse-submodules`)
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

# create a new package repo (unregistered)

create(name::String) = cd(julia_pkgdir()) do
    run(`mkdir -p $name`)
    cd(name) do
        run(`git init`)
        run(`git commit --allow-empty -m "initial empty commit"`)
        run(`touch README.md`)
        run(`mkdir src`)
    end
end

# Create a skeleton package that can be easily filled in
function skeleton(package_name::String)
    try
        mkdir(package_name)
    catch
        error("Unable to create directory for new package: $(package_name)")
    end
    try
        cd(package_name) do
            file_create("LICENSE.md") # Should insert MIT content
            file_create("README.md")
            file_create("REQUIRE")
            mkdir("src")
            file_create(file_path("src", strcat(package_name, ".jl")))
            mkdir("test")
            file_create(file_path("test", strcat("01", ".jl")))
        end
    catch
        error("Unable to initialize contents of new package")
    end
end

skeleton() = skeleton("Example")

# If a package contains data, make it easy to find its location
function package_directory(package_name::String)
  if has(ENV, "JULIA_PKGDIR")
    return file_path(ENV["JULIA_PKGDIR"], package_name)
  else
    return path_expand(file_path("~/.julia", package_name))
  end
end

end # module
