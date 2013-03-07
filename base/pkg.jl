module Pkg
#
# Julia's git-based declarative package manager
#

include("pkg/metadata.jl")
include("pkg/resolve.jl")

using Metadata
using Resolve

import Git

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl.git"

# some utility functions

@unix_only dir() = abspath(get(ENV,"JULIA_PKGDIR",joinpath(ENV["HOME"],".julia")))
@windows_only begin
    dir() = abspath(get(ENV,"JULIA_PKGDIR",joinpath(ENV["HOME"],"packages")))
end
dir(pkg::String...) = joinpath(dir(),pkg...)

function cd_pkgdir(f::Function)
    d = dir()
    if !isdir(d)
        if has(ENV,"JULIA_PKGDIR")
            error("Package directory $d doesn't exist; run Pkg.init() to create it.")
        else
            info("Auto-initializing default package repository $d.")
            init()
        end
    end
    cd(f,d)
end

function packages(reqs::Vector{VersionSet})
    pkgs = sort!(map(r->r.package, reqs))
    for pkg in pkgs
        isdir(pkg) || continue
        head = cd(Git.head,pkg)
        ver = Metadata.version(pkg,head)
        meta = isa(ver,VersionNumber) && cd(pkg) do
            !Git.attached() && success(`git diff --quiet HEAD -- REQUIRE`)
        end
        reqfile = meta ? "METADATA/$pkg/versions/$ver/requires" : "$pkg/REQUIRE"
        # TODO: I thought we had a union! function to do this...
        #       this is just union!(pkgs, map(r->r.package, parse_requires(reqfile)))
        isfile(reqfile) || continue
        for r in sort!(parse_requires(reqfile))
            if !contains(pkgs,r.package) && isdir(r.package)
                push!(pkgs,r.package)
            end
        end
    end
    return pkgs
end
packages() = packages(parse_requires("REQUIRE"))

function print_pkg_status(io::IO, pkg::String)
    if !isdir(pkg)
      error("Package repository $pkg doesn't exist")
    end
    cd(pkg) do
        head = Git.head()
        ver = Git.attached() ? Git.branch() : cd("..") do
            Metadata.version(pkg,head)
        end
        dirty = Git.dirty() ? " (dirty)" : ""
        println(io,"$(rpad(pkg,16)) $ver$dirty")
    end
end

# show the status packages in the repo
status() = status(OUTPUT_STREAM)
status(io::IO) = cd_pkgdir() do
    for pkg in packages()
        print_pkg_status(io, pkg)
    end
end
status(io::IO, pkg::String) = print_pkg_status(io, pkg)
status(pkg::String) = status(OUTPUT_STREAM, pkg)

# create a new empty packge repository

function init(meta::String)
    d = dir()
    isdir(d) && error("Package directory $d already exists.")
    try
        run(`mkdir -p $d`)
        cd(d) do
            # create & configure
            promptuserinfo()
            run(`git init`)
            run(`git commit --allow-empty -m "Initial empty commit"`)
            run(`git remote add origin .`)
        if success(`git config --global github.user` > SpawnNullStream())
                base = basename(d)
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
            run(`git commit -m "Empty package repo"`)
            cd(Git.autoconfig_pushurl,"METADATA")
            Metadata.gen_hashes()
        end
    catch e
        run(`rm -rf $d`)
        rethrow(e)
    end
end
init() = init(DEFAULT_META)

# get/set the origin url for package repo

origin() = cd_pkgdir() do
    try readchomp(`git config remote.origin.url`)
    catch
        return nothing
    end
end
origin(url::String) = cd_pkgdir() do
    run(`git config remote.origin.url $url`)
end

# add and remove packages by name

add(pkgs::Vector{VersionSet}) = cd_pkgdir() do
    run(`git add REQUIRE`)
    try
        for pkg in pkgs
            if !contains(Metadata.packages(),pkg.package)
                error("Unknown package $(pkg.package); Perhaps you need to Pkg.update() for new metadata?")
            end
            reqs = parse_requires("REQUIRE")
            if any(req->req.package==pkg.package,reqs)
                warn("You've already required $pkg, ignoring.")
                return
            end
            open("REQUIRE","a") do io
                print(io,pkg.package)
                for ver in pkg.versions
                    print(io,"\t$ver")
                end
                println(io)
            end
        end
        _resolve()
    catch
        run(`git checkout -- REQUIRE`)
        rethrow()
    end
    run(`git add REQUIRE`)
end
function add(pkgs::Union(String,VersionSet)...)
    pkgs_ = VersionSet[]
    for pkg in pkgs
        Base.push!(pkgs_, isa(pkg,VersionSet) ? pkg : VersionSet(pkg))
    end
    add(pkgs_)
end

rm(pkgs::Vector{String}) = cd_pkgdir() do
    run(`git add REQUIRE`)
    try
        for pkg in pkgs
            if !contains(Metadata.packages(),pkg)
                error("Invalid package: $pkg")
            end
            reqs = parse_requires("REQUIRE")
            if !any(req->req.package==pkg,reqs)
                error("Package not required: $pkg")
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
        _resolve()
    catch
        run(`git checkout -- REQUIRE`)
        rethrow()
    end
    run(`git add REQUIRE`)
end
rm(pkgs::String...) = rm(String[pkgs...])

# list available, required & installed packages

available() = cd_pkgdir() do
    [Metadata.each_package()...]
end

required() = cd_pkgdir() do
    parse_requires("REQUIRE")
end
required(pkg::String) = cd_pkgdir() do
    req = required()
    for vset in req
        if isequal(vset.package, pkg)
            return vset.versions
        end
    end
    return nothing
end

installed() = cd_pkgdir() do
    h = Dict{String,Union(VersionNumber,String)}()
    for pkg in packages()
        isdir(pkg) || continue
        sha1 = cd(Git.head,pkg)
        h[pkg] = Metadata.version(pkg,sha1)
    end
    return h
end
installed(pkg::String) = cd_pkgdir() do
    get(installed(), pkg, nothing)
end

function runbuildscript(pkg)
    d = dir(pkg)
    path = joinpath(d, "deps")
    if isdir(path)
        cd(path) do
            if isfile("build.jl")
                info("Running build script for package $pkg")
                include("build.jl")
            end
        end
    end
end

# update packages from requirements

function _resolve()
    have = (String=>ASCIIString)[]
    reqs = parse_requires("REQUIRE")
    fixed = (String=>VersionNumber)["julia"=>VERSION]
    index = readchomp(`git write-tree`)
    for pkg in packages(parse_requires(`git cat-file blob $index:REQUIRE`))
        isdir(pkg) || continue
        have[pkg] = cd(Git.head,pkg)
        if cd(Git.attached,pkg) && isfile("$pkg/REQUIRE")
            if isfile("$pkg/VERSION")
                fixed[pkg] = convert(VersionNumber,readchomp("$pkg/VERSION"))
            end
            append!(reqs,parse_requires("$pkg/REQUIRE"))
        end
    end
    sort!(reqs)
    want = Resolve.resolve(reqs,fixed)
    pkgs = sort!(keys(merge(want,have)))
    for pkg in pkgs
        if has(have,pkg)
            managed = cd(pkg) do
                !Git.dirty() && !Git.attached()
            end
            if !managed continue end
            if has(want,pkg)
                if have[pkg] != want[pkg]
                    oldver = Metadata.version(pkg,have[pkg])
                    newver = Metadata.version(pkg,want[pkg])
                    up = oldver <= newver ? "Up" : "Down"
                    info("$(up)grading $pkg: v$oldver => v$newver")
                    cd(pkg) do
                        run(`git checkout -q $(want[pkg])`)
                    end
                    run(`git add -- $pkg`)
                    runbuildscript(pkg)
                end
            else
                ver = Metadata.version(pkg,have[pkg])
                info("Removing $pkg v$ver")
                run(`git rm -qrf --cached -- $pkg`)
                Git.modules(`--remove-section submodule.$pkg`)
                run(`git add .gitmodules`)
                run(`rm -rf -- $pkg`)
            end
        else
            ver = Metadata.version(pkg,want[pkg])
            info("Installing $pkg v$ver")
            if ispath(pkg)
                # TODO: maybe if this is a git repo or submodule, just take it over?
                error("Path $pkg already exists! Please remove to allow installation.")
            end
            url = Metadata.pkg_url(pkg)
            run(`git submodule add --reference . $url $pkg`)
            cd(pkg) do
                try run(`git checkout -q $(want[pkg])` .> SpawnNullStream())
                catch
                    run(`git fetch -q`)
                    try run(`git checkout -q $(want[pkg])`)
                    catch
                        error("An invalid SHA1 hash seems to be registered for $pkg. Please contact the package maintainer.")
                    end
                end
                Git.autoconfig_pushurl()
            end
            run(`git add -- $pkg`)
            runbuildscript(pkg)
        end
    end
end
resolve() = cd(_resolve,dir())

# clone a new package repo from a URL

# TODO: this is horribly broken
function clone(url::String)
    d = dir()
    if isdir(d)
        error("Package directory $d already exists.")
    end
    tmpdir = mktempdir()
    run(`git clone $url $tmpdir`)
    cd(tmpdir) do
        gitdir = abspath(readchomp(`git rev-parse --git-dir`))
        for pkg in packages()
            cd(pkg) do
                sha1 = Git.head()
                run(`git fetch-pack $gitdir $sha1`)
            end
        end
    end
    run(`mv $tmpdir $d`)
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

checkout(rev::String) = cd_pkgdir() do
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
    catch
        warn("""

             *** ERROR ENCOUNTERED ***

             Rolling back to HEAD...

             """)
        checkout()
        rethrow()
    end
    if Git.staged() && !Git.unstaged()
        commit(msg)
        run(`git diff --name-only --diff-filter=D HEAD^ HEAD` | `xargs rm -rf`)
        checkout()
    elseif !Git.dirty()
        warn("nothing to commit.")
    else
        error("There are both staged and unstaged changes to packages.")
    end
end

# set package remote in METADATA

get_origin(pkg::String, remote::String) = cd_pkgdir() do
    for line in each_line(`git --git-dir=$(joinpath(pkg,".git")) remote -v`)
        m = match(r"^(\S*)\s*(\S*)\s*\(fetch\)", line)
        if m != nothing && m.captures[1] == remote
            return m.captures[2]
        end
    end
    error("The git remote '", remote, "' is not present in the configuration file")
end
get_origin(pkg::String) = get_origin(pkg, "origin")

set_origin(pkg::String, url::String) = cd_pkgdir() do
    cd(joinpath("METADATA", pkg)) do
        open("url", "w") do io
            println(io, url)
        end
    end
end

pkg_origin(pkg::String, remote::String) = set_origin(pkg, get_origin(pkg, remote))
pkg_origin(pkg::String) = pkg_origin(pkg, "origin")

# push & pull package repos to/from remotes

push() = cd_pkgdir() do
    assert_git_clean()
    tag_submodules()
    run(`git push --tags`)
    run(`git push`)
end

pull() = cd_pkgdir() do
    assert_git_clean()

    # get remote data
    run(`git fetch --tags`)
    run(`git fetch`)

    # see how far git gets with merging
    success(`git merge -m "[jul] pull (simple merge)" FETCH_HEAD`) && return

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
            Cc[key] = vals
            warn("""

                 Modules config conflict for $key:
                   local value  = $(vals[1])
                   remote value = $(vals[2])

                 Both values written to .gitmodules -- please edit and choose one.

                 """)
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
        warn("""

             *** WARNING ***

             You have unresolved merge conflicts in the following files:

             $unmerged

             Please resolve these conflicts, `git add` the files, and commit.

             """)
        error("pull: merge conflicts")
    end

    # try to commit -- fails if unresolved conflicts remain
    run(`git commit -m "[jul] pull (complex merge)"`)
    checkout("HEAD")
    tag_submodules()
end

# update system to latest and greatest

update() = cd_pkgdir() do
    cd("METADATA") do
        run(`git pull`)
    end
    Metadata.gen_hashes()
    run(`git add METADATA`)
    # TODO: handle package deletions
    for pkg in packages()
        url = Metadata.pkg_url(pkg)
        if url != nothing
            Git.modules(`submodule.$pkg.url $url`)
            cd(pkg) do
                if !Git.dirty()
                    if Git.attached()
                        run(ignorestatus(`git pull --ff-only`))
                    else
                        run(`git config remote.origin.url $url`)
                        run(`git fetch -q`)
                    end
                end
            end
        end
    end
    run(`git add .gitmodules`)
    run(`git submodule sync -q`)
    _resolve()
end

latest_version(pkg::String) = cd_pkgdir() do
    vers = VersionNumber[]
    for (ver, _) in Metadata.each_tagged_version(pkg)
        Base.push!(vers, ver)
    end
    max(vers)
end

version(pkg::String, ver::VersionNumber) = cd_pkgdir() do
    if any(pkg .== Metadata.packages())
        if ver != v"0.0.0" && ver <= latest_version(pkg)
            error("The latest version of package $(pkg) is $(string(ver)). You must specify a later version.")
        end
    else
        if !isdir(joinpath("METADATA", pkg))
            mkdir(joinpath("METADATA", pkg))
        end
    end
    sha1 = ""
    cd(pkg) do
        sha1 = readchomp(`git rev-parse HEAD`)
        if ver > v"0.0.0"
            run(`git tag $(string(ver)) HEAD`)
        end
    end
    cd(joinpath("METADATA", pkg)) do
        if !isdir("hashes") mkdir("hashes") end
        if !isdir("versions") mkdir("versions") end
        cd("versions") do
            if !isdir(string(ver)) mkdir(string(ver)) end
            open(joinpath(string(ver), "sha1"), "w") do io
                println(io, sha1)
            end
        end
    end
    if isfile(joinpath(pkg, "REQUIRE"))
        cp(
            joinpath(pkg, "REQUIRE"),
            joinpath("METADATA", pkg, "versions", string(ver), "requires"))
    end
    Metadata.gen_hashes(pkg)
end

function patch(pkg)
    lver = latest_version(pkg)
    if lver > v"0.0.0"
        version(pkg, VersionNumber(lver.major, lver.minor, lver.patch+1))
    else
        version(pkg, v"0.0.0")
    end
end
function minor(pkg)
    lver = latest_version(pkg)
    version(pkg, VersionNumber(lver.major, lver.minor+1))
end
function major(pkg)
    lver = latest_version(pkg)
    version(pkg, VersionNumber(lver.major+1))
end

function promptuserinfo()
    if(isempty(chomp(readall(ignorestatus(`git config --global user.name`)))))
        info("""
             Git would like to know your name to initialize your .julia directory.
             Enter it below:
             """)
        name = chomp(readline(STDIN))
        if(isempty(name))
            error("Could not read name")
        else
            run(`git config --global user.name $name`)
            info("Thank you. You can change it using run(`git config --global user.name NAME`)")
        end
    end
    if(isempty(chomp(readall(ignorestatus(`git config --global user.email`)))))
        info("""
             Git would like to know your email to initialize your .julia directory.
             Enter it below:
             """)
        email = chomp(readline(STDIN))
        if(isempty(email))
            error("Could not read email")
        else
            run(`git config --global user.email $email`)
            info("Thank you. You can change it using run(`git config --global user.email EMAIL`)")
        end
    end
end

function new(pkg::String)
    newpath = joinpath(dir(), pkg)
    cd_pkgdir() do
        if isdir(pkg)
            # This is an existing package that we assume is ready to go
            version(pkg, v"0.0.0")
            try
                pkg_origin(pkg, "origin")
            catch
                error("""
                      Your package in

                        $newpath

                      is almost ready. But the default remote, "origin", does not exist in
                      this repository's configuration. To finish the process, run

                        > Pkg.pkg_origin("$pkg", remotename)

                      with the correct remote name for your repository.
                      """)
            end
        else
            # Create a skeleton package that can be easily filled in
            try
                mkdir(pkg)
            catch
                error("Unable to create directory for new package: $pkg")
            end
            try
                sha1 = ""
                cd(pkg) do
                    promptuserinfo()
                    run(`git init`)
                    run(`git commit --allow-empty -m "Initial empty commit"`)
                    touch("LICENSE.md") # Should insert MIT content
                    touch("README.md")
                    touch("REQUIRE")
                    mkdir("src")
                    touch(joinpath("src", string(pkg, ".jl")))
                    mkdir("test")
                    run(`git add --all`)
                    run(`git commit -m "Scaffold for Julia package $pkg"`)
                    sha1 = readchomp(`git rev-parse HEAD`)
                end
                version(pkg, v"0.0.0")
            catch
                error("Unable to initialize contents of new package")
            end
            println("""
                    You have created a new package in

                      $newpath

                    When the package is ready to submit, push it to a public repository, set it as
                    the remote "origin", then run:

                      > Pkg.pkg_origin("$pkg")
                      > Pkg.patch("$pkg")

                    to prepare METADATA with the details for your package.
                    """)
        end
    end
end

# Remove local traces of a package (broken due to a bad .new(), for instance)
obliterate(pkg::String) = cd_pkgdir() do
    run(`rm -rf $pkg $(joinpath("METADATA", pkg))`)
end

# Repository sanity check
check_repository() = cd_pkgdir() do
    try
        Resolve.sanity_check()
    catch err
        if !isa(err, Resolve.MetadataError)
            rethrow(err)
        end
        warning = "Packages with unsatisfiable requirements found:"
        for (v, pp) in err.info
            warning *= "  $(v.package) v$(v.version) : no valid versions exist for package $pp"
        end
        warn(warning)
        return false
    end
    return true
end

end # module
