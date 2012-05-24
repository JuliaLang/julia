# default locations: local package repo, remote metadata repo

const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"
const PKG_GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# some utility functions for working with git repos

git_dir() = chomp(readall(`git rev-parse --git-dir`))
git_dirty() = !success(`git diff --quiet`)
git_staged() = !success(`git diff --cached --quiet`)

git_modules(args::Cmd) = chomp(readall(`git config -f .gitmodules $args`))

function git_each_submodule(f::Function, recursive::Bool, dir::ByteString)
    cmd = `git submodule foreach --quiet 'echo "$name\t$path\t$sha1"'`
    for line in each_line(cmd)
        name, path, sha1 = match(r"^(.*)\t(.*)\t([0-9a-f]{40})$", line).captures
        @cd dir f(name, path, sha1)
        if recursive
            @cd path git_each_submodule((n,p,s)->(@cd dir f(n,"$path/$p",s)), true, dir)
        end
    end
end
git_each_submodule(f::Function, r::Bool) = git_each_submodule(f, r, cwd())

function git_canonicalize_config(file::String)
    dict = Dict{ByteString,ByteString}()
    # TODO: use --null option for better handling of weird values.
    for line in each_line(`git config -f $file --get-regexp '.*'`)
        key, val = match(r"^(\S+)\s+(.*)$", line).captures
        dict[key] = val
    end
    tmp = cstring(ccall(:tmpnam, Ptr{Uint8}, (Ptr{Uint8},), C_NULL))
    for key in sort!(keys(dict))
        run(`git config -f $tmp $key $(dict[key])`)
    end
    run(`mv -f $tmp $file`)
end

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    @cd dir begin
        run(`git init`)
        run(`git commit --allow-empty -m "[jul] empty package repo"`)
        pkg_install({"METADATA" => meta})
    end
end
pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init() = pkg_init(PKG_DEFAULT_DIR)

# clone a new package repo from a URL

function pkg_clone(dir::String, url::String)
    run(`git clone $url $dir`)
    @cd dir pkg_checkout("HEAD")
end
pkg_clone(url::String) = pkg_clone(PKG_DEFAULT_DIR, url)

# checkpoint a repo, recording submodule commits as tags

function pkg_checkpoint()
    git_each_submodule((name,path,sha1)->begin
        run(`git fetch-pack -q $path HEAD`)
        run(`git tag -f submodules/$path/$(sha1[1:10]) $sha1`)
        run(`git --git-dir=$path/.git gc -q`)
        head = @cd path chomp(readall(`git rev-parse --symbolic-full-name HEAD`))
        m = match(r"^refs/heads/(.*)$", head)
        if m != nothing
            branch = m.captures[1]
            git_modules(`submodule.$name.branch $branch`)
        else
            try git_modules(`--unset submodule.$name.branch`) end
        end
    end, true)
    run(`git add .gitmodules`)
end

# checkout a particular repo version

function pkg_checkout(rev::String)
    dir = cwd()
    run(`git checkout -q $rev`)
    run(`git submodule update --init --reference $dir --recursive`)
    run(`git ls-files --other` | `xargs rm -rf`)
    git_each_submodule((name,path,sha1)->begin
        branch = try git_modules(`submodule.$name.branch`) end
        if branch != nothing
            @cd path run(`git checkout -B $branch $sha1`)
        end
    end, true)
end
pkg_checkout() = pkg_checkout("HEAD")

# commit the current state of the repo with the given message

function pkg_commit(msg::String)
    pkg_checkpoint()
    git_canonicalize_config(".gitmodules")
    run(`git commit -m $msg`)
end

# install packages by name and, optionally, git url

function pkg_install(urls::Associative)
    names = sort!(keys(urls))
    if isempty(names) return end
    dir = cwd()
    for pkg in names
        url = urls[pkg]
        run(`git submodule add --reference $dir $url $pkg`)
    end
    pkg_checkout()
    pkg_commit("[jul] install "*join(names, ", "))
end
function pkg_install(names::AbstractVector)
    urls = Dict()
    for pkg in names
        urls[pkg] = chomp(readall(open("METADATA/$pkg/url")))
    end
    pkg_install(urls)
end
pkg_install(names::String...)      = pkg_install([names...])

# remove packages by name

function pkg_remove(names::AbstractVector)
    if isempty(names) return end
    sort!(names)
    for pkg in names
        run(`git rm --cached $pkg`)
        git_modules(`--remove-section submodule.$pkg`)
    end
    run(`git add .gitmodules`)
    pkg_commit("[jul] remove "*join(names, ", "))
    run(`rm -rf $names`)
end
pkg_remove(names::String...) = pkg_remove([names...])

# push & pull package repos to/from remotes

function pkg_push()
    pkg_checkpoint()
    run(`git push --tags`)
    run(`git push`)
end

function pkg_pull()
    run(`git fetch --tags`)
    run(`git fetch`)
    fetch_head = chomp(readall(`git rev-parse FETCH_HEAD`))
    git_each_submodule((name,path,sha1)->begin
        alt = try chomp(readall(`git rev-parse $fetch_head:$path`)) end
        if alt != nothing
            @cd path run(`git merge --no-edit $alt`)
            run(`git add $path`)
        end
    end, false)
    if git_dirty()
        run(`git commit -m "[jul] pull: merge submodules"`)
    end
    # TODO: if merge in progress, just commit
    run(`git merge -m "[jul] pull: merge main" $fetch_head`)
    pkg_checkout("HEAD")
    pkg_checkpoint()
end
