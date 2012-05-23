# default locations: local package repo, remote metadata repo

const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"
const PKG_GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# some utility functions for working with git repos

git_dir() = chomp(readall(`git rev-parse --git-dir`))
git_head() = chomp(readall(string(git_dir(),"/HEAD")))
git_dirty() = !success(`git diff --quiet`)

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
git_each_submodule(f::Function)          = git_each_submodule(f, false, cwd())

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    @cd dir begin
        run(`git init`)
        run(`git commit --allow-empty -m "[jul] empty package repo"`)
        pkg_install(dir, {"METADATA" => meta})
    end
end
pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init()            = pkg_init(PKG_DEFAULT_DIR)

# checkpoint a repo, recording submodule commits as tags

function pkg_checkpoint(dir::String)
    @cd dir begin
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
end
pkg_checkpoint() = pkg_checkpoint(PKG_DEFAULT_DIR)

# checkout a particular repo version

function pkg_checkout(dir::String, rev::String)
    @cd dir begin
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
end
pkg_checkout(rev::String) = pkg_checkout(PKG_DEFAULT_DIR, rev)

# commit the current state of the repo with the given message

function pkg_commit(dir::String, msg::String)
    pkg_checkpoint(dir)
    @cd dir run(`git commit -m $msg`)
end
pkg_commit(msg::String) = pkg_commit(PKG_DEFAULT_DIR, msg)

# install packages by name and, optionally, git url

function pkg_install(dir::String, urls::Associative)
    names = sort!(keys(urls))
    if isempty(names) return end
    @cd dir begin
        dir = cwd()
        for pkg in names
            url = urls[pkg]
            run(`git submodule add --reference $dir $url $pkg`)
        end
        pkg_checkout(dir, "HEAD")
        pkg_commit(dir, "[jul] install "*join(names, ", "))
    end
end
function pkg_install(dir::String, names::AbstractVector)
    urls = Dict()
    @cd dir for pkg in names
        urls[pkg] = chomp(readall(open("METADATA/$pkg/url")))
    end
    pkg_install(dir, urls)
end
pkg_install(urls::Associative)     = pkg_install(PKG_DEFAULT_DIR, urls)
pkg_install(names::AbstractVector) = pkg_install(PKG_DEFAULT_DIR, names)
pkg_install(names::String...)      = pkg_install([names...])

# remove packages by name

function pkg_remove(dir::String, names::AbstractVector)
    if isempty(names) return end
    sort!(names)
    @cd dir begin
        for pkg in names
            run(`git rm --cached $pkg`)
            git_modules(`--remove-section submodule.$pkg`)
        end
        run(`git add .gitmodules`)
        pkg_commit(dir, "[jul] remove "*join(names, ", "))
        run(`rm -rf $names`)
    end
end
pkg_remove(names::AbstractVector) = pkg_remove(PKG_DEFAULT_DIR, names)
pkg_remove(names::String...)      = pkg_remove([names...])

# push & pull package repos to/from remotes

function pkg_push(dir::String)
    @cd dir begin
        run(`git push --tags`)
        run(`git push`)
    end
end
pkg_push() = pkg_push(PKG_DEFAULT_DIR)

function pkg_pull(dir::String)
    @cd dir begin
        run(`git fetch --tags`)
        run(`git pull`)
        pkg_checkout(dir, "HEAD")
    end
end
pkg_pull() = pkg_pull(PKG_DEFAULT_DIR)

# clone a new package repo from a URL

function pkg_clone(dir::String, url::String)
    run(`git clone $url $dir`)
    pkg_checkout(dir, "HEAD")
end
pkg_clone(url::String) = pkg_clone(PKG_DEFAULT_DIR, url)
