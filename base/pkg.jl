# default locations: local package repo, remote metadata repo

const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    @chdir dir begin
        run(`git init`)
        run(`git commit --allow-empty -m "[jul] empty package repo"`)
        pkg_install(dir, {"METADATA" => meta})
    end
end
pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init()            = pkg_init(PKG_DEFAULT_DIR)

# checkpoint a repo, recording submodule commits as parents

function pkg_checkpoint(dir::String, msg::String)
    @chdir dir begin
        tree = chomp(readall(`git write-tree`))
        commit_tree = `git commit-tree $tree -p HEAD`
        for line in each_line(`git ls-tree $tree`)
            m = match(r"^160000 commit ([0-9a-f]{40})\t(.*)$", line)
            if m != nothing
                sha1, name = m.captures
                run(`git fetch-pack $name HEAD`)
                commit_tree = `$commit_tree -p $sha1`
            end
        end
        commit = chomp(readall((`echo $msg` | commit_tree)))
        run(`git reset $commit`)
    end
end
pkg_checkpoint(msg::String) = pkg_checkpoint(PKG_DEFAULT_DIR, msg)

# install packages by name and, optionally, git url

function pkg_install(dir::String, urls::Associative)
    names = sort!(keys(urls))
    if isempty(names) return end
    @chdir dir begin
        for pkg in names
            url = urls[pkg]
            head = "master"
            run(`git fetch $url +$head:packages/$pkg`)
            run(`git submodule add --reference . -b $head $url $pkg`)
            # TODO: try setting submodule push URL
        end
        pkg_checkpoint(dir, "[jul] install "*join(names, ", "))
    end
end
function pkg_install(dir::String, names::AbstractVector)
    urls = Dict()
    @chdir dir for pkg in names
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
    @chdir dir begin
        for pkg in names
            run(`git rm --cached $pkg`)
            run(`rm -rf $pkg`)
            run(`git config --file .gitmodules --remove-section submodule.$pkg`)
            run(`git add .gitmodules`)
        end
        pkg_checkpoint(dir, "[jul] remove "*join(names, ", "))
    end
end
pkg_remove(names::AbstractVector) = pkg_remove(PKG_DEFAULT_DIR, names)
pkg_remove(names::String...)      = pkg_remove([names...])

# checkout a particular repo version

function pkg_checkout(dir::String, rev::String)
    @chdir dir begin
        run(`git checkout -q $rev`)
        run(`git submodule update`)
        run(`git ls-files --other` | `xargs rm -rf`)
    end
end
pkg_checkout(rev::String) = pkg_checkout(PKG_DEFAULT_DIR, rev)
