const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    @chdir dir begin
        run(`git init`)
        run(`git commit --allow-empty -m "[jul] empty package repo"`)
        head = chomp(readall(`git rev-parse HEAD`))
        sha1 = chomp(readall(`echo [checkpoint] $head` | `git commit-tree $head^{tree}`))
        pkg_write_checkpoint(dir, sha1)
        pkg_install(dir, {"METADATA" => meta})
    end
end
pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init()            = pkg_init(PKG_DEFAULT_DIR)

# checkpoint a repo, recording submodule commits as parents

function pkg_write_checkpoint(dir::String, sha1::String)
    @chdir dir begin
        file = open(".git/refs/heads/checkpoints","w")
        try println(file, sha1)
        catch err
            close(file)
            throw(err)
        end
        close(file)
    end
end

function pkg_checkpoint(dir::String, msg::String)
    @chdir dir begin
        run(`git commit -m $msg`)
        head = chomp(readall(`git rev-parse HEAD`))
        parents = [chomp(readall(".git/refs/heads/checkpoints"))]
        for line in each_line(`git ls-tree $head`)
            m = match(r"^160000 commit ([0-9a-f]{40})\b", line)
            if m != nothing
                push(parents, m.captures[1])
            end
        end
        commit_tree = `git commit-tree $head^{tree}`
        for parent in parents
            commit_tree = `$commit_tree -p $parent`
        end
        commit = chomp(readall((`echo "[checkpoint] $head"` | commit_tree)))
        pkg_write_checkpoint(dir, commit)
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
            run(`git clone --reference . -b $head $url $pkg`)
            run(`git add $pkg`) # submodule without .gitmodules
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
        end
        pkg_checkpoint(dir, "[jul] remove "*join(names, ", "))
    end
end
pkg_remove(names::AbstractVector) = pkg_remove(PKG_DEFAULT_DIR, names)
pkg_remove(names::String...)      = pkg_remove([names...])
