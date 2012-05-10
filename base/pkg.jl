const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    @chdir dir begin
        run(`git init`)
        run(`git commit --allow-empty -m "[jul] empty package repo"`)
        pkg_install({"METADATA" => meta})
    end
end

pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init()            = pkg_init(PKG_DEFAULT_DIR)

# install packages by name and, optionally, git url

function pkg_install(dir::String, urls::Associative)
    names = sort!(keys(urls))
    if isempty(names) return end
    @chdir dir begin
        for pkg in names
            url = urls[pkg]
            run(`git clone $url $pkg`)
            run(`git add $pkg/`) # the slash is crucial
        end
        run(`git commit -m "[jul] install $(join(names, ", "))"`)
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
            run(`git rm -rf $pkg`)
            run(`rm -rf $pkg`) # clean up anything left
        end
        run(`git commit -m "[jul] remove $(join(names, ", "))"`)
    end
end

pkg_remove(names::AbstractVector) = pkg_remove(PKG_DEFAULT_DIR, names)
pkg_remove(names::String...)      = pkg_remove([names...])
