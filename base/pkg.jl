module pkg
#
# Julia's git-based declarative package manager
#
import Base.*
import git

# default locations: local package repo, remote metadata repo

const DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"
const GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# assert that everything is clean or bail

assert_clean() = git.dirty() &&
    error("Uncommited changes found -- please commit, stash or discard.")

# create a new empty packge repository

function init(dir::String, meta::String)
    run(`mkdir $dir`)
    cd(dir) do
        run(`git init`)
        run(`git commit -m --allow-empty "[jul] empty package repo"`)
        run(`mkdir VERSIONS`)
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

# commit or localize package config files

function stage_config(path)
    cfg = git.read_config(cd(git.dir,path)*"/config")
    if success(`test -e $path.local`)
        del_each(cfg, keys(git.read_config("$path.local")))
    end
    cfg = merge(git.read_config("$path.config"), cfg)
    git.write_config("$path.config", cfg)
    run(`git add -- $path.config`)
end

function commit_configs()
    assert_clean()
    git.each_submodule(false) do name, path, sha1
        stage_config(path)
    end
    if git.staged()
        run(`git commit -m "[jul] config changes"`)
    end
end

function localize_configs()
    assert_clean()
    git.each_submodule(false) do name, path, sha1
        diff = Dict()
        original = git.read_config("$path.config")
        modified = git.read_config(cd(git.dir,path)*"/config")
        for (key,val) in modified
            if val != get(original,key,nothing)
                diff[key] = val
            end
        end
        if !isempty(diff)
            git.write_config("$path.local", diff)
        else
            run(`rm -f $path.local`)
        end
    end
end

function each_config(f::Function)
    git.each_submodule(false) do name, path, sha1
        cfg = git.read_config("$path.config")
        if success(`test -e $path.local`)
            merge!(cfg, git.read_config("$path.local"))
        end
        f(name,path,sha1,cfg)
    end
end

function assert_config_clean()
    dirty = Set{ByteString}()
    each_config() do name, path, sha1, cfg
        for (key,val) in merge(cfg, git.read_config(cd(git.dir,path)*"/config"))
            if val != get(cfg,key,nothing)
                add(dirty,name)
                break
            end
        end
    end
    if isempty(dirty) return end
    print(stderr_stream,
        "The following packages have unsaved config changes:\n\n",
        map(pkg->"    $pkg\n", sort!(elements(dirty)))...,
        "\nPlease either commit, localize or clobber these changes.\n"
    )
    error("pkg: unsaved config changes")
end

function clobber_configs()
    each_config() do name, path, sha1, cfg
        git.write_config(cd(git.dir,path)*"/config", cfg)
    end
end

# record all submodule commits as tags

function tag_submodules()
    git.each_submodule(true) do name, path, sha1
        run(`git fetch-pack -q $path HEAD`)
        run(`git tag -f submodules/$path/$(sha1[1:10]) $sha1`)
        run(`git --git-dir=$path/.git gc -q`)
    end
end

function save_heads()
    git.each_submodule(false) do name, path, sha1
        head = readchomp(cd(git.dir,path)*"/HEAD")
        git.modules(`submodule.$name.head $head`)
    end
end

# checkout a particular repo version

function checkout(rev::String)
    dir = cwd()
    run(`git checkout -fq $rev`)
    run(`git submodule update --init --reference $dir --recursive`)
    run(`git ls-files --other` | `xargs rm -rf`)
    git.each_submodule(true) do name, path, sha1
        branch = try git.modules(`submodule.$name.branch`) end
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
    assert_config_clean()
    tag_submodules()
    save_heads()
    git.canonicalize_config(".gitmodules")
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
        stage_config(pkg)
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
        git.modules(`--remove-section submodule.$pkg`)
        run(`git rm $pkg.config`)
        run(`rm -f $pkg.local`)
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
    # abort if things aren't committed
    assert_clean()

    # get remote data
    run(`git fetch --tags`)
    run(`git fetch`)

    # see how far git gets with merging
    if success(`git merge -m "[jul] pull (simple merge)" FETCH_HEAD`) return end

    # get info about local, remote and base trees
    L = readchomp(`git rev-parse --verify HEAD`)
    R = readchomp(`git rev-parse --verify FETCH_HEAD`)
    B = readchomp(`git merge-base $L $R`)
    Rc = git.read_config_blob("$R:.gitmodules")
    Rs = git.config_sections(Rc)

    # intelligently 3-way merge .gitmodules versions
    if git.different(L,R,".gitmodules")
        Bc = git.read_config_blob("$B:.gitmodules")
        Lc = git.read_config_blob("$L:.gitmodules")
        Cc, conflicts, deleted = git.merge_configs(Bc,Lc,Rc)
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
        git.write_config(".gitmodules", Cc)
        if isempty(conflicts) run(`git add .gitmodules`) end
    end

    # merge submodules
    git.each_submodule(false) do name, path, sha1
        if has(Rs,"submodule.$name") && git.different(L,R,path)
            alt = readchomp(`git rev-parse $R:$path`)
            cd(path) do
                run(`git merge --no-edit $alt`)
            end
            run(`git add -- $path`)
        end
    end

    # merge package configs
    git.each_submodule(false) do name, path, sha1
        Bpc = git.read_config_blob("$B:$path.config")
        Lpc = git.read_config_blob("$L:$path.config")
        Rpc = git.read_config_blob("$R:$path.config")
        Cpc, conflicts= git.merge_configs(Bpc,Lpc,Rpc)
        # warn about config conflicts
        for (key,vals) in conflicts
            print(stderr_stream,
                "\nPackage config conflict for package $name and $key:\n",
                "  local value  = $(vals[1])\n",
                "  remote value = $(vals[2])\n",
                "\n",
                "Both values written to $path.config -- please edit and choose one.\n\n",
            )
            Cpc[key] = vals
        end
        # write the result (unconditionally) and stage it (if no conflicts)
        git.write_config("$path.config", Cpc)
        if isempty(conflicts) run(`git add -- $path.config`) end
    end

    # check for remaining merge conflicts
    if git.unstaged()
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

end # module
