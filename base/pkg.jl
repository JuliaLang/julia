# default locations: local package repo, remote metadata repo

const PKG_DEFAULT_DIR = string(ENV["HOME"], "/.julia")
const PKG_DEFAULT_META = "git://github.com/StefanKarpinski/jul-METADATA.git"
const PKG_GITHUB_URL_RE = r"^(?:git@|git://|https://(?:[\w\.\+\-]+@)?)github.com[:/](.*)$"i

# some utility functions for working with git repos

git_dir() = readchomp(`git rev-parse --git-dir`)
git_modules(args::Cmd) = readchomp(`git config -f .gitmodules $args`)
git_different(verA::String, verB::String, path::String) =
    !success(`git diff --quiet $verA $verB -- $path`)

git_dirty() = !success(`git diff --quiet HEAD`)
git_staged() = !success(`git diff --quiet --cached`)
git_unstaged() = !success(`git diff --quiet`)
git_dirty(paths) = !success(`git diff --quiet HEAD -- $paths`)
git_staged(paths) = !success(`git diff --quiet --cached -- $paths`)
git_unstaged(paths) = !success(`git diff --quiet -- $paths`)

function git_each_submodule(f::Function, recursive::Bool, dir::ByteString)
    cmd = `git submodule foreach --quiet 'echo "$name\t$path\t$sha1"'`
    for line in each_line(cmd)
        name, path, sha1 = match(r"^(.*)\t(.*)\t([0-9a-f]{40})$", line).captures
        cd(dir) do
            f(name, path, sha1)
        end
        if recursive
            cd(path) do
                git_each_submodule(true, dir) do n,p,s
                    cd(dir) do
                        f(n,"$path/$p",s)
                    end
                end
            end
        end
    end
end
git_each_submodule(f::Function, r::Bool) = git_each_submodule(f, r, cwd())

function git_read_config(file::String)
    cfg = Dict()
    # TODO: use --null option for better handling of weird values.
    for line in each_line(`git config -f $file --get-regexp '.*'`)
        key, val = match(r"^(\S+)\s+(.*)$", line).captures
        cfg[key] = has(cfg,key) ? [cfg[key],val] : val
    end
    return cfg
end

# TODO: provide a clean way to avoid this disaster
function git_read_config_blob(blob::String)
    tmp = tmpnam()
    open(tmp,"w") do io
        write(io, readall(`git cat-file blob $blob`))
    end
    cfg = git_read_config(tmp)
    run(`rm -f tmp`)
    return cfg
end

function git_write_config(file::String, cfg::Dict)
    tmp = tmpnam()
    for key in sort!(keys(cfg))
        val = cfg[key]
        if isa(val,Array)
            for x in val
                run(`git config -f $tmp --add $key $x`)
            end
        else
            run(`git config -f $tmp $key $val`)
        end
    end
    open(file,"w") do io
        print(io,readall(tmp))
    end
end

git_canonicalize_config(file::String) = git_write_config(file, git_read_config(file))

function git_config_sections(cfg::Dict)
    sections = Set{ByteString}()
    for (key,_) in cfg
        m = match(r"^(.+)\.", key)
        if m != nothing add(sections,m.captures[1]) end
    end
    sections
end

function git_merge_configs(Bc::Dict, Lc::Dict, Rc::Dict)
    # extract section names
    Bs = git_config_sections(Bc)
    Ls = git_config_sections(Lc)
    Rs = git_config_sections(Rc)
    # expunge removed submodules from left and right sides
    deleted = Set{ByteString}()
    for section in Bs - Ls & Rs
        filter!((k,v)->!begins_with("$section.",k),Lc)
        filter!((k,v)->!begins_with("$section.",k),Rc)
        add(deleted, section)
    end
    # merge the remaining config key-value pairs
    cfg = Dict()
    conflicts = Dict()
    for (key,_) in merge(Lc,Rc)
        Lv = get(Lc,key,nothing)
        Rv = get(Rc,key,nothing)
        Bv = get(Bc,key,nothing)
        if Lv == Rv || Rv == Bv
            if Lv != nothing cfg[key] = Lv end
        elseif Lv == Bv
            if Rv != nothing cfg[key] = Rv end
        else # conflict!
            conflicts[key] = [Lv,Rv]
            cfg[key] = Bv
        end
    end
    return cfg, conflicts, deleted
end

# assert that everything is clean or bail

pkg_assert_clean() = (pkg_assert_git_clean(); pkg_assert_config_clean())
pkg_assert_git_clean() = git_dirty() &&
    error("Uncommited changes found -- please commit, stash or discard.")

# create a new empty packge repository

function pkg_init(dir::String, meta::String)
    run(`mkdir $dir`)
    cd(dir) do
        run(`git init`)
        open(".gitignore","w") do io
            println(io, "*.local")
        end
        run(`git add .gitignore`)
        run(`git commit -m "[jul] empty package repo"`)
        pkg_install({"METADATA" => meta})
    end
end
pkg_init(dir::String) = pkg_init(dir, PKG_DEFAULT_META)
pkg_init() = pkg_init(PKG_DEFAULT_DIR)

# clone a new package repo from a URL

function pkg_clone(dir::String, url::String)
    run(`git clone $url $dir`)
    cd(dir) do
        pkg_checkout("HEAD")
    end
end
pkg_clone(url::String) = pkg_clone(PKG_DEFAULT_DIR, url)

# commit or localize package config files

function pkg_stage_config(path)
    cfg = git_read_config(cd(git_dir,path)*"/config")
    if success(`test -e $path.local`)
        del_each(cfg, keys(git_read_config("$path.local")))
    end
    cfg = merge(git_read_config("$path.config"), cfg)
    git_write_config("$path.config", cfg)
    run(`git add -- $path.config`)
end

function pkg_commit_configs()
    pkg_assert_git_clean()
    git_each_submodule(false) do name, path, sha1
        pkg_stage_config(path)
    end
    if git_staged()
        run(`git commit -m "[jul] config changes"`)
    end
end

function pkg_localize_configs()
    pkg_assert_git_clean()
    git_each_submodule(false) do name, path, sha1
        diff = Dict()
        original = git_read_config("$path.config")
        modified = git_read_config(cd(git_dir,path)*"/config")
        for (key,val) in modified
            if val != get(original,key,nothing)
                diff[key] = val
            end
        end
        if !isempty(diff)
            git_write_config("$path.local", diff)
        else
            run(`rm -f $path.local`)
        end
    end
end

function pkg_each_config(f::Function)
    git_each_submodule(false) do name, path, sha1
        cfg = git_read_config("$path.config")
        if success(`test -e $path.local`)
            merge!(cfg, git_read_config("$path.local"))
        end
        f(name,path,sha1,cfg)
    end
end

function pkg_assert_config_clean()
    dirty = Set{ByteString}()
    pkg_each_config() do name, path, sha1, cfg
        for (key,val) in merge(cfg, git_read_config(cd(git_dir,path)*"/config"))
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

function pkg_clobber_configs()
    pkg_each_config() do name, path, sha1, cfg
        git_write_config(cd(git_dir,path)*"/config", cfg)
    end
end

# record all submodule commits as tags

function pkg_tag_submodules()
    git_each_submodule(true) do name, path, sha1
        run(`git fetch-pack -q $path HEAD`)
        run(`git tag -f submodules/$path/$(sha1[1:10]) $sha1`)
        run(`git --git-dir=$path/.git gc -q`)
    end
end

function pkg_save_heads()
    git_each_submodule(false) do name, path, sha1
        head = readchomp(cd(git_dir,path)*"/HEAD")
        git_modules(`submodule.$name.head $head`)
    end
end

# checkout a particular repo version

function pkg_checkout(rev::String)
    dir = cwd()
    run(`git checkout -fq $rev`)
    run(`git submodule update --init --reference $dir --recursive`)
    run(`git ls-files --other` | `xargs rm -rf`)
    git_each_submodule(true) do name, path, sha1
        branch = try git_modules(`submodule.$name.branch`) end
        if branch != nothing
            cd(path) do
                run(`git checkout -B $branch $sha1`)
            end
        end
    end
end
pkg_checkout() = pkg_checkout("HEAD")

# commit the current state of the repo with the given message

function pkg_commit(msg::String)
    pkg_assert_config_clean()
    pkg_tag_submodules()
    pkg_save_heads()
    git_canonicalize_config(".gitmodules")
    run(`git add .gitmodules`)
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
        pkg_stage_config(pkg)
    end
    pkg_commit("[jul] install "*join(names, ", "))
    pkg_checkout()
end
function pkg_install(names::AbstractVector)
    urls = Dict()
    for pkg in names
        urls[pkg] = readchomp("METADATA/$pkg/url")
    end
    pkg_install(urls)
end
pkg_install(names::String...) = pkg_install([names...])

# remove packages by name

function pkg_remove(names::AbstractVector)
    if isempty(names) return end
    sort!(names)
    for pkg in names
        run(`git rm --cached -- $pkg`)
        git_modules(`--remove-section submodule.$pkg`)
        run(`git rm $pkg.config`)
        run(`rm -f $pkg.local`)
    end
    run(`git add .gitmodules`)
    pkg_commit("[jul] remove "*join(names, ", "))
    run(`rm -rf $names`)
end
pkg_remove(names::String...) = pkg_remove([names...])

# push & pull package repos to/from remotes

function pkg_push()
    pkg_tag_submodules()
    run(`git push --tags`)
    run(`git push`)
end

function pkg_pull()
    # abort if things aren't committed
    pkg_assert_clean()

    # get remote data
    run(`git fetch --tags`)
    run(`git fetch`)

    # see how far git gets with merging
    if success(`git merge -m "[jul] pull (simple merge)" FETCH_HEAD`) return end

    # get info about local, remote and base trees
    L = readchomp(`git rev-parse --verify HEAD`)
    R = readchomp(`git rev-parse --verify FETCH_HEAD`)
    B = readchomp(`git merge-base $L $R`)
    Rc = git_read_config_blob("$R:.gitmodules")
    Rs = git_config_sections(Rc)

    # intelligently 3-way merge .gitmodules versions
    if git_different(L,R,".gitmodules")
        Bc = git_read_config_blob("$B:.gitmodules")
        Lc = git_read_config_blob("$L:.gitmodules")
        Cc, conflicts, deleted = git_merge_configs(Bc,Lc,Rc)
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
            if !begins_with("submodule.",section) continue end
            path = get(Lc,"$section.path",nothing)
            if path == nothing continue end
            run(`git rm -qrf --cached --ignore-unmatch -- $path`)
            run(`rm -rf $path`)
        end
        # write the result (unconditionally) and stage it (if no conflicts)
        git_write_config(".gitmodules", Cc)
        if isempty(conflicts) run(`git add .gitmodules`) end
    end

    # merge submodules
    git_each_submodule(false) do name, path, sha1
        if has(Rs,"submodule.$name") && git_different(L,R,path)
            alt = readchomp(`git rev-parse $R:$path`)
            cd(path) do
                run(`git merge --no-edit $alt`)
            end
            run(`git add -- $path`)
        end
    end

    # merge package configs
    git_each_submodule(false) do name, path, sha1
        Bpc = git_read_config_blob("$B:$path.config")
        Lpc = git_read_config_blob("$L:$path.config")
        Rpc = git_read_config_blob("$R:$path.config")
        Cpc, conflicts= git_merge_configs(Bpc,Lpc,Rpc)
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
        git_write_config("$path.config", Cpc)
        if isempty(conflicts) run(`git add -- $path.config`) end
    end

    # check for remaining merge conflicts
    if git_unstaged()
        unmerged = readall(`git ls-files -m` | `sort` | `uniq`)
        unmerged = replace(unmerged, r"^", "    ")
        print(stderr_stream,
            "\n*** WARNING ***\n\n",
            "You have unresolved merge conflicts in the following files:\n\n",
            unmerged,
            "\nPlease resolve these conflicts, `git add` the files, and commit.\n"
        )
        error("pkg_pull: merge conflicts")
    end

    # try to commit -- fails if unresolved conflicts remain
    run(`git commit -m "[jul] pull (complex merge)"`)
    pkg_checkout("HEAD")
    pkg_tag_submodules()
end
