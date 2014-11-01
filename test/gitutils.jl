function write_and_readchomp(data, cmd::Cmd)
    r, w, p = readandwrite(cmd)
    print(w,data); close(w)
    v = readchomp(r)
    wait(p)
    return v
end

function mktree(d::Dict)
    lstree = ""
    for (name, data) in d
        if isa(data, AbstractString)
            sha1 = write_and_readchomp(data, `git hash-object -w --stdin`)
            lstree *= "100644 blob $sha1\t$name\n"
        elseif isa(data, Dict)
            sha1 = mktree(data)
            lstree *= "040000 tree $sha1\t$name\n"
        elseif is(data, nothing)
            # ignore it
        else
            error("mktree: don't know what to do with $name => $data")
        end
    end
    write_and_readchomp(lstree, `git mktree`)
end

function verify_tree(d::Dict, tree::AbstractString)
    # check that tree matches d
    seen = Set()
    for line in eachline(`git ls-tree $tree`)
        m = match(r"^(\d{6}) (\w+) ([0-9a-f]{40})\t(.*)$", line)
        @test m != nothing
        perm, kind, sha1, name = m.captures
        @test haskey(d,name)
        data = d[name]
        if isa(data, AbstractString)
            @test kind == "blob"
            @test data == readall(`git cat-file blob $sha1`)
        elseif isa(data, Dict)
            @test kind == "tree"
            verify_tree(data, sha1)
        else
            error("verify_tree: don't know what to do with $name => $data")
        end
        push!(seen, name)
    end
    # check that nothing was missing from tree
    for (name, data) in d
        @test is(data,nothing) || in(name,seen)
    end
end

function verify_work(d::Dict)
    # check what's in d
    for (name, data) in d
        if is(data, nothing)
            @test !ispath(name)
            continue
        end
        @test ispath(name)
        if isa(data, AbstractString)
            @test isfile(name)
            @test readall(name) == data
        elseif isa(data, Dict)
            cd(name) do
                verify_work(data)
            end
        else
            error("verify_work: don't know what to do with $name => $data")
        end
    end
    # check for anything that's not in d
    for line in eachline(`ls -A`)
        name = chomp(line)
        @test name == ".git" || haskey(d,name)
    end
end

function git_verify(h::Dict, i::Dict, w::Dict)
    verify_tree(h, "HEAD")
    verify_tree(i, readchomp(`git write-tree`))
    verify_work(w)
end

function git_setup(h::Dict, i::Dict, w::Dict, parents::AbstractString...)
    # create tree objects
    headt = mktree(h)
    index = mktree(i)
    work  = mktree(w)

    # clear the repo
    for line in eachline(`ls -A`)
        name = chomp(line)
        name == ".git" || rm(name, recursive=true)
    end

    # create the head commit
    commit_tree = `git commit-tree $headt`
    for parent in parents
        commit_tree = `$commit_tree -p $parent`
    end
    head = write_and_readchomp(headt, commit_tree)
    run(`git reset -q --soft $head`)

    run(`git read-tree $work`)      # read work into the index
    run(`git checkout-index -fa`)   # check the index out
    run(`git read-tree $index`)     # setup the index

    # verify that everything is as expected
    git_verify(h, i, w)
end
git_setup(h::Dict, i::Dict, w::Dict) = git_setup(h, i, w, readchomp(`git rev-parse HEAD`))
