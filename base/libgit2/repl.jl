# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
# Process REPL command in git mode

List of git commands:
- [x] add
- [x] branch
- [x] clone
- [x] checkout
- [x] commit
- [x] fetch
- [x] init
- [x] log
- [x] push
- [x] reset
- [x] rm
- [x] status
- [x] tag
"""
function repl_cmd(ex)
    cmd = split(ex)
    has_params = length(cmd) > 1
    repopath = pwd()

    if cmd[1] == "init"
        if has_params && cmd[2] == "help"
            println("usage: init [bare]")
            return
        end
        try
            repo = init(repopath, has_params ? Cuint(cmd[2] == "bare") : Cuint(0))
            finalize(repo)
            println("Initialized empty Git repository in $repopath")
        catch ex
            warn(ex)
        end
    elseif cmd[1] == "clone"
        if has_params && cmd[2] == "help"
            println("usage: clone [bare] <url>")
            return
        end
        try
            repourl = cmd[2]
            repo = clone(repourl, repopath, isbare=("bare" in cmd), )
            finalize(repo)
            println("Cloned $repourl into $repopath")
        catch ex
            println("usage: clone [bare] <url>")
            warn(ex)
        end
    else
        repo = GitRepoExt(repopath)
        try
            if cmd[1] == "help"
                repl_help()
            elseif cmd[1] == "log"
                msg_count = 10
                if has_params
                    msg_count = Base.get(tryparse(Int, cmd[2]), 0)
                    if msg_count == 0 || cmd[2] == "help"
                        println("usage: log [<num-of-msgs>]")
                        return
                    end
                end
                repl_log(repo, msg_count)
            elseif cmd[1] == "status"
                if has_params && cmd[2] == "help"
                    println("usage: status [long]")
                    return
                end
                repl_status(repo, "long" in cmd)
            elseif cmd[1] == "add"
                if has_params && (cmd[2] == "help" || (cmd[2] == "force" && length(cmd) == 2))
                    println("usage: add [force] <file>...")
                    return
                end
                force_add = has_params && cmd[2] == "force"
                repl_add(repo, force_add, cmd[(force_add ? 3 : 2):end])
            elseif cmd[1] == "rm"
                if has_params && cmd[2] == "help"
                    println("usage: rm <file>...")
                    return
                end
                repl_rm(repo, cmd[2:end])
            elseif cmd[1] == "reset"
                if has_params && cmd[2] == "help"
                    println("usage: reset <file>...")
                    return
                end
                repl_reset(repo, cmd[2:end])
            elseif cmd[1] == "branch"
                runcmd = if has_params
                    if cmd[2] == "local"
                        :listlocal
                    elseif  cmd[2] == "remote"
                        :listremote
                    elseif  cmd[2] == "all"
                        :listall
                    elseif length(cmd) == 3
                        if cmd[2] == "add"
                            :add
                        elseif cmd[2] == "delete"
                            :delete
                        else
                            :help
                        end
                    else
                        :help
                    end
                else
                    :listlocal
                end
                if runcmd == :help
                    println("usage: branch [(local|remote|all)|(add|delete) <branch>]")
                    return
                end
                repl_branch(repo, runcmd, cmd[end])
            elseif cmd[1] == "tag"
                runcmd = if has_params
                    if length(cmd) == 3
                        if cmd[2] == "add"
                            :add
                        elseif cmd[2] == "delete"
                            :delete
                        else
                            :help
                        end
                    else
                        :help
                    end
                else
                    :list
                end
                if runcmd == :help
                    println("usage: tag [(add|delete) <tag>]")
                    return
                end
                repl_tag(repo, runcmd, cmd[end])
            elseif cmd[1] == "commit"
                if !has_params || cmd[2] == "help"
                    println("usage: commit <message>")
                    return
                end
                repl_commit(repo, cmd[2:end])
            elseif cmd[1] == "checkout"
                if !has_params || cmd[2] == "help"
                    println("usage: checkout (<branch>|<commitish>)")
                    return
                end
                repl_checkout(repo, cmd[2])
            elseif cmd[1] == "fetch"
                if has_params && cmd[2] == "help"
                    println("usage: fetch [<repository>] [<refspec>]")
                    return
                end
                url, rs = if has_params
                    length(cmd) == 3 ? (cmd[2], cmd[3]) : (cmd[2], "")
                else
                    ("", "")
                end
                repl_fetch(repo, url, rs)
            elseif cmd[1] == "push"
                if has_params && cmd[2] == "help"
                    println("usage: push [<repository>] [<refspec>]")
                    return
                end
                url, rs = if has_params
                    length(cmd) == 3 ? (cmd[2], cmd[3]) : (cmd[2], "")
                else
                    ("", "")
                end
                repl_push(repo, url, rs)
            else
                warn("unknown command: $ex. Use \'help\' command.")
            end
        finally
            finalize(repo)
        end
    end
end

function repl_help()
    println("""List of commands:
 add      Add file contents to the index
 branch   List, create, or delete branches
 checkout Checkout a branch or paths to the working tree
 clone    Clone a repository into a current directory
 commit   Commit record changes to the repository
 fetch    Download objects and refs from another repository
 init     Create an empty Git repository or reinitialize an existing one in current directory
 log      Show commit logs
 push     Update remote refs along with associated objects
 reset    Reset current HEAD to the specified state
 rm       Remove files from the working tree and from the index
 status   Show the working tree status
 tag      Create, list or delete a tag objects

For particular command parameters use: <command> help""")
end

function repl_add{T<:AbstractString}(repo::GitRepo, force_add::Bool, files::Vector{T})
    forced = force_add ? GitConst.INDEX_ADD_FORCE : GitConst.INDEX_ADD_DEFAULT
    add!(repo, files..., flags = forced)
    return
end

function repl_rm{T<:AbstractString}(repo::GitRepo, files::Vector{T})
    remove!(repo, files...)
    return
end

function repl_fetch{T<:AbstractString}(repo::GitRepo, url::T, refspec::T)
    fetch(repo, remoteurl=url, refspecs=[refspec])
    return
end

function repl_push{T<:AbstractString}(repo::GitRepo, url::T, refspec::T)
    push(repo, remoteurl=url, refspecs=[refspec])
    return
end

function repl_reset{T<:AbstractString}(repo::GitRepo, files::Vector{T})
    with(head(repo)) do href
        with(peel(GitCommit, href)) do hcommit
            reset!(repo, Nullable(hcommit), files...)
        end
    end
    return
end

function repl_branch{T<:AbstractString}(repo::GitRepo, cmd::Symbol, branch::T)
    if cmd == :listlocal || cmd == :listremote || cmd == :listall
        flags = cmd == :listlocal  ? Cint(GitConst.BRANCH_LOCAL) : (
                cmd == :listremote ? Cint(GitConst.BRANCH_REMOTE) :
                                     Cint(GitConst.BRANCH_LOCAL) |  Cint(GitConst.BRANCH_REMOTE))
        # walk through branches
        branches = with(GitBranchIter(repo, flags)) do bi
            map(x-> try
                        (shortname(x[1]), x[2])
                    finally
                        finalize(x[1])
                    end, bi)
        end

        head_name = headname(repo) # get HEAD name

        # prepare output
        output = Tuple{AbstractString,Symbol}[]
        for b in branches
            c = if b[1] == head_name
                :green
            elseif b[2] == 1
                :white
            else
                :red
            end
            push!(output, (b[1], c))
        end

        # if HEAD does not have branch insert it in output
        cmd == :listremote || any(map(b->b[2] == :green, output)) || insert!(output, 1, (head_name, :green))
        for (b, c) in output
            c == :green ? print("* ") : print("  ")
            print_with_color(c, "$b\n")
        end
    elseif cmd == :add
        with(head(repo)) do href
            with(peel(GitCommit, href)) do hcommit
                create_branch(repo, hcommit, branch)
            end
        end
    elseif cmd == :delete
        bref = lookup_branch(repo, branch)
        bref != nothing && try
            delete_branch(bref)
        finally
            finalize(bref)
        end
    end
    return
end

function repl_tag{T<:AbstractString}(repo::GitRepo, cmd::Symbol, tag::T)
    if cmd == :list
        for t in sort(tag_list(repo))
            println(t)
        end
    elseif cmd == :add
        with(head(repo)) do href
            with(peel(GitCommit, href)) do hcommit
                tag_create(repo, tag,  string(Oid(hcommit)))
            end
        end
    elseif cmd == :delete
        tag_delete(repo, tag)
    end
    return
end

function repl_commit{T<:AbstractString}(repo::GitRepo, msg::Vector{T})
    m = join(msg, " ")*"\n"
    cmd_oid = commit(repo, m)
    head_name = headname(repo) # get HEAD name
    print("[$head_name $(string(cmd_oid)[1:7])] $m")
    return
end

function repl_checkout{T<:AbstractString}(repo::GitRepo, branch::T)
    # get branch tree
    btree_oid = revparseid(repo, "$branch^{tree}")

    # checout selected branch
    with(get(GitTree, repo, btree_oid)) do btree
        checkout_tree(repo, btree)
    end

    # switch head to the branch
    branch_ref = lookup_branch(repo, branch)
    if branch_ref != nothing
        head!(repo, branch_ref)
        finalize(branch_ref)
    else
        # detatch HEAD
        boid = revparseid(repo, branch)
        checkout!(repo, string(boid))
    end

    println("Switched to ",(branch_ref == nothing ? "" : "branch "),"'$branch'")
    return
end

function repl_log(repo::GitRepo, msg_count::Int)
    msgs = with(GitRevWalker(repo)) do walker
        map((oid,r)->with(get(GitCommit, r, oid)) do cmt
                        sig = author(cmt)
                        msg = message(cmt)
                        (Oid(cmt), sig, msg)
                    end,
            walker, count = msg_count)
    end
    for msg in msgs
        print_with_color(:yellow, "Commit: $(string(msg[1]))\n")
        println("Author:\t$(msg[2].name) $(msg[2].email)")
        println("Date:\t$(Dates.unix2datetime(msg[2].time))")
        println('\t',join(split(msg[3],'\n'),"\n\t"))
    end
end

function repl_status(repo::GitRepo, islong::Bool=false)
    isbare(repo) && error("Cannot report status on bare repository")

    # Show branch
    with(head(repo)) do bref
        bname = shortname(bref)
        if islong
            println("# On branch: ", isempty(bname) ? "Not currently on any branch." : bname)
        else
            println("## ", isempty(bname) ? "HEAD (no branch)" : bname)
        end
    end

    header = changes_in_index = changed_in_workdir = rm_in_workdir = false

    # Show status
    with(GitStatus, repo) do status
        for i in 1:length(status)
            s = status[i]
            s.status == GitConst.STATUS_CURRENT && continue

            istatus = wstatus = " "

            if s.status & GitConst.STATUS_INDEX_NEW > 0
                istatus = islong ? "new file: " : "A"
            end
            if s.status & GitConst.STATUS_INDEX_MODIFIED > 0
                istatus = islong ? "modified: " : "M"
            end
            if s.status & GitConst.STATUS_INDEX_DELETED > 0
                istatus = islong ? "deleted:  " : "D"
            end
            if s.status & GitConst.STATUS_INDEX_RENAMED > 0
                istatus = islong ? "renamed:  " : "R"
            end
            if s.status & GitConst.STATUS_INDEX_TYPECHANGE > 0
                istatus = islong ? "typechange:" : "T"
            end

            if s.status & GitConst.STATUS_WT_NEW > 0
                if istatus == " " && !islong
                    istatus = "?"
                end
                wstatus = "?"
            end
            if s.status & GitConst.STATUS_WT_MODIFIED    > 0; wstatus = "M"; end
            if s.status & GitConst.STATUS_WT_DELETED     > 0
                wstatus = "D"
                rm_in_workdir = true
            end
            if s.status & GitConst.STATUS_WT_RENAMED     > 0; wstatus = "R"; end
            if s.status & GitConst.STATUS_WT_TYPECHANGE  > 0; wstatus = "T"; end

            if islong
                istatus == " " && continue
            else
                if s.status & GitConst.STATUS_IGNORED > 0
                    istatus = "!"
                    wstatus = "!"
                end
                istatus == "?" && wstatus == "?" && continue
            end

            if islong
                if !header
                    println(" Changes to be committed:");
                    println("   (use \"reset <file>...\" to unstage)\n")
                    header = true
                end
                shi_diff = unsafe_load(convert(Ptr{DiffDelta}, s.head_to_index), 1)
                old_path = shi_diff.old_file.path == Cstring_NULL ? "" : bytestring(shi_diff.old_file.path)
                new_path = shi_diff.new_file.path == Cstring_NULL ? "" : bytestring(shi_diff.new_file.path)

                if !isempty(old_path) && !isempty(new_path) && old_path != new_path
                    println("\t$istatus $old_path -> $new_path")
                else
                    println("\t$istatus $(isempty(old_path) ? new_path : old_path)")
                end
            else
                a = b = c = ""
                if s.head_to_index != C_NULL
                    shi_diff = unsafe_load(convert(Ptr{DiffDelta}, s.head_to_index), 1)
                    a = shi_diff.old_file.path == Cstring_NULL ? "" : bytestring(shi_diff.old_file.path)
                    b = shi_diff.new_file.path == Cstring_NULL ? "" : bytestring(shi_diff.new_file.path)
                end
                if s.index_to_workdir != C_NULL
                    siw_diff = unsafe_load(convert(Ptr{DiffDelta}, s.index_to_workdir), 1)
                    if isempty(a)
                        a = siw_diff.old_file.path == Cstring_NULL ? "" : bytestring(siw_diff.old_file.path)
                    end
                    if isempty(b)
                        b = siw_diff.old_file.path == Cstring_NULL ? "" : bytestring(siw_diff.old_file.path)
                    end
                    c = siw_diff.new_file.path == Cstring_NULL ? "" : bytestring(siw_diff.new_file.path)
                end

                if istatus == "R"
                    if wstatus == "R"
                        println("$istatus$wstatus $a $b $c")
                    else
                        println("$istatus$wstatus $a $b")
                    end
                else
                    if wstatus == "R"
                        println("$istatus$wstatus $a $c")
                    else
                        println("$istatus$wstatus $a")
                    end
                end
            end
        end

        if islong
            if header
                changes_in_index = true
                println("")
            end
            header = false

            # Print workdir changes to tracked files.
            for i in 1:length(status)
                s = status[i]
                (s.status == GitConst.STATUS_CURRENT || s.index_to_workdir == C_NULL) && continue

                wstatus = ""
                if s.status & GitConst.STATUS_WT_MODIFIED    > 0; wstatus = "modified: "; end
                if s.status & GitConst.STATUS_WT_DELETED     > 0; wstatus = "deleted:  "; end
                if s.status & GitConst.STATUS_WT_RENAMED     > 0; wstatus = "renamed:  "; end
                if s.status & GitConst.STATUS_WT_TYPECHANGE  > 0; wstatus = "typechange:"; end

                isempty(wstatus) && continue

                if !header
                    println(" Changes not staged for commit:")
                    println("   (use \"add$(rm_in_workdir ? "/rm" : "") <file>...\" to update what will be committed)")
                    println("   (use \"checkout <file>...\" to discard changes in working directory)\n")
                    header = true
                end
                siw_diff = unsafe_load(convert(Ptr{DiffDelta}, s.index_to_workdir), 1)
                old_path = siw_diff.old_file.path == Cstring_NULL ? "" : bytestring(siw_diff.old_file.path)
                new_path = siw_diff.new_file.path == Cstring_NULL ? "" : bytestring(siw_diff.new_file.path)

                if !isempty(old_path) && !isempty(new_path) && old_path != new_path
                    println("\t$wstatus $old_path -> $new_path")
                else
                    println("\t$wstatus $(isempty(old_path) ? new_path : old_path)")
                end
            end

            if header
                changed_in_workdir = true
                println("")
            end
            header = false
        end

        # Print untracked files.
        for i in 1:length(status)
            s = status[i]
            if s.status == GitConst.STATUS_WT_NEW
                siw_diff = unsafe_load(convert(Ptr{DiffDelta}, s.index_to_workdir), 1)

                if islong
                    if !header
                        println(" Untracked files:")
                        println("   (use \"add <file>...\" to include in what will be committed)\n")
                        header = true
                    end
                    println("\t", bytestring(siw_diff.old_file.path))
                else
                    println("?? ", bytestring(siw_diff.old_file.path))
                end
            end
        end

        # Print ignored files (long version).
        if islong
            header = false
            for i in 1:length(status)
                s = status[i]
                if s.status == GitConst.STATUS_IGNORED
                    if !header
                        println(" Ignored files:")
                        println("   (use \"add force <file>...\" to include in what will be committed)\n")
                        header = true
                    end
                    siw_diff = unsafe_load(convert(Ptr{DiffDelta}, s.index_to_workdir), 1)
                    println("\t", bytestring(siw_diff.old_file.path))
                end
            end

            !changes_in_index && changed_in_workdir && println("no changes added to commit (use \"add\" and/or \"commit -a\")")
        end

    end
    return
end
