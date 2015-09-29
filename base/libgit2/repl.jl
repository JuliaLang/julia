# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
# Process REPL command in git mode

List of git commands:
- [x] add
- [ ] branch
- [x] clone
- [ ] commit
- [ ] fetch
- [x] init
- [x] log
- [x] reset
- [x] rm
- [x] status
- [ ] tag
"""

function repl_cmd(ex)
    cmd = split(ex)
    has_params = length(cmd) > 1
    repopath = pwd()

    if cmd[1] == "init"
        if has_params && cmd[2] == "help"
            println("usage: init [--bare]")
            return
        end
        try
            repo = init(repopath, has_params ? Cuint(cmd[2] == "--bare") : Cuint(0))
            finalize(repo)
            println("Initialized empty Git repository in $repopath")
        catch ex
            warn(ex)
        end
    elseif cmd[1] == "clone"
        if has_params && cmd[2] == "help"
            repl_clone_help()
            return
        end
        try
            repourl = cmd[2]
            repo = clone(repourl, repopath, isbare=("--bare" in cmd), )
            finalize(repo)
            println("Cloned $repourl into $repopath")
        catch ex
            println("usage: clone [--bare] <url>")
            warn(ex)
        end
    else
        repo = GitRepo(repopath)
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
                    println("usage: status [-long]")
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
            else
                warn("unknown command: $ex. Use \'help\' command.")
            end
        finally
            finalize(repo)
        end
    end
end

function repl_help()
    println("""List of git commands:
 add\t Add file contents to the index
 clone\t Clone a repository into a current directory
 init\t Create an empty Git repository or reinitialize an existing one in current directory
 log\t Show commit logs
 reset\t\ Reset current HEAD to the specified state
 rm\t Remove files from the working tree and from the index
 status\t Show the working tree status

For particular command parameters use: <command> help
""")
# branch\t\tList, create, or delete branches
# commit\t\tRecord changes to the repository
# fetch\t\tDownload objects and refs from another repository
# tag\t\tCreate, list, delete or verify a tag object signed with GPG
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

function repl_reset{T<:AbstractString}(repo::GitRepo, files::Vector{T})
    with(head(repo)) do href
        with(peel(GitCommit, href)) do hcommit
            reset!(repo, Nullable(hcommit), files...)
        end
    end
    return
end

function repl_log(repo::GitRepo, msg_count::Int)
    msgs = map(
        (oid,r)->with(get(GitCommit, r, oid)) do cmt
                        sig = author(cmt)
                        msg = message(cmt)
                        (Oid(cmt), sig, msg)
                    end, repo, count = msg_count)
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
