"""
# Process REPL command in git mode

List of git commands:
- [/] add
- [ ] branch
- [x] clone
- [ ] commit
- [ ] fetch
- [x] init
- [x] log
- [ ] reset
- [ ] rm
- [/] status
- [ ] tag
"""

function repl_cmd(ex)
    cmd = split(ex)
    has_params = length(cmd) > 1
    repopath = pwd()

    if cmd[1] == "init"
        if has_params && cmd[2] == "help"
            repl_init_help()
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
            repo = clone(repourl, repopath, bare=("--bare" in cmd), )
            finalize(repo)
            println("Cloned $repourl into $repopath")
        catch ex
            repl_clone_help()
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
                    repl_status_help()
                    return
                end
            elseif cmd[1] == "add"
                if has_params && cmd[2] == "help"
                    repl_add_help()
                    return
                end
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
status\t Show the working tree status

For particular command parameters use: <command> help
""")
# branch\t\tList, create, or delete branches
# commit\t\tRecord changes to the repository
# fetch\t\tDownload objects and refs from another repository
# reset\t\tReset current HEAD to the specified state
# rm\t\tRemove files from the working tree and from the index
# tag\t\tCreate, list, delete or verify a tag object signed with GPG
end

function repl_init_help()
    println("usage: init [--bare]")
end

function repl_clone_help()
    println("usage: clone [--bare] <url>")
end

function repl_add_help()
    println("usage: add [<num-of-msgs>]")
end

function repl_status_help()
    println("usage: status")
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