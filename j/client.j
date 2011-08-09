## client.j - frontend handling command line options, environment setup,
##            and REPL

roottask = current_task()
roottask_wi = WorkItem(roottask)

function repl_callback(ast, show_value)
    # use root task to execute user input
    roottask_wi.argument = (ast, show_value)
    perform_work(roottask_wi)
end

function run_repl()
    ccall(:repl_callback_enable, Void, ())

    while true
        add_fd_handler(STDIN.fd, fd->ccall(:jl_stdin_callback, Void, ()))
        (ast, show_value) = yield()
        del_fd_handler(STDIN.fd)
        roottask_wi.requeue = true
        ccall(:jl_eval_user_input, Void, (Any, Int32),
              ast, show_value)
        roottask_wi.requeue = false
    end
end

function parse_input_line(s::String)
    s = cstring(s)
    # (expr, pos) = parse(s, 1, true)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{Uint8},Int32,Int32),
    #                   s, int32(pos)-1, 1)
    # if !is(ex,())
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{Uint8},), s)
end

function process_options(args::Array{Any,1})
    quiet = false
    repl = true
    if has(ENV, "JL_POST_BOOT")
        eval(parse_input_line(ENV["JL_POST_BOOT"]))
    end
    i = 1
    while i <= length(args)
        if args[i]=="-q" || args[i]=="--quiet"
            quiet = true
        elseif args[i]=="--worker"
            start_worker()
            # doesn't return
        elseif args[i]=="-e"
            # TODO: support long options
            repl = false
            i+=1
            eval(parse_input_line(args[i]))
        elseif args[i]=="-E"
            repl = false
            i+=1
            show(eval(parse_input_line(args[i])))
            println()
        elseif args[i]=="-P"
            i+=1
            eval(parse_input_line(args[i]))
        elseif args[i]=="-L"
            i+=1
            load(args[i])
        elseif args[i]=="-p"
            i+=1
            np = int32(args[i])
            addprocs_local(np-1)
        elseif args[i][1]!='-'
            # program
            repl = false
            global ARGS
            # remove julia's arguments
            ARGS = ARGS[i:end]
            load(args[i])
            exit(0)
        else
            error("unknown option: ", args[i])
        end
        i += 1
    end
    return (quiet,repl)
end

function color_available()
    if run(`tput setaf 0`)
        return true
    end
    if has(ENV, "TERM")
        term = ENV["TERM"]
        return term=="xterm" || term=="xterm-color"
    end
    false
end

jl_version_string = "Version $VERSION_STRING"
jl_version_clean = VERSION_CLEAN ? "" : "*"
jl_commit_string = "Commit $(VERSION_COMMIT[1:10]) ($VERSION_TIME)$jl_version_clean"

jl_banner_plain =
I"               _
   _       _ _(_)_     |
  (_)     | (_) (_)    |  A fresh approach to technical computing
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |  $jl_version_string
  | | |_| | | | (_| |  |  $jl_commit_string
 _/ |\__'_|_|_|\__'_|  |
|__/                   |

"

begin
local tx = "\033[0m\033[1m" # text
local jl = "\033[0m\033[1m" # julia
local d1 = "\033[34m" # first dot
local d2 = "\033[31m" # second dot
local d3 = "\033[32m" # third dot
local d4 = "\033[35m" # fourth dot
jl_banner_color =
"\033[1m               $(d3)_
   $(d1)_       $(jl)_$(tx) $(d2)_$(d3)(_)$(d4)_$(tx)     |
  $(d1)(_)$(jl)     | $(d2)(_)$(tx) $(d4)(_)$(tx)    |  A fresh approach to technical computing
   $(jl)_ _   _| |_  __ _$(tx)   |
  $(jl)| | | | | | |/ _` |$(tx)  |  $jl_version_string
  $(jl)| | |_| | | | (_| |$(tx)  |  $jl_commit_string
 $(jl)_/ |\\__'_|_|_|\\__'_|$(tx)  |
$(jl)|__/$(tx)                   |

\033[0m"
end

function _start()
    try
        ccall(:jl_register_toplevel_eh, Void, ())
        ccall(:jl_start_io_thread, Void, ())
        global Workqueue = {}
        global Waiting = HashTable(64)

        if !anyp(a->(a=="--worker"), ARGS)
            # start in "head node" mode
            global Scheduler = Task(()->event_loop(true), 1024*1024)
            global PGRP = ProcessGroup(1, {LocalProcess()}, {Location("",0)})
        else
            global PGRP = ProcessGroup(0, {}, {})
        end

        global VARIABLES = {}

        # Load customized startup
        try
            load(strcat(getcwd(),"/custom.j"))
        catch
        end

        (quiet,repl) = process_options(ARGS)

        if repl
            if !quiet
                if color_available()
                    print(jl_banner_color)
                else
                    print(jl_banner_plain)
                end
            end
            run_repl()
        end
    catch e
        show(e)
        println()
    end
end
