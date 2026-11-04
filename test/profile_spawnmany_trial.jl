# TRIAL, do not merge
function trial_gdb(label, env...; nhits = 300, sym = "_Uelf64_get_proc_name", ipreg = Sys.ARCH === :aarch64 ? "\$x2" : "\$rdx")
    dir = mktempdir()
    script = joinpath(dir, "cmds.gdb")
    write(script, """
        set pagination off
        set confirm off
        set auto-load off
        set startup-with-shell off
        set breakpoint pending on
        set print thread-events off
        set print inferior-events off
        handle all nostop noprint pass
        set \$hits = 0
        break $sym
        commands
        silent
        set \$hits = \$hits + 1
        set \$ip = $ipreg
        printf "HIT %d ip=%p\\n", \$hits, \$ip
        info symbol \$ip
        if \$hits <= 3
          bt 12
          x/8i \$ip - 16
        end
        if \$hits >= $nhits
          printf "HIT_LIMIT\\n"
          info breakpoints
          kill
          quit
        end
        continue
        end
        run
        info breakpoints
        printf "INFERIOR_ENDED hits=%d\\n", \$hits
        quit
        """)
    log = joinpath(dir, "gdb.log")
    julia = Base.julia_cmd().exec
    cmd = `gdb -q -nx --batch -x $script --args $julia --depwarn=error --startup-file=no profile_spawnmany_exec.jl`
    cmd = addenv(Cmd(cmd; dir = @__DIR__), "SPAWN_TRIAL" => label, env...)
    proc = run(pipeline(ignorestatus(cmd); stdout = log, stderr = log); wait = false)
    start = time()
    timer = Timer(_ -> kill(proc, Base.SIGKILL), 300)
    wait(proc)
    close(timer)
    lines = readlines(log)
    println(stderr, "SPAWN_TRIAL_GDB ", label, " exit=", proc.exitcode, " sig=", proc.termsignal, " secs=", round(time() - start; digits=1), " loglines=", length(lines))
    where = Dict{String,Int}()
    for (i, l) in enumerate(lines)
        if startswith(l, "HIT ") && i < length(lines)
            # Offsets vary within a function, so count by symbol and library
            s = replace(lines[i+1], r" \+ \d+" => "", r"No symbol matches.*" => "no symbol (not in a loaded library's symbols)")
            where[s] = get(where, s, 0) + 1
        end
    end
    for (s, c) in sort!(collect(where); by = last, rev = true)[1:min(end, 25)]
        println(stderr, "SPAWN_TRIAL_GDB ", label, " count=", c, " ", s)
    end
    first_hits = 0
    for l in lines
        startswith(l, "HIT ") && (first_hits += 1)
        first_hits > 4 && break
        println(stderr, "SPAWN_TRIAL_GDBLOG ", label, " ", first(l, 180))
    end
    for l in lines[max(1, end-6):end]
        println(stderr, "SPAWN_TRIAL_GDBEND ", label, " ", first(l, 180))
    end
end

function spawn_trial(phase)
    Sys.islinux() && Sys.ARCH in (:aarch64, :x86_64) && Sys.which("gdb") !== nothing || return
    for (label, env) in (("spin", ("SPAWN_WORKLOAD" => "spin",)),
                         ("spawn200k", ("NTASKS" => "200000", "SPAWN_WORKLOAD" => "spawn")),
                         ("spawn2M", ("NTASKS" => "2000000", "SPAWN_WORKLOAD" => "spawn")))
        try
            trial_gdb(label, env...)
        catch err
            println(stderr, "SPAWN_TRIAL_GDB ", label, " error ", sprint(showerror, err))
        end
    end
end
