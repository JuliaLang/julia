# TRIAL, do not merge
function trial_cmd(env...; prefix = String[])
    cmd = `$prefix $(Base.julia_cmd().exec) --depwarn=error --rr-detach --startup-file=no profile_spawnmany_exec.jl`
    return addenv(Cmd(cmd; dir = @__DIR__), env...)
end

function trial_run(cmd, limit)
    proc = run(pipeline(ignorestatus(cmd); stdout = stderr, stderr); wait = false)
    start = time()
    timer = Timer(_ -> kill(proc, Base.SIGKILL), limit)
    wait(proc)
    close(timer)
    return proc, round(time() - start; digits=1)
end

function trial_tools()
    paranoid = try read("/proc/sys/kernel/perf_event_paranoid", String) catch err; string(typeof(err)) end
    println(stderr, "SPAWN_TRIAL_TOOLS paranoid=", strip(paranoid), " kernel=", strip(read(`uname -r`, String)),
            " page_size=", ccall(:jl_getpagesize, Clong, ()),
            " ", join(("$t=$(something(Sys.which(t), "none"))" for t in ("perf", "strace", "ltrace", "gdb", "lldb", "bpftrace")), " "))
    env = ("NTASKS" => "2000000", "SPAWN_WORKLOAD" => "spawn")
    if Sys.which("perf") !== nothing
        data = joinpath(mktempdir(), "perf.data")
        # The sandbox may refuse perf events, so report what happens and carry on
        proc, secs = trial_run(trial_cmd(env...; prefix = ["perf", "record", "-q", "-g", "-F", "499", "-o", data, "--"]), 400)
        println(stderr, "SPAWN_TRIAL_PERF record exit=", proc.exitcode, " secs=", secs)
        if success(proc)
            for sort in ("dso", "symbol")
                out = read(ignorestatus(`perf report -i $data --no-children --sort=$sort --stdio -g none`), String)
                for l in Iterators.take(Iterators.filter(l -> !isempty(l) && !startswith(l, '#'), eachline(IOBuffer(out))), 30)
                    println(stderr, "SPAWN_TRIAL_PERF ", sort, " ", l)
                end
            end
        end
    end
    if Sys.which("strace") !== nothing
        out = tempname()
        proc, secs = trial_run(trial_cmd(env...; prefix = ["strace", "-f", "-c", "-o", out]), 400)
        println(stderr, "SPAWN_TRIAL_STRACE exit=", proc.exitcode, " secs=", secs)
        isfile(out) && for l in Iterators.take(eachline(out), 16)
            println(stderr, "SPAWN_TRIAL_STRACE ", l)
        end
    end
end

function spawn_trial(phase)
    Sys.islinux() || return
    try
        trial_tools()
    catch err
        println(stderr, "SPAWN_TRIAL_TOOLS error ", sprint(showerror, err))
    end
    trial_run(trial_cmd("SPAWN_WORKLOAD" => "backtrace", "SPAWN_TRIAL" => "backtrace"), 400)
    for (workload, n) in (("spin", 0), ("spawn", 20000), ("spawn", 200000), ("spawn", 2000000)),
            arm in ("profile", "noprofile", "noprofile", "profile")
        env = ["NTASKS" => string(n), "SPAWN_TRIAL" => "$workload-$n-$arm", "SPAWN_WORKLOAD" => workload]
        arm == "noprofile" && push!(env, "SPAWN_NOPROFILE" => "1")
        proc, secs = trial_run(trial_cmd(env...), 400)
        println(stderr, "SPAWN_TRIAL_RESULT phase=", phase, " workload=", workload, " n=", n, " arm=", arm,
                " elapsed_s=", secs, " termsignal=", proc.termsignal)
    end
end
