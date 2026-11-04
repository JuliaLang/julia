# TRIAL, do not merge
function cgroup_cpu()
    dirs = String["/sys/fs/cgroup", "/sys/fs/cgroup/cpu"]
    for line in eachline("/proc/self/cgroup")
        p = split(line, ':'; limit=3)[3]
        push!(dirs, "/sys/fs/cgroup" * p, "/sys/fs/cgroup/cpu" * p)
    end
    out = String[]
    for d in unique(dirs), f in ("cpu.max", "cpu.cfs_quota_us", "cpu.stat")
        path = joinpath(d, f)
        try
            isfile(path) && push!(out, string(path, ": ", join(split(read(path, String)), ' ')))
        catch err
            push!(out, string(path, ": ", typeof(err)))
        end
    end
    return isempty(out) ? "no cgroup cpu files visible" : join(out, " | ")
end

function spawn_trial(phase)
    Sys.islinux() || return
    println(stderr, "SPAWN_TRIAL_CGROUP phase=", phase, " before ", cgroup_cpu())
    for arm in ("profile", "noprofile", "noprofile", "profile")
        env = ["NTASKS" => "2000000", "SPAWN_TRIAL" => "$phase-$arm"]
        arm == "noprofile" && push!(env, "SPAWN_NOPROFILE" => "1")
        cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no profile_spawnmany_exec.jl`
        cmd = ignorestatus(addenv(Cmd(cmd; dir = @__DIR__), env...))
        load = read("/proc/loadavg", String)
        proc = run(pipeline(cmd; stdout = stderr, stderr); wait = false)
        start = time()
        timer = Timer(_ -> kill(proc, Base.SIGKILL), 400)
        wait(proc)
        close(timer)
        println(stderr, "SPAWN_TRIAL_RESULT phase=", phase, " arm=", arm, " elapsed_s=", round(time() - start; digits=1),
                " termsignal=", proc.termsignal, " load_before=", join(split(load)[1:2], ','))
        println(stderr, "SPAWN_TRIAL_CGROUP phase=", phase, " arm=", arm, " after ", cgroup_cpu())
    end
end
