type ProcessStatus
struct ProcessExited   <: ProcessStatus; status::Int32; end
struct ProcessSignaled <: ProcessStatus; signal::Int32; end
struct ProcessStopped  <: ProcessStatus; signal::Int32; end

process_exited(s::Int32) =
    (ccall(dlsym(JuliaDLHandle,"jl_process_exited"),   Int32, (Int32,), s) != 0)
process_signaled(s::Int32) =
    (ccall(dlsym(JuliaDLHandle,"jl_process_signaled"), Int32, (Int32,), s) != 0)
process_stopped(s::Int32) =
    (ccall(dlsym(JuliaDLHandle,"jl_process_stopped"),  Int32, (Int32,), s) != 0)

process_exit_status(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_exit_status"), Int32, (Int32,), s)
process_term_signal(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_term_signal"), Int32, (Int32,), s)
process_stop_signal(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_stop_signal"), Int32, (Int32,), s)

function process_status(s::Int32)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

function run(cmd::String, args...)
    pid = fork()
    if pid == 0
        try
            exec(cmd, args...)
        catch e
            show(e)
            exit(0xff)
        end
    end
    process_status(wait(pid))
end
