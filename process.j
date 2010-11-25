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

struct FileDes; fd::Int32; end

global STDIN  = FileDes((()->ccall(dlsym(JuliaDLHandle,"jl_stdin"),  Int32, ()))())
global STDOUT = FileDes((()->ccall(dlsym(JuliaDLHandle,"jl_stdout"), Int32, ()))())
global STDERR = FileDes((()->ccall(dlsym(JuliaDLHandle,"jl_stderr"), Int32, ()))())

function make_pipe()
    fds = Array(Int32, 2)
    ret = ccall(dlsym(libc,"pipe"), Int32, (Ptr{Int32},), fds)
    system_error("make_pipe", ret != 0)
    FileDes(fds[1]), FileDes(fds[2])
end

function dup2(fd1::FileDes, fd2::FileDes)
    ret = ccall(dlsym(libc,"dup2"), Int32, (Int32, Int32), fd1.fd, fd2.fd)
    system_error("dup2", ret == -1)
end

function close(fd::FileDes)
    ret = ccall(dlsym(libc,"close"), Int32, (Int32,), fd.fd)
    system_error("close", ret != 0)
end

function pipe(cmd1, cmd2)
    r,w = make_pipe()
    pid1 = fork()
    if pid1 == 0
        try
            close(r)
            dup2(w,STDOUT)
            exec(cmd1...)
        catch e
            show(e)
            exit(0xff)
        end
    end
    close(w)
    pid2 = fork()
    if pid2 == 0
        try
            dup2(r,STDIN)
            exec(cmd2...)
        catch e
            show(e)
            exit(0xff)
        end
    end
    close(r)
    wait(pid1)
    process_status(wait(pid2))
end
