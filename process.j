type ProcessStatus
struct ProcessExited   <: ProcessStatus; status::Int32; end
struct ProcessSignaled <: ProcessStatus; signal::Int32; end
struct ProcessStopped  <: ProcessStatus; signal::Int32; end

process_exited(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_exited"),   Int32, (Int32,), s) != 0
process_signaled(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_signaled"), Int32, (Int32,), s) != 0
process_stopped(s::Int32) =
    ccall(dlsym(JuliaDLHandle,"jl_process_stopped"),  Int32, (Int32,), s) != 0

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

global STDIN  = FileDes(ccall(dlsym(JuliaDLHandle,"jl_stdin"),  Int32, ()))
global STDOUT = FileDes(ccall(dlsym(JuliaDLHandle,"jl_stdout"), Int32, ()))
global STDERR = FileDes(ccall(dlsym(JuliaDLHandle,"jl_stderr"), Int32, ()))

==(fd1::FileDes, fd2::FileDes) = (fd1.fd == fd2.fd)

show(fd::FileDes) =
    fd == STDIN  ? print("STDIN")  :
    fd == STDOUT ? print("STDOUT") :
    fd == STDERR ? print("STDERR") :
    invoke(show, (Any,), fd)

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

typealias Executable Union((String,Tuple),Function)

exec(cmd::(String,Tuple)) = exec(cmd[1], cmd[2]...)
exec(thunk::Function) = thunk()

struct Cmd
    exec::Executable
    spawn::Set{Cmd}
    close::Set{FileDes}
    dup2::Set # TODO: Set{(FileDes,FileDes)}
    pid::Int32

    Cmd(exec::Executable) = new(exec, Set(Cmd), Set(FileDes), Set(), 0)
    Cmd(cmd::String, args...) = Cmd((cmd,args))
end

show(cmd::Cmd) = show(cmd.exec)
exec(cmd::Cmd) = exec(cmd.exec)

==(c1::Cmd, c2::Cmd) = is(c1,c2)

struct Port
    cmd::Cmd
    fd::FileDes
end

fd(cmd::Cmd, f::FileDes) = Port(cmd,f)

stdin (cmd::Cmd) = fd(cmd,STDIN)
stdout(cmd::Cmd) = fd(cmd,STDOUT)
stderr(cmd::Cmd) = fd(cmd,STDERR)

function pipe(src::Port, dst::Port)
    r, w = make_pipe()
    add(src.cmd.close, r, w)
    add(src.cmd.close, dst.cmd.close)
    dst.cmd.close = src.cmd.close
    add(src.cmd.spawn, dst.cmd)
    add(src.cmd.dup2, (w, src.fd))
    add(dst.cmd.spawn, src.cmd)
    add(dst.cmd.dup2, (r, dst.fd))
    dst.cmd
end

pipe(src::Port, dst::Cmd ) = pipe(src, stdin(dst))
pipe(src::Cmd , dst::Port) = pipe(stdout(src), dst)
pipe(src::Cmd , dst::Cmd ) = pipe(stdout(src), stdin(dst))

(|)(src::Union(Cmd,Port), dst::Union(Cmd,Port)) = pipe(src,dst)

running(cmd::Cmd) = (cmd.pid > 0)

function spawn(cmd::Cmd, root::Bool)
    if running(cmd)
        error("already running: ", cmd)
    end
    # spawn this process
    cmd.pid = fork()
    if cmd.pid == 0
        try
            cl = Set(FileDes)
            add(cl,cmd.close)
            for (fd1,fd2) = cmd.dup2
                if fd1 != fd2
                    dup2(fd1,fd2)
                    del(cl,fd1)
                end
            end
            for fd = cl
                close(fd)
            end
            exec(cmd)
        catch err
            show(err)
            exit(0xff)
        end
        exit(0)
    end
    # spawn rest of pipeline
    cmds = Set(Cmd)
    add(cmds, cmd)
    for c = cmd.spawn
        if !running(c)
            add(cmds, spawn(c,false))
        end
    end
    # close all child desciptors
    if root
        for fd = cmd.close
            close(fd)
        end
    end
    # return spawned commands
    cmds
end

spawn(cmd::Cmd) = spawn(cmd,true)

function run(cmd::Cmd)
    statuses = Set(ProcessStatus)
    for cmd = spawn(cmd)
        add(statuses, process_status(wait(cmd.pid)))
    end
    statuses
end
