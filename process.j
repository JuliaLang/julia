type ProcessStatus
struct ProcessNotRun   <: ProcessStatus; end
struct ProcessRunning  <: ProcessStatus; end
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

process_success(s::ProcessStatus) = false
process_success(s::ProcessExited) = (s.status == 0)

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

hash(fd::FileDes) = hash(fd.fd)

show(fd::FileDes) =
    fd == STDIN  ? print("STDIN")  :
    fd == STDOUT ? print("STDOUT") :
    fd == STDERR ? print("STDERR") :
    invoke(show, (Any,), fd)

struct Pipe
    in::FileDes
    out::FileDes

    function Pipe(in::FileDes, out::FileDes)
        if in == out
            error("identical in and out file descriptors")
        end
        new(in,out)
    end
end

==(p1::Pipe, p2::Pipe) = (p1.in == p2.in && p1.out == p2.out)

type PipeEnd
struct PipeIn  <: PipeEnd; pipe::Pipe; end
struct PipeOut <: PipeEnd; pipe::Pipe; end

==(p1::PipeEnd, p2::PipeEnd) = false
==(p1::PipeIn , p2::PipeIn ) = (p1.pipe == p2.pipe)
==(p1::PipeOut, p2::PipeOut) = (p1.pipe == p2.pipe)

in (p::Pipe) = PipeIn(p)
out(p::Pipe) = PipeOut(p)
in (p::PipeEnd) = in(p.pipe)
out(p::PipeEnd) = out(p.pipe)

fd(p::PipeIn)  = p.pipe.in
fd(p::PipeOut) = p.pipe.out
other(p::PipeIn)  = p.pipe.out
other(p::PipeOut) = p.pipe.in

function make_pipe()
    fds = Array(Int32, 2)
    ret = ccall(dlsym(libc,"pipe"), Int32, (Ptr{Int32},), fds)
    system_error("make_pipe", ret != 0)
    Pipe(FileDes(fds[2]), FileDes(fds[1]))
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
    pipes::HashTable{FileDes,PipeEnd}
    pipeline::Set{Cmd}
    pid::Int32
    status::ProcessStatus

    function Cmd(exec::Executable)
        this = new(exec,
                   HashTable(FileDes,PipeEnd),
                   Set(Cmd),
                   0,
                   ProcessNotRun())
        add(this.pipeline, this)
        this
    end
    Cmd(cmd::String, args...) = Cmd((cmd,args))
end

function show(cmd::Cmd)
    if isa(cmd.exec,(String,Tuple))
        esc = shell_escape(cmd.exec[1], cmd.exec[2]...)
        print('`')
        for c = esc
            if c == '`'
                print('\\')
            end
            print(c)
        end
        print('`')
    else
        invoke(show, (Any,), cmd)
    end
end

exec(cmd::Cmd) = exec(cmd.exec)

==(c1::Cmd, c2::Cmd) = is(c1,c2)

struct Port
    cmd::Cmd
    fd::FileDes
end

fd(cmd::Cmd, f::FileDes) = Port(cmd,f)

function fd(cmds::Set{Cmd}, f::FileDes)
    set = Set(Port)
    for cmd = cmds
        if !has(cmd.pipes, f)
            add(set, fd(cmd,f))
        end
    end
    if isempty(set)
        error("no ", f, " available: ", cmds)
    end
    set
end

typealias Cmds  Union(Cmd,Set{Cmd})
typealias Ports Union(Port,Set{Port})

stdin (cmds::Cmds) = fd(cmds,STDIN)
stdout(cmds::Cmds) = fd(cmds,STDOUT)
stderr(cmds::Cmds) = fd(cmds,STDERR)

function (&)(cmds::Cmds...)
    set = Set(Cmd)
    for cmd = cmds
        add(set, cmd)
    end
    set
end

function (&)(ports::Ports...)
    set = Set(Port)
    for port = ports
        add(set, port)
    end
    set
end

output(cmds::Cmds) = stdout(cmds) & stderr(cmds)

function connect(port::Port, pend::PipeEnd)
    if !has(port.cmd.pipes, port.fd)
        port.cmd.pipes[port.fd] = pend
    elseif port.cmd.pipes[port.fd] != pend
        error(port.cmd, " is already connected to ",
              fd(port.cmd.pipes[port.fd]))
    end
end

function (|)(src::Port, dst::Port)
    if has(dst.cmd.pipes, dst.fd)
        connect(src, in(dst.cmd.pipes[dst.fd]))
    elseif has(src.cmd.pipes, src.fd)
        connect(dst, out(src.cmd.pipes[src.fd]))
    else
        p = make_pipe()
        connect(src, in(p))
        connect(dst, out(p))
    end
    pipeline = union(src.cmd.pipeline, dst.cmd.pipeline)
    for cmd = pipeline
        cmd.pipeline = pipeline
    end
    return ()
end

(|)(src::Port, dsts::Ports) = for dst = dsts; src | dst; end
(|)(srcs::Ports, dst::Port) = for src = srcs; src | dst; end
(|)(srcs::Ports, dsts::Ports) = for src = srcs, dst = dsts; src | dst; end

(|)(src::Cmds, dst::Ports) = stdout(src) | dst
(|)(src::Ports, dst::Cmds) = (src | stdin(dst); dst)
(|)(src::Cmds,  dst::Cmds) = (stdout(src)| stdin(dst); src & dst)

running(cmd::Cmd) = (cmd.pid > 0)

function spawn(cmd::Cmd)
    fds = Set(FileDes)
    for c = cmd.pipeline
        if running(c)
            error("already running: ", c)
        end
        for (f,p) = c.pipes
            add(fds, fd(p), other(p))
        end
    end
    for c = cmd.pipeline
        c.pid = fork()
        c.status = ProcessRunning()
        if c.pid == 0
            try
                for (f,p) = c.pipes
                    dup2(fd(p), f)
                    del(fds, fd(p))
                end
                for f = fds
                    close(f)
                end
                exec(c)
            catch err
                show(err)
                exit(0xff)
            end
            exit(0)
        end
    end
    for f = fds
        close(f)
    end
    cmd.pipeline
end

function run(cmds::Cmds)
    pipeline = Set(Cmd)
    add(pipeline, cmds)
    for cmd = pipeline
        if !running(cmd)
            spawn(cmd)
        end
    end
    success = true
    for cmd = pipeline
        cmd.status = process_status(wait(cmd.pid))
        success &= process_success(cmd.status)
    end
    success
end

macro cmd(str)
    quote Cmd($shell_split(str)...) end
end
