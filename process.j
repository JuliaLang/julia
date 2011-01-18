## process status ##

abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::Int32; end
type ProcessSignaled <: ProcessStatus; signal::Int32; end
type ProcessStopped  <: ProcessStatus; signal::Int32; end

process_exited(s::Int32)   = ccall(:jl_process_exited,   Int32, (Int32,), s) != 0
process_signaled(s::Int32) = ccall(:jl_process_signaled, Int32, (Int32,), s) != 0
process_stopped(s::Int32)  = ccall(:jl_process_stopped,  Int32, (Int32,), s) != 0

process_exit_status(s::Int32) = ccall(:jl_process_exit_status, Int32, (Int32,), s)
process_term_signal(s::Int32) = ccall(:jl_process_term_signal, Int32, (Int32,), s)
process_stop_signal(s::Int32) = ccall(:jl_process_stop_signal, Int32, (Int32,), s)

function process_status(s::Int32)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

process_success(s::ProcessStatus) = false
process_success(s::ProcessExited) = (s.status == 0)

## file descriptors and pipes ##

type FileDes; fd::Int32; end

global STDIN  = FileDes(ccall(:jl_stdin,  Int32, ()))
global STDOUT = FileDes(ccall(:jl_stdout, Int32, ()))
global STDERR = FileDes(ccall(:jl_stderr, Int32, ()))

==(fd1::FileDes, fd2::FileDes) = (fd1.fd == fd2.fd)

hash(fd::FileDes) = hash(fd.fd)

show(fd::FileDes) =
    fd == STDIN  ? print("STDIN")  :
    fd == STDOUT ? print("STDOUT") :
    fd == STDERR ? print("STDERR") :
    invoke(show, (Any,), fd)

type Pipe
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

abstract PipeEnd
type PipeIn  <: PipeEnd; pipe::Pipe; end
type PipeOut <: PipeEnd; pipe::Pipe; end

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
    ret = ccall(dlsym(libc, :pipe), Int32, (Ptr{Int32},), fds)
    system_error(:make_pipe, ret != 0)
    Pipe(FileDes(fds[2]), FileDes(fds[1]))
end

## core system calls for processes ##

function fork()
    pid = ccall(dlsym(libc, :fork), Int32, ())
    system_error(:fork, pid < 0)
    return pid
end

function exec(cmd::String, args...)
    cmd = cstring(cmd)
    arr = Array(Ptr{Uint8}, length(args)+2)
    arr[1] = cmd.data
    for i = 1:length(args)
        arr[i+1] = cstring(args[i]).data
    end
    arr[length(args)+2] = C_NULL
    ccall(dlsym(libc, :execvp), Int32,
          (Ptr{Uint8}, Ptr{Ptr{Uint8}}),
          arr[1], arr)
    system_error(:exec, true)
end

function wait(pid::Int32)
    status = Array(Int32,1)
    ret = ccall(dlsym(libc, :waitpid), Int32,
              (Int32, Ptr{Int32}, Int32),
              pid, status, 0)
    system_error(:wait, ret == -1)
    status[1]
end

exit(n) = ccall(dlsym(libc, :exit), Void, (Int32,), int32(n))
exit() = exit(0)

function dup2(fd1::FileDes, fd2::FileDes)
    ret = ccall(dlsym(libc, :dup2), Int32, (Int32, Int32), fd1.fd, fd2.fd)
    system_error(:dup2, ret == -1)
end

function close(fd::FileDes)
    ret = ccall(dlsym(libc, :close), Int32, (Int32,), fd.fd)
    system_error(:close, ret != 0)
end

## Executable: things that can be exec'd ##

typealias Executable Union((String,Tuple),Function)

exec(cmd::(String,Tuple)) = exec(cmd[1], cmd[2]...)
exec(thunk::Function) = thunk()

type Cmd
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

## Port: a file descriptor on a particular command ##

type Port
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

cmds(port::Port) = set(port.cmd)

function cmds(ports::Ports)
    c = Set(Cmd)
    for port = ports
        add(c, port.cmd)
    end
    return c
end

## building connected and disconnected pipelines ##

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
    return pend
end

function connect(ports::Ports, pend::PipeEnd)
    for port = ports
        connect(port, pend)
    end
    return pend
end

function join(cmds::Cmds)
    if length(cmds) > 1
        pipeline = Set(Cmd)
        for cmd = cmds
            add(pipeline, cmd.pipeline)
        end
        for cmd = pipeline
            cmd.pipeline = pipeline
        end
    end
end

function read_from(ports::Ports)
    join(cmds(ports))
    other(connect(ports, in(make_pipe())))
end

function write_to(ports::Ports)
    join(cmds(ports))
    other(connect(ports, out(make_pipe())))
end

read_from(cmds::Cmds) = read_from(stdout(cmds))
write_to(cmds::Cmds) = write_to(stdin(cmds))

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
    join(src.cmd & dst.cmd)
end

(|)(src::Port, dsts::Ports) = for dst = dsts; src | dst; end
(|)(srcs::Ports, dst::Port) = for src = srcs; src | dst; end
(|)(srcs::Ports, dsts::Ports) = for src = srcs, dst = dsts; src | dst; end

(|)(src::Cmds, dst::Ports) = stdout(src) | dst
(|)(src::Ports, dst::Cmds) = (src | stdin(dst); dst)
(|)(src::Cmds,  dst::Cmds) = (stdout(src)| stdin(dst); src & dst)

## running commands and pipelines ##

running(cmd::Cmd) = (cmd.pid > 0)

# spawn(cmd) starts all processes connected to cmd

function spawn(cmd::Cmd)
    fds = Set(FileDes)
    for c = cmd.pipeline
        if running(c)
            error("already running: ", c)
        end
        for (f,p) = c.pipes
            add(fds, fd(p))
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
end

# spawn(cmds) starts all pipelines of a set of commands
#   they may not be connected, e.g.: `foo` & `bar`

function spawn(cmds::Cmds)
    for cmd = cmds
        if !running(cmd)
            spawn(cmd)
        end
    end
end

# wait for a single command process to finish

function wait(cmd::Cmd)
    cmd.status = process_status(wait(cmd.pid))
    process_success(cmd.status)
end

# wait for a set of command processes to finish

function wait(cmds::Cmds)
    success = true
    for cmd = cmds
        success &= wait(cmd)
    end
    success
end

# spawn and wait for a set of commands

function run(cmds::Cmds)
    spawn(cmds)
    wait(cmds)
end

# run some commands and read all output

function _readall(ports::Ports, cmds::Cmds)
    r = read_from(ports)
    spawn(cmds)
    o = readall(fdio(r.fd))
    if !wait(cmds)
        error("pipeline failed: $cmds")
    end
    return o
end

readall(ports::Ports) = _readall(ports, cmds(ports))
readall(cmds::Cmds) = _readall(stdout(cmds), cmds)

function _each_line(ports::Ports, cmds::Cmds)
    local fh
    create = @thunk begin
        r = read_from(ports)
        spawn(cmds)
        fh = fdio(r.fd)
        LineIterator(fh)
    end
    destroy = @thunk close(fh)
    ShivaIterator(create, destroy)
end

each_line(ports::Ports) = _each_line(ports, cmds(ports))
each_line(cmds::Cmds) = _each_line(stdout(cmds), cmds)

## implementation of `cmd` syntax ##

arg_gen(x::String) = (x,)

function arg_gen(head)
    if applicable(start,head)
        vals = ()
        for x = head
            vals = append(vals,(string(x),))
        end
        return vals
    else
        return (string(head),)
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ()
    for h = head, t = tail
        vals = append(vals,(strcat(h,t),))
    end
    vals
end

function cmd_gen(parsed)
    args = ()
    for arg = parsed
        args = append(args,arg_gen(arg...))
    end
    Cmd(args...)
end

macro cmd(str)
    :(cmd_gen($shell_parse(str)))
end
