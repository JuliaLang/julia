## process status ##

abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::Int32; end
type ProcessSignaled <: ProcessStatus; signal::Int32; end
type ProcessStopped  <: ProcessStatus; signal::Int32; end

process_exited  (s::Int32) = ccall(:jl_process_exited,   Int32, (Int32,), s) != 0
process_signaled(s::Int32) = ccall(:jl_process_signaled, Int32, (Int32,), s) != 0
process_stopped (s::Int32) = ccall(:jl_process_stopped,  Int32, (Int32,), s) != 0

process_exit_status(s::Int32) = ccall(:jl_process_exit_status, Int32, (Int32,), s)
process_term_signal(s::Int32) = ccall(:jl_process_term_signal, Int32, (Int32,), s)
process_stop_signal(s::Int32) = ccall(:jl_process_stop_signal, Int32, (Int32,), s)

function process_status(s::Int32)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## file descriptors and pipes ##

type FileDes; fd::Int32; end

global const STDIN  = FileDes(ccall(:jl_stdin,  Int32, ()))
global const STDOUT = FileDes(ccall(:jl_stdout, Int32, ()))
global const STDERR = FileDes(ccall(:jl_stderr, Int32, ()))

isequal(fd1::FileDes, fd2::FileDes) = (fd1.fd == fd2.fd)

hash(fd::FileDes) = hash(fd.fd)

show(io, fd::FileDes) =
    fd == STDIN  ? print(io, "STDIN")  :
    fd == STDOUT ? print(io, "STDOUT") :
    fd == STDERR ? print(io, "STDERR") :
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

isequal(p1::Pipe, p2::Pipe) = (p1.in == p2.in && p1.out == p2.out)

abstract PipeEnd
type PipeIn  <: PipeEnd; pipe::Pipe; end
type PipeOut <: PipeEnd; pipe::Pipe; end

isequal(p1::PipeEnd, p2::PipeEnd) = false
isequal(p1::PipeIn , p2::PipeIn ) = (p1.pipe == p2.pipe)
isequal(p1::PipeOut, p2::PipeOut) = (p1.pipe == p2.pipe)

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
    ret = ccall(:pipe, Int32, (Ptr{Int32},), fds)
    system_error(:make_pipe, ret != 0)
    Pipe(FileDes(fds[2]), FileDes(fds[1]))
end

## core system calls for processes ##

function fork()
    pid = ccall(:fork, Int32, ())
    system_error(:fork, pid < 0)
    return pid
end

# WARNING: do not call this and keep the returned array of pointers
# around longer than the args vector and then use array of pointers.
# this could cause a segfault. this is really just for use by the
# spawn function below so that we can exec more efficiently.
#
function _jl_pre_exec(args::Vector{ByteString})
    if length(args) < 1
        error("exec: too few words to exec")
    end
    ptrs = Array(Ptr{Uint8}, length(args)+1)
    for i = 1:length(args)
        ptrs[i] = args[i].data
    end
    ptrs[length(args)+1] = C_NULL
    return ptrs
end

function exec(args::Vector{ByteString})
    ptrs = _jl_pre_exec(args)
    ccall(:execvp, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}), ptrs[1], ptrs)
    system_error(:exec, true)
end

function exec(args::String...)
    arr = Array(ByteString, length(args))
    for i = 1:length(args)
        arr[i] = cstring(args[i])
    end
    exec(arr)
end

function wait(pid::Int32)
    status = Array(Int32,1)
    while true
        ret = ccall(:waitpid, Int32, (Int32, Ptr{Int32}, Int32), pid, status, 0)
        if ret != -1
            break
        end
        system_error(:wait, errno() != EINTR)
    end
    status[1]
end

exit(n) = ccall(:exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

function dup2(fd1::FileDes, fd2::FileDes)
    ret = ccall(:dup2, Int32, (Int32, Int32), fd1.fd, fd2.fd)
    system_error(:dup2, ret == -1)
end

function close(fd::FileDes)
    ret = ccall(:close, Int32, (Int32,), fd.fd)
    system_error(:close, ret != 0)
end

## Executable: things that can be exec'd ##

typealias Executable Union(Vector{ByteString},Function)

function exec(thunk::Function)
    try
        thunk()
    catch e
        show(io, e)
        exit(0xff)
    end
    exit(0)
end

type Cmd
    exec::Executable
    pipes::Dict{FileDes,PipeEnd}
    pipeline::Set{Cmd}
    pid::Int32
    status::ProcessStatus
    successful::Function

    function Cmd(exec::Executable)
        if isa(exec,Vector{ByteString}) && length(exec) < 1
            error("Cmd: too few words to exec")
        end
        this = new(exec,
                   Dict{FileDes,PipeEnd}(),
                   Set{Cmd}(),
                   0,
                   ProcessNotRun(),
                   status->status==0)
        add(this.pipeline, this)
        this
    end
end

setsuccess(cmd::Cmd, f::Function) = (cmd.successful=f; cmd)
ignorestatus(cmd::Cmd) = setsuccess(cmd, status->status!=0xff)

function show(io, cmd::Cmd)
    if isa(cmd.exec,Vector{ByteString})
        esc = shell_escape(cmd.exec...)
        print(io, '`')
        for c in esc
            if c == '`'
                print(io, '\\')
            end
            print(io, c)
        end
        print(io, '`')
    else
        invoke(show, (Any,), cmd.exec)
    end
end

exec(cmd::Cmd) = exec(cmd.exec)

## Port: a file descriptor on a particular command ##

type Port
    cmd::Cmd
    fd::FileDes
end

fd(cmd::Cmd, f::FileDes) = Port(cmd,f)

function fd(cmds::Set{Cmd}, f::FileDes)
    set = Set{Port}()
    for cmd in cmds
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

cmds(port::Port) = Set(port.cmd)

function cmds(ports::Ports)
    c = Set{Cmd}()
    for port in ports
        add(c, port.cmd)
    end
    return c
end

## building connected and disconnected pipelines ##

add_cmds(set::Set{Cmd}, cmd::Cmd) = add(set, cmd)
add_cmds(set::Set{Cmd}, cmds::Set{Cmd}) = add_each(set, cmds)

function (&)(cmds::Cmds...)
    set = Set{Cmd}()
    for cmd in cmds
        add_cmds(set, cmd)
    end
    set
end

add_ports(set::Set{Port}, port::Port) = add(set, port)
add_ports(set::Set{Port}, ports::Set{Port}) = add_each(set, ports)

function (&)(ports::Ports...)
    set = Set{Port}()
    for port in ports
        add_ports(set, port)
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
    for port in ports
        connect(port, pend)
    end
    return pend
end

function merge(cmds::Cmds)
    if numel(cmds) > 1
        pipeline = Set{Cmd}()
        for cmd in cmds
            add_each(pipeline, cmd.pipeline)
        end
        for cmd in pipeline
            cmd.pipeline = pipeline
        end
    end
end

function read_from(ports::Ports)
    merge(cmds(ports))
    other(connect(ports, in(make_pipe())))
end

function write_to(ports::Ports)
    merge(cmds(ports))
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
    merge(src.cmd & dst.cmd)
end

(|)(src::Port, dsts::Ports) = for dst = dsts; src | dst; end
(|)(srcs::Ports, dst::Port) = for src = srcs; src | dst; end
(|)(srcs::Ports, dsts::Ports) = for src = srcs, dst = dsts; src | dst; end

(|)(src::Cmds, dst::Ports) = stdout(src) | dst
(|)(src::Ports, dst::Cmds) = (src | stdin(dst); dst)
(|)(src::Cmds,  dst::Cmds) = (stdout(src) | stdin(dst); src & dst)

# spawn(cmd) starts all processes connected to cmd

function spawn(cmd::Cmd)
    fds = Set{FileDes}()
    fds_ = Set{FileDes}()
    for c in cmd.pipeline
        if !isa(cmd.status,ProcessNotRun)
            if isa(cmd.status,ProcessRunning)
                error("already running: ", c)
            else
                error("already run: ", c)
            end
        end
        for (f,p) in c.pipes
            add(fds, fd(p))
            add(fds, other(p))
            add(fds_, fd(p))
        end
    end
    gc_disable()
    for c = cmd.pipeline
        # minimize work after fork, in particular no writing
        c.status = ProcessRunning()
        ptrs = isa(c.exec,Vector{ByteString}) ? _jl_pre_exec(c.exec) : nothing
        dup2_fds = Array(Int32, 2*numel(c.pipes))
        close_fds_ = copy(fds)
        i = 0
        for (f,p) in c.pipes
            dup2_fds[i+=1] = fd(p).fd
            dup2_fds[i+=1] = f.fd
            del(close_fds_, fd(p))
        end
        close_fds = Array(Int32, numel(close_fds_))
        i = 0
        for f in close_fds_
            close_fds[i+=1] = f.fd
        end
        # now actually do the fork and exec without writes
        pid = fork()
        if pid == 0
            i = 1
            n = length(dup2_fds)
            while i <= n
                # dup2 manually inlined to avoid potential heap stomping
                r = ccall(:dup2, Int32, (Int32, Int32), dup2_fds[i], dup2_fds[i+1])
                if r == -1
                    println(stderr, "dup2: ", strerror())
                    exit(0xff)
                end
                i += 2
            end
            i = 1
            n = length(close_fds)
            while i <= n
                # close manually inlined to avoid potential heap stomping
                r = ccall(:close, Int32, (Int32,), close_fds[i])
                if r != 0
                    println(stderr, "close: ", strerror())
                    exit(0xff)
                end
                i += 1
            end
            if ptrs != nothing
                ccall(:execvp, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}), ptrs[1], ptrs)
                println(stderr, "exec: ", strerror())
                exit(0xff)
            end
            # other ways of execing (e.g. a julia function)
            gc_enable()
            c.pid = pid
            try
                exec(c)
            catch err
                show(stderr, err)
                exit(0xff)
            end
            error("exec should not return but has")
        end
        c.pid = pid
    end
    for f in fds_
        close(f)
    end
    gc_enable()
end

# spawn(cmds) starts all pipelines of a set of commands
#   they may not be connected, e.g.: `foo` & `bar`

function spawn(cmds::Cmds)
    for cmd in cmds
        if isa(cmd.status,ProcessNotRun)
            spawn(cmd)
        end
    end
end

# wait for a single command process to finish

successful(cmd::Cmd) = isa(cmd.status, ProcessExited) &&
                       cmd.successful(cmd.status.status)

function wait(cmd::Cmd)
    cmd.status = process_status(wait(cmd.pid))
    successful(cmd)
end

# wait for a set of command processes to finish

function wait(cmds::Cmds)
    success = true
    for cmd in cmds
        success &= wait(cmd)
    end
    success
end

# report what went wrong with a pipeline

pipeline_error(cmd::Cmd) = error("failed process: ", cmd)
function pipeline_error(cmds::Cmds)
    failed = Cmd[]
    for cmd in cmds
        if !successful(cmd)
            push(failed, cmd)
        end
    end
    if numel(failed) == 0
        error("pipeline error but no processes failed!?")
    end
    if numel(failed) == 1
        error("failed process: ", failed[1])
    end
    error("failed processes: ", join(failed, ", "))
end

# spawn and wait for a set of commands

success(cmds::Cmds) = (spawn(cmds); wait(cmds))
run(cmds::Cmds) = success(cmds) ? nothing : pipeline_error(cmds)

# run some commands and read all output

function _readall(ports::Ports, cmds::Cmds)
    r = read_from(ports)
    spawn(cmds)
    o = readall(fdio(r.fd, true))
    if !wait(cmds)
        pipeline_error(cmds)
    end
    return o
end

readall(ports::Ports) = _readall(ports, cmds(ports))
readall(cmds::Cmds)   = _readall(stdout(cmds), cmds)

function _each_line(ports::Ports, cmds::Cmds)
    r = read_from(ports)
    spawn(cmds)
    fh = fdio(r.fd, true)
    EachLine(fh)
end

each_line(ports::Ports) = _each_line(ports, cmds(ports))
each_line(cmds::Cmds)   = _each_line(stdout(cmds), cmds)

cmd_stdin_stream (cmds::Cmds) = fdio(write_to(cmds).fd)
cmd_stdout_stream(cmds::Cmds) = fdio(read_from(cmds).fd)

## implementation of `cmd` syntax ##

arg_gen(x::String) = ByteString[x]

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push(vals,cstring(x))
        end
        return vals
    else
        return ByteString[cstring(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push(vals, cstring(strcat(h,t)))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args, arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($_jl_shell_parse(str)))
end
