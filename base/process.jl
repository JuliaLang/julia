## process status ##

abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::Int32; end
type ProcessSignaled <: ProcessStatus; signal::Int32; end
type ProcessStopped  <: ProcessStatus; signal::Int32; end

process_running (s::Int32) = s == -1
process_exited  (s::Int32) = ccall(:jl_process_exited,   Int32, (Int32,), s) != 0
process_signaled(s::Int32) = ccall(:jl_process_signaled, Int32, (Int32,), s) != 0
process_stopped (s::Int32) = ccall(:jl_process_stopped,  Int32, (Int32,), s) != 0

process_exit_status(s::Int32) = ccall(:jl_process_exit_status, Int32, (Int32,), s)
process_term_signal(s::Int32) = ccall(:jl_process_term_signal, Int32, (Int32,), s)
process_stop_signal(s::Int32) = ccall(:jl_process_stop_signal, Int32, (Int32,), s)

function process_status(s::Int32)
    process_running (s) ? ProcessRunning() :
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## file descriptors and pipes ##

type FileDes; fd::Int32; end

const STDIN  = FileDes(ccall(:jl_stdin,  Int32, ()))
const STDOUT = FileDes(ccall(:jl_stdout, Int32, ()))
const STDERR = FileDes(ccall(:jl_stderr, Int32, ()))

isequal(fd1::FileDes, fd2::FileDes) = (fd1.fd == fd2.fd)

hash(fd::FileDes) = hash(fd.fd)

show(io, fd::FileDes) =
    fd == STDIN  ? print(io, "STDIN")  :
    fd == STDOUT ? print(io, "STDOUT") :
    fd == STDERR ? print(io, "STDERR") :
    invoke(show, (Any, Any), io, fd)

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

pipe_in (p::Pipe) = PipeIn(p)
pipe_out(p::Pipe) = PipeOut(p)
pipe_in (p::PipeEnd) = pipe_in(p.pipe)
pipe_out(p::PipeEnd) = pipe_out(p.pipe)

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
        arr[i] = bytestring(args[i])
    end
    exec(arr)
end

function wait(pid::Int32, nohang::Bool)
    status = Int32[0]
    while true
        ret = ccall(:waitpid, Int32, (Int32, Ptr{Int32}, Int32), pid, status, nohang)
        if ret ==  0 return int32(-1) end
        if ret != -1 break end
        system_error(:wait, errno() != EINTR)
    end
    status[1]
end
wait(x) = wait(x,false)
wait_nohang(x) = wait(x,true)

exit(n) = ccall(:exit, Void, (Int32,), n)
_exit(n) = ccall(:_exit, Void, (Int32,), n)
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
        _exit(0x7f)
    end
    _exit(0)
end

type FileSink
    s::IOStream
    own::Bool
    function FileSink(s::IOStream, own::Bool)
        if fd(s) == -1
            error("Cannot use the given IOStream as FileSink")
        end
        this = new(s, own)
        if own
            finalizer(this, close_sink)
        end
        return this
    end
end

FileSink(s::IOStream) = FileSink(s, false)

function FileSink(filename::String, args...)
    s = open(filename, args...)
    return FileSink(s, true)
end

function close_sink(sink::FileSink)
    if sink.own
        close(sink.s)
    end
end

fd(sink::FileSink) = fd(sink.s)

type Cmd
    exec::Executable
    name::String
    pipes::Dict{FileDes,PipeEnd}
    sinks::Dict{FileDes,FileSink}
    closed_fds::Vector{FileDes}
    pipeline::Set{Cmd}
    pid::Int32
    status::ProcessStatus
    successful::Function

    function Cmd(exec::Executable)
        if isa(exec,Vector{ByteString}) && length(exec) < 1
            error("Cmd: too few words to exec")
        end
        this = new(exec,
                   "",
                   (FileDes=>PipeEnd)[],
                   (FileDes=>FileSink)[],
                   FileDes[],
                   Set{Cmd}(),
                   0,
                   ProcessNotRun(),
                   default_success)
        add(this.pipeline, this)
        this
    end
end

default_success(status::ProcessStatus) = false
default_success(status::ProcessExited) = status.status==0

ignore_success(status::ProcessStatus) = true
ignore_success(status::ProcessExited) = status.status!=0x7f

setsuccess(cmd::Cmd, f::Function) = (cmd.successful=f; cmd)
ignorestatus(cmd::Cmd) = setsuccess(cmd, ignore_success)

function show(io, cmd::Cmd)
    if cmd.name != ""
        show(io, cmd.name)
    elseif isa(cmd.exec,Vector{ByteString})
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
        invoke(show, (Any, Any,), io, cmd.exec)
    end
end

exec(cmd::Cmd) = exec(cmd.exec)

function close_sinks(cmd::Cmd)
    for (f,s) in cmd.sinks
        close_sink(s)
    end
end

## Port: a file descriptor on a particular command ##

type Port
    cmd::Cmd
    fd::FileDes
end

function fd(cmd::Cmd, f::FileDes)
    if !has(cmd.pipes, f) && !has(cmd.sinks, f) && !contains(cmd.closed_fds, f)
        return Port(cmd,f)
    end
    error("no ", f, " available in ", cmd)
end

function fd(cmds::Set{Cmd}, f::FileDes)
    set = Set{Port}()
    for cmd in cmds
        if !has(cmd.pipes, f) && !has(cmd.sinks, f) && !contains(cmd.closed_fds, f)
            add(set, fd(cmd,f))
        end
    end
    if isempty(set)
        error("no ", f, " available in ", cmds)
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
    if contains(port.cmd.closed_fds, port.fd)
        error(port.cmd, " port ", port.fd, " is closed")
    end
    if !has(port.cmd.pipes, port.fd) && !has(port.cmd.sinks, port.fd)
        port.cmd.pipes[port.fd] = pend
    elseif has(port.cmd.pipes, port.fd) && port.cmd.pipes[port.fd] != pend
        error(port.cmd, " is already connected to ",
              fd(port.cmd.pipes[port.fd]))
    elseif has(port.cmd.sinks, port.fd)
        error(port.cmd, " is already connected to ",
              fd(port.cmd.sinks[port.fd]))
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
    other(connect(ports, pipe_in(make_pipe())))
end

function write_to(ports::Ports)
    merge(cmds(ports))
    other(connect(ports, pipe_out(make_pipe())))
end

read_from(cmds::Cmds) = read_from(stdout(cmds))
write_to(cmds::Cmds) = write_to(stdin(cmds))

function (|)(src::Port, dst::Port)
    if has(dst.cmd.pipes, dst.fd)
        connect(src, pipe_in(dst.cmd.pipes[dst.fd]))
    elseif has(src.cmd.pipes, src.fd)
        connect(dst, pipe_out(src.cmd.pipes[src.fd]))
    else
        p = make_pipe()
        connect(src, pipe_in(p))
        connect(dst, pipe_out(p))
    end
    merge(src.cmd & dst.cmd)
end

(|)(src::Port, dsts::Ports) = for dst = dsts; src | dst; end
(|)(srcs::Ports, dst::Port) = for src = srcs; src | dst; end
(|)(srcs::Ports, dsts::Ports) = for src = srcs, dst = dsts; src | dst; end

(|)(src::Cmds, dst::Ports) = stdout(src) | dst
(|)(src::Ports, dst::Cmds) = (src | stdin(dst); dst)
(|)(src::Cmds,  dst::Cmds) = (stdout(src) | stdin(dst); src & dst)

redir(port::Port, sink::FileSink) = port.cmd.sinks[port.fd] = sink
function redir(ports::Ports, sink::FileSink)
    for port in ports
        redir(port, sink)
    end
end

# redirect stdout
function (>)(src::String, dst::Cmds)
    redir(stdin(dst), FileSink(src, "r"))
    return dst
end

(<)(dst::Cmds, src::String) = (>)(src, dst)

function (>)(src::IOStream, dst::Cmds)
    redir(stdin(dst), FileSink(src))
    return dst
end

(<)(dst::Cmds, src::IOStream) = (>)(src, dst)

function (>)(src::Cmds, dst::String)
    redir(stdout(src), FileSink(dst, "w"))
    return src
end

function (>>)(src::Cmds, dst::String)
    redir(stdout(src), FileSink(dst, "a"))
    return src
end

(<)(dst::String, src::Cmds) = (>)(src, dst)
(<<)(dst::String, src::Cmds) = (>>)(src, dst)

function (>)(src::Cmds, dst::IOStream)
    redir(stdout(src), FileSink(dst))
    return src
end

(<)(dst::IOStream, src::Cmds) = (>)(src, dst)

# redirect stderr
function (.>)(src::Cmds, dst::String)
    redir(stderr(src), FileSink(dst, "w"))
    return src
end

function (.>>)(src::Cmds, dst::String)
    redir(stderr(src), FileSink(dst, "a"))
    return src
end

(.<)(dst::String, src::Cmds) = (.>)(src, dst)
(.<<)(dst::String, src::Cmds) = (.>>)(src, dst)

function (.>)(src::Cmds, dst::IOStream)
    redir(stderr(src), FileSink(dst))
    return src
end

(.<)(dst::IOStream, src::Cmds) = (.>)(src, dst)

# redirect both stdout and stderr
function (&>)(src::Cmds, dst::String)
    redir(output(src), FileSink(dst, "w"))
    return src
end

function (&>>)(src::Cmds, dst::String)
    redir(output(src), FileSink(dst, "a"))
    return src
end

(&<)(dst::String, src::Cmds) = (&>)(src, dst)
(&<<)(dst::String, src::Cmds) = (&>>)(src, dst)

function (&>)(src::Cmds, dst::IOStream)
    redir(output(src), FileSink(dst))
    return src
end

(&<)(dst::IOStream, src::Cmds) = (&>)(src, dst)

# here-strings:
function (>>>)(src::String, dst::Cmds)
    hscmd = Cmd(()->print(src))
    push(hscmd.closed_fds, STDIN)
    push(hscmd.closed_fds, STDERR)
    hscmd.name = "here-string<" * src * ">"
    return hscmd | dst
end

#(<<<)(dst::Cmds, src::String) = (>>>)(src, dst)


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
        dup2_sinks = Array(Int32, 2*numel(c.sinks))
        close_fds_ = copy(fds)
        i = 0
        for (f,p) in c.pipes
            dup2_fds[i+=1] = fd(p).fd
            dup2_fds[i+=1] = f.fd
            del(close_fds_, fd(p))
        end
        i = 0
        for (f,s) in c.sinks
            dup2_sinks[i+=1] = fd(s)
            dup2_sinks[i+=1] = f.fd
        end
        close_fds = Array(Int32, numel(close_fds_))
        i = 0
        for f in close_fds_
            close_fds[i+=1] = f.fd
        end

        # save the stderr descriptor because it may be redirected, but we may need to
        # print errors from Julia
        bk_stderr_fd = ccall(:dup, Int32, (Int32,), STDERR.fd)
        if bk_stderr_fd == -1
            println(stderr_stream, "dup: ", strerror())
            exit(0x7f)
        end
        bk_stderr_stream = fdio(bk_stderr_fd, true)

        # now actually do the fork and exec without writes
        pid = fork()
        if pid == 0
            ccall(:setpgid, Int32, (Int32,Int32), 0, 0)
            i = 1
            n = length(dup2_fds)
            while i <= n
                # dup2 manually inlined to avoid potential heap stomping
                r = ccall(:dup2, Int32, (Int32, Int32), dup2_fds[i], dup2_fds[i+1])
                if r == -1
                    println(bk_stderr_stream, "dup2: ", strerror())
                    _exit(0x7f)
                end
                i += 2
            end
            i = 1
            n = length(dup2_sinks)
            while i <= n
                # dup2 manually inlined to avoid potential heap stomping
                r = ccall(:dup2, Int32, (Int32, Int32), dup2_sinks[i], dup2_sinks[i+1])
                if r == -1
                    println(bk_stderr_stream, "dup2: ", strerror())
                    _exit(0x7f)
                end
                i += 2
            end
            i = 1
            n = length(close_fds)
            while i <= n
                # close manually inlined to avoid potential heap stomping
                r = ccall(:close, Int32, (Int32,), close_fds[i])
                if r != 0
                    println(bk_stderr_stream, "close: ", strerror())
                    _exit(0x7f)
                end
                i += 1
            end
            if !isequal(ptrs, nothing)
                ccall(:execvp, Int32, (Ptr{Uint8}, Ptr{Ptr{Uint8}}), ptrs[1], ptrs)
                println(bk_stderr_stream, "exec: ", strerror())
                _exit(0x7f)
            end
            # other ways of execing (e.g. a julia function)
            gc_enable()
            c.pid = pid
            try
                exec(c)
            catch err
                show(bk_stderr_stream, err)
                _exit(0x7f)
            end
            error("exec should not return but has")
        end
        c.pid = pid
        ccall(:setpgid, Int32, (Int32,Int32), pid, pid)
        close(bk_stderr_stream) # do it manually since gc is disabled
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

successful(cmd::Cmd) =
    isa(cmd.status,ProcessRunning) || cmd.successful(cmd.status)

wait(cmd::Cmd, nohang::Bool) =
    (cmd.status = process_status(wait(cmd.pid,nohang)); close_sinks(cmd); successful(cmd))

# wait for a set of command processes to finish

function wait(cmds::Cmds, nohang::Bool)
    success = true
    for cmd in cmds
        success &= wait(cmd, nohang)
    end
    success
end

# report what went wrong with a pipeline

pipeline_error(cmd::Cmd) = error("failed process: ",cmd," [",cmd.status,"]")
function pipeline_error(cmds::Cmds)
    failed = Cmd[]
    for cmd in cmds
        if !successful(cmd)
            push(failed, cmd)
        end
    end
    if numel(failed)==0 error("WTF: pipeline error but no processes failed!?") end
    if numel(failed)==1 pipeline_error(failed[1]) end
    msg = "failed processes:"
    for cmd in failed
        msg = string(msg,"\n  ",cmd," [",cmd.status,"]")
    end
    error(msg)
end

# spawn and wait for a set of commands

success(cmds::Cmds) = (spawn(cmds); wait(cmds))
run(cmds::Cmds) = success(cmds) ? nothing : pipeline_error(cmds)

# run some commands and read all output

function _readall(ports::Ports, cmds::Cmds)
    r = read_from(ports)
    spawn(cmds)
    o = readall(fdio(r.fd, false))
    if !wait(cmds)
        close(r)
        pipeline_error(cmds)
    end
    close(r)
    return o
end

readall(ports::Ports) = _readall(ports, cmds(ports))
readall(cmds::Cmds)   = _readall(stdout(cmds), cmds)

function _each_line(ports::Ports, cmds::Cmds)
    r = read_from(ports)
    spawn(cmds)
    fh = fdio(r.fd, true)
    EachLine(fh, ()->wait(cmds))
end

each_line(ports::Ports) = _each_line(ports, cmds(ports))
each_line(cmds::Cmds)   = _each_line(stdout(cmds), cmds)

cmd_stdin_stream (cmds::Cmds) = fdio(write_to(cmds).fd)
cmd_stdout_stream(cmds::Cmds) = fdio(read_from(cmds).fd)

## implementation of `cmd` syntax ##

arg_gen(x::String) = ByteString[x]
arg_gen(cmd::Cmd)  = cmd.exec

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push(vals,string(x))
        end
        return vals
    else
        return ByteString[string(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h in head, t in tail
        push(vals,bytestring(strcat(h,t)))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args,arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($(_jl_shell_parse(str))))
end
