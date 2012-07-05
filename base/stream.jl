#TODO: allocate unused (spawn-only) streams on the stack if possible & safe
#TODO: function readline(???)
#TODO: function writeall(Cmd, String)
#TODO: function ignorestatus(Cmd|Process)
#TODO: stop leaking all the handles (process, closure, I/O)
#TODO: cleanup methods duplicated with io.jl
#TODO: fix examples in manual (run return value, STDIO parameters, const first, dup)
#TODO: remove ProcessStatus if not used
#TODO: allow waiting on handles other than processes
#TODO: don't allow waiting on close'd handles
#TODO: libuv process_events w/o blocking


typealias PtrSize Int
const UVHandle = Ptr{Void}
const IOStreamHandle = Ptr{Void}
localEventLoop() = ccall(:jl_local_event_loop,Ptr{Void},())
globalEventLoop() = ccall(:jl_global_event_loop,Ptr{Void},())
mkNewEventLoop() = ccall(:jl_new_event_loop,Ptr{Void},())

typealias Executable Union(Vector{ByteString},Function)
typealias Callback Union(Function,Bool)

abstract AsyncStream <: Stream
typealias StreamOrNot Union(Bool,AsyncStream)
typealias BufOrNot Union(Bool,IOStream)
typealias StdIOSet (StreamOrNot, StreamOrNot, StreamOrNot)

abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Executable
    Cmd(exec::Executable) = new(exec)
end

type OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

#typealias StreamHandle Union(PtrSize,AsyncStream)

type Process
    cmd::Cmd
    handle::Ptr{Void}
    in::StreamOrNot
    out::StreamOrNot
    err::StreamOrNot
    exit_code::Int32
    term_signal::Int32
    exitcb::Callback
    closecb::Callback
    Process(cmd::Cmd,handle::Ptr{Void},in::StreamOrNot,out::StreamOrNot,err::StreamOrNot)=new(cmd,handle,in,out,err,-2,-2,false,false)
end

type ProcessChain
    processes::Vector{Process}
    in::StreamOrNot
    out::StreamOrNot
    err::StreamOrNot
    ProcessChain(stdios::StdIOSet) = new(Process[],stdios[1],stdios[2],stdios[3])
end
typealias ProcessChainOrNot Union(Bool,ProcessChain)

const _jl_wait_for = Union(Process,)[]
function _jl_wait_for_(p::Process)
    if !process_exited(p)
        push(_jl_wait_for, p)
    end
end
_jl_wait_for_(pc::ProcessChain) = map(_jl_wait_for_, pc.processes)

type NamedPipe <: AsyncStream
    read_handle::Ptr{Void}
    write_handle::Ptr{Void}
    buf::BufOrNot
    closed::Bool
    NamedPipe(read_handle::Ptr{Void},write_handle::Ptr{Void},buf::IOStream) = new(read_handle,write_handle,buf,false)
    NamedPipe(read_handle::Ptr{Void},write_handle::Ptr{Void}) = new(read_handle,write_handle,false,false)
end

type TTY <: AsyncStream
    handle::Ptr{Void}
    buf::BufOrNot
    closed::Bool
end


convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)
read_handle(s::AsyncStream) = s.handle
write_handle(s::AsyncStream) = s.handle
read_handle(s::NamedPipe) = s.read_handle
write_handle(s::NamedPipe) = s.write_handle
read_handle(s::Bool) = s ? error("cannot get read handle of true") : C_NULL
write_handle(s::Bool) = s ? error("cannot get write handle of false") : C_NULL

make_stdout_stream() = TTY(ccall(:jl_stdout_stream, Ptr{Void}, ()),memio(),false)

function _uv_tty2tty(handle::Ptr{Void})
    TTY(handle,memio(),false)
end

OUTPUT_STREAM = make_stdout_stream()

## SOCKETS ##

abstract Socket <: AsyncStream

type TcpSocket <: Socket
    handle::Ptr{Void}
    buf::BufOrNot
    open::Bool
    TcpSocket(handle::Ptr{Void})=new(handle,false,false)
end

type UdpSocket <: Socket
    handle::Ptr{Void}
    buf::BufOrNot
    open::Bool
    UdpSocket(handle::Ptr{Void})=new(handle,false,false)
end

function _init_buf(stream::AsyncStream)
    if(!isa(stream.buf,IOStream))
        stream.buf=memio()
    end
end

_jl_tcp_init(loop::Ptr{Void}) = ccall(:jl_tcp_init,Ptr{Void},(Ptr{Void},),loop)
_jl_udp_init(loop::Ptr{Void}) = ccall(:jl_udp_init,Ptr{Void},(Ptr{Void},),loop)

abstract IpAddr

type Ip4Addr <: IpAddr
    port::Uint16
    host::Uint32
end

type Ip6Addr <: IpAddr
    port::Uint16
    host::Array{Uint8,1} #this should be fixed at 16 bytes is fixed size arrays are implemented
    flow_info::Uint32
    scope::Uint32
end

_jl_listen(sock::AsyncStream,backlog::Int32,cb::Function) = ccall(:jl_listen,Int32,(Ptr{Void},Int32,Function),sock.handle,backlog,make_callback(cb))

_jl_tcp_bind(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp_bind,Int32,(Ptr{Void},Uint32,Uint16),sock.handle,hton(addr.port),addr.host)
_jl_tcp_connect(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp_connect,Int32,(Ptr{Void},Uint32,Uint16,Function),sock.handle,addr.host,hton(addr.port))
_jl_tcp_accept(server::Ptr,client::Ptr) = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
_jl_tcp_accept(server::TcpSocket,client::TcpSocket) = _jl_tcp_accept(server.handle,client.handle)

function open_any_tcp_port(preferred_port::Uint16,cb::Function)
    socket = TcpSocket(_jl_tcp_init(globalEventLoop()));
    if(socket.handle==0)
        error("open_any_tcp_port: could not create socket")
    end
    addr = Ip4Addr(preferred_port,uint32(0)) #bind prefereed port on all adresses
	while true
		if _jl_tcp_bind(socket,addr)!=0
		    error("open_any_tcp_port: could not bind to socket")
		end
		if(_jl_listen(socket,int32(4),cb) == 0)
			break
		end
		addr.port+=1;
	end
    return (addr.port,socket)
end
open_any_tcp_port(preferred_port::Integer,cb::Function)=open_any_tcp_port(uint16(preferred_port),cb)

abstract AsyncWork

type SingleAsyncWork <: AsyncWork
    handle::Ptr{Void}
    SingleAsyncWork(handle::Ptr{Void})=new(handle)
end

type IdleAsyncWork <: AsyncWork
    handle::Ptr{Void}
end

type TimeoutAsyncWork <: AsyncWork
    handle::Ptr{Void}
end

const dummySingleAsync = SingleAsyncWork(C_NULL)

function createSingleAsyncWork(loop::Ptr{Void},cb::Function)
    return SingleAsyncWork(ccall(:jl_make_async,Ptr{Void},(Ptr{Void},Function),loop,cb))
end

function initIdleAsync(loop::Ptr{Void})
    IdleAsyncWork(ccall(:jl_idle_init,Ptr{Void},(Ptr{Void},),int(loop)))
end

function initTimeoutAsync(loop::Ptr{Void})
    TimeoutAsyncWork(ccall(:jl_timer_init,Ptr{Void},(Ptr{Void},),loop))
end

function startTimer(timer::TimeoutAsyncWork,cb::Function,timeout::Int64,repeat::Int64)
    ccall(:jl_timer_start,Int32,(Ptr{Void},Function,Int64,Int64),timer.handle,cb,timeout,repeat)
end

function stopTimer(timer::TimeoutAsyncWork)
    ccall(:jl_timer_stop,Int32,(Ptr{Void},),timer.handle)
end

assignIdleAsyncWork(work::IdleAsyncWork,cb::Function) = ccall(:jl_idle_start,Ptr{Void},(Ptr{Void},Function),work.handle,cb)

function add_idle_cb(loop::Ptr{Void},cb::Function)
    work = initIdleAsyncWork(loop)
    assignIdleAsyncWork(work,cb)
    work
end

function queueAsync(work::SingleAsyncWork)
    ccall(:jl_async_send,Void,(Ptr{Void},),work.handle)
end

# process status #
abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::PtrSize; end
type ProcessSignaled <: ProcessStatus; signal::PtrSize; end
type ProcessStopped  <: ProcessStatus; signal::PtrSize; end

process_exited  (s::Process) = (s.exit_code != -2)
process_signaled(s::Process) = (s.term_signal > 0)
process_stopped (s::Process) = 0 #not supported by libuv. Do we need this?

process_exit_status(s::Process) = s.exit_code
process_term_signal(s::Process) = s.term_signal
process_stop_signal(s::Process) = 0 #not supported by libuv. Do we need this?

function process_status(s::PtrSize)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## types

##event loop
function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
run_event_loop() = run_event_loop(localEventLoop())

function break_one_loop(loop::Ptr{Void})
    ccall(:uv_break_one,Void,(Ptr{Void},),loop)
end
break_one_loop() = break_one_loop(localEventLoop())

function process_events(loop::Ptr{Void})
    ccall(:jl_process_events,Void,(Ptr{Void},),loop)
end
process_events() = process_events(localEventLoop())

##pipe functions

function make_pipe(readable_julia_only::Bool, writeable_julia_only::Bool)
    #make the pipe an unbuffered stream for now
    pipe = NamedPipe(
        ccall(:jl_make_pipe, Ptr{Void}, (Bool,Bool), 0, readable_julia_only),
        ccall(:jl_make_pipe, Ptr{Void}, (Bool,Bool), 1, writeable_julia_only))
    error = ccall(:uv_pipe_link, Int, (Ptr{Void}, Ptr{Void}), read_handle(pipe), write_handle(pipe))
    if error != 0 # don't use assert here as $string isn't be defined yet
        error("uv_pipe_link failed")
    end
    pipe
end

function close(stream::AsyncStream)
    if(!stream.closed)
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.closed=true
    end
end

function close(stream::NamedPipe)
    if(!stream.closed)
        ccall(:jl_close_uv,Void,(Ptr{Void},),read_handle(stream))
        ccall(:jl_close_uv,Void,(Ptr{Void},),write_handle(stream))
        stream.closed=true
    end
end

##stream functions

function start_reading(stream::AsyncStream,cb::Function)
    ccall(:jl_start_reading,Bool,(Ptr{Void},Ptr{Void},Function),stream.handle,stream.buf.ios,cb!=0?cb:C_NULL)
end
start_reading(stream::AsyncStream) = start_reading(stream,0)

function stop_reading(stream::AsyncStream)
    ccall(:jl_stop_reading,Bool,(Ptr{Void},),stream.handle)
end

function readall(stream::AsyncStream)
    start_reading(stream)
    run_event_loop()
    return takebuf_string(stream.buf)
end

show(io, p::Process) = print(io, "Process(", p.cmd, ")")

function finish_read(pipe::NamedPipe)
    close(pipe) #handles to UV and ios will be invalid after this point
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function end_process(p::Process,h::Ptr{Void},e::Int32, t::Int32)
    p.exit_code=e
    p.term_signal=t
end

function _jl_spawn(cmd::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, loop::Ptr{Void},
        in::Ptr{Void}, out::Ptr{Void}, err::Ptr{Void},
        exitcb::Callback, closecb::Callback)
    return ccall(:jl_spawn, PtrSize,
        (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void},
            Union(Function, Ptr{Void}),  Union(Function, Ptr{Void})),
         cmd,        argv,            loop,      in,        out,       err,
            exitcb==false?C_NULL:exitcb, closecb==false?C_NULL:closecb)
end


function spawn(pc::ProcessChainOrNot,cmd::Cmd,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    loop = localEventLoop()
    in = stdios[1]
    out = stdios[2]
    err = stdios[3]
    pp = Process(cmd,C_NULL,in,out,err);
    ptrs = _jl_pre_exec(cmd.exec)
    pp.exitcb = exitcb
    pp.closecb = closecb
    exitcb2 = make_callback( function(args...)
        if pp.exitcb == false || !isa(pp.exitcb(pp,args...), Nothing)
            process_exited_chain(pp,args...)
        end
    end)
    closecb2 = make_callback( function(args...)
        if pp.closecb == false || !isa(pp.exitcb(pp,args...), Nothing)
            process_closed_chain(pp,args...)
        end
    end)
    pp.handle=_jl_spawn(ptrs[1], convert(Ptr{Ptr{Uint8}}, ptrs), loop,
        read_handle(in), write_handle(out), write_handle(err),
        exitcb2, closecb2)
    if pc != false
        push(pc.processes, pp)
    end
    pp
end

function process_exited_chain(p::Process,h::Ptr,e::Int32,t::Int32)
    p.exit_code = e
    p.term_signal = t
    true
end

function process_closed_chain(p::Process,h::Ptr)
    done = process_exited(p)
    i = findfirst(_jl_wait_for, p)
    if i > 0
        del(_jl_wait_for, i)
        if length(_jl_wait_for) == 0
            break_one_loop()
        end
    end
    done
end

function spawn(pc::ProcessChainOrNot,cmds::OrCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    n = make_pipe(false,false)
    if pc == false
        pc = ProcessChain(stdios)
    end
    spawn(pc, cmds.a, (stdios[1], n, stdios[3]), exitcb, closecb)
    spawn(pc, cmds.b, (n, stdios[2], stdios[3]), exitcb, closecb)
    close(n)
    pc
end

function spawn(pc::ProcessChainOrNot,cmds::AndCmds,stdios::StdIOSet,exitcb::Callback,closecb::Callback)
    if pc == false
        pc = ProcessChain(stdios)
    end
    spawn(pc, cmds.a, stdios, exitcb, closecb)
    spawn(pc, cmds.b, stdios, exitcb, closecb)
    pc
end

spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,stdios::StdIOSet,exitcb::Callback) = spawn(pc,cmds,stdios,exitcb,false)
spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,stdios::StdIOSet) = spawn(pc,cmds,stdios,false,false)
spawn(pc::ProcessChainOrNot,cmds::AbstractCmd) = spawn(pc,cmds,(false,false,false),false,false)
spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,in::StreamOrNot) = spawn(pc,cmds,(in,false,false),false,false)
spawn(pc::ProcessChainOrNot,cmds::AbstractCmd,in::StreamOrNot,out::StreamOrNot) = spawn(pc,cmds,(in,out,false),false,false)
spawn_nostdin(pc::ProcessChainOrNot,cmd::AbstractCmd,out::StreamOrNot) = spawn(pc,cmd,(false,out,false),false,false)

spawn(cmds::AbstractCmd) = spawn(false,cmds,(false,false,false),false,false)
spawn(cmds::AbstractCmd,in::StreamOrNot) = spawn(false,cmds,(in,false,false),false,false)
spawn(cmds::AbstractCmd,in::StreamOrNot,out::StreamOrNot) = spawn(false,cmds,(in,out,false),false,false)
spawn_nostdin(cmd::AbstractCmd,out::StreamOrNot) = spawn(false,cmd,(false,out,false),false,false)

#returns a pipe to read from the last command in the pipelines
read_from(cmds::AbstractCmd)=read_from(cmds, false)
function read_from(cmds::AbstractCmd, stdin::StreamOrNot)
    out=make_pipe(true,false)
    _init_buf(out) #create buffer for reading
    processes = spawn(false, cmds, (stdin,out,false))
    ccall(:jl_start_reading,Bool,(Ptr{Void},Ptr{Void},Ptr{Void}),read_handle(out),out.buf.ios,C_NULL)
    (out, processes)
end

function change_readcb(stream::AsyncStream,readcb::Function)
    ccall(:jl_change_readcb,Int16,(Ptr{Void},Function),stream.handle,readcb)
end

write_to(cmds::AbstractCmd) = write_to(cmds, false)
function write_to(cmds::AbstractCmd, stdout::StreamOrNot)
    in = make_pipe(false,true)
    processes = spawn(false, cmds, (in,stdout,false))
    (in, processes)
end

readall(cmd::AbstractCmd) = readall(cmd, false)
function readall(cmd::AbstractCmd,stdin::StreamOrNot)
    (out,pc)=read_from(cmd, stdin)
    if !wait(pc)
        pipeline_error(pc)
    end
    return takebuf_string(out.buf)
end

function run(cmds::AbstractCmd,args...)
    ps = spawn(cmds,args...)
    success = wait(ps)
    if success
        return nothing
    else
        return pipeline_error(ps)
    end
end

success(proc::Process) = (assert(process_exited(proc)); proc.exit_code==0)
success(procs::ProcessChain) = all(map(success, procs.processes))
success(cmd::AbstractCmd) = wait(spawn(cmd))

pipeline_error(proc::Process) = error("failed process: ", proc)
function pipeline_error(procs::ProcessChain)
    failed = Process[]
    for p = procs.processes
        if !success(p)
            push(failed, p)
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

function exec(thunk::Function)
    try
        thunk()
    catch e
        show(e)
        exit(0xff)
    end
    exit(0)
end

function wait(procs::Union(Process,ProcessChain))
    assert(length(_jl_wait_for) == 0)
    _jl_wait_for_(procs)
    if length(_jl_wait_for) > 0
        try
            run_event_loop() #wait(procs)
        catch e
            kill(procs)
            del_all(_jl_wait_for)
            process_events() #join(procs)
            throw(e)
        end
        assert(length(_jl_wait_for) == 0)
    end
    return success(procs)
end

_jl_kill(p::Process,signum::Int32) = ccall(:uv_process_kill,Int32,(Ptr{Void},Int32),p.handle,signum)
function kill(p::Process,signum::Int32)
    if p.exit_code == -2
        _jl_kill(p, int32(9))
    end
end
kill(ps::ProcessChain) = map(kill, ps.processes)
kill(p::Process) = kill(p,int32(9))

function _contains_newline(bufptr::Ptr{Void},len::Int32)
    return (ccall(:memchr,Ptr{Uint8},(Ptr{Void},Int32,Uint),bufptr,'\n',len)!=C_NULL)
end

function linebuffer_cb(cb::Function,stream::AsyncStream,handle::Ptr,nread::PtrSize,base::Ptr,buflen::Int32)
    if(!isa(stream.buf,IOStream))
        error("Linebuffering only supported on membuffered ASyncStreams")
    end
    if(nread>0)
        #search for newline
        pd::Ptr{Uint8} = ccall(:memchr,Ptr{Uint8},(Ptr{Uint8},Int32,PtrSize),box(Ptr{Uint8},unbox(Int,base)),'\n',nread)
        if(pd!=C_NULL)
            #newline found - split buffer
            to=memio()
            ccall(:ios_splitbuf,Void,(Ptr{Void},Ptr{Void},Ptr{Uint8}),to.ios,stream.buf.ios,pd)
            cb(stream,takebuf_string(to))
        end
    end
end


## process status ##

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

## low-level calls

write(s::AsyncStream, b::ASCIIString) =
    ccall(:jl_puts, Int32, (Ptr{Uint8},Ptr{Void}),b.data,write_handle(s))
write(s::AsyncStream, b::Uint8) =
    ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b, write_handle(s))
write(s::AsyncStream, c::Char) =
    ccall(:jl_pututf8, Int32, (Ptr{Void},Char), write_handle(s), c)
write{T<:BitsKind}(s::AsyncStream, a::Array{T}) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint32),write_handle(s), a, uint(numel(a)*sizeof(T)))
write(s::AsyncStream, p::Ptr, nb::Integer) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),write_handle(s), p, uint(nb))
_write(s::AsyncStream, p::Ptr{Void}, nb::Integer) = ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),write_handle(s),p,uint(nb))

(&)(left::AbstractCmd,right::AbstractCmd) = AndCmds(left,right)
(|)(src::AbstractCmd,dest::AbstractCmd) = OrCmds(src,dest)

function show(io, cmd::Cmd)
    if isa(cmd.exec,Vector{ByteString})
        esc = shell_escape(cmd.exec...)
        print(io,'`')
        for c in esc
            if c == '`'
                print(io,'\\')
            end
            print(io,c)
        end
        print(io,'`')
    else
        print(io, cmd.exec)
    end
end

function show(io, cmds::OrCmds)
    if isa(cmds.a, AndCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" | ")
    if isa(cmds.b, AndCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

function show(io, cmds::AndCmds)
    if isa(cmds.a, OrCmds)
        print("(")
        show(io, cmds.a)
        print(")")
    else
        show(io, cmds.a)
    end
    print(" & ")
    if isa(cmds.b, OrCmds)
        print("(")
        show(io, cmds.b)
        print(")")
    else
        show(io, cmds.b)
    end
end

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void},cb::Function) = ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void},Function),sock.handle,sockaddr,cb)
_jl_getaddrinfo(loop::Ptr,host::ByteString,service::Ptr,cb::Function) = ccall(:jl_getaddrinfo,Int32,(Ptr{Void},Ptr{Uint8},Ptr{Uint8},Function),loop,host,service,cb)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr) = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr,),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)
_uv_lasterror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)

function connect_callback(sock::TcpSocket,status::Int32,breakLoop::Bool)
    if(status==-1)
        error("Socket connection failed: ",_uv_lasterror(globalEventLoop()))
    end
    sock.open=true;
    if(breakLoop)
        break_one_loop(globalEventLoop())
    end
end

function getaddrinfo_callback(breakLoop::Bool,sock::TcpSocket,status::Int32,port::Uint16,addrinfo_list::Ptr)
    if(status==-1)
        error("Name lookup failed")
    end
    sockaddr = _jl_sockaddr_from_addrinfo(addrinfo_list) #only use first entry of the list for now
    _jl_sockaddr_set_port(sockaddr,hton(port))
    err = _jl_connect_raw(sock,sockaddr,(req::Ptr,status::Int32)->connect_callback(sock,status,breakLoop))
    if(err != 0)
        error("Failed to connect to host")
    end
end

#function readuntil(s::IOStream, delim)
#    # TODO: faster versions that avoid the encoding check
#    ccall(:jl_readuntil, ByteString, (Ptr{Void}, Uint8), s.ios, delim)
#end
#readline(s::IOStream) = readuntil(s, uint8('\n'))

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

function connect_to_host(host::ByteString,port::Uint16)
    sock = TcpSocket(_jl_tcp_init(globalEventLoop()))
    err = _jl_getaddrinfo(globalEventLoop(),host,C_NULL,(addrinfo::Ptr,status::Int32)->getaddrinfo_callback(true,sock,status,port,addrinfo))
    if(err!=0)
        error("Failed to  initilize request to resolve hostname: ",host)
    end
    run_event_loop(globalEventLoop())
    return sock
end
