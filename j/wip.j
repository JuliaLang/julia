module System

if true
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    print(a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
    print(s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
    print(s::ASCIIString) = print(s.data)
    print(x) = show(x)
    println(x) = (print(x);print("\n"))
    show(x) = ccall(:jl_show_any, Void, (Any,), x)
    show(s::ASCIIString) = print(s.data)
    show(s::Symbol) = print(s)
    show(b::Bool) = print(b ? "true" : "false")
    show(n::Int64) = ccall(:jl_print_int64, Void, (Int64,), n)
    show(n::Integer)  = show(int64(n))
    print(a...) = for x=a; print(x); end
    function show(e::Expr)
        print(e.head)
        print("(")
        for i=1:arraylen(e.args)
            show(arrayref(e.args,i))
            print(", ")
        end
        print(")\n")
    end
end
## Load essential files and libraries

include("base.j")

# core operations & types
include("range.j")
include("tuple.j")
include("cell.j")
include("expr.j")
include("error.j")

# core numeric operations & types
include("bool.j")
include("number.j")
include("int.j")
include("promotion.j")
include("operators.j")
include("float.j")
include("pointer.j")
include("char.j")
include("reduce.j")
include("complex.j")
include("rational.j")

# core data structures (used by type inference)
include("abstractarray.j")
include("subarray.j")
include("array.j")
include("intset.j")
include("table.j")
include("set.j")

# compiler
include("inference.j")

# I/O, strings & printing
include("io.j")
set_current_output_stream(make_stdout_stream()) # for error reporting
include("string.j")
include("ascii.j")
include("utf8.j")
include("regex.j")
include("show.j")
include("grisu.j")
include("printf.j")

# system & environment
include("libc.j")
include("env.j")
include("errno_h.j")

# concurrency and parallelism
include("iterator.j")
include("task.j")

# core math functions
include("intfuncs.j")
include("floatfuncs.j")
include("math.j")
include("math_libm.j")
include("sort.j")
include("combinatorics.j")
include("statistics.j")

include("utf8.j")
include("string.j")

typealias Executable Vector{ByteString}

type Cmd
    exec::Executable
end

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

global const STDIN = FileDes(ccall(:jl_stdin,  Int32, ()))
global const STDOUT = FileDes(ccall(:jl_stdout, Int32, ()))
global const STDERR = FileDes(ccall(:jl_stderr, Int32, ()))

isequal(fd1::FileDes, fd2::FileDes) = (fd1.fd == fd2.fd)

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

include("stream.j")

readall(`ls`)
end #module
