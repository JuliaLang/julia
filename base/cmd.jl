# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractCmd end

# libuv process option flags
const UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS = UInt32(1 << 2)
const UV_PROCESS_DETACHED = UInt32(1 << 3)
const UV_PROCESS_WINDOWS_HIDE = UInt32(1 << 4)
const UV_PROCESS_WINDOWS_DISABLE_EXACT_NAME = UInt32(1 << 7)

struct Cmd <: AbstractCmd
    exec::Vector{String}
    ignorestatus::Bool
    flags::UInt32 # libuv process flags
    env::Union{Vector{String},Nothing}
    dir::String
    cpus::Union{Nothing,Vector{UInt16}}
    Cmd(exec::Vector{<:AbstractString}) =
        new(exec, false, 0x00, nothing, "", nothing)
    Cmd(cmd::Cmd, ignorestatus, flags, env, dir, cpus = nothing) =
        new(cmd.exec, ignorestatus, flags, env,
            dir === cmd.dir ? dir : cstr(dir), cpus)
    function Cmd(cmd::Cmd; ignorestatus::Bool=cmd.ignorestatus, env=cmd.env, dir::AbstractString=cmd.dir,
                 cpus::Union{Nothing,Vector{UInt16}} = cmd.cpus,
                 detach::Bool = 0 != cmd.flags & UV_PROCESS_DETACHED,
                 windows_verbatim::Bool = 0 != cmd.flags & UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS,
                 windows_hide::Bool = 0 != cmd.flags & UV_PROCESS_WINDOWS_HIDE)
        flags = detach * UV_PROCESS_DETACHED |
                windows_verbatim * UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS |
                windows_hide * UV_PROCESS_WINDOWS_HIDE
        new(cmd.exec, ignorestatus, flags, byteenv(env),
            dir === cmd.dir ? dir : cstr(dir), cpus)
    end
end

has_nondefault_cmd_flags(c::Cmd) =
    c.ignorestatus ||
    c.flags != 0x00 ||
    c.env !== nothing ||
    c.dir !== "" ||
    c.cpus !== nothing

"""
    Cmd(cmd::Cmd; ignorestatus, detach, windows_verbatim, windows_hide, env, dir)
    Cmd(exec::Vector{String})

Construct a new `Cmd` object, representing an external program and arguments, from `cmd`,
while changing the settings of the optional keyword arguments:

* `ignorestatus::Bool`: If `true` (defaults to `false`), then the `Cmd` will not throw an
  error if the return code is nonzero.
* `detach::Bool`: If `true` (defaults to `false`), then the `Cmd` will be run in a new
  process group, allowing it to outlive the `julia` process and not have Ctrl-C passed to
  it.
* `windows_verbatim::Bool`: If `true` (defaults to `false`), then on Windows the `Cmd` will
  send a command-line string to the process with no quoting or escaping of arguments, even
  arguments containing spaces. (On Windows, arguments are sent to a program as a single
  "command-line" string, and programs are responsible for parsing it into arguments. By
  default, empty arguments and arguments with spaces or tabs are quoted with double quotes
  `"` in the command line, and `\\` or `"` are preceded by backslashes.
  `windows_verbatim=true` is useful for launching programs that parse their command line in
  nonstandard ways.) Has no effect on non-Windows systems.
* `windows_hide::Bool`: If `true` (defaults to `false`), then on Windows no new console
  window is displayed when the `Cmd` is executed. This has no effect if a console is
  already open or on non-Windows systems.
* `env`: Set environment variables to use when running the `Cmd`. `env` is either a
  dictionary mapping strings to strings, an array of strings of the form `"var=val"`, an
  array or tuple of `"var"=>val` pairs. In order to modify (rather than replace) the
  existing environment, initialize `env` with `copy(ENV)` and then set `env["var"]=val` as
  desired.  To add to an environment block within a `Cmd` object without replacing all
  elements, use [`addenv()`](@ref) which will return a `Cmd` object with the updated environment.
* `dir::AbstractString`: Specify a working directory for the command (instead
  of the current directory).

For any keywords that are not specified, the current settings from `cmd` are used.

Note that the `Cmd(exec)` constructor does not create a copy of `exec`. Any subsequent changes to `exec` will be reflected in the `Cmd` object.

The most common way to construct a `Cmd` object is with command literals (backticks), e.g.

    `ls -l`

This can then be passed to the `Cmd` constructor to modify its settings, e.g.

    Cmd(`echo "Hello world"`, ignorestatus=true, detach=false)
"""
Cmd

hash(x::Cmd, h::UInt) = hash(x.exec, hash(x.env, hash(x.ignorestatus, hash(x.dir, hash(x.flags, h)))))
==(x::Cmd, y::Cmd) = x.exec == y.exec && x.env == y.env && x.ignorestatus == y.ignorestatus &&
                     x.dir == y.dir && isequal(x.flags, y.flags)

struct OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

struct ErrOrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    ErrOrCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

struct AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a, b)
end

hash(x::AndCmds, h::UInt) = hash(x.a, hash(x.b, h))
==(x::AndCmds, y::AndCmds) = x.a == y.a && x.b == y.b

shell_escape(cmd::Cmd; special::AbstractString="") =
    shell_escape(cmd.exec..., special=special)
shell_escape_posixly(cmd::Cmd) =
    shell_escape_posixly(cmd.exec...)
shell_escape_csh(cmd::Cmd) =
    shell_escape_csh(cmd.exec...)
escape_microsoft_c_args(cmd::Cmd) =
    escape_microsoft_c_args(cmd.exec...)
escape_microsoft_c_args(io::IO, cmd::Cmd) =
    escape_microsoft_c_args(io::IO, cmd.exec...)

function show(io::IO, cmd::Cmd)
    print_env = cmd.env !== nothing
    print_dir = !isempty(cmd.dir)
    (print_env || print_dir) && print(io, "setenv(")
    print_cpus = cmd.cpus !== nothing
    print_cpus && print(io, "setcpuaffinity(")
    print(io, '`')
    join(io, map(cmd.exec) do arg
        replace(sprint(context=io) do io
            with_output_color(:underline, io) do io
                print_shell_word(io, arg, shell_special)
            end
        end, '`' => "\\`")
    end, ' ')
    print(io, '`')
    if print_cpus
        print(io, ", ")
        show(io, collect(Int, something(cmd.cpus)))
        print(io, ")")
    end
    print_env && (print(io, ","); show(io, cmd.env))
    print_dir && (print(io, "; dir="); show(io, cmd.dir))
    (print_dir || print_env) && print(io, ")")
    nothing
end

function show(io::IO, cmds::Union{OrCmds,ErrOrCmds})
    print(io, "pipeline(")
    show(io, cmds.a)
    print(io, ", ")
    print(io, isa(cmds, ErrOrCmds) ? "stderr=" : "stdout=")
    show(io, cmds.b)
    print(io, ")")
end

function show(io::IO, cmds::AndCmds)
    show(io, cmds.a)
    print(io, " & ")
    show(io, cmds.b)
end

const STDIN_NO  = 0
const STDOUT_NO = 1
const STDERR_NO = 2

struct FileRedirect
    filename::String
    append::Bool
    FileRedirect(filename::AbstractString, append::Bool) = FileRedirect(convert(String, filename), append)
    function FileRedirect(filename::String, append::Bool)
        if lowercase(filename) == (@static Sys.iswindows() ? "nul" : "/dev/null")
            @warn "For portability use devnull instead of a file redirect" maxlog=1
        end
        return new(filename, append)
    end
end

# setup_stdio ≈ cconvert
# rawhandle ≈ unsafe_convert
rawhandle(::DevNull) = C_NULL
rawhandle(x::OS_HANDLE) = x
if OS_HANDLE !== RawFD
    rawhandle(x::RawFD) = Libc._get_osfhandle(x)
end
setup_stdio(stdio::Union{DevNull,OS_HANDLE,RawFD}, ::Bool) = (stdio, false)

const Redirectable = Union{IO, FileRedirect, RawFD, OS_HANDLE}
const StdIOSet = NTuple{3, Redirectable}

struct CmdRedirect <: AbstractCmd
    cmd::AbstractCmd
    handle::Redirectable
    stream_no::Int
    readable::Bool
end
CmdRedirect(cmd, handle, stream_no) = CmdRedirect(cmd, handle, stream_no, stream_no == STDIN_NO)

function show(io::IO, cr::CmdRedirect)
    print(io, "pipeline(")
    show(io, cr.cmd)
    print(io, ", ")
    if cr.stream_no == STDOUT_NO
        print(io, "stdout")
    elseif cr.stream_no == STDERR_NO
        print(io, "stderr")
    elseif cr.stream_no == STDIN_NO
        print(io, "stdin")
    else
        print(io, cr.stream_no)
    end
    print(io, cr.readable ? "<" : ">")
    show(io, cr.handle)
    print(io, ")")
end

"""
    ignorestatus(command)

Mark a command object so that running it will not throw an error if the result code is non-zero.
"""
ignorestatus(cmd::Cmd) = Cmd(cmd, ignorestatus=true)
ignorestatus(cmd::Union{OrCmds,AndCmds}) =
    typeof(cmd)(ignorestatus(cmd.a), ignorestatus(cmd.b))

"""
    detach(command)

Mark a command object so that it will be run in a new process group, allowing it to outlive the julia process, and not have Ctrl-C interrupts passed to it.
"""
detach(cmd::Cmd) = Cmd(cmd; detach=true)

# like String(s), but throw an error if s contains NUL, since
# libuv requires NUL-terminated strings
function cstr(s)
    if Base.containsnul(s)
        throw(ArgumentError("strings containing NUL cannot be passed to spawned processes"))
    end
    return String(s)::String
end

# convert various env representations into an array of "key=val" strings
byteenv(env::AbstractArray{<:AbstractString}) =
    String[cstr(x) for x in env]
byteenv(env::AbstractDict) =
    String[cstr(string(k)*"="*string(v)) for (k,v) in env]
byteenv(env::Nothing) = nothing
byteenv(env::Union{AbstractVector{Pair{T,V}}, Tuple{Vararg{Pair{T,V}}}}) where {T<:AbstractString,V} =
    String[cstr(k*"="*string(v)) for (k,v) in env]

"""
    setenv(command::Cmd, env; dir)

Set environment variables to use when running the given `command`. `env` is either a
dictionary mapping strings to strings, an array of strings of the form `"var=val"`, or
zero or more `"var"=>val` pair arguments. In order to modify (rather than replace) the
existing environment, create `env` through `copy(ENV)` and then setting `env["var"]=val`
as desired, or use [`addenv`](@ref).

The `dir` keyword argument can be used to specify a working directory for the command.
`dir` defaults to the currently set `dir` for `command` (which is the current working
directory if not specified already).

See also [`Cmd`](@ref), [`addenv`](@ref), [`ENV`](@ref), [`pwd`](@ref).
"""
setenv(cmd::Cmd, env; dir=cmd.dir) = Cmd(cmd; env=byteenv(env), dir=dir)
setenv(cmd::Cmd, env::Pair{<:AbstractString}...; dir=cmd.dir) =
    setenv(cmd, env; dir=dir)
setenv(cmd::Cmd; dir=cmd.dir) = Cmd(cmd; dir=dir)

# split environment entry string into before and after first `=` (key and value)
function splitenv(e::String)
    i = findnext('=', e, 2)
    if i === nothing
        throw(ArgumentError("malformed environment entry"))
    end
    e[1:prevind(e, i)], e[nextind(e, i):end]
end

"""
    addenv(command::Cmd, env...; inherit::Bool = true)

Merge new environment mappings into the given [`Cmd`](@ref) object, returning a new `Cmd` object.
Duplicate keys are replaced.  If `command` does not contain any environment values set already,
it inherits the current environment at time of `addenv()` call if `inherit` is `true`.
Keys with value `nothing` are deleted from the env.

See also [`Cmd`](@ref), [`setenv`](@ref), [`ENV`](@ref).

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
function addenv(cmd::Cmd, env::Dict; inherit::Bool = true)
    new_env = Dict{String,String}()
    if cmd.env === nothing
        if inherit
            merge!(new_env, ENV)
        end
    else
        for (k, v) in splitenv.(cmd.env)
            new_env[string(k)::String] = string(v)::String
        end
    end
    for (k, v) in env
        if v === nothing
            delete!(new_env, string(k)::String)
        else
            new_env[string(k)::String] = string(v)::String
        end
    end
    return setenv(cmd, new_env)
end

function addenv(cmd::Cmd, pairs::Pair{<:AbstractString}...; inherit::Bool = true)
    return addenv(cmd, Dict(k => v for (k, v) in pairs); inherit)
end

function addenv(cmd::Cmd, env::Vector{<:AbstractString}; inherit::Bool = true)
    return addenv(cmd, Dict(k => v for (k, v) in splitenv.(env)); inherit)
end

"""
    setcpuaffinity(original_command::Cmd, cpus) -> command::Cmd

Set the CPU affinity of the `command` by a list of CPU IDs (1-based) `cpus`.  Passing
`cpus = nothing` means to unset the CPU affinity if the `original_command` has any.

This function is supported only in Linux and Windows.  It is not supported in macOS because
libuv does not support affinity setting.

!!! compat "Julia 1.8"
    This function requires at least Julia 1.8.

# Examples

In Linux, the `taskset` command line program can be used to see how `setcpuaffinity` works.

```julia
julia> run(setcpuaffinity(`sh -c 'taskset -p \$\$'`, [1, 2, 5]));
pid 2273's current affinity mask: 13
```

Note that the mask value `13` reflects that the first, second, and the fifth bits (counting
from the least significant position) are turned on:

```julia
julia> 0b010011
0x13
```
"""
function setcpuaffinity end
setcpuaffinity(cmd::Cmd, ::Nothing) = Cmd(cmd; cpus = nothing)
setcpuaffinity(cmd::Cmd, cpus) = Cmd(cmd; cpus = collect(UInt16, cpus))

(&)(left::AbstractCmd, right::AbstractCmd) = AndCmds(left, right)
redir_out(src::AbstractCmd, dest::AbstractCmd) = OrCmds(src, dest)
redir_err(src::AbstractCmd, dest::AbstractCmd) = ErrOrCmds(src, dest)

# Stream Redirects
redir_out(dest::Redirectable, src::AbstractCmd) = CmdRedirect(src, dest, STDIN_NO)
redir_out(src::AbstractCmd, dest::Redirectable) = CmdRedirect(src, dest, STDOUT_NO)
redir_err(src::AbstractCmd, dest::Redirectable) = CmdRedirect(src, dest, STDERR_NO)

# File redirects
redir_out(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, false), STDOUT_NO)
redir_out(src::AbstractString, dest::AbstractCmd) = CmdRedirect(dest, FileRedirect(src, false), STDIN_NO)
redir_err(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, false), STDERR_NO)
redir_out_append(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, true), STDOUT_NO)
redir_err_append(src::AbstractCmd, dest::AbstractString) = CmdRedirect(src, FileRedirect(dest, true), STDERR_NO)

"""
    pipeline(command; stdin, stdout, stderr, append=false)

Redirect I/O to or from the given `command`. Keyword arguments specify which of the
command's streams should be redirected. `append` controls whether file output appends to the
file. This is a more general version of the 2-argument `pipeline` function.
`pipeline(from, to)` is equivalent to `pipeline(from, stdout=to)` when `from` is a command,
and to `pipeline(to, stdin=from)` when `from` is another kind of data source.

**Examples**:

```julia
run(pipeline(`dothings`, stdout="out.txt", stderr="errs.txt"))
run(pipeline(`update`, stdout="log.txt", append=true))
```
"""
function pipeline(cmd::AbstractCmd; stdin=nothing, stdout=nothing, stderr=nothing, append::Bool=false)
    if append && stdout === nothing && stderr === nothing
        throw(ArgumentError("append set to true, but no output redirections specified"))
    end
    if stdin !== nothing
        cmd = redir_out(stdin, cmd)
    end
    if stdout !== nothing
        cmd = append ? redir_out_append(cmd, stdout) : redir_out(cmd, stdout)
    end
    if stderr !== nothing
        cmd = append ? redir_err_append(cmd, stderr) : redir_err(cmd, stderr)
    end
    return cmd
end

pipeline(cmd::AbstractCmd, dest) = pipeline(cmd, stdout=dest)
pipeline(src::Union{Redirectable,AbstractString}, cmd::AbstractCmd) = pipeline(cmd, stdin=src)

"""
    pipeline(from, to, ...)

Create a pipeline from a data source to a destination. The source and destination can be
commands, I/O streams, strings, or results of other `pipeline` calls. At least one argument
must be a command. Strings refer to filenames. When called with more than two arguments,
they are chained together from left to right. For example, `pipeline(a,b,c)` is equivalent to
`pipeline(pipeline(a,b),c)`. This provides a more concise way to specify multi-stage
pipelines.

**Examples**:

```julia
run(pipeline(`ls`, `grep xyz`))
run(pipeline(`ls`, "out.txt"))
run(pipeline("out.txt", `grep xyz`))
```
"""
pipeline(a, b, c, d...) = pipeline(pipeline(a, b), c, d...)


## implementation of `cmd` syntax ##

cmd_interpolate(xs...) = cstr(string(map(cmd_interpolate1, xs)...))
cmd_interpolate1(x) = x
cmd_interpolate1(::Nothing) = throw(ArgumentError("`nothing` can not be interpolated into commands (`Cmd`)"))

arg_gen() = String[]
arg_gen(x::AbstractString) = String[cstr(x)]
function arg_gen(cmd::Cmd)
    if has_nondefault_cmd_flags(cmd)
        throw(ArgumentError("Non-default environment behavior is only permitted for the first interpolant."))
    end
    cmd.exec
end

function arg_gen(head)
    if isiterable(typeof(head))
        vals = String[]
        for x in head
            push!(vals, cmd_interpolate(x))
        end
        return vals
    else
        return String[cmd_interpolate(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = String[]
    for h = head, t = tail
        push!(vals, cmd_interpolate(h,t))
    end
    return vals
end

function cmd_gen(parsed)
    args = String[]
    if length(parsed) >= 1 && isa(parsed[1], Tuple{Cmd})
        cmd = (parsed[1]::Tuple{Cmd})[1]
        (ignorestatus, flags, env, dir) = (cmd.ignorestatus, cmd.flags, cmd.env, cmd.dir)
        append!(args, cmd.exec)
        for arg in tail(parsed)
            append!(args, Base.invokelatest(arg_gen, arg...)::Vector{String})
        end
        return Cmd(Cmd(args), ignorestatus, flags, env, dir)
    else
        for arg in parsed
            append!(args, arg_gen(arg...)::Vector{String})
        end
        return Cmd(args)
    end
end

@assume_effects :foldable !:consistent function cmd_gen(
    parsed::Tuple{Vararg{Tuple{Vararg{Union{String, SubString{String}}}}}}
)
    return @invoke cmd_gen(parsed::Any)
end

"""
    @cmd str

Similar to ``` `str` ```, generate a `Cmd` from the `str` string which represents the shell command(s) to be executed.
The [`Cmd`](@ref) object can be run as a process and can outlive the spawning julia process (see `Cmd` for more).

# Examples
```jldoctest
julia> cm = @cmd " echo 1 "
`echo 1`

julia> run(cm)
1
Process(`echo 1`, ProcessExited(0))
```
"""
macro cmd(str::String)
    cmd_ex = shell_parse(str, special=shell_special, filename=String(__source__.file))[1]
    return :(cmd_gen($(esc(cmd_ex))))
end
