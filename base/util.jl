# This file is a part of Julia. License is MIT: http://julialang.org/license

# timing

# time() in libc.jl

# high-resolution relative time, in nanoseconds

"""
    time_ns()

Get the time in nanoseconds. The time corresponding to 0 is undefined, and wraps every 5.8 years.
"""
time_ns() = ccall(:jl_hrtime, UInt64, ())

# This type must be kept in sync with the C struct in src/gc.h
immutable GC_Num
    allocd      ::Int64 # GC internal
    deferred_alloc::Int64 # GC internal
    freed       ::Int64 # GC internal
    malloc      ::UInt64
    realloc     ::UInt64
    poolalloc   ::UInt64
    bigalloc    ::UInt64
    freecall    ::UInt64
    total_time  ::UInt64
    total_allocd::UInt64 # GC internal
    since_sweep ::UInt64 # GC internal
    collect     ::Csize_t # GC internal
    pause       ::Cint
    full_sweep  ::Cint
end

gc_num() = ccall(:jl_gc_num, GC_Num, ())

# This type is to represent differences in the counters, so fields may be negative
immutable GC_Diff
    allocd      ::Int64 # Bytes allocated
    malloc      ::Int64 # Number of GC aware malloc()
    realloc     ::Int64 # Number of GC aware realloc()
    poolalloc   ::Int64 # Number of pool allocation
    bigalloc    ::Int64 # Number of big (non-pool) allocation
    freecall    ::Int64 # Number of GC aware free()
    total_time  ::Int64 # Time spent in garbage collection
    pause       ::Int64 # Number of GC pauses
    full_sweep  ::Int64 # Number of GC full collection
end

gc_total_bytes(gc_num::GC_Num) =
    (gc_num.allocd + gc_num.deferred_alloc +
     Int64(gc_num.collect) + Int64(gc_num.total_allocd))

function GC_Diff(new::GC_Num, old::GC_Num)
    # logic from `src/gc.c:jl_gc_total_bytes`
    old_allocd = gc_total_bytes(old)
    new_allocd = gc_total_bytes(new)
    return GC_Diff(new_allocd - old_allocd,
                   Int64(new.malloc       - old.malloc),
                   Int64(new.realloc      - old.realloc),
                   Int64(new.poolalloc    - old.poolalloc),
                   Int64(new.bigalloc     - old.bigalloc),
                   Int64(new.freecall     - old.freecall),
                   Int64(new.total_time   - old.total_time),
                   new.pause              - old.pause,
                   new.full_sweep         - old.full_sweep)
end

function gc_alloc_count(diff::GC_Diff)
    diff.malloc + diff.realloc + diff.poolalloc + diff.bigalloc
end


# total time spend in garbage collection, in nanoseconds
gc_time_ns() = ccall(:jl_gc_total_hrtime, UInt64, ())

# total number of bytes allocated so far
gc_bytes() = ccall(:jl_gc_total_bytes, Int64, ())

"""
    tic()

Set a timer to be read by the next call to [`toc`](@ref) or [`toq`](@ref). The
macro call `@time expr` can also be used to time evaluation.

```julia
julia> tic()
0x0000c45bc7abac95

julia> sleep(0.3)

julia> toc()
elapsed time: 0.302745944 seconds
0.302745944
```
"""
function tic()
    t0 = time_ns()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

"""
    toq()

Return, but do not print, the time elapsed since the last [`tic`](@ref). The
macro calls `@timed expr` and `@elapsed expr` also return evaluation time.

```julia
julia> tic()
0x0000c46477a9675d

julia> sleep(0.3)

julia> toq()
0.302251004
```
"""
function toq()
    t1 = time_ns()
    timers = get(task_local_storage(), :TIMERS, ())
    if timers === ()
        error("toc() without tic()")
    end
    t0 = timers[1]::UInt64
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

"""
    toc()

Print and return the time elapsed since the last [`tic`](@ref). The macro call
`@time expr` can also be used to time evaluation.

```julia
julia> tic()
0x0000c45bc7abac95

julia> sleep(0.3)

julia> toc()
elapsed time: 0.302745944 seconds
0.302745944
```
"""
function toc()
    t = toq()
    println("elapsed time: ", t, " seconds")
    return t
end

# print elapsed time, return expression value
const _mem_units = ["byte", "KiB", "MiB", "GiB", "TiB", "PiB"]
const _cnt_units = ["", " k", " M", " G", " T", " P"]
function prettyprint_getunits(value, numunits, factor)
    if value == 0 || value == 1
        return (value, 1)
    end
    unit = ceil(Int, log(value) / log(factor))
    unit = min(numunits, unit)
    number = value/factor^(unit-1)
    return number, unit
end

function padded_nonzero_print(value,str)
    if value != 0
        blanks = "                "[1:18-length(str)]
        println("$str:$blanks$value")
    end
end

function time_print(elapsedtime, bytes, gctime, allocs)
    @printf("%10.6f seconds", elapsedtime/1e9)
    if bytes != 0 || allocs != 0
        bytes, mb = prettyprint_getunits(bytes, length(_mem_units), Int64(1024))
        allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
        if ma == 1
            @printf(" (%d%s allocation%s: ", allocs, _cnt_units[ma], allocs==1 ? "" : "s")
        else
            @printf(" (%.2f%s allocations: ", allocs, _cnt_units[ma])
        end
        if mb == 1
            @printf("%d %s%s", bytes, _mem_units[mb], bytes==1 ? "" : "s")
        else
            @printf("%.3f %s", bytes, _mem_units[mb])
        end
        if gctime > 0
            @printf(", %.2f%% gc time", 100*gctime/elapsedtime)
        end
        print(")")
    elseif gctime > 0
        @printf(", %.2f%% gc time", 100*gctime/elapsedtime)
    end
    println()
end

function timev_print(elapsedtime, diff::GC_Diff)
    allocs = gc_alloc_count(diff)
    time_print(elapsedtime, diff.allocd, diff.total_time, allocs)
    print("elapsed time (ns): $elapsedtime\n")
    padded_nonzero_print(diff.total_time,   "gc time (ns)")
    padded_nonzero_print(diff.allocd,       "bytes allocated")
    padded_nonzero_print(diff.poolalloc,    "pool allocs")
    padded_nonzero_print(diff.bigalloc,     "non-pool GC allocs")
    padded_nonzero_print(diff.malloc,       "malloc() calls")
    padded_nonzero_print(diff.realloc,      "realloc() calls")
    padded_nonzero_print(diff.freecall,     "free() calls")
    padded_nonzero_print(diff.pause,        "GC pauses")
    padded_nonzero_print(diff.full_sweep,   "full collections")
end

"""
    @time

A macro to execute an expression, printing the time it took to execute, the number of
allocations, and the total number of bytes its execution caused to be allocated, before
returning the value of the expression.

See also [`@timev`](@ref), [`@timed`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

```julia
julia> @time rand(10^6);
  0.001525 seconds (7 allocations: 7.630 MiB)

julia> @time begin
           sleep(0.3)
           1+1
       end
  0.301395 seconds (8 allocations: 336 bytes)
```
"""
macro time(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        time_print(elapsedtime, diff.allocd, diff.total_time,
                   gc_alloc_count(diff))
        val
    end
end

"""
    @timev

This is a verbose version of the `@time` macro. It first prints the same information as
`@time`, then any non-zero memory allocation counters, and then returns the value of the
expression.

See also [`@time`](@ref), [`@timed`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

```julia
julia> @timev rand(10^6);
  0.001006 seconds (7 allocations: 7.630 MiB)
elapsed time (ns): 1005567
bytes allocated:   8000256
pool allocs:       6
malloc() calls:    1
```
"""
macro timev(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        timev_print(elapsedtime, GC_Diff(gc_num(), stats))
        val
    end
end

"""
    @elapsed

A macro to evaluate an expression, discarding the resulting value, instead returning the
number of seconds it took to execute as a floating-point number.

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@allocated`](@ref).

```julia
julia> @elapsed sleep(0.3)
0.301391426
```
"""
macro elapsed(ex)
    quote
        local t0 = time_ns()
        local val = $(esc(ex))
        (time_ns()-t0)/1e9
    end
end

# measure bytes allocated without *most* contamination from compilation
# Note: This reports a different value from the @time macros, because
# it wraps the call in a function, however, this means that things
# like:  @allocated y = foo()
# will not work correctly, because it will set y in the context of
# the local function made by the macro, not the current function
"""
    @allocated

A macro to evaluate an expression, discarding the resulting value, instead returning the
total number of bytes allocated during evaluation of the expression. Note: the expression is
evaluated inside a local function, instead of the current context, in order to eliminate the
effects of compilation, however, there still may be some allocations due to JIT compilation.
This also makes the results inconsistent with the `@time` macros, which do not try to adjust
for the effects of compilation.

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@elapsed`](@ref).

```julia
julia> @allocated rand(10^6)
8000080
```
"""
macro allocated(ex)
    quote
        let
            local f
            function f()
                b0 = gc_bytes()
                $(esc(ex))
                gc_bytes() - b0
            end
            f()
        end
    end
end

"""
    @timed

A macro to execute an expression, and return the value of the expression, elapsed time,
total bytes allocated, garbage collection time, and an object with various memory allocation
counters.

See also [`@time`](@ref), [`@timev`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

```julia
julia> val, t, bytes, gctime, memallocs = @timed rand(10^6);

julia> t
0.006634834

julia> bytes
8000256

julia> gctime
0.0055765

julia> fieldnames(typeof(memallocs))
9-element Array{Symbol,1}:
 :allocd
 :malloc
 :realloc
 :poolalloc
 :bigalloc
 :freecall
 :total_time
 :pause
 :full_sweep

julia> memallocs.total_time
5576500
```
"""
macro timed(ex)
    quote
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        val, elapsedtime/1e9, diff.allocd, diff.total_time/1e9, diff
    end
end

function fftw_vendor()
    if Base.libfftw_name == "libmkl_rt"
        return :mkl
    else
        return :fftw
    end
end


## printing with color ##

function with_output_color(f::Function, color::Union{Int, Symbol}, io::IO, args...; bold::Bool = false)
    buf = IOBuffer()
    have_color && bold && print(buf, text_colors[:bold])
    have_color && print(buf, get(text_colors, color, color_normal))
    try f(IOContext(buf, io), args...)
    finally
        have_color && color != :nothing && print(buf, get(disable_text_style, color, text_colors[:default]))
        have_color && (bold || color == :bold) && print(buf, disable_text_style[:bold])
        print(io, String(take!(buf)))
    end
end

"""
    print_with_color(color::Union{Symbol, Int}, [io], strings...; bold::Bool = false)

Print strings in a color specified as a symbol.

`color` may take any of the values $(Base.available_text_colors_docstring)
or an integer between 0 and 255 inclusive. Note that not all terminals support 256 colors.
If the keyword `bold` is given as `true`, the result will be printed in bold.
"""
print_with_color(color::Union{Int, Symbol}, io::IO, msg::AbstractString...; bold::Bool = false) =
    with_output_color(print, color, io, msg...; bold = bold)
print_with_color(color::Union{Int, Symbol}, msg::AbstractString...; bold::Bool = false) =
    print_with_color(color, STDOUT, msg...; bold = bold)
println_with_color(color::Union{Int, Symbol}, io::IO, msg::AbstractString...; bold::Bool = false) =
    with_output_color(println, color, io, msg...; bold = bold)
println_with_color(color::Union{Int, Symbol}, msg::AbstractString...; bold::Bool = false) =
    println_with_color(color, STDOUT, msg...; bold = bold)

## warnings and messages ##

const log_info_to = Dict{Tuple{Union{Module,Void},Union{Symbol,Void}},IO}()
const log_warn_to = Dict{Tuple{Union{Module,Void},Union{Symbol,Void}},IO}()
const log_error_to = Dict{Tuple{Union{Module,Void},Union{Symbol,Void}},IO}()

function _redirect(io::IO, log_to::Dict, sf::StackTraces.StackFrame)
    isnull(sf.linfo) && return io
    mod = get(sf.linfo).def.module
    fun = sf.func
    if haskey(log_to, (mod,fun))
        return log_to[(mod,fun)]
    elseif haskey(log_to, (mod,nothing))
        return log_to[(mod,nothing)]
    elseif haskey(log_to, (nothing,nothing))
        return log_to[(nothing,nothing)]
    else
        return io
    end
end

function _redirect(io::IO, log_to::Dict, fun::Symbol)
    clos = string("#",fun,"#")
    kw = string("kw##",fun)
    local sf
    break_next_frame = false
    for trace in backtrace()
        stack::Vector{StackFrame} = StackTraces.lookup(trace)
        filter!(frame -> !frame.from_c, stack)
        for frame in stack
            isnull(frame.linfo) && continue
            sf = frame
            break_next_frame && (@goto skip)
            get(frame.linfo).def.module == Base || continue
            sff = string(frame.func)
            if frame.func == fun || startswith(sff, clos) || startswith(sff, kw)
                break_next_frame = true
            end
        end
    end
    @label skip
    _redirect(io, log_to, sf)
end

@inline function redirect(io::IO, log_to::Dict, arg::Union{Symbol,StackTraces.StackFrame})
    if isempty(log_to)
        return io
    else
        if length(log_to)==1 && haskey(log_to,(nothing,nothing))
            return log_to[(nothing,nothing)]
        else
            return _redirect(io, log_to, arg)
        end
    end
end

"""
    logging(io [, m [, f]][; kind=:all])
    logging([; kind=:all])

Stream output of informational, warning, and/or error messages to `io`,
overriding what was otherwise specified.  Optionally, divert stream only for
module `m`, or specifically function `f` within `m`.  `kind` can be `:all` (the
default), `:info`, `:warn`, or `:error`.  See `Base.log_{info,warn,error}_to`
for the current set of redirections.  Call `logging` with no arguments (or just
the `kind`) to reset everything.
"""
function logging(io::IO, m::Union{Module,Void}=nothing, f::Union{Symbol,Void}=nothing;
                 kind::Symbol=:all)
    (kind==:all || kind==:info)  && (log_info_to[(m,f)] = io)
    (kind==:all || kind==:warn)  && (log_warn_to[(m,f)] = io)
    (kind==:all || kind==:error) && (log_error_to[(m,f)] = io)
    nothing
end

function logging(;  kind::Symbol=:all)
    (kind==:all || kind==:info)  && empty!(log_info_to)
    (kind==:all || kind==:warn)  && empty!(log_warn_to)
    (kind==:all || kind==:error) && empty!(log_error_to)
    nothing
end

"""
    info([io, ] msg..., [prefix="INFO: "])

Display an informational message.
Argument `msg` is a string describing the information to be displayed.
The `prefix` keyword argument can be used to override the default
prepending of `msg`.

```jldoctest
julia> info("hello world")
INFO: hello world

julia> info("hello world"; prefix="MY INFO: ")
MY INFO: hello world
```

See also [`logging`](@ref).
"""
function info(io::IO, msg...; prefix="INFO: ")
    io = redirect(io, log_info_to, :info)
    print_with_color(info_color(), io, prefix; bold = true)
    println_with_color(info_color(), io, chomp(string(msg...)))
    return
end
info(msg...; prefix="INFO: ") = info(STDERR, msg..., prefix=prefix)

# print a warning only once

const have_warned = Set()

warn_once(io::IO, msg...) = warn(io, msg..., once=true)
warn_once(msg...) = warn(STDERR, msg..., once=true)

"""
    warn([io, ] msg..., [prefix="WARNING: ", once=false, key=nothing, bt=nothing, filename=nothing, lineno::Int=0])

Display a warning. Argument `msg` is a string describing the warning to be
displayed.  Set `once` to true and specify a `key` to only display `msg` the
first time `warn` is called.  If `bt` is not `nothing` a backtrace is displayed.
If `filename` is not `nothing` both it and `lineno` are displayed.

See also [`logging`](@ref).
"""
function warn(io::IO, msg...;
              prefix="WARNING: ", once=false, key=nothing, bt=nothing,
              filename=nothing, lineno::Int=0)
    str = chomp(string(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in have_warned) && return
        push!(have_warned, key)
    end
    io = redirect(io, log_warn_to, :warn)
    print_with_color(warn_color(), io, prefix; bold = true)
    print_with_color(warn_color(), io, str)
    if bt !== nothing
        show_backtrace(io, bt)
    end
    if filename !== nothing
        print(io, "\nwhile loading $filename, in expression starting on line $lineno")
    end
    println(io)
    return
end

"""
    warn(msg)

Display a warning. Argument `msg` is a string describing the warning to be displayed.

```jldoctest
julia> warn("Beep Beep")
WARNING: Beep Beep
```
"""
warn(msg...; kw...) = warn(STDERR, msg...; kw...)

warn(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    warn(io, sprint(buf->showerror(buf, err)), prefix=prefix; kw...)

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(STDERR, err, prefix=prefix; kw...)

info(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    info(io, sprint(buf->showerror(buf, err)), prefix=prefix; kw...)

info(err::Exception; prefix="ERROR: ", kw...) =
    info(STDERR, err, prefix=prefix; kw...)

function julia_cmd(julia=joinpath(JULIA_HOME, julia_exename()))
    opts = JLOptions()
    cpu_target = unsafe_string(opts.cpu_target)
    image_file = unsafe_string(opts.image_file)
    compile = if opts.compile_enabled == 0
                  "no"
              elseif opts.compile_enabled == 2
                  "all"
              elseif opts.compile_enabled == 3
                  "min"
              else
                  "yes"
              end
    depwarn = if opts.depwarn == 0
                  "no"
              elseif opts.depwarn == 2
                  "error"
              else
                  "yes"
              end
    `$julia -C$cpu_target -J$image_file --compile=$compile --depwarn=$depwarn`
end

julia_exename() = ccall(:jl_is_debugbuild,Cint,())==0 ? "julia" : "julia-debug"

"""
    securezero!(o)

`securezero!` fills the memory associated with an object `o` with zeros.
Unlike `fill!(o,0)` and similar code, which might be optimized away by
the compiler for objects about to be discarded, the `securezero!` function
will always be called.
"""
function securezero! end
@noinline securezero!{T<:Number}(a::AbstractArray{T}) = fill!(a, 0)
securezero!(s::String) = unsafe_securezero!(pointer(s), sizeof(s))
@noinline unsafe_securezero!{T}(p::Ptr{T}, len::Integer=1) =
    ccall(:memset, Ptr{T}, (Ptr{T}, Cint, Csize_t), p, 0, len*sizeof(T))
unsafe_securezero!(p::Ptr{Void}, len::Integer=1) = Ptr{Void}(unsafe_securezero!(Ptr{UInt8}(p), len))

if is_windows()
function getpass(prompt::AbstractString)
    print(prompt)
    flush(STDOUT)
    p = Array{UInt8}(128) # mimic Unix getpass in ignoring more than 128-char passwords
                          # (also avoids any potential memory copies arising from push!)
    try
        plen = 0
        while true
            c = ccall(:_getch, UInt8, ())
            if c == 0xff || c == UInt8('\n') || c == UInt8('\r')
                break # EOF or return
            elseif c == 0x00 || c == 0xe0
                ccall(:_getch, UInt8, ()) # ignore function/arrow keys
            elseif c == UInt8('\b') && plen > 0
                plen -= 1 # delete last character on backspace
            elseif !iscntrl(Char(c)) && plen < 128
                p[plen += 1] = c
            end
        end
        return unsafe_string(pointer(p), plen) # use unsafe_string rather than String(p[1:plen])
                                               # to be absolutely certain we never make an extra copy
    finally
        securezero!(p)
    end

    return ""
end
else
getpass(prompt::AbstractString) = unsafe_string(ccall(:getpass, Cstring, (Cstring,), prompt))
end

# Windows authentication prompt
if is_windows()
    immutable CREDUI_INFO
        cbSize::UInt32
        parent::Ptr{Void}
        pszMessageText::Ptr{UInt16}
        pszCaptionText::Ptr{UInt16}
        banner::Ptr{Void}
    end

    const CREDUIWIN_GENERIC                 = 0x0001
    const CREDUIWIN_IN_CRED_ONLY            = 0x0020
    const CREDUIWIN_ENUMERATE_CURRENT_USER  = 0x0200

    const CRED_PACK_GENERIC_CREDENTIALS     = 0x0004

    const ERROR_SUCCESS                     = 0x0000
    const ERROR_CANCELLED                   = 0x04c7

    function winprompt(message, caption, default_username; prompt_username = true)
        # Step 1: Create an encrypted username/password bundle that will be used to set
        #         the default username (in theory could also provide a default password)
        credbuf = Array{UInt8,1}(1024)
        credbufsize = Ref{UInt32}(sizeof(credbuf))
        succeeded = ccall((:CredPackAuthenticationBufferW, "credui.dll"), stdcall, Bool,
            (UInt32, Cwstring, Cwstring, Ptr{UInt8}, Ptr{UInt32}),
             CRED_PACK_GENERIC_CREDENTIALS, default_username, "", credbuf, credbufsize)
        @assert succeeded

        # Step 2: Create the actual dialog
        #      2.1: Set up the window
        messageArr = Base.cwstring(message)
        captionArr = Base.cwstring(caption)
        pfSave = Ref{Bool}(false)
        cred = Ref{CREDUI_INFO}(CREDUI_INFO(sizeof(CREDUI_INFO), C_NULL, pointer(messageArr), pointer(captionArr), C_NULL))
        dwflags = CREDUIWIN_GENERIC | CREDUIWIN_ENUMERATE_CURRENT_USER
        if !prompt_username
            # Disable setting anything other than default_username
            dwflags |= CREDUIWIN_IN_CRED_ONLY
        end
        authPackage = Ref{Culong}(0)
        outbuf_data = Ref{Ptr{Void}}(C_NULL)
        outbuf_size = Ref{Culong}(0)

        #      2.2: Do the actual request
        code = ccall((:CredUIPromptForWindowsCredentialsW, "credui.dll"), stdcall, UInt32, (Ptr{CREDUI_INFO}, UInt32, Ptr{Culong},
            Ptr{Void}, Culong, Ptr{Ptr{Void}}, Ptr{Culong}, Ptr{Bool}, UInt32), cred, 0, authPackage, credbuf, credbufsize[],
            outbuf_data, outbuf_size, pfSave, dwflags)

        #      2.3: If that failed for any reason other than the user canceling, error out.
        #           If the user canceled, just return a nullable
        if code == ERROR_CANCELLED
            return Nullable{Tuple{String,String}}()
        elseif code != ERROR_SUCCESS
            error(Base.Libc.FormatMessage(code))
        end

        # Step 3: Convert encrypted credentials back to plain text
        passbuf = Array{UInt16,1}(1024)
        passlen = Ref{UInt32}(length(passbuf))
        usernamebuf = Array{UInt16,1}(1024)
        usernamelen = Ref{UInt32}(length(usernamebuf))
        # Need valid buffers for domain, even though we don't care
        dummybuf = Array{UInt16,1}(1024)
        succeeded = ccall((:CredUnPackAuthenticationBufferW, "credui.dll"), Bool,
            (UInt32, Ptr{Void}, UInt32, Ptr{UInt16}, Ptr{UInt32}, Ptr{UInt16}, Ptr{UInt32}, Ptr{UInt16}, Ptr{UInt32}),
            0, outbuf_data[], outbuf_size[], usernamebuf, usernamelen, dummybuf, Ref{UInt32}(1024), passbuf, passlen)
        if !succeeded
            error(Base.Libc.FormatMessage())
        end

        # Step 4: Free the encrypted buffer
        # ccall(:SecureZeroMemory, Ptr{Void}, (Ptr{Void}, Csize_t), outbuf_data[], outbuf_size[]) - not an actual function
        unsafe_securezero!(outbuf_data[], outbuf_size[])
        ccall((:CoTaskMemFree, "ole32.dll"), Void, (Ptr{Void},), outbuf_data[])

        # Done.
        passbuf_ = passbuf[1:passlen[]-1]
        result = Nullable((String(transcode(UInt8, usernamebuf[1:usernamelen[]-1])),
            String(transcode(UInt8, passbuf_))))
        securezero!(passbuf_)
        securezero!(passbuf)

        return result
    end

end

"""
    crc32c(data, crc::UInt32=0x00000000)
Compute the CRC-32c checksum of the given `data`, which can be
an `Array{UInt8}` or a `String`.  Optionally, you can pass
a starting `crc` integer to be mixed in with the checksum.
(Technically, a little-endian checksum is computed.)
"""
function crc32c end
crc32c(a::Union{Array{UInt8},String}, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, sizeof(a))

"""
    @kwdef typedef

This is a helper macro that automatically defines a keyword-based constructor for the type
declared in the expression `typedef`, which must be a `type` or `immutable`
expression. The default argument is supplied by declaring fields of the form `field::T =
default`. If no default is provided then the default is provided by the `kwdef_val(T)`
function.

```julia
@kwdef immutable Foo
    a::Cint            # implied default Cint(0)
    b::Cint = 1        # specified default
    z::Cstring         # implied default Cstring(C_NULL)
    y::Bar             # implied default Bar()
end
```
"""
macro kwdef(expr)
    expr = macroexpand(expr) # to expand @static
    T = expr.args[2]
    params_ex = Expr(:parameters)
    call_ex = Expr(:call, T)
    _kwdef!(expr.args[3], params_ex, call_ex)
    quote
        Base.@__doc__($(esc(expr)))
        $(esc(Expr(:call,T,params_ex))) = $(esc(call_ex))
    end
end

# @kwdef helper function
# mutates arguments inplace
function _kwdef!(blk, params_ex, call_ex)
    for i in eachindex(blk.args)
        ei = blk.args[i]
        isa(ei, Expr) || continue
        if ei.head == :(=)
            # var::Typ = defexpr
            dec = ei.args[1]  # var::Typ
            var = dec.args[1] # var
            def = ei.args[2]  # defexpr
            push!(params_ex.args, Expr(:kw, var, def))
            push!(call_ex.args, var)
            blk.args[i] = dec
        elseif ei.head == :(::)
            dec = ei # var::Typ
            var = dec.args[1] # var
            def = :(Base.kwdef_val($(ei.args[2])))
            push!(params_ex.args, Expr(:kw, var, def))
            push!(call_ex.args, dec.args[1])
        elseif ei.head == :block
            # can arise with use of @static inside type decl
            _kwdef!(ei, params_ex, call_ex)
        end
    end
    blk
end



"""
    kwdef_val(T)

The default value for a type for use with the `@kwdef` macro. Returns:

 - null pointer for pointer types (`Ptr{T}`, `Cstring`, `Cwstring`)
 - zero for integer types
 - no-argument constructor calls (e.g. `T()`) for all other types
"""
function kwdef_val end

kwdef_val{T}(::Type{Ptr{T}}) = Ptr{T}(C_NULL)
kwdef_val(::Type{Cstring}) = Cstring(C_NULL)
kwdef_val(::Type{Cwstring}) = Cwstring(C_NULL)

kwdef_val{T<:Integer}(::Type{T}) = zero(T)

kwdef_val{T}(::Type{T}) = T()
