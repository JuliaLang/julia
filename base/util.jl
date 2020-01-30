# This file is a part of Julia. License is MIT: https://julialang.org/license


# This type must be kept in sync with the C struct in src/gc.h
struct GC_Num
    allocd          ::Int64 # GC internal
    deferred_alloc  ::Int64 # GC internal
    freed           ::Int64 # GC internal
    malloc          ::Int64
    realloc         ::Int64
    poolalloc       ::Int64
    bigalloc        ::Int64
    freecall        ::Int64
    total_time      ::Int64
    total_allocd    ::Int64 # GC internal
    since_sweep     ::Int64 # GC internal
    collect         ::Csize_t # GC internal
    pause           ::Cint
    full_sweep      ::Cint
end

gc_num() = ccall(:jl_gc_num, GC_Num, ())

# This type is to represent differences in the counters, so fields may be negative
struct GC_Diff
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
    gc_num.allocd + gc_num.deferred_alloc + gc_num.total_allocd

function GC_Diff(new::GC_Num, old::GC_Num)
    # logic from `src/gc.c:jl_gc_total_bytes`
    old_allocd = gc_total_bytes(old)
    new_allocd = gc_total_bytes(new)
    return GC_Diff(new_allocd - old_allocd,
                   new.malloc       - old.malloc,
                   new.realloc      - old.realloc,
                   new.poolalloc    - old.poolalloc,
                   new.bigalloc     - old.bigalloc,
                   new.freecall     - old.freecall,
                   new.total_time   - old.total_time,
                   new.pause        - old.pause,
                   new.full_sweep   - old.full_sweep)
end

function gc_alloc_count(diff::GC_Diff)
    diff.malloc + diff.realloc + diff.poolalloc + diff.bigalloc
end


# total time spend in garbage collection, in nanoseconds
gc_time_ns() = ccall(:jl_gc_total_hrtime, UInt64, ())

"""
    Base.gc_live_bytes()

Return the total size (in bytes) of objects currently in memory.
This is computed as the total size of live objects after
the last garbage collection, plus the number of bytes allocated
since then.
"""
function gc_live_bytes()
    num = gc_num()
    Int(ccall(:jl_gc_live_bytes, Int64, ())) + num.allocd + num.deferred_alloc
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

function padded_nonzero_print(value, str)
    if value != 0
        blanks = "                "[1:(18 - length(str))]
        println(str, ":", blanks, value)
    end
end


function format_bytes(bytes) # also used by InteractiveUtils
    bytes, mb = prettyprint_getunits(bytes, length(_mem_units), Int64(1024))
    if mb == 1
        return string(Int(bytes), " ", _mem_units[mb], bytes==1 ? "" : "s")
    else
        return string(Ryu.writefixed(Float64(bytes), 3), " ", _mem_units[mb])
    end
end

function time_print(elapsedtime, bytes=0, gctime=0, allocs=0)
    timestr = Ryu.writefixed(Float64(elapsedtime/1e9), 6)
    length(timestr) < 10 && print(" "^(10 - length(timestr)))
    print(timestr, " seconds")
    if bytes != 0 || allocs != 0
        allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
        if ma == 1
            print(" (", Int(allocs), _cnt_units[ma], allocs==1 ? " allocation: " : " allocations: ")
        else
            print(" (", Ryu.writefixed(Float64(allocs), 2), _cnt_units[ma], " allocations: ")
        end
        print(format_bytes(bytes))
    end
    if gctime > 0
        print(", ", Ryu.writefixed(Float64(100*gctime/elapsedtime), 2), "% gc time")
    end
    if bytes != 0 || allocs != 0
        print(")")
    end
    nothing
end

function timev_print(elapsedtime, diff::GC_Diff)
    allocs = gc_alloc_count(diff)
    time_print(elapsedtime, diff.allocd, diff.total_time, allocs)
    print("\nelapsed time (ns): $elapsedtime\n")
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

!!! note
    For more serious benchmarking, consider the `@btime` macro from the BenchmarkTools.jl
    package which among other things evaluates the function multiple times in order to
    reduce noise.

```julia-repl
julia> @time rand(10^6);
  0.001525 seconds (7 allocations: 7.630 MiB)

julia> @time begin
           sleep(0.3)
           1+1
       end
  0.301395 seconds (8 allocations: 336 bytes)
2
```
"""
macro time(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        time_print(elapsedtime, diff.allocd, diff.total_time,
                   gc_alloc_count(diff))
        println()
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

```julia-repl
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
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
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

```julia-repl
julia> @elapsed sleep(0.3)
0.301391426
```
"""
macro elapsed(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local t0 = time_ns()
        $(esc(ex))
        (time_ns() - t0) / 1e9
    end
end

# total number of bytes allocated so far
gc_bytes(b::Ref{Int64}) = ccall(:jl_gc_get_total_bytes, Cvoid, (Ptr{Int64},), b)
# NOTE: gc_bytes() is deprecated
function gc_bytes()
    b = Ref{Int64}()
    gc_bytes(b)
    b[]
end

"""
    @allocated

A macro to evaluate an expression, discarding the resulting value, instead returning the
total number of bytes allocated during evaluation of the expression.

See also [`@time`](@ref), [`@timev`](@ref), [`@timed`](@ref),
and [`@elapsed`](@ref).

```julia-repl
julia> @allocated rand(10^6)
8000080
```
"""
macro allocated(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local b0 = Ref{Int64}(0)
        local b1 = Ref{Int64}(0)
        gc_bytes(b0)
        $(esc(ex))
        gc_bytes(b1)
        b1[] - b0[]
    end
end

"""
    @timed

A macro to execute an expression, and return the value of the expression, elapsed time,
total bytes allocated, garbage collection time, and an object with various memory allocation
counters.

See also [`@time`](@ref), [`@timev`](@ref), [`@elapsed`](@ref), and
[`@allocated`](@ref).

```julia-repl
julia> stats = @timed rand(10^6);

julia> stats.time
0.006634834

julia> stats.bytes
8000256

julia> stats.gctime
0.0055765

julia> propertynames(stats.gcstats)
(:allocd, :malloc, :realloc, :poolalloc, :bigalloc, :freecall, :total_time, :pause, :full_sweep)

julia> stats.gcstats.total_time
5576500
```

!!! compat "Julia 1.5"
    The return type of this macro was changed from `Tuple` to `NamedTuple` in Julia 1.5.
"""
macro timed(ex)
    quote
        while false; end # compiler heuristic: compile this block (alter this if the heuristic changes)
        local stats = gc_num()
        local elapsedtime = time_ns()
        local val = $(esc(ex))
        elapsedtime = time_ns() - elapsedtime
        local diff = GC_Diff(gc_num(), stats)
        (value=val, time=elapsedtime/1e9, bytes=diff.allocd, gctime=diff.total_time/1e9, gcstats=diff)
    end
end


## printing with color ##

const text_colors = Dict{Union{Symbol,Int},String}(
    :black         => "\033[30m",
    :red           => "\033[31m",
    :green         => "\033[32m",
    :yellow        => "\033[33m",
    :blue          => "\033[34m",
    :magenta       => "\033[35m",
    :cyan          => "\033[36m",
    :white         => "\033[37m",
    :light_black   => "\033[90m", # gray
    :light_red     => "\033[91m",
    :light_green   => "\033[92m",
    :light_yellow  => "\033[93m",
    :light_blue    => "\033[94m",
    :light_magenta => "\033[95m",
    :light_cyan    => "\033[96m",
    :normal        => "\033[0m",
    :default       => "\033[39m",
    :bold          => "\033[1m",
    :underline     => "\033[4m",
    :blink         => "\033[5m",
    :reverse       => "\033[7m",
    :hidden        => "\033[8m",
    :nothing       => "",
)

for i in 0:255
    text_colors[i] = "\033[38;5;$(i)m"
end

const disable_text_style = Dict{Symbol,String}(
    :bold      => "\033[22m",
    :underline => "\033[24m",
    :blink     => "\033[25m",
    :reverse   => "\033[27m",
    :hidden    => "\033[28m",
    :normal    => "",
    :default   => "",
    :nothing   => "",
)

# Create a docstring with an automatically generated list
# of colors.
available_text_colors = collect(Iterators.filter(x -> !isa(x, Integer), keys(text_colors)))
const possible_formatting_symbols = [:normal, :bold, :default]
available_text_colors = cat(
    sort!(intersect(available_text_colors, possible_formatting_symbols), rev=true),
    sort!(setdiff(  available_text_colors, possible_formatting_symbols));
    dims=1)

const available_text_colors_docstring =
    string(join([string("`:", key,"`")
                 for key in available_text_colors], ",\n", ", or \n"))

"""Dictionary of color codes for the terminal.

Available colors are: $available_text_colors_docstring as well as the integers 0 to 255 inclusive.

The color `:default` will print text in the default color while the color `:normal`
will print text with all text properties (like boldness) reset.
Printing with the color `:nothing` will print the string without modifications.
"""
text_colors

function with_output_color(f::Function, color::Union{Int, Symbol}, io::IO, args...; bold::Bool = false)
    buf = IOBuffer()
    iscolor = get(io, :color, false)
    try f(IOContext(buf, io), args...)
    finally
        str = String(take!(buf))
        if !iscolor
            print(io, str)
        else
            bold && color === :bold && (color = :nothing)
            enable_ansi  = get(text_colors, color, text_colors[:default]) *
                               (bold ? text_colors[:bold] : "")
            disable_ansi = (bold ? disable_text_style[:bold] : "") *
                               get(disable_text_style, color, text_colors[:default])
            first = true
            for line in split(str, '\n')
                first || print(buf, '\n')
                first = false
                isempty(line) && continue
                print(buf, enable_ansi, line, disable_ansi)
            end
            print(io, String(take!(buf)))
        end
    end
end

"""
    printstyled([io], xs...; bold::Bool=false, color::Union{Symbol,Int}=:normal)

Print `xs` in a color specified as a symbol or integer, optionally in bold.

`color` may take any of the values $(Base.available_text_colors_docstring)
or an integer between 0 and 255 inclusive. Note that not all terminals support 256 colors.
If the keyword `bold` is given as `true`, the result will be printed in bold.
"""
printstyled(io::IO, msg...; bold::Bool=false, color::Union{Int,Symbol}=:normal) =
    with_output_color(print, color, io, msg...; bold=bold)
printstyled(msg...; bold::Bool=false, color::Union{Int,Symbol}=:normal) =
    printstyled(stdout, msg...; bold=bold, color=color)

"""
    Base.julia_cmd(juliapath=joinpath(Sys.BINDIR::String, julia_exename()))

Return a julia command similar to the one of the running process.
Propagates any of the `--cpu-target`, `--sysimage`, `--compile`, `--sysimage-native-code`,
`--compiled-modules`, `--inline`, `--check-bounds`, `--optimize`, `-g`,
`--code-coverage`, and `--depwarn`
command line arguments that are not at their default values.

Among others, `--math-mode`, `--warn-overwrite`, and `--trace-compile` are notably not propagated currently.

!!! compat "Julia 1.1"
    Only the `--cpu-target`, `--sysimage`, `--depwarn`, `--compile` and `--check-bounds` flags were propagated before Julia 1.1.
"""
function julia_cmd(julia=joinpath(Sys.BINDIR::String, julia_exename()))
    opts = JLOptions()
    cpu_target = unsafe_string(opts.cpu_target)
    image_file = unsafe_string(opts.image_file)
    addflags = String[]
    let compile = if opts.compile_enabled == 0
                      "no"
                  elseif opts.compile_enabled == 2
                      "all"
                  elseif opts.compile_enabled == 3
                      "min"
                  else
                      "" # default = "yes"
                  end
        isempty(compile) || push!(addflags, "--compile=$compile")
    end
    let depwarn = if opts.depwarn == 0
                      "no"
                  elseif opts.depwarn == 2
                      "error"
                  else
                      "" # default = "yes"
                  end
        isempty(depwarn) || push!(addflags, "--depwarn=$depwarn")
    end
    let check_bounds = if opts.check_bounds == 1
                      "yes" # on
                  elseif opts.check_bounds == 2
                      "no" # off
                  else
                      "" # "default"
                  end
        isempty(check_bounds) || push!(addflags, "--check-bounds=$check_bounds")
    end
    opts.can_inline == 0 && push!(addflags, "--inline=no")
    opts.use_compiled_modules == 0 && push!(addflags, "--compiled-modules=no")
    opts.opt_level == 2 || push!(addflags, "-O$(opts.opt_level)")
    push!(addflags, "-g$(opts.debug_level)")
    if opts.code_coverage != 0
        # Forward the code-coverage flag only if applicable (if the filename is pid-dependent)
        coverage_file = (opts.output_code_coverage != C_NULL) ?  unsafe_string(opts.output_code_coverage) : ""
        if isempty(coverage_file) || occursin("%p", coverage_file)
            if opts.code_coverage == 1
                push!(addflags, "--code-coverage=user")
            elseif opts.code_coverage == 2
                push!(addflags, "--code-coverage=all")
            end
            isempty(coverage_file) || push!(addflags, "--code-coverage=$coverage_file")
        end
    end
    if opts.malloc_log != 0
        if opts.malloc_log == 1
            push!(addflags, "--track-allocation=user")
        elseif opts.malloc_log == 2
            push!(addflags, "--track-allocation=all")
        end
    end
    return `$julia -C$cpu_target -J$image_file $addflags`
end

function julia_exename()
    if ccall(:jl_is_debugbuild, Cint, ()) == 0
        return @static Sys.iswindows() ? "julia.exe" : "julia"
    else
        return @static Sys.iswindows() ? "julia-debug.exe" : "julia-debug"
    end
end

"""
    securezero!(o)

`securezero!` fills the memory associated with an object `o` with zeros.
Unlike `fill!(o,0)` and similar code, which might be optimized away by
the compiler for objects about to be discarded, the `securezero!` function
will always be called.
"""
function securezero! end
@noinline securezero!(a::AbstractArray{<:Number}) = fill!(a, 0)
@noinline unsafe_securezero!(p::Ptr{T}, len::Integer=1) where {T} =
    ccall(:memset, Ptr{T}, (Ptr{T}, Cint, Csize_t), p, 0, len*sizeof(T))
unsafe_securezero!(p::Ptr{Cvoid}, len::Integer=1) = Ptr{Cvoid}(unsafe_securezero!(Ptr{UInt8}(p), len))

"""
    Base.getpass(message::AbstractString) -> Base.SecretBuffer

Display a message and wait for the user to input a secret, returning an `IO`
object containing the secret.

Note that on Windows, the secret might be displayed as it is typed; see
`Base.winprompt` for securely retrieving username/password pairs from a
graphical interface.
"""
function getpass end

if Sys.iswindows()
function getpass(input::TTY, output::IO, prompt::AbstractString)
    input === stdin || throw(ArgumentError("getpass only works for stdin"))
    print(output, prompt, ": ")
    flush(output)
    s = SecretBuffer()
    plen = 0
    while true
        c = UInt8(ccall(:_getch, Cint, ()))
        if c == 0xff || c == UInt8('\n') || c == UInt8('\r')
            break # EOF or return
        elseif c == 0x00 || c == 0xe0
            ccall(:_getch, Cint, ()) # ignore function/arrow keys
        elseif c == UInt8('\b') && plen > 0
            plen -= 1 # delete last character on backspace
        elseif !iscntrl(Char(c)) && plen < 128
            write(s, c)
        end
    end
    return seekstart(s)
end
else
function getpass(input::TTY, output::IO, prompt::AbstractString)
    (input === stdin && output === stdout) || throw(ArgumentError("getpass only works for stdin"))
    msg = string(prompt, ": ")
    unsafe_SecretBuffer!(ccall(:getpass, Cstring, (Cstring,), msg))
end
end

# allow new getpass methods to be defined if stdin has been
# redirected to some custom stream, e.g. in IJulia.
getpass(prompt::AbstractString) = getpass(stdin, stdout, prompt)

"""
    prompt(message; default="") -> Union{String, Nothing}

Displays the `message` then waits for user input. Input is terminated when a newline (\\n)
is encountered or EOF (^D) character is entered on a blank line. If a `default` is provided
then the user can enter just a newline character to select the `default`.

See also `Base.getpass` and `Base.winprompt` for secure entry of passwords.
"""
function prompt(input::IO, output::IO, message::AbstractString; default::AbstractString="")
    msg = !isempty(default) ? "$message [$default]: " : "$message: "
    print(output, msg)
    uinput = readline(input, keep=true)
    isempty(uinput) && return nothing  # Encountered an EOF
    uinput = chomp(uinput)
    isempty(uinput) ? default : uinput
end

# allow new prompt methods to be defined if stdin has been
# redirected to some custom stream, e.g. in IJulia.
prompt(message::AbstractString; default::AbstractString="") = prompt(stdin, stdout, message, default=default)

# Windows authentication prompt
if Sys.iswindows()
    struct CREDUI_INFO
        cbSize::UInt32
        parent::Ptr{Cvoid}
        pszMessageText::Ptr{UInt16}
        pszCaptionText::Ptr{UInt16}
        banner::Ptr{Cvoid}
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
        credbuf = Vector{UInt8}(undef, 1024)
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
        outbuf_data = Ref{Ptr{Cvoid}}(C_NULL)
        outbuf_size = Ref{Culong}(0)

        #      2.2: Do the actual request
        code = ccall((:CredUIPromptForWindowsCredentialsW, "credui.dll"), stdcall, UInt32, (Ptr{CREDUI_INFO}, UInt32, Ptr{Culong},
            Ptr{Cvoid}, Culong, Ptr{Ptr{Cvoid}}, Ptr{Culong}, Ptr{Bool}, UInt32), cred, 0, authPackage, credbuf, credbufsize[],
            outbuf_data, outbuf_size, pfSave, dwflags)

        #      2.3: If that failed for any reason other than the user canceling, error out.
        #           If the user canceled, just return nothing
        code == ERROR_CANCELLED && return nothing
        windowserror(:winprompt, code != ERROR_SUCCESS)

        # Step 3: Convert encrypted credentials back to plain text
        passbuf = Vector{UInt16}(undef, 1024)
        passlen = Ref{UInt32}(length(passbuf))
        usernamebuf = Vector{UInt16}(undef, 1024)
        usernamelen = Ref{UInt32}(length(usernamebuf))
        # Need valid buffers for domain, even though we don't care
        dummybuf = Vector{UInt16}(undef, 1024)
        succeeded = ccall((:CredUnPackAuthenticationBufferW, "credui.dll"), Bool,
            (UInt32, Ptr{Cvoid}, UInt32, Ptr{UInt16}, Ptr{UInt32}, Ptr{UInt16}, Ptr{UInt32}, Ptr{UInt16}, Ptr{UInt32}),
            0, outbuf_data[], outbuf_size[], usernamebuf, usernamelen, dummybuf, Ref{UInt32}(1024), passbuf, passlen)
        windowserror(:winprompt, !succeeded)

        # Step 4: Free the encrypted buffer
        # ccall(:SecureZeroMemory, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), outbuf_data[], outbuf_size[]) - not an actual function
        unsafe_securezero!(outbuf_data[], outbuf_size[])
        ccall((:CoTaskMemFree, "ole32.dll"), Cvoid, (Ptr{Cvoid},), outbuf_data[])

        # Done.
        passbuf_ = passbuf[1:passlen[]-1]
        result = (String(transcode(UInt8, usernamebuf[1:usernamelen[]-1])),
                  SecretBuffer!(transcode(UInt8, passbuf_)))
        securezero!(passbuf_)
        securezero!(passbuf)

        return result
    end

end

unsafe_crc32c(a, n, crc) = ccall(:jl_crc32c, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, n)

_crc32c(a::Union{Array{UInt8},FastContiguousSubArray{UInt8,N,<:Array{UInt8}} where N}, crc::UInt32=0x00000000) =
    unsafe_crc32c(a, length(a) % Csize_t, crc)

_crc32c(s::String, crc::UInt32=0x00000000) = unsafe_crc32c(s, sizeof(s) % Csize_t, crc)

function _crc32c(io::IO, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0, got $nb"))
    # use block size 24576=8192*3, since that is the threshold for
    # 3-way parallel SIMD code in the underlying jl_crc32c C function.
    buf = Vector{UInt8}(undef, min(nb, 24576))
    while !eof(io) && nb > 24576
        n = readbytes!(io, buf)
        crc = unsafe_crc32c(buf, n, crc)
        nb -= n
    end
    return unsafe_crc32c(buf, readbytes!(io, buf, min(nb, length(buf))), crc)
end
_crc32c(io::IO, crc::UInt32=0x00000000) = _crc32c(io, typemax(Int64), crc)
_crc32c(io::IOStream, crc::UInt32=0x00000000) = _crc32c(io, filesize(io)-position(io), crc)
_crc32c(uuid::UUID, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt128}, Csize_t), crc, uuid.value, 16)

"""
    @kwdef typedef

This is a helper macro that automatically defines a keyword-based constructor for the type
declared in the expression `typedef`, which must be a `struct` or `mutable struct`
expression. The default argument is supplied by declaring fields of the form `field::T =
default` or `field = default`. If no default is provided then the keyword argument becomes
a required keyword argument in the resulting type constructor.

Inner constructors can still be defined, but at least one should accept arguments in the
same form as the default inner constructor (i.e. one positional argument per field) in
order to function correctly with the keyword outer constructor.

!!! compat "Julia 1.1"
    `Base.@kwdef` for parametric structs, and structs with supertypes
    requires at least Julia 1.1.

# Examples
```jldoctest
julia> Base.@kwdef struct Foo
           a::Int = 1         # specified default
           b::String          # required keyword
       end
Foo

julia> Foo(b="hi")
Foo(1, "hi")

julia> Foo()
ERROR: UndefKeywordError: keyword argument b not assigned
Stacktrace:
[...]
```
"""
macro kwdef(expr)
    expr = macroexpand(__module__, expr) # to expand @static
    expr isa Expr && expr.head === :struct || error("Invalid usage of @kwdef")
    T = expr.args[2]
    if T isa Expr && T.head === :<:
        T = T.args[1]
    end

    params_ex = Expr(:parameters)
    call_args = Any[]

    _kwdef!(expr.args[3], params_ex.args, call_args)
    # Only define a constructor if the type has fields, otherwise we'll get a stack
    # overflow on construction
    if !isempty(params_ex.args)
        if T isa Symbol
            kwdefs = :(($(esc(T)))($params_ex) = ($(esc(T)))($(call_args...)))
        elseif T isa Expr && T.head === :curly
            # if T == S{A<:AA,B<:BB}, define two methods
            #   S(...) = ...
            #   S{A,B}(...) where {A<:AA,B<:BB} = ...
            S = T.args[1]
            P = T.args[2:end]
            Q = [U isa Expr && U.head === :<: ? U.args[1] : U for U in P]
            SQ = :($S{$(Q...)})
            kwdefs = quote
                ($(esc(S)))($params_ex) =($(esc(S)))($(call_args...))
                ($(esc(SQ)))($params_ex) where {$(esc.(P)...)} =
                    ($(esc(SQ)))($(call_args...))
            end
        else
            error("Invalid usage of @kwdef")
        end
    else
        kwdefs = nothing
    end
    quote
        Base.@__doc__($(esc(expr)))
        $kwdefs
    end
end

# @kwdef helper function
# mutates arguments inplace
function _kwdef!(blk, params_args, call_args)
    for i in eachindex(blk.args)
        ei = blk.args[i]
        if ei isa Symbol
            #  var
            push!(params_args, ei)
            push!(call_args, ei)
        elseif ei isa Expr
            if ei.head === :(=)
                lhs = ei.args[1]
                if lhs isa Symbol
                    #  var = defexpr
                    var = lhs
                elseif lhs isa Expr && lhs.head === :(::) && lhs.args[1] isa Symbol
                    #  var::T = defexpr
                    var = lhs.args[1]
                else
                    # something else, e.g. inline inner constructor
                    #   F(...) = ...
                    continue
                end
                defexpr = ei.args[2]  # defexpr
                push!(params_args, Expr(:kw, var, esc(defexpr)))
                push!(call_args, var)
                blk.args[i] = lhs
            elseif ei.head === :(::) && ei.args[1] isa Symbol
                # var::Typ
                var = ei.args[1]
                push!(params_args, var)
                push!(call_args, var)
            elseif ei.head === :block
                # can arise with use of @static inside type decl
                _kwdef!(ei, params_args, call_args)
            end
        end
    end
    blk
end

# testing

"""
    Base.runtests(tests=["all"]; ncores=ceil(Int, Sys.CPU_THREADS / 2),
                  exit_on_error=false, [seed])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `ncores` processors. If `exit_on_error` is `false`, when one test
fails, all remaining tests in other files will still be run; they are otherwise discarded,
when `exit_on_error == true`.
If a seed is provided via the keyword argument, it is used to seed the
global RNG in the context where the tests are run; otherwise the seed is chosen randomly.
"""
function runtests(tests = ["all"]; ncores = ceil(Int, Sys.CPU_THREADS / 2),
                  exit_on_error=false,
                  seed::Union{BitInteger,Nothing}=nothing)
    if isa(tests,AbstractString)
        tests = split(tests)
    end
    exit_on_error && push!(tests, "--exit-on-error")
    seed !== nothing && push!(tests, "--seed=0x$(string(seed % UInt128, base=16))") # cast to UInt128 to avoid a minus sign
    ENV2 = copy(ENV)
    ENV2["JULIA_CPU_THREADS"] = "$ncores"
    try
        run(setenv(`$(julia_cmd()) $(joinpath(Sys.BINDIR::String,
            Base.DATAROOTDIR, "julia", "test", "runtests.jl")) $tests`, ENV2))
        nothing
    catch
        buf = PipeBuffer()
        Base.require(Base, :InteractiveUtils).versioninfo(buf)
        error("A test has failed. Please submit a bug report (https://github.com/JuliaLang/julia/issues)\n" *
              "including error messages above and the output of versioninfo():\n$(read(buf, String))")
    end
end
