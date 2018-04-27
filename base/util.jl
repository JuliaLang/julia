# This file is a part of Julia. License is MIT: https://julialang.org/license


# This type must be kept in sync with the C struct in src/gc.h
struct GC_Num
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

function format_bytes(bytes)
    bytes, mb = prettyprint_getunits(bytes, length(_mem_units), Int64(1024))
    if mb == 1
        Printf.@sprintf("%d %s%s", bytes, _mem_units[mb], bytes==1 ? "" : "s")
    else
        Printf.@sprintf("%.3f %s", bytes, _mem_units[mb])
    end
end

function time_print(elapsedtime, bytes=0, gctime=0, allocs=0)
    Printf.@printf("%10.6f seconds", elapsedtime/1e9)
    if bytes != 0 || allocs != 0
        allocs, ma = prettyprint_getunits(allocs, length(_cnt_units), Int64(1000))
        if ma == 1
            Printf.@printf(" (%d%s allocation%s: ", allocs, _cnt_units[ma], allocs==1 ? "" : "s")
        else
            Printf.@printf(" (%.2f%s allocations: ", allocs, _cnt_units[ma])
        end
        print(format_bytes(bytes))
        if gctime > 0
            Printf.@printf(", %.2f%% gc time", 100*gctime/elapsedtime)
        end
        print(")")
    elseif gctime > 0
        Printf.@printf(", %.2f%% gc time", 100*gctime/elapsedtime)
    end
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

```julia-repl
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

```julia-repl
julia> val, t, bytes, gctime, memallocs = @timed rand(10^6);

julia> t
0.006634834

julia> bytes
8000256

julia> gctime
0.0055765

julia> fieldnames(typeof(memallocs))
(:allocd, :malloc, :realloc, :poolalloc, :bigalloc, :freecall, :total_time, :pause, :full_sweep)

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


## printing with color ##

function with_output_color(f::Function, color::Union{Int, Symbol}, io::IO, args...; bold::Bool = false)
    buf = IOBuffer()
    iscolor = get(io, :color, false)
    try f(IOContext(buf, io), args...)
    finally
        str = String(take!(buf))
        if !iscolor
            print(io, str)
        else
            bold && color == :bold && (color = :nothing)
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

function julia_cmd(julia=joinpath(Sys.BINDIR, julia_exename()))
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
securezero!(s::String) = unsafe_securezero!(pointer(s), sizeof(s))
@noinline unsafe_securezero!(p::Ptr{T}, len::Integer=1) where {T} =
    ccall(:memset, Ptr{T}, (Ptr{T}, Cint, Csize_t), p, 0, len*sizeof(T))
unsafe_securezero!(p::Ptr{Cvoid}, len::Integer=1) = Ptr{Cvoid}(unsafe_securezero!(Ptr{UInt8}(p), len))

if Sys.iswindows()
function getpass(prompt::AbstractString)
    print(prompt)
    flush(stdout)
    p = Vector{UInt8}(undef, 128) # mimic Unix getpass in ignoring more than 128-char passwords
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

"""
    prompt(message; default="", password=false) -> Union{String, Nothing}

Displays the `message` then waits for user input. Input is terminated when a newline (\\n)
is encountered or EOF (^D) character is entered on a blank line. If a `default` is provided
then the user can enter just a newline character to select the `default`. Alternatively,
when the `password` keyword is `true` the characters entered by the user will not be
displayed.
"""
function prompt(message::AbstractString; default::AbstractString="", password::Bool=false)
    if Sys.iswindows() && password
        error("Command line prompt not supported for password entry on windows. Use `Base.winprompt` instead")
    end
    msg = !isempty(default) ? "$message [$default]:" : "$message:"
    if password
        # `getpass` automatically chomps. We cannot tell an EOF from a '\n'.
        uinput = getpass(msg)
    else
        print(msg)
        uinput = readline(keep=true)
        isempty(uinput) && return nothing  # Encountered an EOF
        uinput = chomp(uinput)
    end
    isempty(uinput) ? default : uinput
end

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
        if code == ERROR_CANCELLED
            return nothing
        elseif code != ERROR_SUCCESS
            error(Base.Libc.FormatMessage(code))
        end

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
        if !succeeded
            error(Base.Libc.FormatMessage())
        end

        # Step 4: Free the encrypted buffer
        # ccall(:SecureZeroMemory, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), outbuf_data[], outbuf_size[]) - not an actual function
        unsafe_securezero!(outbuf_data[], outbuf_size[])
        ccall((:CoTaskMemFree, "ole32.dll"), Cvoid, (Ptr{Cvoid},), outbuf_data[])

        # Done.
        passbuf_ = passbuf[1:passlen[]-1]
        result = (String(transcode(UInt8, usernamebuf[1:usernamelen[]-1])),
                  String(transcode(UInt8, passbuf_)))
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
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0"))
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
default`. If no default is provided then the default is provided by the `kwdef_val(T)`
function.

# Examples
```jldoctest
julia> struct Bar end

julia> Base.@kwdef struct Foo
           a::Cint            # implied default Cint(0)
           b::Cint = 1        # specified default
           z::Cstring         # implied default Cstring(C_NULL)
           y::Bar             # implied default Bar()
       end
Foo

julia> Foo()
Foo(0, 1, Cstring(0x0000000000000000), Bar())
```
"""
macro kwdef(expr)
    expr = macroexpand(__module__, expr) # to expand @static
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

# Examples
```jldoctest
julia> struct Foo
           i::Int
       end

julia> Base.kwdef_val(::Type{Foo}) = Foo(42)

julia> Base.@kwdef struct Bar
           y::Foo
       end
Bar

julia> Bar()
Bar(Foo(42))
```
"""
function kwdef_val end

kwdef_val(::Type{Ptr{T}}) where {T} = Ptr{T}(C_NULL)
kwdef_val(::Type{Cstring}) = Cstring(C_NULL)
kwdef_val(::Type{Cwstring}) = Cwstring(C_NULL)

kwdef_val(::Type{T}) where {T<:Integer} = zero(T)

kwdef_val(::Type{T}) where {T} = T()

# testing

"""
    Base.runtests(tests=["all"]; ncores=ceil(Int, Sys.CPU_CORES / 2),
                  exit_on_error=false, [seed])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `ncores` processors. If `exit_on_error` is `false`, when one test
fails, all remaining tests in other files will still be run; they are otherwise discarded,
when `exit_on_error == true`.
If a seed is provided via the keyword argument, it is used to seed the
global RNG in the context where the tests are run; otherwise the seed is chosen randomly.
"""
function runtests(tests = ["all"]; ncores = ceil(Int, Sys.CPU_CORES / 2),
                  exit_on_error=false,
                  seed::Union{BitInteger,Nothing}=nothing)
    if isa(tests,AbstractString)
        tests = split(tests)
    end
    exit_on_error && push!(tests, "--exit-on-error")
    seed != nothing && push!(tests, "--seed=0x$(string(seed % UInt128, base=16))") # cast to UInt128 to avoid a minus sign
    ENV2 = copy(ENV)
    ENV2["JULIA_CPU_CORES"] = "$ncores"
    try
        run(setenv(`$(julia_cmd()) $(joinpath(Sys.BINDIR,
            Base.DATAROOTDIR, "julia", "test", "runtests.jl")) $tests`, ENV2))
    catch
        buf = PipeBuffer()
        Base.require(Base, :InteractiveUtils).versioninfo(buf)
        error("A test has failed. Please submit a bug report (https://github.com/JuliaLang/julia/issues)\n" *
              "including error messages above and the output of versioninfo():\n$(read(buf, String))")
    end
end
