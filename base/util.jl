# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    :light_white   => "\033[97m",
    :normal        => "\033[0m",
    :default       => "\033[39m",
    :bold          => "\033[1m",
    :italic        => "\033[3m",
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
    :italic    => "\033[23m",
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
let color_syms = collect(Iterators.filter(x -> !isa(x, Integer), keys(text_colors))),
    formatting_syms = [:normal, :bold, :italic, :default]
    global const available_text_colors = cat(
        sort!(intersect(color_syms, formatting_syms), rev=true),
        sort!(setdiff(  color_syms, formatting_syms));
        dims=1)
end

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

function with_output_color(@nospecialize(f::Function), color::Union{Int, Symbol}, io::IO, args...;
        bold::Bool = false, italic::Bool = false, underline::Bool = false, blink::Bool = false,
        reverse::Bool = false, hidden::Bool = false)
    buf = IOBuffer()
    iscolor = get(io, :color, false)::Bool
    try f(IOContext(buf, io), args...)
    finally
        str = String(take!(buf))
        if !iscolor
            print(io, str)
        else
            bold && color === :bold && (color = :nothing)
            italic && color === :italic && (color = :nothing)
            underline && color === :underline && (color = :nothing)
            blink && color === :blink && (color = :nothing)
            reverse && color === :reverse && (color = :nothing)
            hidden && color === :hidden && (color = :nothing)
            enable_ansi  = get(text_colors, color, text_colors[:default]) *
                               (bold ? text_colors[:bold] : "") *
                               (italic ? text_colors[:italic] : "") *
                               (underline ? text_colors[:underline] : "") *
                               (blink ? text_colors[:blink] : "") *
                               (reverse ? text_colors[:reverse] : "") *
                               (hidden ? text_colors[:hidden] : "")

            disable_ansi = (hidden ? disable_text_style[:hidden] : "") *
                           (reverse ? disable_text_style[:reverse] : "") *
                           (blink ? disable_text_style[:blink] : "") *
                           (underline ? disable_text_style[:underline] : "") *
                           (bold ? disable_text_style[:bold] : "") *
                           (italic ? disable_text_style[:italic] : "") *
                               get(disable_text_style, color, text_colors[:default])
            first = true
            for line in eachsplit(str, '\n')
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
    printstyled([io], xs...; bold::Bool=false, italic::Bool=false, underline::Bool=false, blink::Bool=false, reverse::Bool=false, hidden::Bool=false, color::Union{Symbol,Int}=:normal)

Print `xs` in a color specified as a symbol or integer, optionally in bold.

Keyword `color` may take any of the values $(Base.available_text_colors_docstring)
or an integer between 0 and 255 inclusive. Note that not all terminals support 256 colors.

Keywords `bold=true`, `italic=true`, `underline=true`, `blink=true` are self-explanatory.
Keyword `reverse=true` prints with foreground and background colors exchanged,
and `hidden=true` should be invisible in the terminal but can still be copied.
These properties can be used in any combination.

See also [`print`](@ref), [`println`](@ref), [`show`](@ref).

!!! note
    Not all terminals support italic output. Some terminals interpret italic as reverse or
    blink.

!!! compat "Julia 1.7"
    Keywords except `color` and `bold` were added in Julia 1.7.
!!! compat "Julia 1.10"
    Support for italic output was added in Julia 1.10.
"""
@constprop :none printstyled(io::IO, msg...; bold::Bool=false, italic::Bool=false, underline::Bool=false, blink::Bool=false, reverse::Bool=false, hidden::Bool=false, color::Union{Int,Symbol}=:normal) =
    with_output_color(print, color, io, msg...; bold=bold, italic=italic, underline=underline, blink=blink, reverse=reverse, hidden=hidden)
@constprop :none printstyled(msg...; bold::Bool=false, italic::Bool=false, underline::Bool=false, blink::Bool=false, reverse::Bool=false, hidden::Bool=false, color::Union{Int,Symbol}=:normal) =
    printstyled(stdout, msg...; bold=bold, italic=italic, underline=underline, blink=blink, reverse=reverse, hidden=hidden, color=color)

"""
    Base.julia_cmd(juliapath=joinpath(Sys.BINDIR, julia_exename()); cpu_target::Union{Nothing,String}=nothing)

Return a julia command similar to the one of the running process.
Propagates any of the `--cpu-target`, `--sysimage`, `--compile`, `--sysimage-native-code`,
`--compiled-modules`, `--pkgimages`, `--inline`, `--check-bounds`, `--optimize`, `--min-optlevel`, `-g`,
`--code-coverage`, `--track-allocation`, `--color`, `--startup-file`, and `--depwarn`
command line arguments that are not at their default values.

Among others, `--math-mode`, `--warn-overwrite`, and `--trace-compile` are notably not propagated currently.

Unless set to `nothing`, the `cpu_target` keyword argument can be used to override the CPU target set for the running process.

To get the julia command without propagated command line arguments, `julia_cmd()[1]` can be used.

!!! compat "Julia 1.1"
    Only the `--cpu-target`, `--sysimage`, `--depwarn`, `--compile` and `--check-bounds` flags were propagated before Julia 1.1.

!!! compat "Julia 1.5"
    The flags `--color` and `--startup-file` were added in Julia 1.5.

!!! compat "Julia 1.9"
    The keyword argument `cpu_target` was added in 1.9.
    The flag `--pkgimages` was added in Julia 1.9.
"""
function julia_cmd(julia=joinpath(Sys.BINDIR, julia_exename()); cpu_target::Union{Nothing,String} = nothing)
    opts = JLOptions()
    if cpu_target === nothing
        cpu_target = unsafe_string(opts.cpu_target)
    end
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
    let depwarn = if opts.depwarn == 1
                      "yes"
                  elseif opts.depwarn == 2
                      "error"
                  else
                      "" # default = "no"
                  end
        isempty(depwarn) || push!(addflags, "--depwarn=$depwarn")
    end
    let check_bounds = if opts.check_bounds == 1
                      "yes" # on
                  elseif opts.check_bounds == 2
                      "no" # off
                  else
                      "" # default = "auto"
                  end
        isempty(check_bounds) || push!(addflags, "--check-bounds=$check_bounds")
    end
    opts.can_inline == 0 && push!(addflags, "--inline=no")
    opts.use_compiled_modules == 0 && push!(addflags, "--compiled-modules=no")
    opts.use_compiled_modules == 2 && push!(addflags, "--compiled-modules=existing")
    opts.use_compiled_modules == 3 && push!(addflags, "--compiled-modules=strict")
    opts.use_pkgimages == 0 && push!(addflags, "--pkgimages=no")
    opts.use_pkgimages == 2 && push!(addflags, "--pkgimages=existing")
    opts.opt_level == 2 || push!(addflags, "-O$(opts.opt_level)")
    opts.opt_level_min == 0 || push!(addflags, "--min-optlevel=$(opts.opt_level_min)")
    push!(addflags, "-g$(opts.debug_level)")
    if opts.code_coverage != 0
        # Forward the code-coverage flag only if applicable (if the filename is pid-dependent)
        coverage_file = (opts.output_code_coverage != C_NULL) ?  unsafe_string(opts.output_code_coverage) : ""
        if isempty(coverage_file) || occursin("%p", coverage_file)
            if opts.code_coverage == 1
                push!(addflags, "--code-coverage=user")
            elseif opts.code_coverage == 2
                push!(addflags, "--code-coverage=all")
            elseif opts.code_coverage == 3
                push!(addflags, "--code-coverage=@$(unsafe_string(opts.tracked_path))")
            end
            isempty(coverage_file) || push!(addflags, "--code-coverage=$coverage_file")
        end
    end
    if opts.malloc_log == 1
        push!(addflags, "--track-allocation=user")
    elseif opts.malloc_log == 2
        push!(addflags, "--track-allocation=all")
    elseif opts.malloc_log == 3
        push!(addflags, "--track-allocation=@$(unsafe_string(opts.tracked_path))")
    end
    if opts.color == 1
        push!(addflags, "--color=yes")
    elseif opts.color == 2
        push!(addflags, "--color=no")
    end
    if opts.startupfile == 2
        push!(addflags, "--startup-file=no")
    end
    if opts.use_sysimage_native_code == 0
        push!(addflags, "--sysimage-native-code=no")
    end
    return `$julia -C $cpu_target -J$image_file $addflags`
end

function julia_exename()
    if !isdebugbuild()
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
    memset(p, 0, len*sizeof(T))
unsafe_securezero!(p::Ptr{Cvoid}, len::Integer=1) = Ptr{Cvoid}(unsafe_securezero!(Ptr{UInt8}(p), len))

"""
    Base.getpass(message::AbstractString; with_suffix::Bool=true) -> Base.SecretBuffer

Display a message and wait for the user to input a secret, returning an `IO`
object containing the secret. If `with_suffix` is `true` (the default), the
suffix `": "` will be appended to `message`.

!!! info "Windows"
    Note that on Windows, the secret might be displayed as it is typed; see
    `Base.winprompt` for securely retrieving username/password pairs from a
    graphical interface.

!!! compat "Julia 1.12"
    The `with_suffix` keyword argument requires at least Julia 1.12.

# Examples

```julia-repl
julia> Base.getpass("Secret")
Secret: SecretBuffer("*******")

julia> Base.getpass("Secret> "; with_suffix=false)
Secret> SecretBuffer("*******")
```
"""
function getpass end

# Note, this helper only works within `with_raw_tty()` on POSIX platforms!
function _getch()
    @static if Sys.iswindows()
        return UInt8(ccall(:_getch, Cint, ()))
    else
        return read(stdin, UInt8)
    end
end

const termios_size = Int(ccall(:jl_termios_size, Cint, ()))
make_termios() = zeros(UInt8, termios_size)

# These values seem to hold on all OSes we care about:
# glibc Linux, musl Linux, macOS, FreeBSD
@enum TCSETATTR_FLAGS TCSANOW=0 TCSADRAIN=1 TCSAFLUSH=2

function tcgetattr(fd::RawFD, termios)
    ret = ccall(:tcgetattr, Cint, (Cint, Ptr{Cvoid}), fd, termios)
    if ret != 0
        throw(IOError("tcgetattr failed", ret))
    end
end
function tcsetattr(fd::RawFD, termios, mode::TCSETATTR_FLAGS = TCSADRAIN)
    ret = ccall(:tcsetattr, Cint, (Cint, Cint, Ptr{Cvoid}), fd, Cint(mode), termios)
    if ret != 0
        throw(IOError("tcsetattr failed", ret))
    end
end
cfmakeraw(termios) = ccall(:cfmakeraw, Cvoid, (Ptr{Cvoid},), termios)

function with_raw_tty(f::Function, input::TTY)
    input === stdin || throw(ArgumentError("with_raw_tty only works for stdin"))
    fd = RawFD(0)

    # If we're on windows, we do nothing, as we have access to `_getch()` quite easily
    @static if Sys.iswindows()
        return f()
    end

    # Get the current terminal mode
    old_termios = make_termios()
    tcgetattr(fd, old_termios)
    try
        # Set a new, raw, terminal mode
        new_termios = copy(old_termios)
        cfmakeraw(new_termios)
        tcsetattr(fd, new_termios)

        # Call the user-supplied callback
        f()
    finally
        # Always restore the terminal mode
        tcsetattr(fd, old_termios)
    end
end

function getpass(input::TTY, output::IO, prompt::AbstractString; with_suffix::Bool=true)
    input === stdin || throw(ArgumentError("getpass only works for stdin"))
    with_raw_tty(stdin) do
        print(output, prompt)
        with_suffix && print(output, ": ")
        flush(output)

        s = SecretBuffer()
        plen = 0
        while true
            c = _getch()
            if c == 0xff || c == UInt8('\n') || c == UInt8('\r') || c == 0x04
                break # EOF or return
            elseif c == 0x00 || c == 0xe0
                _getch() # ignore function/arrow keys
            elseif c == UInt8('\b') && plen > 0
                plen -= 1 # delete last character on backspace
            elseif !iscntrl(Char(c)) && plen < 128
                write(s, c)
            end
        end
        return seekstart(s)
    end
end

# allow new getpass methods to be defined if stdin has been
# redirected to some custom stream, e.g. in IJulia.
getpass(prompt::AbstractString; with_suffix::Bool=true) = getpass(stdin, stdout, prompt; with_suffix)

"""
    prompt(message; default="") -> Union{String, Nothing}

Displays the `message` then waits for user input. Input is terminated when a newline (\\n)
is encountered or EOF (^D) character is entered on a blank line. If a `default` is provided
then the user can enter just a newline character to select the `default`.

See also `Base.winprompt` (for Windows) and `Base.getpass` for secure entry of passwords.

# Examples

```julia-repl
julia> your_name = Base.prompt("Enter your name");
Enter your name: Logan

julia> your_name
"Logan"
```
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
        if !succeeded
            credbuf = resize!(credbuf, credbufsize[])
            succeeded = ccall((:CredPackAuthenticationBufferW, "credui.dll"), stdcall, Bool,
                (UInt32, Cwstring, Cwstring, Ptr{UInt8}, Ptr{UInt32}),
                 CRED_PACK_GENERIC_CREDENTIALS, default_username, "", credbuf, credbufsize)
            @assert succeeded
        end

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

_crc32c(a::NTuple{<:Any, UInt8}, crc::UInt32=0x00000000) =
    unsafe_crc32c(Ref(a), length(a) % Csize_t, crc)

function _crc32c(a::DenseUInt8OrInt8, crc::UInt32=0x00000000)
    unsafe_crc32c(a, length(a) % Csize_t, crc)
end

function _crc32c(s::Union{String, SubString{String}}, crc::UInt32=0x00000000)
    unsafe_crc32c(s, sizeof(s) % Csize_t, crc)
end

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
_crc32c(x::UInt128, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt128}, Csize_t), crc, x, 16)
_crc32c(x::UInt64, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt64}, Csize_t), crc, x, 8)
_crc32c(x::UInt32, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt32}, Csize_t), crc, x, 4)
_crc32c(x::UInt16, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt16}, Csize_t), crc, x, 2)
_crc32c(x::UInt8, crc::UInt32=0x00000000) =
    ccall(:jl_crc32c, UInt32, (UInt32, Ref{UInt8}, Csize_t), crc, x, 1)

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

!!! compat "Julia 1.9"
    This macro is exported as of Julia 1.9.

# Examples
```jldoctest
julia> @kwdef struct Foo
           a::Int = 1         # specified default
           b::String          # required keyword
       end
Foo

julia> Foo(b="hi")
Foo(1, "hi")

julia> Foo()
ERROR: UndefKeywordError: keyword argument `b` not assigned
Stacktrace:
[...]
```
"""
macro kwdef(expr)
    expr = macroexpand(__module__, expr) # to expand @static
    isexpr(expr, :struct) || error("Invalid usage of @kwdef")
    _, T, fieldsblock = expr.args
    if T isa Expr && T.head === :<:
        T = T.args[1]
    end

    fieldnames = Any[]
    defvals = Any[]
    extract_names_and_defvals_from_kwdef_fieldblock!(fieldsblock, fieldnames, defvals)
    parameters = map(fieldnames, defvals) do fieldname, defval
        if isnothing(defval)
            return fieldname
        else
            return Expr(:kw, fieldname, esc(defval))
        end
    end

    # Only define a constructor if the type has fields, otherwise we'll get a stack
    # overflow on construction
    if !isempty(parameters)
        T_no_esc = Meta.unescape(T)
        if T_no_esc isa Symbol
            sig = Expr(:call, esc(T), Expr(:parameters, parameters...))
            body = Expr(:block, __source__, Expr(:call, esc(T), fieldnames...))
            kwdefs = Expr(:function, sig, body)
        elseif isexpr(T_no_esc, :curly)
            # if T == S{A<:AA,B<:BB}, define two methods
            #   S(...) = ...
            #   S{A,B}(...) where {A<:AA,B<:BB} = ...
            S = T.args[1]
            P = T.args[2:end]
            Q = Any[isexpr(U, :<:) ? U.args[1] : U for U in P]
            SQ = :($S{$(Q...)})
            body1 = Expr(:block, __source__, Expr(:call, esc(S), fieldnames...))
            sig1 = Expr(:call, esc(S), Expr(:parameters, parameters...))
            def1 = Expr(:function, sig1, body1)
            body2 = Expr(:block, __source__, Expr(:call, esc(SQ), fieldnames...))
            sig2 = :($(Expr(:call, esc(SQ), Expr(:parameters, parameters...))) where {$(esc.(P)...)})
            def2 = Expr(:function, sig2, body2)
            kwdefs = Expr(:block, def1, def2)
        else
            error("Invalid usage of @kwdef")
        end
    else
        kwdefs = nothing
    end
    return quote
        $(esc(:($Base.@__doc__ $expr)))
        $kwdefs
    end
end

# @kwdef helper function
# mutates arguments inplace
function extract_names_and_defvals_from_kwdef_fieldblock!(block, names, defvals)
    for (i, item) in pairs(block.args)
        if isexpr(item, :block)
            extract_names_and_defvals_from_kwdef_fieldblock!(item, names, defvals)
        elseif item isa Expr && item.head in (:escape, :var"hygienic-scope")
            n = length(names)
            extract_names_and_defvals_from_kwdef_fieldblock!(item, names, defvals)
            for j in n+1:length(defvals)
                if !isnothing(defvals[j])
                    defvals[j] = Expr(item.head, defvals[j])
                end
            end
        else
            def, name, defval = @something(def_name_defval_from_kwdef_fielddef(item), continue)
            block.args[i] = def
            push!(names, name)
            push!(defvals, defval)
        end
    end
end

function def_name_defval_from_kwdef_fielddef(kwdef)
    if kwdef isa Symbol
        return kwdef, kwdef, nothing
    elseif isexpr(kwdef, :(::))
        name, _ = kwdef.args
        return kwdef, Meta.unescape(name), nothing
    elseif isexpr(kwdef, :(=))
        lhs, rhs = kwdef.args
        def, name, _ = @something(def_name_defval_from_kwdef_fielddef(lhs), return nothing)
        return def, name, rhs
    elseif kwdef isa Expr && kwdef.head in (:const, :atomic)
        def, name, defval = @something(def_name_defval_from_kwdef_fielddef(kwdef.args[1]), return nothing)
        return Expr(kwdef.head, def), name, defval
    elseif kwdef isa Expr && kwdef.head in (:escape, :var"hygienic-scope")
        def, name, defval = @something(def_name_defval_from_kwdef_fielddef(kwdef.args[1]), return nothing)
        return Expr(kwdef.head, def), name, isnothing(defval) ? defval : Expr(kwdef.head, defval)
    end
end

# testing

"""
    Base.runtests(tests=["all"]; ncores=ceil(Int, Sys.CPU_THREADS / 2),
                  exit_on_error=false, revise=false, propagate_project=true, [seed], [julia_args::Cmd])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `ncores` processors. If `exit_on_error` is `false`, when one test
fails, all remaining tests in other files will still be run; they are otherwise discarded,
when `exit_on_error == true`.
If `revise` is `true`, the `Revise` package is used to load any modifications to `Base` or
to the standard libraries before running the tests.
If `propagate_project` is true the current project is propagated to the test environment.
If a seed is provided via the keyword argument, it is used to seed the
global RNG in the context where the tests are run; otherwise the seed is chosen randomly.
The argument `julia_args` can be used to pass custom `julia` command line flags to the test process.
"""
function runtests(tests = ["all"]; ncores::Int = ceil(Int, Sys.CPU_THREADS / 2),
                  exit_on_error::Bool=false,
                  revise::Bool=false,
                  propagate_project::Bool=false,
                  seed::Union{BitInteger,Nothing}=nothing,
                  julia_args::Cmd=``)
    if isa(tests,AbstractString)
        tests = split(tests)
    end
    exit_on_error && push!(tests, "--exit-on-error")
    revise && push!(tests, "--revise")
    seed !== nothing && push!(tests, "--seed=0x$(string(seed % UInt128, base=16))") # cast to UInt128 to avoid a minus sign
    ENV2 = copy(ENV)
    ENV2["JULIA_CPU_THREADS"] = "$ncores"
    pathsep = Sys.iswindows() ? ";" : ":"
    ENV2["JULIA_DEPOT_PATH"] = string(mktempdir(; cleanup = true), pathsep) # make sure the default depots can be loaded
    ENV2["JULIA_LOAD_PATH"] = string("@", pathsep, "@stdlib")
    ENV2["JULIA_TESTS"] = "true"
    delete!(ENV2, "JULIA_PROJECT")
    project_flag = propagate_project ? `--project` : ``
    try
        run(setenv(`$(julia_cmd()) $julia_args $project_flag $(joinpath(Sys.BINDIR,
            Base.DATAROOTDIR, "julia", "test", "runtests.jl")) $tests`, ENV2))
        nothing
    catch
        buf = PipeBuffer()
        let InteractiveUtils = Base.require_stdlib(PkgId(UUID(0xb77e0a4c_d291_57a0_90e8_8db25a27a240), "InteractiveUtils"))
            @invokelatest InteractiveUtils.versioninfo(buf)
        end
        error("A test has failed. Please submit a bug report (https://github.com/JuliaLang/julia/issues)\n" *
              "including error messages above and the output of versioninfo():\n$(read(buf, String))")
    end
end

"""
    isdebugbuild()

Return `true` if julia is a debug version.
"""
function isdebugbuild()
    return ccall(:jl_is_debugbuild, Cint, ()) != 0
end
