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
let color_syms = collect(Iterators.filter(x -> !isa(x, Integer), keys(text_colors))),
    formatting_syms = [:normal, :bold, :default]
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

function with_output_color(@nospecialize(f::Function), color::Union{Int, Symbol}, io::IO, args...; bold::Bool = false)
    buf = IOBuffer()
    iscolor = get(io, :color, false)::Bool
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
`--code-coverage`, `--track-allocation`, `--color`, `--startup-file`, and `--depwarn`
command line arguments that are not at their default values.

Among others, `--math-mode`, `--warn-overwrite`, and `--trace-compile` are notably not propagated currently.

!!! compat "Julia 1.1"
    Only the `--cpu-target`, `--sysimage`, `--depwarn`, `--compile` and `--check-bounds` flags were propagated before Julia 1.1.

!!! compat "Julia 1.5"
    The flags `--color` and `--startup-file` were added in Julia 1.5.
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
    if opts.malloc_log == 1
        push!(addflags, "--track-allocation=user")
    elseif opts.malloc_log == 2
        push!(addflags, "--track-allocation=all")
    end
    if opts.color == 1
        push!(addflags, "--color=yes")
    elseif opts.color == 2
        push!(addflags, "--color=no")
    end
    if opts.startupfile == 2
        push!(addflags, "--startup-file=no")
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
    expr = expr::Expr
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
            T = T::Expr
            # if T == S{A<:AA,B<:BB}, define two methods
            #   S(...) = ...
            #   S{A,B}(...) where {A<:AA,B<:BB} = ...
            S = T.args[1]
            P = T.args[2:end]
            Q = Any[U isa Expr && U.head === :<: ? U.args[1] : U for U in P]
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
                  exit_on_error=false, revise=false, [seed])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `ncores` processors. If `exit_on_error` is `false`, when one test
fails, all remaining tests in other files will still be run; they are otherwise discarded,
when `exit_on_error == true`.
If `revise` is `true`, the `Revise` package is used to load any modifications to `Base` or
to the standard libraries before running the tests.
If a seed is provided via the keyword argument, it is used to seed the
global RNG in the context where the tests are run; otherwise the seed is chosen randomly.
"""
function runtests(tests = ["all"]; ncores = ceil(Int, Sys.CPU_THREADS / 2),
                  exit_on_error::Bool=false,
                  revise::Bool=false,
                  seed::Union{BitInteger,Nothing}=nothing)
    if isa(tests,AbstractString)
        tests = split(tests)
    end
    exit_on_error && push!(tests, "--exit-on-error")
    revise && push!(tests, "--revise")
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
