module TermOSC

using Base.Threads
using Base.Terminals: UnixTerminal, raw!

public receive_osc, query, get_all_colors

struct OSCResponse
    opcode::Int
    parts::Vector{SubString{String}}
end

const PENDING_RESPONSES = OSCResponse[]
const RESPONSE_POLLERS = Tuple{Function, Timer}[]

const OSC_LOCK = ReentrantLock()

const OSC_TIMEOUT = 1.0 # seconds

"""
    query(callback::Function, terminal::UnixTerminal, query::String)

Send an OSC `query` to the `terminal` and register a `callback` to handle the response.

The `callback` function is called with the vector of all `OSCResponse`s received so far.

It is responsible for determining which responses it wants to handle, and deleting them from the vector.

The callback should return `true` if it has finished processing and can be removed from the pollers list,
or `false` if it wants to be called again when more responses arrive.
"""
function query(callback::Function, term::UnixTerminal, query::String)
    raw!(term, true)
    @lock OSC_LOCK begin
        print(term.out_stream, query)
        flush(term.out_stream)
        push!(RESPONSE_POLLERS, (callback, Timer(expire_poller, OSC_TIMEOUT)))
    end
    nothing
end

"""
    read_osc_response(in::IO)

Read an OSC response from the provided input stream `in`.

Accepts the standard OSC terminators BEL ('\a') and ST (`\e\\`)
in addition to Ctrl-C (`\3`) in case the response is malformed/incomplete
and is making the REPL hang.

Assumes that the OSC sequence introducer (`\e]`) has already been consumed.
"""
function read_osc_response(in::IO)
    data = Base.StringVector(0)
    while true
        c = read(in, UInt8)
        c == UInt8('\3') && break # ^C
        c == 0x9c && break # ST
        c == UInt8('\a') && break
        if c == UInt8('\e')
            c = read(in, UInt8)
            c == UInt8('\\') && break
            push!(data, UInt8('\e'))
        end
        push!(data, c)
    end
    String(data)
end

"""
    receive_osc(term::UnixTerminal)

Read an OSC response from the terminal and process it.

Assumes that the OSC sequence introducer (`\e]`) has already been consumed.
"""
function receive_osc(term::UnixTerminal)
    osc = read_osc_response(term.in_stream)
    parts = split(osc, ';')
    opcode = tryparse(Int, first(parts))
    isnothing(opcode) && return
    popfirst!(parts)
    @lock OSC_LOCK push!(PENDING_RESPONSES, OSCResponse(opcode, parts))
    run_pollers()
end

function run_pollers()
    @lock OSC_LOCK begin
        finished = Int[]
        for (i, (poller, _)) in enumerate(RESPONSE_POLLERS)
            try
                poller(PENDING_RESPONSES)::Bool &&
                    push!(finished, i)
            catch ex
                showerror(stderr, ex)
                Base.show_backtrace(stderr, catch_backtrace())
                println(stderr)
                push!(finished, i)
            end
        end
        if !isempty(finished)
            foreach(close ∘ last, view(RESPONSE_POLLERS, finished))
            deleteat!(RESPONSE_POLLERS, finished)
        end
        isempty(RESPONSE_POLLERS) && empty!(PENDING_RESPONSES)
    end
    nothing
end

function expire_poller(timer::Timer)
    @lock OSC_LOCK begin
        idx = findfirst(t -> last(t) === timer, RESPONSE_POLLERS)
        isnothing(idx) && return
        poller = first(RESPONSE_POLLERS[idx])
        try
            poller(PENDING_RESPONSES)::Bool
        catch ex
            showerror(stderr, ex)
            Base.show_backtrace(stderr, catch_backtrace())
            println(stderr)
        end
        deleteat!(RESPONSE_POLLERS, idx)
        isempty(RESPONSE_POLLERS) && empty!(PENDING_RESPONSES)
    end
    nothing
end

# Helper functions

const RGBTuple = @NamedTuple{r::UInt8, g::UInt8, b::UInt8}

"""
    interpret_color(data::AbstractString)

Interpret a color specification string, or return `nothing` if invalid.

The string may take any of the following forms:
- `rgb:RR/GG/BB`
- `rgb:RRRR/GGGG/BBBB`
- `rgba:RR/GG/BB/AA`
- `rgba:RRRR/GGGG/BBBB/AAAA`

Returns `$RGBTuple` if interpreted successfully.
"""
function interpret_color(data::AbstractString)
    if startswith(data, "rgb:")
        data = @view data[ncodeunits("rgb:")+1:end]
        components = split(data, '/')
        length(components) == 3 || return
    elseif startswith(data, "rgba:")
        data = @view data[ncodeunits("rgba:")+1:end]
        components = split(data, '/')
        length(components) == 4 || return
    else
        return
    end
    function tryparsecolor(chex)
        cnum = tryparse(UInt16, chex, base=16)
        isnothing(cnum) && return nothing
        UInt8(cnum ÷ 16^(ncodeunits(chex) - 2))
    end
    r = tryparsecolor(components[1])
    isnothing(r) && return
    g = tryparsecolor(components[2])
    isnothing(g) && return
    b = tryparsecolor(components[3])
    isnothing(b) && return
    (; r, g, b)
end

const ANSI_COLOR_ORDER = (
    :black, :red, :green, :yellow, :blue, :magenta, :cyan, :white,
    :bright_black, :bright_red, :bright_green, :bright_yellow,
    :bright_blue, :bright_magenta, :bright_cyan, :bright_white)

"""
    ColorCallbackWrapper{F <: Function}

A special closure that calls a function `F` with all terminal colors.

Expires after `OSC_TIMEOUT` seconds.

This exists solely to make writing `precompile` statements easier/possible
(by avoiding anonymous functions).
"""
struct ColorCallbackWrapper{F <: Function} <: Function
    callback::F
    ctime::Float64
end

function (o::ColorCallbackWrapper{F})(oscs::Vector{OSCResponse}) where {F}
    expired = time() - o.ctime > OSC_TIMEOUT
    colors = read_all_colors!(oscs, expired)
    isnothing(colors) && return expired
    o.callback(colors)
    true
end

ColorCallbackWrapper(f::Function) = ColorCallbackWrapper(f, time())

"""
    get_all_colors(callback::Function, term::UnixTerminal)

Query the terminal for all colors (foreground, background, and ANSI 0-15).

If/when the terminal responds, the `callback` function is called with a vector of
pairs of color names and `$RGBTuple` values.

# Event sequence

1. Call `get_all_colors`
2. `query` is invoked, which:
   - Sends the OSC queries to the terminal
   - Registers a wrapper around `callback` as a poller
3. The terminal (hopefully) responds with OSC sequences
4. The keymap detects the start of an OSC sequence and calls `receive_osc`
5. `receive_osc` reads the OSC response and appends it to `PENDING_RESPONSES`
6. `receive_osc` runs all registered pollers
7. If a complete set of responses is found, the wrapper calls `callback`

!!! warning
    To catch the responses, you must be asynchronously reading from `stdin`
    and calling `TermOSC.receive_osc(term)` whenever an OSC response is detected.
"""
function get_all_colors(callback::Function, term::UnixTerminal)
    fgbg_query = "\e]10;?\e\\\e]11;?\e\\"
    # NOTE: In theory, as per <https://www.xfree86.org/current/ctlseqs.html>
    # 'Operating System Controls' > 'P s = 4', multiple queries may be provided:
    #
    #   "Because more than one pair of color number and specification can be given
    #    in one control sequence, xterm can make more than one reply."
    #
    # However, in practice, while some terminals are good and support this
    # (e.g. Kitty, Wezterm, and Foot) others, even those with good reputations
    # for being faithful VTs, do not (e.g. Ghostty, Alacritty, Konsole).
    #
    # So, we resort to sending 16 individual OSC queries instead of
    # one large one 🥲.
    ansi_query = join(("\e]4;$n;?\e\\" for n in 0:15))
    callback_wrapper = ColorCallbackWrapper(callback)
    query(callback_wrapper, term, fgbg_query * ansi_query)
end

"""
    read_all_colors!(oscs::Vector{OSCResponse})

Read all color responses from the provided `oscs` vector.

If `oscs` contains complete responses for foreground (10), background (11),
and the 16 ANSI colors (4), those responses are removed from `oscs` and
a vector of pairs of color names and `$RGBTuple` values is returned.

Otherwise, `nothing` is returned and `oscs` is left unmodified.
"""
function read_all_colors!(oscs::Vector{OSCResponse}, fgbg_only::Bool = false)
    hasfg = false
    hasbg = false
    hasansi = zeros(Bool, 16)
    for (; opcode, parts) in oscs
        if opcode == 10 && length(parts) == 1
            hasfg = true
        elseif opcode == 11 && length(parts) == 1
            hasbg = true
        elseif !fgbg_only && opcode == 4
            for part in parts
                all(isdigit, part) || continue
                opc = tryparse(Int, part)
                isnothing(opc) && break
                hasansi[clamp(opc, 0, 15)+1] = true
            end
        end
    end
    (hasfg && hasbg && (fgbg_only || all(hasansi))) || return
    colors = Pair{Symbol, RGBTuple}[]
    consumed = Int[]
    for (i, (; opcode, parts)) in enumerate(oscs)
        if opcode == 10 && length(parts) == 1
            col = interpret_color(first(parts))
            push!(consumed, i)
            isnothing(col) && continue
            push!(colors, :foreground => col)
        elseif opcode == 11 && length(parts) == 1
            col = interpret_color(first(parts))
            push!(consumed, i)
            isnothing(col) && continue
            push!(colors, :background => col)
        elseif !fgbg_only && opcode == 4
            colornum = -1
            for part in parts
                if all(isdigit, part)
                    colornum = something(tryparse(Int, part), -1)
                elseif colornum ∈ 0:15
                    col = interpret_color(part)
                    isnothing(col) && continue
                    push!(colors, ANSI_COLOR_ORDER[colornum+1] => col)
                    colornum = -1
                end
            end
            push!(consumed, i)
        end
    end
    deleteat!(oscs, consumed)
    colors
end

end
