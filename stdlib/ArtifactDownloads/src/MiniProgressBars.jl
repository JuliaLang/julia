module MiniProgressBars

export MiniProgressBar, start_progress, end_progress, show_progress, print_progress_bottom

import Printf: @sprintf

# Until Base.format_bytes supports sigdigits
function pkg_format_bytes(bytes; binary=true, sigdigits::Integer=3)
    units = binary ? Base._mem_units : Base._cnt_units
    factor = binary ? 1024 : 1000
    bytes, mb = Base.prettyprint_getunits(bytes, length(units), Int64(factor))
    if mb == 1
        return string(Int(bytes), " ", Base._mem_units[mb], bytes==1 ? "" : "s")
    else
        return string(Base.Ryu.writefixed(Float64(bytes), sigdigits), binary ? " $(units[mb])" : "$(units[mb])B")
    end
end

Base.@kwdef mutable struct MiniProgressBar
    max::Int = 1
    header::String = ""
    color::Symbol = :nothing
    width::Int = 40
    current::Int = 0
    status::String = "" # If not empty this string replaces the bar
    prev::Int = 0
    has_shown::Bool = false
    time_shown::Float64 = 0.0
    mode::Symbol = :percentage # :percentage :int :data
    always_reprint::Bool = false
    indent::Int = 4
    main::Bool = true
end

const PROGRESS_BAR_TIME_GRANULARITY = Ref(1 / 30.0) # 30 fps
const PROGRESS_BAR_PERCENTAGE_GRANULARITY = Ref(0.1)

function start_progress(io::IO, _::MiniProgressBar)
    ansi_disablecursor = "\e[?25l"
    print(io, ansi_disablecursor)
end

function show_progress(io::IO, p::MiniProgressBar; termwidth=nothing, carriagereturn=true)
    if p.max == 0
        perc = 0.0
        prev_perc = 0.0
    else
        perc = p.current / p.max * 100
        prev_perc = p.prev / p.max * 100
    end
    # Bail early if we are not updating the progress bar,
    # Saves printing to the terminal
    if !p.always_reprint && p.has_shown && !((perc - prev_perc) > PROGRESS_BAR_PERCENTAGE_GRANULARITY[])
        return
    end
    t = time()
    if !p.always_reprint && p.has_shown && (t - p.time_shown) < PROGRESS_BAR_TIME_GRANULARITY[]
        return
    end
    p.time_shown = t
    p.prev = p.current
    p.has_shown = true

    progress_text = if p.mode == :percentage
        @sprintf "%2.1f %%" perc
    elseif p.mode == :int
        string(p.current, "/",  p.max)
    elseif p.mode == :data
        lpad(string(pkg_format_bytes(p.current; sigdigits=1), "/", pkg_format_bytes(p.max; sigdigits=1)), 20)
    else
        error("Unknown mode $(p.mode)")
    end
    termwidth = @something termwidth displaysize(io)[2]
    max_progress_width = max(0, min(termwidth - textwidth(p.header) - textwidth(progress_text) - 10 , p.width))
    n_filled = ceil(Int, max_progress_width * perc / 100)
    n_left = max_progress_width - n_filled
    headers = split(p.header)
    to_print = sprint(; context=io) do io
        print(io, " "^p.indent)
        if p.main
            printstyled(io, headers[1], " "; color=:green, bold=true)
            length(headers) > 1 && printstyled(io, join(headers[2:end], ' '), " ")
        else
            print(io, p.header, " ")
        end
        print(io, " ")
        if !isempty(p.status)
            print(io, p.status)
        else
            printstyled(io, "━"^n_filled; color=p.color)
            printstyled(io, perc >= 95 ? "━" : "╸"; color=p.color)
            printstyled(io, "━"^n_left, " "; color=:light_black)
            print(io, progress_text)
        end
        carriagereturn && print(io, "\r")
    end
    # Print everything in one call
    print(io, to_print)
end

function end_progress(io, p::MiniProgressBar)
    ansi_enablecursor = "\e[?25h"
    ansi_clearline = "\e[2K"
    print(io, ansi_enablecursor * ansi_clearline)
end

# Useful when writing a progress bar in the bottom
# makes the bottom progress bar not flicker
# prog = MiniProgressBar(...)
# prog.end = n
# for progress in 1:n
#     print_progress_bottom(io)
#     println("stuff")
#     prog.current = progress
#     showprogress(io, prog)
#  end
#
function print_progress_bottom(io::IO)
    ansi_clearline = "\e[2K"
    ansi_movecol1 = "\e[1G"
    ansi_moveup(n::Int) = string("\e[", n, "A")
    print(io, "\e[S" * ansi_moveup(1) * ansi_clearline * ansi_movecol1)
end

end
