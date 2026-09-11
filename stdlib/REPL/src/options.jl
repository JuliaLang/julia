# This file is a part of Julia. License is MIT: https://julialang.org/license

## User Options

mutable struct Options
    hascolor::Bool
    extra_keymap::Union{Dict,Vector{<:Dict}}
    # controls the presumed tab width of code pasted into the REPL.
    # Must satisfy `0 < tabwidth <= 16`.
    tabwidth::Int
    # Maximum number of entries in the kill ring queue.
    # Beyond this number, oldest entries are discarded first.
    kill_ring_max::Int
    region_animation_duration::Float64
    beep_duration::Float64
    beep_blink::Float64
    beep_maxduration::Float64
    beep_colors::Vector{String}
    beep_use_current::Bool
    backspace_align::Bool
    backspace_adjust::Bool
    confirm_exit::Bool # ^D must be repeated to confirm exit
    auto_indent::Bool # indent a newline like line above
    auto_indent_tmp_off::Bool # switch auto_indent temporarily off if copy&paste
    auto_indent_bracketed_paste::Bool # set to true if terminal knows paste mode
    # cancel auto-indent when next character is entered within this time frame :
    auto_indent_time_threshold::Float64
    # refresh after time delay
    auto_refresh_time_delay::Float64
    hint_tab_completes::Bool
    auto_insert_closing_bracket::Bool # automatically insert closing brackets, quotes, etc.
    style_input::Bool # enable syntax highlighting for input
    # default IOContext settings at the REPL
    iocontext::Dict{Symbol,Any}
end

Options(;
        hascolor = true,
        extra_keymap = AnyDict[],
        tabwidth = 8,
        kill_ring_max = 100,
        region_animation_duration = 0.2,
        beep_duration = 0.2, beep_blink = 0.2, beep_maxduration = 1.0,
        beep_colors = ["\e[90m"], # gray (text_colors not yet available)
        beep_use_current = true,
        backspace_align = true, backspace_adjust = backspace_align,
        confirm_exit = false,
        auto_indent = true,
        auto_indent_tmp_off = false,
        auto_indent_bracketed_paste = false,
        auto_indent_time_threshold = 0.005,
        auto_refresh_time_delay = 0.0, # this no longer seems beneficial
        hint_tab_completes = true,
        auto_insert_closing_bracket = false,
        style_input = true,
        iocontext = Dict{Symbol,Any}()) =
            Options(hascolor, extra_keymap, tabwidth,
                    kill_ring_max, region_animation_duration,
                    beep_duration, beep_blink, beep_maxduration,
                    beep_colors, beep_use_current,
                    backspace_align, backspace_adjust, confirm_exit,
                    auto_indent, auto_indent_tmp_off, auto_indent_bracketed_paste,
                    auto_indent_time_threshold, auto_refresh_time_delay,
                    hint_tab_completes, auto_insert_closing_bracket, style_input,
                    iocontext)

# for use by REPLs not having an options field
const GlobalOptions = Options()
