# This file is a part of Julia. License is MIT: https://julialang.org/license

# Updating this listing is fairly easy, assuming existence of a unix system,
# posix shell, and `awk`. Just update the version string in the commented out
# `NCURSES_VERSION` variable, and run this file. This works because this file is
# a bit of a quine.

#=
awk '/^#=run/{flag=1;next}/=#/{flag=0}flag{gsub(/__FILE__/,"\"'"$0"'\"");print}' "$0" | \
  julia --startup-file=no -E 'readchomp("/dev/fd/0") |> Meta.parse |> eval' && echo "Done"; exit
=#

"""
    struct TermCapability

Specification of a single terminal capability.

!!! warning
  This is not part of the public API, and thus subject to change without notice.

# Fields

- `name::Symbol`: The name of the terminfo capability variable
- `capname::Symbol`: The *Cap-name* of the capability
- `description::String`: A description of the purpose of the capability

See also: `TermInfo`, `TERM_FLAGS`, `TERM_NUMBERS`, and `TERM_STRINGS`.
"""
struct TermCapability
    name::Symbol
    capname::Symbol
    description::String
end

#=run
begin

using Downloads

version_info = IOBuffer()
standard_caps = IOBuffer()
user_caps = IOBuffer()

Downloads.download("https://raw.githubusercontent.com/mirror/ncurses/master/VERSION", version_info)
Downloads.download("https://raw.githubusercontent.com/mirror/ncurses/master/include/Caps", standard_caps)
Downloads.download("https://raw.githubusercontent.com/mirror/ncurses/master/include/Caps-ncurses", user_caps)

const TERM_FLAGS   = NTuple{3, String}[]
const TERM_NUMBERS = NTuple{3, String}[]
const TERM_STRINGS = NTuple{3, String}[]
const TERM_USER    = NTuple{3, String}[]

_, ncurses_version, ncurses_date = split(read(seekstart(version_info), String))

for line in eachline(seekstart(standard_caps))
    startswith(line, '#') && continue
    components = split(line, '\t', keepempty=false)
    if length(components) ∉ 8:9
        @warn "Malformed line: $(sprint(show, line))"
        continue
    end
    name, shortcode, type, _, _, _, _, description, _... = components
    caplist = if type == "bool" TERM_FLAGS
    elseif type == "num" TERM_NUMBERS
    elseif type == "str" TERM_STRINGS
    else
        @warn "Unrecognised capability type: $type"
        continue
    end
    push!(caplist, (name, shortcode, description))
end

for line in eachline(seekstart(user_caps))
    startswith(line, '#') && continue
    !startswith(line, "userdef") && continue
    line = line[1+ncodeunits("userdef "):end]
    components = split(line, '\t', keepempty=false)
    if length(components) ∉ 4:5
        @warn "Malformed line: $(sprint(show, line))"
        continue
    end
    code, type, _, description, _... = components
    if code == "xm"
        components[3] == "-" || continue
        description = "mouse response"
    end
    dtype = get(Dict("bool" => "Bool", "num" => "Int", "str" => "String"), type, nothing)
    if isnothing(dtype)
        @warn "Unrecognised data type: $type"
        continue
    end
    push!(TERM_USER, (dtype, code, description))
end

push!(TERM_USER, ("Bool", "Tc", "tmux extension to indicate 24-bit truecolor support"))
push!(TERM_USER, ("Bool", "Su", "kitty extension to indicate styled underline support"))

const SENTINEL = "\n## GENERATED CODE BEYOND THIS POINT ##"
const PREAMBLE = readuntil(__FILE__, SENTINEL, keep=true)

out = IOBuffer()
write(out, PREAMBLE, "\n\n# Terminfo Capabilities as of NCurses $ncurses_version-$ncurses_date\n",
      "const NCURSES_VERSION = v\"$ncurses_version.$ncurses_date\"\n")

for (ftype, list) in [("flag", TERM_FLAGS), ("number", TERM_NUMBERS), ("string", TERM_STRINGS)]
    print(out, "\n\"\"\"\n\
          Ordered list of known terminal capability $ftype fields, as of NCurses $ncurses_version-$ncurses_date.\n\
          \"\"\"\n\
          const TERM_$(uppercase(ftype))S = [")
    namepad = maximum(textwidth, getindex.(list, 1)) + 1
    codepad = maximum(textwidth, getindex.(list, 2)) + 1
    for (name, shortcode, description) in list
        print(out, "\n    TermCapability(:", name, ',', ' '^(namepad - textwidth(name)),
              ':', shortcode, ',', ' '^(codepad - textwidth(shortcode)),
              '"', escape_string(description), "\"),")
    end
    println(out, "\n]")
end

function getcustomalias(allterms::Vector{NTuple{3, String}}, type, short, description)
    specific_aliases = Dict{String, String}(
        "smxx"  => ":enter_strikeout_mode",
        "rmxx"  => ":exit_strikeout_mode",
        "Smol"  => ":enter_overline_mode",
        "Rmol"  => ":exit_overline_mode",
        "Cs"    => ":set_cursor_color",
        "Cr"    => ":reset_cursor_color",
        "Ss"    => ":set_cursor_style",
        "Se"    => ":reset_cursor_style",
        "Smulx" => ":set_underline_style",
        "Su"    => ":can_style_underline",
        "csl"   => ":clear_status_line",
        "Ms"    => ":set_host_clipboard",
        "Tc"    => ":truecolor",
        "XF"    => ":xterm_focus")
    if startswith(short, 'k') && !occursin("keypad", description)
        return ":key_" * replace(lowercase(description), r"[^a-z]" => '_')
    end
    return get(specific_aliases, short, "nothing")
end

print(out, "\n\"\"\"\nTerminfo extensions that NCurses $ncurses_version-$ncurses_date is aware of.\n\"\"\"",
           "\nconst TERM_USER = Dict{Tuple{DataType, Symbol}, Union{Tuple{Nothing, String}, Tuple{Symbol, String}}}(")
shortpad = maximum(textwidth, getindex.(TERM_USER, 2)) + 1
for (type, short, description) in TERM_USER
    print(out, "\n    ($(rpad(type * ',', 7)) :$short)", ' '^(shortpad - textwidth(short)),
          "=> (", getcustomalias(TERM_USER, type, short, description), ", \"",
          escape_string(description), "\"),")
end
println(out, "\n)")

open(io -> write(io, seekstart(out)), __FILE__, "w")

end
=#

## GENERATED CODE BEYOND THIS POINT ##

# Terminfo Capabilities as of NCurses 6.4-20230311
const NCURSES_VERSION = v"6.4.20230311"

"""
Ordered list of known terminal capability flag fields, as of NCurses 6.4-20230311.
"""
const TERM_FLAGS = [
    TermCapability(:auto_left_margin,         :bw,    "cub1 wraps from column 0 to last column"),
    TermCapability(:auto_right_margin,        :am,    "terminal has automatic margins"),
    TermCapability(:no_esc_ctlc,              :xsb,   "beehive (f1=escape, f2=ctrl C)"),
    TermCapability(:ceol_standout_glitch,     :xhp,   "standout not erased by overwriting (hp)"),
    TermCapability(:eat_newline_glitch,       :xenl,  "newline ignored after 80 cols (concept)"),
    TermCapability(:erase_overstrike,         :eo,    "can erase overstrikes with a blank"),
    TermCapability(:generic_type,             :gn,    "generic line type"),
    TermCapability(:hard_copy,                :hc,    "hardcopy terminal"),
    TermCapability(:has_meta_key,             :km,    "Has a meta key (i.e., sets 8th-bit)"),
    TermCapability(:has_status_line,          :hs,    "has extra status line"),
    TermCapability(:insert_null_glitch,       :in,    "insert mode distinguishes nulls"),
    TermCapability(:memory_above,             :da,    "display may be retained above the screen"),
    TermCapability(:memory_below,             :db,    "display may be retained below the screen"),
    TermCapability(:move_insert_mode,         :mir,   "safe to move while in insert mode"),
    TermCapability(:move_standout_mode,       :msgr,  "safe to move while in standout mode"),
    TermCapability(:over_strike,              :os,    "terminal can overstrike"),
    TermCapability(:status_line_esc_ok,       :eslok, "escape can be used on the status line"),
    TermCapability(:dest_tabs_magic_smso,     :xt,    "tabs destructive, magic so char (t1061)"),
    TermCapability(:tilde_glitch,             :hz,    "cannot print ~'s (Hazeltine)"),
    TermCapability(:transparent_underline,    :ul,    "underline character overstrikes"),
    TermCapability(:xon_xoff,                 :xon,   "terminal uses xon/xoff handshaking"),
    TermCapability(:needs_xon_xoff,           :nxon,  "padding will not work, xon/xoff required"),
    TermCapability(:prtr_silent,              :mc5i,  "printer will not echo on screen"),
    TermCapability(:hard_cursor,              :chts,  "cursor is hard to see"),
    TermCapability(:non_rev_rmcup,            :nrrmc, "smcup does not reverse rmcup"),
    TermCapability(:no_pad_char,              :npc,   "pad character does not exist"),
    TermCapability(:non_dest_scroll_region,   :ndscr, "scrolling region is non-destructive"),
    TermCapability(:can_change,               :ccc,   "terminal can re-define existing colors"),
    TermCapability(:back_color_erase,         :bce,   "screen erased with background color"),
    TermCapability(:hue_lightness_saturation, :hls,   "terminal uses only HLS color notation (Tektronix)"),
    TermCapability(:col_addr_glitch,          :xhpa,  "only positive motion for hpa/mhpa caps"),
    TermCapability(:cr_cancels_micro_mode,    :crxm,  "using cr turns off micro mode"),
    TermCapability(:has_print_wheel,          :daisy, "printer needs operator to change character set"),
    TermCapability(:row_addr_glitch,          :xvpa,  "only positive motion for vpa/mvpa caps"),
    TermCapability(:semi_auto_right_margin,   :sam,   "printing in last column causes cr"),
    TermCapability(:cpi_changes_res,          :cpix,  "changing character pitch changes resolution"),
    TermCapability(:lpi_changes_res,          :lpix,  "changing line pitch changes resolution"),
    TermCapability(:backspaces_with_bs,       :OTbs,  "uses ^H to move left"),
    TermCapability(:crt_no_scrolling,         :OTns,  "crt cannot scroll"),
    TermCapability(:no_correctly_working_cr,  :OTnc,  "no way to go to start of line"),
    TermCapability(:gnu_has_meta_key,         :OTMT,  "has meta key"),
    TermCapability(:linefeed_is_newline,      :OTNL,  "move down with \\n"),
    TermCapability(:has_hardware_tabs,        :OTpt,  "has 8-char tabs invoked with ^I"),
    TermCapability(:return_does_clr_eol,      :OTxr,  "return clears the line"),
]

"""
Ordered list of known terminal capability number fields, as of NCurses 6.4-20230311.
"""
const TERM_NUMBERS = [
    TermCapability(:columns,                 :cols,   "number of columns in a line"),
    TermCapability(:init_tabs,               :it,     "tabs initially every # spaces"),
    TermCapability(:lines,                   :lines,  "number of lines on screen or page"),
    TermCapability(:lines_of_memory,         :lm,     "lines of memory if > line. 0 means varies"),
    TermCapability(:magic_cookie_glitch,     :xmc,    "number of blank characters left by smso or rmso"),
    TermCapability(:padding_baud_rate,       :pb,     "lowest baud rate where padding needed"),
    TermCapability(:virtual_terminal,        :vt,     "virtual terminal number (CB/unix)"),
    TermCapability(:width_status_line,       :wsl,    "number of columns in status line"),
    TermCapability(:num_labels,              :nlab,   "number of labels on screen"),
    TermCapability(:label_height,            :lh,     "rows in each label"),
    TermCapability(:label_width,             :lw,     "columns in each label"),
    TermCapability(:max_attributes,          :ma,     "maximum combined attributes terminal can handle"),
    TermCapability(:maximum_windows,         :wnum,   "maximum number of definable windows"),
    TermCapability(:max_colors,              :colors, "maximum number of colors on screen"),
    TermCapability(:max_pairs,               :pairs,  "maximum number of color-pairs on the screen"),
    TermCapability(:no_color_video,          :ncv,    "video attributes that cannot be used with colors"),
    TermCapability(:buffer_capacity,         :bufsz,  "numbers of bytes buffered before printing"),
    TermCapability(:dot_vert_spacing,        :spinv,  "spacing of pins vertically in pins per inch"),
    TermCapability(:dot_horz_spacing,        :spinh,  "spacing of dots horizontally in dots per inch"),
    TermCapability(:max_micro_address,       :maddr,  "maximum value in micro_..._address"),
    TermCapability(:max_micro_jump,          :mjump,  "maximum value in parm_..._micro"),
    TermCapability(:micro_col_size,          :mcs,    "character step size when in micro mode"),
    TermCapability(:micro_line_size,         :mls,    "line step size when in micro mode"),
    TermCapability(:number_of_pins,          :npins,  "numbers of pins in print-head"),
    TermCapability(:output_res_char,         :orc,    "horizontal resolution in units per line"),
    TermCapability(:output_res_line,         :orl,    "vertical resolution in units per line"),
    TermCapability(:output_res_horz_inch,    :orhi,   "horizontal resolution in units per inch"),
    TermCapability(:output_res_vert_inch,    :orvi,   "vertical resolution in units per inch"),
    TermCapability(:print_rate,              :cps,    "print rate in characters per second"),
    TermCapability(:wide_char_size,          :widcs,  "character step size when in double wide mode"),
    TermCapability(:buttons,                 :btns,   "number of buttons on mouse"),
    TermCapability(:bit_image_entwining,     :bitwin, "number of passes for each bit-image row"),
    TermCapability(:bit_image_type,          :bitype, "type of bit-image device"),
    TermCapability(:magic_cookie_glitch_ul,  :OTug,   "number of blanks left by ul"),
    TermCapability(:carriage_return_delay,   :OTdC,   "pad needed for CR"),
    TermCapability(:new_line_delay,          :OTdN,   "pad needed for LF"),
    TermCapability(:backspace_delay,         :OTdB,   "padding required for ^H"),
    TermCapability(:horizontal_tab_delay,    :OTdT,   "padding required for ^I"),
    TermCapability(:number_of_function_keys, :OTkn,   "count of function keys"),
]

"""
Ordered list of known terminal capability string fields, as of NCurses 6.4-20230311.
"""
const TERM_STRINGS = [
    TermCapability(:back_tab,                  :cbt,      "back tab (P)"),
    TermCapability(:bell,                      :bel,      "audible signal (bell) (P)"),
    TermCapability(:carriage_return,           :cr,       "carriage return (P*) (P*)"),
    TermCapability(:change_scroll_region,      :csr,      "change region to line #1 to line #2 (P)"),
    TermCapability(:clear_all_tabs,            :tbc,      "clear all tab stops (P)"),
    TermCapability(:clear_screen,              :clear,    "clear screen and home cursor (P*)"),
    TermCapability(:clr_eol,                   :el,       "clear to end of line (P)"),
    TermCapability(:clr_eos,                   :ed,       "clear to end of screen (P*)"),
    TermCapability(:column_address,            :hpa,      "horizontal position #1, absolute (P)"),
    TermCapability(:command_character,         :cmdch,    "terminal settable cmd character in prototype !?"),
    TermCapability(:cursor_address,            :cup,      "move to row #1 columns #2"),
    TermCapability(:cursor_down,               :cud1,     "down one line"),
    TermCapability(:cursor_home,               :home,     "home cursor (if no cup)"),
    TermCapability(:cursor_invisible,          :civis,    "make cursor invisible"),
    TermCapability(:cursor_left,               :cub1,     "move left one space"),
    TermCapability(:cursor_mem_address,        :mrcup,    "memory relative cursor addressing, move to row #1 columns #2"),
    TermCapability(:cursor_normal,             :cnorm,    "make cursor appear normal (undo civis/cvvis)"),
    TermCapability(:cursor_right,              :cuf1,     "non-destructive space (move right one space)"),
    TermCapability(:cursor_to_ll,              :ll,       "last line, first column (if no cup)"),
    TermCapability(:cursor_up,                 :cuu1,     "up one line"),
    TermCapability(:cursor_visible,            :cvvis,    "make cursor very visible"),
    TermCapability(:delete_character,          :dch1,     "delete character (P*)"),
    TermCapability(:delete_line,               :dl1,      "delete line (P*)"),
    TermCapability(:dis_status_line,           :dsl,      "disable status line"),
    TermCapability(:down_half_line,            :hd,       "half a line down"),
    TermCapability(:enter_alt_charset_mode,    :smacs,    "start alternate character set (P)"),
    TermCapability(:enter_blink_mode,          :blink,    "turn on blinking"),
    TermCapability(:enter_bold_mode,           :bold,     "turn on bold (extra bright) mode"),
    TermCapability(:enter_ca_mode,             :smcup,    "string to start programs using cup"),
    TermCapability(:enter_delete_mode,         :smdc,     "enter delete mode"),
    TermCapability(:enter_dim_mode,            :dim,      "turn on half-bright mode"),
    TermCapability(:enter_insert_mode,         :smir,     "enter insert mode"),
    TermCapability(:enter_secure_mode,         :invis,    "turn on blank mode (characters invisible)"),
    TermCapability(:enter_protected_mode,      :prot,     "turn on protected mode"),
    TermCapability(:enter_reverse_mode,        :rev,      "turn on reverse video mode"),
    TermCapability(:enter_standout_mode,       :smso,     "begin standout mode"),
    TermCapability(:enter_underline_mode,      :smul,     "begin underline mode"),
    TermCapability(:erase_chars,               :ech,      "erase #1 characters (P)"),
    TermCapability(:exit_alt_charset_mode,     :rmacs,    "end alternate character set (P)"),
    TermCapability(:exit_attribute_mode,       :sgr0,     "turn off all attributes"),
    TermCapability(:exit_ca_mode,              :rmcup,    "strings to end programs using cup"),
    TermCapability(:exit_delete_mode,          :rmdc,     "end delete mode"),
    TermCapability(:exit_insert_mode,          :rmir,     "exit insert mode"),
    TermCapability(:exit_standout_mode,        :rmso,     "exit standout mode"),
    TermCapability(:exit_underline_mode,       :rmul,     "exit underline mode"),
    TermCapability(:flash_screen,              :flash,    "visible bell (may not move cursor)"),
    TermCapability(:form_feed,                 :ff,       "hardcopy terminal page eject (P*)"),
    TermCapability(:from_status_line,          :fsl,      "return from status line"),
    TermCapability(:init_1string,              :is1,      "initialization string"),
    TermCapability(:init_2string,              :is2,      "initialization string"),
    TermCapability(:init_3string,              :is3,      "initialization string"),
    TermCapability(:init_file,                 :if,       "name of initialization file"),
    TermCapability(:insert_character,          :ich1,     "insert character (P)"),
    TermCapability(:insert_line,               :il1,      "insert line (P*)"),
    TermCapability(:insert_padding,            :ip,       "insert padding after inserted character"),
    TermCapability(:key_backspace,             :kbs,      "backspace key"),
    TermCapability(:key_catab,                 :ktbc,     "clear-all-tabs key"),
    TermCapability(:key_clear,                 :kclr,     "clear-screen or erase key"),
    TermCapability(:key_ctab,                  :kctab,    "clear-tab key"),
    TermCapability(:key_dc,                    :kdch1,    "delete-character key"),
    TermCapability(:key_dl,                    :kdl1,     "delete-line key"),
    TermCapability(:key_down,                  :kcud1,    "down-arrow key"),
    TermCapability(:key_eic,                   :krmir,    "sent by rmir or smir in insert mode"),
    TermCapability(:key_eol,                   :kel,      "clear-to-end-of-line key"),
    TermCapability(:key_eos,                   :ked,      "clear-to-end-of-screen key"),
    TermCapability(:key_f0,                    :kf0,      "F0 function key"),
    TermCapability(:key_f1,                    :kf1,      "F1 function key"),
    TermCapability(:key_f10,                   :kf10,     "F10 function key"),
    TermCapability(:key_f2,                    :kf2,      "F2 function key"),
    TermCapability(:key_f3,                    :kf3,      "F3 function key"),
    TermCapability(:key_f4,                    :kf4,      "F4 function key"),
    TermCapability(:key_f5,                    :kf5,      "F5 function key"),
    TermCapability(:key_f6,                    :kf6,      "F6 function key"),
    TermCapability(:key_f7,                    :kf7,      "F7 function key"),
    TermCapability(:key_f8,                    :kf8,      "F8 function key"),
    TermCapability(:key_f9,                    :kf9,      "F9 function key"),
    TermCapability(:key_home,                  :khome,    "home key"),
    TermCapability(:key_ic,                    :kich1,    "insert-character key"),
    TermCapability(:key_il,                    :kil1,     "insert-line key"),
    TermCapability(:key_left,                  :kcub1,    "left-arrow key"),
    TermCapability(:key_ll,                    :kll,      "lower-left key (home down)"),
    TermCapability(:key_npage,                 :knp,      "next-page key"),
    TermCapability(:key_ppage,                 :kpp,      "previous-page key"),
    TermCapability(:key_right,                 :kcuf1,    "right-arrow key"),
    TermCapability(:key_sf,                    :kind,     "scroll-forward key"),
    TermCapability(:key_sr,                    :kri,      "scroll-backward key"),
    TermCapability(:key_stab,                  :khts,     "set-tab key"),
    TermCapability(:key_up,                    :kcuu1,    "up-arrow key"),
    TermCapability(:keypad_local,              :rmkx,     "leave 'keyboard_transmit' mode"),
    TermCapability(:keypad_xmit,               :smkx,     "enter 'keyboard_transmit' mode"),
    TermCapability(:lab_f0,                    :lf0,      "label on function key f0 if not f0"),
    TermCapability(:lab_f1,                    :lf1,      "label on function key f1 if not f1"),
    TermCapability(:lab_f10,                   :lf10,     "label on function key f10 if not f10"),
    TermCapability(:lab_f2,                    :lf2,      "label on function key f2 if not f2"),
    TermCapability(:lab_f3,                    :lf3,      "label on function key f3 if not f3"),
    TermCapability(:lab_f4,                    :lf4,      "label on function key f4 if not f4"),
    TermCapability(:lab_f5,                    :lf5,      "label on function key f5 if not f5"),
    TermCapability(:lab_f6,                    :lf6,      "label on function key f6 if not f6"),
    TermCapability(:lab_f7,                    :lf7,      "label on function key f7 if not f7"),
    TermCapability(:lab_f8,                    :lf8,      "label on function key f8 if not f8"),
    TermCapability(:lab_f9,                    :lf9,      "label on function key f9 if not f9"),
    TermCapability(:meta_off,                  :rmm,      "turn off meta mode"),
    TermCapability(:meta_on,                   :smm,      "turn on meta mode (8th-bit on)"),
    TermCapability(:newline,                   :nel,      "newline (behave like cr followed by lf)"),
    TermCapability(:pad_char,                  :pad,      "padding char (instead of null)"),
    TermCapability(:parm_dch,                  :dch,      "delete #1 characters (P*)"),
    TermCapability(:parm_delete_line,          :dl,       "delete #1 lines (P*)"),
    TermCapability(:parm_down_cursor,          :cud,      "down #1 lines (P*)"),
    TermCapability(:parm_ich,                  :ich,      "insert #1 characters (P*)"),
    TermCapability(:parm_index,                :indn,     "scroll forward #1 lines (P)"),
    TermCapability(:parm_insert_line,          :il,       "insert #1 lines (P*)"),
    TermCapability(:parm_left_cursor,          :cub,      "move #1 characters to the left (P)"),
    TermCapability(:parm_right_cursor,         :cuf,      "move #1 characters to the right (P*)"),
    TermCapability(:parm_rindex,               :rin,      "scroll back #1 lines (P)"),
    TermCapability(:parm_up_cursor,            :cuu,      "up #1 lines (P*)"),
    TermCapability(:pkey_key,                  :pfkey,    "program function key #1 to type string #2"),
    TermCapability(:pkey_local,                :pfloc,    "program function key #1 to execute string #2"),
    TermCapability(:pkey_xmit,                 :pfx,      "program function key #1 to transmit string #2"),
    TermCapability(:print_screen,              :mc0,      "print contents of screen"),
    TermCapability(:prtr_off,                  :mc4,      "turn off printer"),
    TermCapability(:prtr_on,                   :mc5,      "turn on printer"),
    TermCapability(:repeat_char,               :rep,      "repeat char #1 #2 times (P*)"),
    TermCapability(:reset_1string,             :rs1,      "reset string"),
    TermCapability(:reset_2string,             :rs2,      "reset string"),
    TermCapability(:reset_3string,             :rs3,      "reset string"),
    TermCapability(:reset_file,                :rf,       "name of reset file"),
    TermCapability(:restore_cursor,            :rc,       "restore cursor to position of last save_cursor"),
    TermCapability(:row_address,               :vpa,      "vertical position #1 absolute (P)"),
    TermCapability(:save_cursor,               :sc,       "save current cursor position (P)"),
    TermCapability(:scroll_forward,            :ind,      "scroll text up (P)"),
    TermCapability(:scroll_reverse,            :ri,       "scroll text down (P)"),
    TermCapability(:set_attributes,            :sgr,      "define video attributes #1-#9 (PG9)"),
    TermCapability(:set_tab,                   :hts,      "set a tab in every row, current columns"),
    TermCapability(:set_window,                :wind,     "current window is lines #1-#2 cols #3-#4"),
    TermCapability(:tab,                       :ht,       "tab to next 8-space hardware tab stop"),
    TermCapability(:to_status_line,            :tsl,      "move to status line, column #1"),
    TermCapability(:underline_char,            :uc,       "underline char and move past it"),
    TermCapability(:up_half_line,              :hu,       "half a line up"),
    TermCapability(:init_prog,                 :iprog,    "path name of program for initialization"),
    TermCapability(:key_a1,                    :ka1,      "upper left of keypad"),
    TermCapability(:key_a3,                    :ka3,      "upper right of keypad"),
    TermCapability(:key_b2,                    :kb2,      "center of keypad"),
    TermCapability(:key_c1,                    :kc1,      "lower left of keypad"),
    TermCapability(:key_c3,                    :kc3,      "lower right of keypad"),
    TermCapability(:prtr_non,                  :mc5p,     "turn on printer for #1 bytes"),
    TermCapability(:char_padding,              :rmp,      "like ip but when in insert mode"),
    TermCapability(:acs_chars,                 :acsc,     "graphics charset pairs, based on vt100"),
    TermCapability(:plab_norm,                 :pln,      "program label #1 to show string #2"),
    TermCapability(:key_btab,                  :kcbt,     "back-tab key"),
    TermCapability(:enter_xon_mode,            :smxon,    "turn on xon/xoff handshaking"),
    TermCapability(:exit_xon_mode,             :rmxon,    "turn off xon/xoff handshaking"),
    TermCapability(:enter_am_mode,             :smam,     "turn on automatic margins"),
    TermCapability(:exit_am_mode,              :rmam,     "turn off automatic margins"),
    TermCapability(:xon_character,             :xonc,     "XON character"),
    TermCapability(:xoff_character,            :xoffc,    "XOFF character"),
    TermCapability(:ena_acs,                   :enacs,    "enable alternate char set"),
    TermCapability(:label_on,                  :smln,     "turn on soft labels"),
    TermCapability(:label_off,                 :rmln,     "turn off soft labels"),
    TermCapability(:key_beg,                   :kbeg,     "begin key"),
    TermCapability(:key_cancel,                :kcan,     "cancel key"),
    TermCapability(:key_close,                 :kclo,     "close key"),
    TermCapability(:key_command,               :kcmd,     "command key"),
    TermCapability(:key_copy,                  :kcpy,     "copy key"),
    TermCapability(:key_create,                :kcrt,     "create key"),
    TermCapability(:key_end,                   :kend,     "end key"),
    TermCapability(:key_enter,                 :kent,     "enter/send key"),
    TermCapability(:key_exit,                  :kext,     "exit key"),
    TermCapability(:key_find,                  :kfnd,     "find key"),
    TermCapability(:key_help,                  :khlp,     "help key"),
    TermCapability(:key_mark,                  :kmrk,     "mark key"),
    TermCapability(:key_message,               :kmsg,     "message key"),
    TermCapability(:key_move,                  :kmov,     "move key"),
    TermCapability(:key_next,                  :knxt,     "next key"),
    TermCapability(:key_open,                  :kopn,     "open key"),
    TermCapability(:key_options,               :kopt,     "options key"),
    TermCapability(:key_previous,              :kprv,     "previous key"),
    TermCapability(:key_print,                 :kprt,     "print key"),
    TermCapability(:key_redo,                  :krdo,     "redo key"),
    TermCapability(:key_reference,             :kref,     "reference key"),
    TermCapability(:key_refresh,               :krfr,     "refresh key"),
    TermCapability(:key_replace,               :krpl,     "replace key"),
    TermCapability(:key_restart,               :krst,     "restart key"),
    TermCapability(:key_resume,                :kres,     "resume key"),
    TermCapability(:key_save,                  :ksav,     "save key"),
    TermCapability(:key_suspend,               :kspd,     "suspend key"),
    TermCapability(:key_undo,                  :kund,     "undo key"),
    TermCapability(:key_sbeg,                  :kBEG,     "shifted begin key"),
    TermCapability(:key_scancel,               :kCAN,     "shifted cancel key"),
    TermCapability(:key_scommand,              :kCMD,     "shifted command key"),
    TermCapability(:key_scopy,                 :kCPY,     "shifted copy key"),
    TermCapability(:key_screate,               :kCRT,     "shifted create key"),
    TermCapability(:key_sdc,                   :kDC,      "shifted delete-character key"),
    TermCapability(:key_sdl,                   :kDL,      "shifted delete-line key"),
    TermCapability(:key_select,                :kslt,     "select key"),
    TermCapability(:key_send,                  :kEND,     "shifted end key"),
    TermCapability(:key_seol,                  :kEOL,     "shifted clear-to-end-of-line key"),
    TermCapability(:key_sexit,                 :kEXT,     "shifted exit key"),
    TermCapability(:key_sfind,                 :kFND,     "shifted find key"),
    TermCapability(:key_shelp,                 :kHLP,     "shifted help key"),
    TermCapability(:key_shome,                 :kHOM,     "shifted home key"),
    TermCapability(:key_sic,                   :kIC,      "shifted insert-character key"),
    TermCapability(:key_sleft,                 :kLFT,     "shifted left-arrow key"),
    TermCapability(:key_smessage,              :kMSG,     "shifted message key"),
    TermCapability(:key_smove,                 :kMOV,     "shifted move key"),
    TermCapability(:key_snext,                 :kNXT,     "shifted next key"),
    TermCapability(:key_soptions,              :kOPT,     "shifted options key"),
    TermCapability(:key_sprevious,             :kPRV,     "shifted previous key"),
    TermCapability(:key_sprint,                :kPRT,     "shifted print key"),
    TermCapability(:key_sredo,                 :kRDO,     "shifted redo key"),
    TermCapability(:key_sreplace,              :kRPL,     "shifted replace key"),
    TermCapability(:key_sright,                :kRIT,     "shifted right-arrow key"),
    TermCapability(:key_srsume,                :kRES,     "shifted resume key"),
    TermCapability(:key_ssave,                 :kSAV,     "shifted save key"),
    TermCapability(:key_ssuspend,              :kSPD,     "shifted suspend key"),
    TermCapability(:key_sundo,                 :kUND,     "shifted undo key"),
    TermCapability(:req_for_input,             :rfi,      "send next input char (for ptys)"),
    TermCapability(:key_f11,                   :kf11,     "F11 function key"),
    TermCapability(:key_f12,                   :kf12,     "F12 function key"),
    TermCapability(:key_f13,                   :kf13,     "F13 function key"),
    TermCapability(:key_f14,                   :kf14,     "F14 function key"),
    TermCapability(:key_f15,                   :kf15,     "F15 function key"),
    TermCapability(:key_f16,                   :kf16,     "F16 function key"),
    TermCapability(:key_f17,                   :kf17,     "F17 function key"),
    TermCapability(:key_f18,                   :kf18,     "F18 function key"),
    TermCapability(:key_f19,                   :kf19,     "F19 function key"),
    TermCapability(:key_f20,                   :kf20,     "F20 function key"),
    TermCapability(:key_f21,                   :kf21,     "F21 function key"),
    TermCapability(:key_f22,                   :kf22,     "F22 function key"),
    TermCapability(:key_f23,                   :kf23,     "F23 function key"),
    TermCapability(:key_f24,                   :kf24,     "F24 function key"),
    TermCapability(:key_f25,                   :kf25,     "F25 function key"),
    TermCapability(:key_f26,                   :kf26,     "F26 function key"),
    TermCapability(:key_f27,                   :kf27,     "F27 function key"),
    TermCapability(:key_f28,                   :kf28,     "F28 function key"),
    TermCapability(:key_f29,                   :kf29,     "F29 function key"),
    TermCapability(:key_f30,                   :kf30,     "F30 function key"),
    TermCapability(:key_f31,                   :kf31,     "F31 function key"),
    TermCapability(:key_f32,                   :kf32,     "F32 function key"),
    TermCapability(:key_f33,                   :kf33,     "F33 function key"),
    TermCapability(:key_f34,                   :kf34,     "F34 function key"),
    TermCapability(:key_f35,                   :kf35,     "F35 function key"),
    TermCapability(:key_f36,                   :kf36,     "F36 function key"),
    TermCapability(:key_f37,                   :kf37,     "F37 function key"),
    TermCapability(:key_f38,                   :kf38,     "F38 function key"),
    TermCapability(:key_f39,                   :kf39,     "F39 function key"),
    TermCapability(:key_f40,                   :kf40,     "F40 function key"),
    TermCapability(:key_f41,                   :kf41,     "F41 function key"),
    TermCapability(:key_f42,                   :kf42,     "F42 function key"),
    TermCapability(:key_f43,                   :kf43,     "F43 function key"),
    TermCapability(:key_f44,                   :kf44,     "F44 function key"),
    TermCapability(:key_f45,                   :kf45,     "F45 function key"),
    TermCapability(:key_f46,                   :kf46,     "F46 function key"),
    TermCapability(:key_f47,                   :kf47,     "F47 function key"),
    TermCapability(:key_f48,                   :kf48,     "F48 function key"),
    TermCapability(:key_f49,                   :kf49,     "F49 function key"),
    TermCapability(:key_f50,                   :kf50,     "F50 function key"),
    TermCapability(:key_f51,                   :kf51,     "F51 function key"),
    TermCapability(:key_f52,                   :kf52,     "F52 function key"),
    TermCapability(:key_f53,                   :kf53,     "F53 function key"),
    TermCapability(:key_f54,                   :kf54,     "F54 function key"),
    TermCapability(:key_f55,                   :kf55,     "F55 function key"),
    TermCapability(:key_f56,                   :kf56,     "F56 function key"),
    TermCapability(:key_f57,                   :kf57,     "F57 function key"),
    TermCapability(:key_f58,                   :kf58,     "F58 function key"),
    TermCapability(:key_f59,                   :kf59,     "F59 function key"),
    TermCapability(:key_f60,                   :kf60,     "F60 function key"),
    TermCapability(:key_f61,                   :kf61,     "F61 function key"),
    TermCapability(:key_f62,                   :kf62,     "F62 function key"),
    TermCapability(:key_f63,                   :kf63,     "F63 function key"),
    TermCapability(:clr_bol,                   :el1,      "Clear to beginning of line"),
    TermCapability(:clear_margins,             :mgc,      "clear right and left soft margins"),
    TermCapability(:set_left_margin,           :smgl,     "set left soft margin at current column."),
    TermCapability(:set_right_margin,          :smgr,     "set right soft margin at current column"),
    TermCapability(:label_format,              :fln,      "label format"),
    TermCapability(:set_clock,                 :sclk,     "set clock, #1 hrs #2 mins #3 secs"),
    TermCapability(:display_clock,             :dclk,     "display clock"),
    TermCapability(:remove_clock,              :rmclk,    "remove clock"),
    TermCapability(:create_window,             :cwin,     "define a window #1 from #2,#3 to #4,#5"),
    TermCapability(:goto_window,               :wingo,    "go to window #1"),
    TermCapability(:hangup,                    :hup,      "hang-up phone"),
    TermCapability(:dial_phone,                :dial,     "dial number #1"),
    TermCapability(:quick_dial,                :qdial,    "dial number #1 without checking"),
    TermCapability(:tone,                      :tone,     "select touch tone dialing"),
    TermCapability(:pulse,                     :pulse,    "select pulse dialing"),
    TermCapability(:flash_hook,                :hook,     "flash switch hook"),
    TermCapability(:fixed_pause,               :pause,    "pause for 2-3 seconds"),
    TermCapability(:wait_tone,                 :wait,     "wait for dial-tone"),
    TermCapability(:user0,                     :u0,       "User string #0"),
    TermCapability(:user1,                     :u1,       "User string #1"),
    TermCapability(:user2,                     :u2,       "User string #2"),
    TermCapability(:user3,                     :u3,       "User string #3"),
    TermCapability(:user4,                     :u4,       "User string #4"),
    TermCapability(:user5,                     :u5,       "User string #5"),
    TermCapability(:user6,                     :u6,       "User string #6"),
    TermCapability(:user7,                     :u7,       "User string #7"),
    TermCapability(:user8,                     :u8,       "User string #8"),
    TermCapability(:user9,                     :u9,       "User string #9"),
    TermCapability(:orig_pair,                 :op,       "Set default pair to its original value"),
    TermCapability(:orig_colors,               :oc,       "Set all color pairs to the original ones"),
    TermCapability(:initialize_color,          :initc,    "initialize color #1 to (#2,#3,#4)"),
    TermCapability(:initialize_pair,           :initp,    "Initialize color pair #1 to fg=(#2,#3,#4), bg=(#5,#6,#7)"),
    TermCapability(:set_color_pair,            :scp,      "Set current color pair to #1"),
    TermCapability(:set_foreground,            :setf,     "Set foreground color #1"),
    TermCapability(:set_background,            :setb,     "Set background color #1"),
    TermCapability(:change_char_pitch,         :cpi,      "Change number of characters per inch to #1"),
    TermCapability(:change_line_pitch,         :lpi,      "Change number of lines per inch to #1"),
    TermCapability(:change_res_horz,           :chr,      "Change horizontal resolution to #1"),
    TermCapability(:change_res_vert,           :cvr,      "Change vertical resolution to #1"),
    TermCapability(:define_char,               :defc,     "Define a character #1, #2 dots wide, descender #3"),
    TermCapability(:enter_doublewide_mode,     :swidm,    "Enter double-wide mode"),
    TermCapability(:enter_draft_quality,       :sdrfq,    "Enter draft-quality mode"),
    TermCapability(:enter_italics_mode,        :sitm,     "Enter italic mode"),
    TermCapability(:enter_leftward_mode,       :slm,      "Start leftward carriage motion"),
    TermCapability(:enter_micro_mode,          :smicm,    "Start micro-motion mode"),
    TermCapability(:enter_near_letter_quality, :snlq,     "Enter NLQ mode"),
    TermCapability(:enter_normal_quality,      :snrmq,    "Enter normal-quality mode"),
    TermCapability(:enter_shadow_mode,         :sshm,     "Enter shadow-print mode"),
    TermCapability(:enter_subscript_mode,      :ssubm,    "Enter subscript mode"),
    TermCapability(:enter_superscript_mode,    :ssupm,    "Enter superscript mode"),
    TermCapability(:enter_upward_mode,         :sum,      "Start upward carriage motion"),
    TermCapability(:exit_doublewide_mode,      :rwidm,    "End double-wide mode"),
    TermCapability(:exit_italics_mode,         :ritm,     "End italic mode"),
    TermCapability(:exit_leftward_mode,        :rlm,      "End left-motion mode"),
    TermCapability(:exit_micro_mode,           :rmicm,    "End micro-motion mode"),
    TermCapability(:exit_shadow_mode,          :rshm,     "End shadow-print mode"),
    TermCapability(:exit_subscript_mode,       :rsubm,    "End subscript mode"),
    TermCapability(:exit_superscript_mode,     :rsupm,    "End superscript mode"),
    TermCapability(:exit_upward_mode,          :rum,      "End reverse character motion"),
    TermCapability(:micro_column_address,      :mhpa,     "Like column_address in micro mode"),
    TermCapability(:micro_down,                :mcud1,    "Like cursor_down in micro mode"),
    TermCapability(:micro_left,                :mcub1,    "Like cursor_left in micro mode"),
    TermCapability(:micro_right,               :mcuf1,    "Like cursor_right in micro mode"),
    TermCapability(:micro_row_address,         :mvpa,     "Like row_address #1 in micro mode"),
    TermCapability(:micro_up,                  :mcuu1,    "Like cursor_up in micro mode"),
    TermCapability(:order_of_pins,             :porder,   "Match software bits to print-head pins"),
    TermCapability(:parm_down_micro,           :mcud,     "Like parm_down_cursor in micro mode"),
    TermCapability(:parm_left_micro,           :mcub,     "Like parm_left_cursor in micro mode"),
    TermCapability(:parm_right_micro,          :mcuf,     "Like parm_right_cursor in micro mode"),
    TermCapability(:parm_up_micro,             :mcuu,     "Like parm_up_cursor in micro mode"),
    TermCapability(:select_char_set,           :scs,      "Select character set, #1"),
    TermCapability(:set_bottom_margin,         :smgb,     "Set bottom margin at current line"),
    TermCapability(:set_bottom_margin_parm,    :smgbp,    "Set bottom margin at line #1 or (if smgtp is not given) #2 lines from bottom"),
    TermCapability(:set_left_margin_parm,      :smglp,    "Set left (right) margin at column #1"),
    TermCapability(:set_right_margin_parm,     :smgrp,    "Set right margin at column #1"),
    TermCapability(:set_top_margin,            :smgt,     "Set top margin at current line"),
    TermCapability(:set_top_margin_parm,       :smgtp,    "Set top (bottom) margin at row #1"),
    TermCapability(:start_bit_image,           :sbim,     "Start printing bit image graphics"),
    TermCapability(:start_char_set_def,        :scsd,     "Start character set definition #1, with #2 characters in the set"),
    TermCapability(:stop_bit_image,            :rbim,     "Stop printing bit image graphics"),
    TermCapability(:stop_char_set_def,         :rcsd,     "End definition of character set #1"),
    TermCapability(:subscript_characters,      :subcs,    "List of subscriptable characters"),
    TermCapability(:superscript_characters,    :supcs,    "List of superscriptable characters"),
    TermCapability(:these_cause_cr,            :docr,     "Printing any of these characters causes CR"),
    TermCapability(:zero_motion,               :zerom,    "No motion for subsequent character"),
    TermCapability(:char_set_names,            :csnm,     "Produce #1'th item from list of character set names"),
    TermCapability(:key_mouse,                 :kmous,    "Mouse event has occurred"),
    TermCapability(:mouse_info,                :minfo,    "Mouse status information"),
    TermCapability(:req_mouse_pos,             :reqmp,    "Request mouse position"),
    TermCapability(:get_mouse,                 :getm,     "Curses should get button events, parameter #1 not documented."),
    TermCapability(:set_a_foreground,          :setaf,    "Set foreground color to #1, using ANSI escape"),
    TermCapability(:set_a_background,          :setab,    "Set background color to #1, using ANSI escape"),
    TermCapability(:pkey_plab,                 :pfxl,     "Program function key #1 to type string #2 and show string #3"),
    TermCapability(:device_type,               :devt,     "Indicate language/codeset support"),
    TermCapability(:code_set_init,             :csin,     "Init sequence for multiple codesets"),
    TermCapability(:set0_des_seq,              :s0ds,     "Shift to codeset 0 (EUC set 0, ASCII)"),
    TermCapability(:set1_des_seq,              :s1ds,     "Shift to codeset 1"),
    TermCapability(:set2_des_seq,              :s2ds,     "Shift to codeset 2"),
    TermCapability(:set3_des_seq,              :s3ds,     "Shift to codeset 3"),
    TermCapability(:set_lr_margin,             :smglr,    "Set both left and right margins to #1, #2.  (ML is not in BSD termcap)."),
    TermCapability(:set_tb_margin,             :smgtb,    "Sets both top and bottom margins to #1, #2"),
    TermCapability(:bit_image_repeat,          :birep,    "Repeat bit image cell #1 #2 times"),
    TermCapability(:bit_image_newline,         :binel,    "Move to next row of the bit image"),
    TermCapability(:bit_image_carriage_return, :bicr,     "Move to beginning of same row"),
    TermCapability(:color_names,               :colornm,  "Give name for color #1"),
    TermCapability(:define_bit_image_region,   :defbi,    "Define rectangular bit image region"),
    TermCapability(:end_bit_image_region,      :endbi,    "End a bit-image region"),
    TermCapability(:set_color_band,            :setcolor, "Change to ribbon color #1"),
    TermCapability(:set_page_length,           :slines,   "Set page length to #1 lines"),
    TermCapability(:display_pc_char,           :dispc,    "Display PC character #1"),
    TermCapability(:enter_pc_charset_mode,     :smpch,    "Enter PC character display mode"),
    TermCapability(:exit_pc_charset_mode,      :rmpch,    "Exit PC character display mode"),
    TermCapability(:enter_scancode_mode,       :smsc,     "Enter PC scancode mode"),
    TermCapability(:exit_scancode_mode,        :rmsc,     "Exit PC scancode mode"),
    TermCapability(:pc_term_options,           :pctrm,    "PC terminal options"),
    TermCapability(:scancode_escape,           :scesc,    "Escape for scancode emulation"),
    TermCapability(:alt_scancode_esc,          :scesa,    "Alternate escape for scancode emulation"),
    TermCapability(:enter_horizontal_hl_mode,  :ehhlm,    "Enter horizontal highlight mode"),
    TermCapability(:enter_left_hl_mode,        :elhlm,    "Enter left highlight mode"),
    TermCapability(:enter_low_hl_mode,         :elohlm,   "Enter low highlight mode"),
    TermCapability(:enter_right_hl_mode,       :erhlm,    "Enter right highlight mode"),
    TermCapability(:enter_top_hl_mode,         :ethlm,    "Enter top highlight mode"),
    TermCapability(:enter_vertical_hl_mode,    :evhlm,    "Enter vertical highlight mode"),
    TermCapability(:set_a_attributes,          :sgr1,     "Define second set of video attributes #1-#6"),
    TermCapability(:set_pglen_inch,            :slength,  "Set page length to #1 hundredth of an inch (some implementations use sL for termcap)."),
    TermCapability(:termcap_init2,             :OTi2,     "secondary initialization string"),
    TermCapability(:termcap_reset,             :OTrs,     "terminal reset string"),
    TermCapability(:linefeed_if_not_lf,        :OTnl,     "use to move down"),
    TermCapability(:backspace_if_not_bs,       :OTbc,     "move left, if not ^H"),
    TermCapability(:other_non_function_keys,   :OTko,     "list of self-mapped keycaps"),
    TermCapability(:arrow_key_map,             :OTma,     "map motion-keys for vi version 2"),
    TermCapability(:acs_ulcorner,              :OTG2,     "single upper left"),
    TermCapability(:acs_llcorner,              :OTG3,     "single lower left"),
    TermCapability(:acs_urcorner,              :OTG1,     "single upper right"),
    TermCapability(:acs_lrcorner,              :OTG4,     "single lower right"),
    TermCapability(:acs_ltee,                  :OTGR,     "tee pointing right"),
    TermCapability(:acs_rtee,                  :OTGL,     "tee pointing left"),
    TermCapability(:acs_btee,                  :OTGU,     "tee pointing up"),
    TermCapability(:acs_ttee,                  :OTGD,     "tee pointing down"),
    TermCapability(:acs_hline,                 :OTGH,     "single horizontal line"),
    TermCapability(:acs_vline,                 :OTGV,     "single vertical line"),
    TermCapability(:acs_plus,                  :OTGC,     "single intersection"),
    TermCapability(:memory_lock,               :meml,     "lock memory above cursor"),
    TermCapability(:memory_unlock,             :memu,     "unlock memory"),
    TermCapability(:box_chars_1,               :box1,     "box characters primary set"),
]

"""
Terminfo extensions that NCurses 6.4-20230311 is aware of.
"""
const TERM_USER = Dict{Tuple{DataType, Symbol}, Union{Tuple{Nothing, String}, Tuple{Symbol, String}}}(
    (Int,    :CO )    => (nothing, "number of indexed colors overlaying RGB space"),
    (String, :E3)     => (nothing, "clears the terminal's scrollback buffer."),
    (Bool,   :NQ)     => (nothing, "terminal does not support query/response"),
    (Bool,   :RGB)    => (nothing, "use direct colors with 1/3 of color-pair bits per color."),
    (Int,    :RGB)    => (nothing, "use direct colors with given number of bits per color."),
    (String, :RGB)    => (nothing, "use direct colors with given bit-layout."),
    (String, :TS)     => (nothing, "like \"tsl\", but uses no parameter."),
    (Int,    :U8)     => (nothing, "terminal does/does not support VT100 SI/SO when processing UTF-8 encoding."),
    (String, :XM)     => (nothing, "initialize alternate xterm mouse mode"),
    (String, :grbom)  => (nothing, "disable real bold (not intensity bright) mode."),
    (String, :gsbom)  => (nothing, "enable real bold (not intensity bright) mode."),
    (String, :xm)     => (nothing, "mouse response"),
    (String, :Rmol)   => (:exit_overline_mode, "remove overline-mode"),
    (String, :Smol)   => (:enter_overline_mode, "set overline-mode"),
    (String, :blink2) => (nothing, "turn on rapid blinking"),
    (String, :norm)   => (nothing, "turn off bold and half-bright mode"),
    (String, :opaq)   => (nothing, "turn off blank mode"),
    (String, :setal)  => (nothing, "set underline-color"),
    (String, :smul2)  => (nothing, "begin double underline mode"),
    (Bool,   :AN)     => (nothing, "turn on autonuke."),
    (Bool,   :AX)     => (nothing, "understands ANSI set default fg/bg color (\\E[39m / \\E[49m)."),
    (String, :C0)     => (nothing, "use the string as a conversion table for font '0', like acsc."),
    (Bool,   :C8)     => (nothing, "terminal shows bold as high-intensity colors."),
    (String, :CE)     => (nothing, "switch cursor-keys back to normal mode."),
    (String, :CS)     => (nothing, "switch cursor-keys to application mode."),
    (String, :E0)     => (nothing, "switch charset 'G0' back to standard charset. Default is '\\E(B'."),
    (Bool,   :G0)     => (nothing, "terminal can deal with ISO 2022 font selection sequences."),
    (String, :KJ)     => (nothing, "set the encoding of the terminal."),
    (Int,    :OL)     => (nothing, "set the screen program's output buffer limit."),
    (String, :S0)     => (nothing, "switch charset 'G0' to the specified charset. Default is '\\E(%.'."),
    (Bool,   :TF)     => (nothing, "add missing capabilities to screen's termcap/info entry. (Set by default)."),
    (String, :WS)     => (nothing, "resize display. This capability has the desired width and height as arguments. SunView(tm) example: '\\E[8;%d;%dt'."),
    (String, :XC)     => (nothing, "describe a translation of characters to strings depending on the current font."),
    (Bool,   :XT)     => (nothing, "terminal understands special xterm sequences (OSC, mouse tracking)."),
    (String, :Z0)     => (nothing, "change width to 132 columns."),
    (String, :Z1)     => (nothing, "change width to 80 columns."),
    (String, :Cr)     => (:reset_cursor_color, "restore the default cursor color."),
    (String, :Cs)     => (:set_cursor_color, "set the cursor color."),
    (String, :Csr)    => (nothing, "change the cursor style, overriding Ss."),
    (String, :Ms)     => (:set_host_clipboard, "store the current buffer in the host terminal's selection (clipboard)."),
    (String, :Se)     => (:reset_cursor_style, "reset the cursor style to the terminal initial state."),
    (String, :Smulx)  => (:set_underline_style, "modify the appearance of underlines in VTE."),
    (String, :Ss)     => (:set_cursor_style, "change the cursor style."),
    (String, :rmxx)   => (:exit_strikeout_mode, "reset ECMA-48 strikeout/crossed-out attributes."),
    (String, :smxx)   => (:enter_strikeout_mode, "set ECMA-48 strikeout/crossed-out attributes."),
    (String, :BD)     => (nothing, "disables bracketed paste"),
    (String, :BE)     => (nothing, "enables bracketed paste"),
    (String, :PE)     => (nothing, "is sent after pasted text"),
    (String, :PS)     => (nothing, "is sent before pasted text"),
    (String, :RV)     => (nothing, "report terminal secondary device attributes"),
    (String, :XR)     => (nothing, "report terminal version as a free-format string."),
    (Bool,   :XF)     => (:xterm_focus, "terminal supports xterm focus in/out"),
    (String, :rv)     => (nothing, "response to RV, regular expression"),
    (String, :xr)     => (nothing, "response to XR, regular expression"),
    (String, :csl)    => (:clear_status_line, "clear status line"),
    (String, :kDC3)   => (:key_alt_delete_character, "alt delete-character"),
    (String, :kDC4)   => (:key_shift_alt_delete_character, "shift+alt delete-character"),
    (String, :kDC5)   => (:key_control_delete_character, "control delete-character"),
    (String, :kDC6)   => (:key_shift_control_delete_character, "shift+control delete-character"),
    (String, :kDC7)   => (:key_alt_control_delete_character, "alt+control delete-character"),
    (String, :kDN)    => (:key_shift_down_cursor, "shift down-cursor"),
    (String, :kDN3)   => (:key_alt_down_cursor, "alt down-cursor"),
    (String, :kDN4)   => (:key_shift_alt_down_cursor, "shift+alt down-cursor"),
    (String, :kDN5)   => (:key_control_down_cursor, "control down-cursor"),
    (String, :kDN6)   => (:key_shift_control_down_cursor, "shift+control down-cursor"),
    (String, :kDN7)   => (:key_alt_control_down_cursor, "alt+control down-cursor"),
    (String, :kEND3)  => (:key_alt_end, "alt end"),
    (String, :kEND4)  => (:key_shift_alt_end, "shift+alt end"),
    (String, :kEND5)  => (:key_control_end, "control end"),
    (String, :kEND6)  => (:key_shift_control_end, "shift+control end"),
    (String, :kEND7)  => (:key_alt_control_end, "alt+control end"),
    (String, :kHOM3)  => (:key_alt_home, "alt home"),
    (String, :kHOM4)  => (:key_shift_alt_home, "shift+alt home"),
    (String, :kHOM5)  => (:key_control_home, "control home"),
    (String, :kHOM6)  => (:key_shift_control_home, "shift+control home"),
    (String, :kHOM7)  => (:key_alt_control_home, "alt+control home"),
    (String, :kIC3)   => (:key_alt_insert_character, "alt insert-character"),
    (String, :kIC4)   => (:key_shift_alt_insert_character, "shift+alt insert-character"),
    (String, :kIC5)   => (:key_control_insert_character, "control insert-character"),
    (String, :kIC6)   => (:key_shift_control_insert_character, "shift+control insert-character"),
    (String, :kIC7)   => (:key_alt_control_insert_character, "alt+control insert-character"),
    (String, :kLFT3)  => (:key_alt_left_cursor, "alt left-cursor"),
    (String, :kLFT4)  => (:key_shift_alt_left_cursor, "shift+alt left-cursor"),
    (String, :kLFT5)  => (:key_control_left_cursor, "control left-cursor"),
    (String, :kLFT6)  => (:key_shift_control_left_cursor, "shift+control left-cursor"),
    (String, :kLFT7)  => (:key_alt_control_left_cursor, "alt+control left-cursor"),
    (String, :kNXT3)  => (:key_alt_next, "alt next"),
    (String, :kNXT4)  => (:key_shift_alt_next, "shift+alt next"),
    (String, :kNXT5)  => (:key_control_next, "control next"),
    (String, :kNXT6)  => (:key_shift_control_next, "shift+control next"),
    (String, :kNXT7)  => (:key_alt_control_next, "alt+control next"),
    (String, :kPRV3)  => (:key_alt_previous, "alt previous"),
    (String, :kPRV4)  => (:key_shift_alt_previous, "shift+alt previous"),
    (String, :kPRV5)  => (:key_control_previous, "control previous"),
    (String, :kPRV6)  => (:key_shift_control_previous, "shift+control previous"),
    (String, :kPRV7)  => (:key_alt_control_previous, "alt+control previous"),
    (String, :kRIT3)  => (:key_alt_right_cursor, "alt right-cursor"),
    (String, :kRIT4)  => (:key_shift_alt_right_cursor, "shift+alt right-cursor"),
    (String, :kRIT5)  => (:key_control_right_cursor, "control right-cursor"),
    (String, :kRIT6)  => (:key_shift_control_right_cursor, "shift+control right-cursor"),
    (String, :kRIT7)  => (:key_alt_control_right_cursor, "alt+control right-cursor"),
    (String, :kUP)    => (:key_shift_up_cursor, "shift up-cursor"),
    (String, :kUP3)   => (:key_alt_up_cursor, "alt up-cursor"),
    (String, :kUP4)   => (:key_shift_alt_up_cursor, "shift+alt up-cursor"),
    (String, :kUP5)   => (:key_control_up_cursor, "control up-cursor"),
    (String, :kUP6)   => (:key_shift_control_up_cursor, "shift+control up-cursor"),
    (String, :kUP7)   => (:key_alt_control_up_cursor, "alt+control up-cursor"),
    (String, :ka2)    => (nothing, "vt220-keypad extensions"),
    (String, :kb1)    => (nothing, "vt220-keypad extensions"),
    (String, :kb3)    => (nothing, "vt220-keypad extensions"),
    (String, :kc2)    => (nothing, "vt220-keypad extensions"),
    (String, :kxIN)   => (:key_mouse_response_on_focus_in, "mouse response on focus-in"),
    (String, :kxOUT)  => (:key_mouse_response_on_focus_out, "mouse response on focus-out"),
    (Bool,   :Tc)     => (:truecolor, "tmux extension to indicate 24-bit truecolor support"),
    (Bool,   :Su)     => (:can_style_underline, "kitty extension to indicate styled underline support"),
)
