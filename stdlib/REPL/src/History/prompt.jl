# This file is a part of Julia. License is MIT: https://julialang.org/license

const PROMPT_TEXT = "▪: "

struct Event <: Function
    info::Channel{Symbol}
    name::Symbol
end

function (e::Event)(_...)
    push!(e.info, e.name)
    :ignore
end

"""
    select_keymap(events::Channel{Symbol})

Build a REPL.LineEdit keymap that pushes symbols into `events`.

Binds arrows, page keys, Tab, Ctrl-C/D/S, and meta-< / > to
`Event` or `Returns` actions for driving the prompt loop.
"""
function select_keymap(events::Channel{Symbol})
    REPL.LineEdit.keymap([
        Dict{Any, Any}(
            # Up Arrow
            "\e[A" => Event(events, :up),
            "^P" => Event(events, :up),
            # Down Arrow
            "\e[B" => Event(events, :down),
            "^N" => Event(events, :down),
            # Tab
            '\t' => Event(events, :tab),
            # Page up
            "\e[5~" => Event(events, :pageup),
            "\ev" => Event(events, :pageup),
            # Page down
            "\e[6~" => Event(events, :pagedown),
            "^V" => Event(events, :pagedown),
            # Meta + < / >
            "\e<" => Event(events, :jumpfirst),
            "\e>" => Event(events, :jumplast),
            #
            "^L" => Event(events, :clear),
            # Exits
            "^C" => Returns(:abort),
            "^D" => Returns(:abort),
            "^G" => Returns(:abort),
            "\e\e" => Returns(:abort),
            "^S" => Returns(:save),
            "^Y" => Returns(:copy),
        ),
        REPL.LineEdit.default_keymap,
        REPL.LineEdit.escape_defaults])
end

"""
    create_prompt(events::Channel{Symbol}, term, prefix, terminal_properties)

Initialize a custom REPL prompt tied to `events` using the existing `term`.

The `terminal_properties` from the main REPL's MIState are shared with the
history search's MIState so that DA1/OSC responses parsed by the keymap
update the same object.

Returns a tuple `(term, prompt, istate, pstate)` ready for
input handling and display.
"""
function create_prompt(events::Channel{Symbol}, term, prefix::String = "\e[90m",
                       terminal_properties::REPL.LineEdit.TerminalProperties = REPL.LineEdit.TerminalProperties())
    prompt = REPL.LineEdit.Prompt(
        PROMPT_TEXT, # prompt
        prefix, "\e[0m", # prompt_prefix, prompt_suffix
        "", "", "", # output_prefix, output_prefix_prefix, output_prefix_suffix
        select_keymap(events), # keymap_dict
        nothing, # repl
        REPL.LatexCompletions(), # complete
        _ -> true, # on_enter
        () -> nothing, # on_done
        REPL.LineEdit.EmptyHistoryProvider(), # hist
        false, # sticky
        REPL.StylingPasses.StylingPass[]) # styling_passes
    interface = REPL.LineEdit.ModalInterface([prompt])
    istate = REPL.LineEdit.init_state(term, interface)
    # Share the main REPL's terminal_properties so that DA1/OSC responses
    # parsed by the keymap during history search update the same object.
    istate.terminal_properties = terminal_properties
    pstate = istate.mode_state[prompt]
    (; term, prompt, istate, pstate)
end

"""
    runprompt!((; term,prompt,pstate,istate), events::Channel{Symbol}, terminal_properties)

Drive the prompt input loop until confirm, save, or abort.

Emits `:edit`, `:confirm`, `:save`, or `:abort` into `events`,
manages raw mode and bracketed paste, and cleans up on exit.
"""
function runprompt!((; term, prompt, pstate, istate), events::Channel{Symbol},
                    terminal_properties::REPL.LineEdit.TerminalProperties)
    Base.reseteof(term)
    REPL.LineEdit.raw!(term, true)
    REPL.LineEdit.enable_bracketed_paste(term)
    try
        pstate.ias = REPL.LineEdit.InputAreaState(0, 0)
        REPL.LineEdit.refresh_multi_line(term, pstate)
        while true
            kmap = REPL.LineEdit.keymap(pstate, prompt)
            matchfn = REPL.LineEdit.match_input(kmap, istate)
            kdata = REPL.LineEdit.keymap_data(pstate, prompt)
            status = matchfn(istate, kdata)
            if status === :ok
                push!(events, :edit)
            elseif status === :ignore
                istate.last_action = istate.current_action
            elseif status === :done
                print("\e[F")
                push!(events, :confirm)
                break
            elseif status === :copy
                print("\e[1G\e[J")
                push!(events, status)
                break
            elseif status === :save
                print("\e[1G\e[J")
                dest = savedest(term, terminal_properties)
                if isnothing(dest)
                    push!(events, :redraw)
                else
                    push!(events, dest)
                    break
                end
            else
                push!(events, :abort)
                break
            end
        end
    finally
        REPL.LineEdit.raw!(term, false) &&
            REPL.LineEdit.disable_bracketed_paste(term)
    end
end

function savedest(term::Base.Terminals.TTYTerminal, props::REPL.LineEdit.TerminalProperties)
    out = term.out_stream
    inp = term.in_stream
    print(out, "\e[1G\e[J")
    has_clip = clipboard_available(props)
    # If DA1 already received and no clipboard available, skip straight to file save
    if !has_clip && props.da1 !== nothing
        return :filesave
    end
    clipsave = has_clip
    # State machine for escape sequence parsing:
    #   :none - normal input
    #   :esc  - read \e, waiting for next byte
    #   :csi  - read \e[, consuming CSI sequence
    esc_state = :none
    try
        print(out, get(Base.current_terminfo(), :cursor_invisible, ""))
        while true
            # Re-check clipboard availability (may have changed via DA1)
            new_has_clip = clipboard_available(props)
            if new_has_clip != has_clip
                has_clip = new_has_clip
                clipsave = has_clip
            end
            # Only render when not mid-escape-sequence
            if esc_state === :none
                if has_clip
                    fclip = clipsave ? [:emphasis, :bold] : [:grey]
                    ffile = clipsave ? [:grey] : [:emphasis, :bold]
                    print(out, S"\e[1G\e[2K{bold,grey:history>} {bold,emphasis:save to} {$fclip,inverse: Clipboard } {$ffile,inverse: File }   {shadow:Tab to toggle ⋅ ⏎ to select}")
                else
                    # DA1 hasn't arrived yet, show file-only with detection message
                    print(out, S"\e[1G\e[2K{bold,grey:history>} {bold,emphasis:save to} {emphasis,bold,inverse: File }   {shadow:Detecting clipboard…}")
                end
            end
            ichar = read(inp, Char)
            if esc_state === :none
                if ichar == '\e'
                    esc_state = :esc
                elseif ichar ∈ ('\x03', '\x18', '\a')
                    return nothing
                elseif ichar == '\r'
                    if !has_clip
                        clipsave = false
                    end
                    break
                elseif has_clip
                    clipsave = !clipsave
                end
            elseif esc_state === :esc
                if ichar == '\e'
                    return nothing  # double-escape = abort
                elseif ichar == '['
                    esc_state = :csi
                elseif ichar == ']'
                    REPL.LineEdit.receive_osc!(props, inp)
                    esc_state = :none
                else
                    # Unknown escape sequence byte; treat as regular key
                    esc_state = :none
                    if ichar ∈ ('\x03', '\x18', '\a')
                        return nothing
                    elseif ichar == '\r'
                        if !has_clip
                            clipsave = false
                        end
                        break
                    elseif has_clip
                        clipsave = !clipsave
                    end
                end
            elseif esc_state === :csi
                if ichar == '?'
                    # DA1 response (\e[?...c): blocking read until 'c'
                    REPL.LineEdit.receive_da1!(props, inp)
                    esc_state = :none
                elseif UInt8(ichar) >= 0x40 && UInt8(ichar) <= 0x7e
                    # CSI final byte, sequence complete
                    esc_state = :none
                end
                # else: CSI parameter/intermediate byte, stay in :csi
            end
        end
    finally
        # NOTE: While it may look like `:cursor_visible` would be the
        # appropriate choice to reverse `:cursor_invisible`, unfortunately
        # tmux-256color declares a sequence that doesn't actually make
        # the cursor become visible again.
        print(out, get(Base.current_terminfo(), :cursor_normal, ""))
        print(out, "\e[1G\e[2K")
    end
    if clipsave; :copy else :filesave end
end
