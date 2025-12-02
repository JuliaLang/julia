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
    create_prompt(events::Channel{Symbol}, term)

Initialize a custom REPL prompt tied to `events` using the existing `term`.

Returns a tuple `(term, prompt, istate, pstate)` ready for
input handling and display.
"""
function create_prompt(events::Channel{Symbol}, term, prefix::String = "\e[90m")
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
    pstate = istate.mode_state[prompt]
    (; term, prompt, istate, pstate)
end

"""
    runprompt!((; term,prompt,pstate,istate), events::Channel{Symbol})

Drive the prompt input loop until confirm, save, or abort.

Emits `:edit`, `:confirm`, `:save`, or `:abort` into `events`,
manages raw mode and bracketed paste, and cleans up on exit.
"""
function runprompt!((; term, prompt, pstate, istate), events::Channel{Symbol})
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
                dest = savedest(term)
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

function savedest(term::Base.Terminals.TTYTerminal)
    out = term.out_stream
    print(out, "\e[1G\e[J")
    clipsave = true
    try
        print(out, get(Base.current_terminfo(), :cursor_invisible, ""))
        fclip, ffile = [:emphasis, :bold], [:grey]
        char = '\0'
        while true
            print(out, S"\e[1G\e[2K{bold,grey:history>} {bold,emphasis:save to} {$fclip,inverse: Clipboard } {$ffile,inverse: File }   {shadow:Tab to toggle ⋅ ⏎ to select}")
            ichar = read(term.in_stream, Char)
            if ichar ∈ ('\x03', '\x18', '\a') || char == ichar == '\e'
                return
            elseif ichar == '\r'
                break
            end
            char = ichar
            fclip, ffile = ffile, fclip
            clipsave = !clipsave
        end
    finally
        # NOTE: While it may look like `:cursor_visible` would be the
        # appropriate choice to reverse `:cursor_invisible`, unfortunately
        # tmux-256color declares a sequence that doesn't actually make
        # the cursor become visible again 😑.
        print(out, get(Base.current_terminfo(), :cursor_normal, ""))
        print(out, "\e[1G\e[2K")
    end
    if clipsave; :copy else :filesave end
end
