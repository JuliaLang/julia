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
            "^K" => Event(events, :up),
            # Down Arrow
            "\e[B" => Event(events, :down),
            "^N" => Event(events, :down),
            "^J" => Event(events, :down),
            # Tab
            '\t' => Event(events, :tab),
            # Page up
            "\e[5~" => Event(events, :pageup),
            "^B" => Event(events, :pageup),
            # Page down
            "\e[6~" => Event(events, :pagedown),
            "^F" => Event(events, :pagedown),
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
            "^Y" => Returns(:clipboard),
        ),
        REPL.LineEdit.default_keymap,
        REPL.LineEdit.escape_defaults])
end

"""
    create_prompt(events::Channel{Symbol})

Initialize a custom REPL prompt tied to `events`.

Returns a tuple `(term, prompt, istate, pstate)` ready for
input handling and display.
"""
function create_prompt(events::Channel{Symbol})
    term = REPL.Terminals.TTYTerminal(
        get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"),
        stdin, stdout, stderr)
    prompt = REPL.LineEdit.Prompt(
        PROMPT_TEXT, # prompt
        "\e[90m", "\e[0m", # prompt_prefix, prompt_suffix
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
            elseif status ∈ (:save, :clipboard)
                print("\e[1G\e[J")
                REPL.LineEdit.raw!(term, false) &&
                    REPL.LineEdit.disable_bracketed_paste(term)
                push!(events, status)
                break
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
