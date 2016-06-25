module FlispREPL

import Base: LineEdit, REPL

type FlispCompletionProvider <: LineEdit.CompletionProvider
    l::REPL.LineEditREPL
end

function return_callback(p::LineEdit.PromptState)
    # TODO ... something less dumb
    buf = takebuf_string(copy(LineEdit.buffer(p)))
    local sp = 0
    for c in buf
      c == '(' && (sp += 1; continue)
      c == ')' && (sp -= 1; continue)
    end
    sp <= 0
end

fl_eval_string(x::String) = ccall(:jl_flisp_eval, Any, (Ptr{UInt8}, Csize_t), x, sizeof(x))

function evaluate_flisp(s::String)
    local res
    try
        res = fl_eval_string(s)
    catch e
        display("error during flisp evaluation: ", e)
        return nothing
    end
    print(STDOUT, res)
end

function setup_repl(enabled::Bool)
    # bail out if we don't have a repl
    if !isdefined(Base, :active_repl) return end

    repl = Base.active_repl
    main_mode = Base.active_repl.interface.modes[1]

    # disable repl if requested
    if (!enabled)
        delete!(main_mode.keymap_dict, ')')
        return
    end

    panel = LineEdit.Prompt("flisp> ";
                            prompt_prefix = Base.text_colors[:white],
                            prompt_suffix = main_mode.prompt_suffix,
                            on_enter = return_callback)

    hp = main_mode.hist
    hp.mode_mapping[:flisp] = panel
    panel.hist = hp
    panel.on_done = REPL.respond(evaluate_flisp, repl, panel;
                                 pass_empty = false)
    panel.complete = nothing

    const flisp_keymap = Dict{Any,Any}(
        ')' =>
            function (s,args...)
                if isempty(s) || position(LineEdit.buffer(s)) == 0
                    buf = copy(LineEdit.buffer(s))
                    LineEdit.transition(s, panel) do
                        LineEdit.state(s, panel).input_buffer = buf
                    end
                else
                    LineEdit.edit_insert(s, ')')
                end
            end)

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)
    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    panel.keymap_dict = LineEdit.keymap(b)

    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, flisp_keymap)
    nothing
end

end # module

toggle_flisp_repl(; enabled::Bool = true) = FlispREPL.setup_repl(enabled)
