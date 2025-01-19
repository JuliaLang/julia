module JuliaLoweringREPL

import ReplMaker
import REPL

using JuliaLowering: JuliaLowering, SyntaxTree, children
using JuliaSyntax

function is_incomplete(prompt_state)
    str = String(take!(copy(REPL.LineEdit.buffer(prompt_state))))
    stream = JuliaSyntax.ParseStream(str)
    JuliaSyntax.parse!(stream, rule=:all)
    if JuliaSyntax.any_error(stream)
        tree = JuliaSyntax.build_tree(SyntaxNode, stream)
        tag = JuliaSyntax._incomplete_tag(tree, 1)
        return tag != :none
    else
        return false
    end
end

function eval_ish(mod, ex, do_eval)
    k = kind(ex)
    if k == K"toplevel"
        x = nothing
        for e in children(ex)
            x = eval_ish(mod, e, do_eval)
        end
        return x
    end
    linear_ir = JuliaLowering.lower(mod, ex)
    JuliaLowering.print_ir(stdout, linear_ir)
    if do_eval
        println(stdout, "#----------------------")
        expr_form = JuliaLowering.to_lowered_expr(mod, linear_ir)
        Base.eval(mod, expr_form)
    end
end

DO_EVAL::Bool = false
function opts(; do_eval=false)
    global DO_EVAL = do_eval
end

function handle_input(str)
    global DO_EVAL
    if str == "DO_EVAL"
        DO_EVAL = true
        return
    elseif str == "!DO_EVAL"
        DO_EVAL = false
        return
    end
    ex = parseall(SyntaxTree, str; filename="REPL")
    eval_ish(Main, ex, DO_EVAL)
end

function init()
    ReplMaker.initrepl(handle_input,
                       valid_input_checker = !is_incomplete,
                       prompt_text="Lowering> ",
                       prompt_color = :blue,
                       start_key=")",
                       mode_name=:JuliaLowering)
end

function __init__()
    init()
end

end
