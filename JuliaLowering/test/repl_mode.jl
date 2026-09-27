# JuliaLowering REPL mode: an interactive test utility for lowering code (not
# part of the unit tests)

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

function eval_ish(mod::Module, ex::SyntaxTree, do_eval::Bool, do_print_ir::Bool)
    k = kind(ex)
    if k == K"toplevel"
        x = nothing
        for e in children(ex)
            x = eval_ish(mod, e, do_eval, do_print_ir)
        end
        return x
    end
    linear_ir = JuliaLowering.lower(mod, ex)
    if do_print_ir
        JuliaLowering.print_ir(stdout, linear_ir)
    end
    if do_eval
        println(stdout, "#----------------------")
        expr_form = JuliaLowering.to_lowered_expr(linear_ir)
        Base.eval(mod, expr_form)
    end
end

PRINT_IR::Bool = true
DO_EVAL::Bool = false
function opts(; do_eval=false, print_ir=false)
    global DO_EVAL = do_eval
    global PRINT_IR = print_ir
end

function handle_input(str)
    global DO_EVAL, PRINT_IR
    if str == "DO_EVAL"
        DO_EVAL = true
        return
    elseif str == "!DO_EVAL"
        DO_EVAL = false
        return
    elseif str == "PRINT_IR"
        PRINT_IR = true
        return
    elseif str == "!PRINT_IR"
        PRINT_IR = false
        return
    end
    ex = parseall(SyntaxTree, str; filename="REPL")
    eval_ish(Main, ex, DO_EVAL, PRINT_IR)
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
