#include "repl.h"

char jl_prompt_color[] = "\033[1m\033[32mjulia> \033[37m";

void init_repl_environment()
{
}

void exit_repl_environment()
{
}

DLLEXPORT void jl_input_line_callback(char *input)
{
    jl_value_t *ast;
    int end=0, doprint=1;

    if (!input || ios_eof(ios_stdin)) {
        end = 1;
        ast = NULL;
    }
    else {
        ast = jl_parse_input_line(input);
        // TODO
        //if (jl_is_expr(ast) && ((jl_expr_t*)ast)->head == jl_continue_sym)
        doprint = !ends_with_semicolon(input);
    }
    handle_input(ast, end, doprint);
}

void read_expr(char *prompt)
{
    char *input;
    //ios_printf(ios_stdout, prompt);
    //ios_flush(ios_stdout);
    input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);
    jl_input_line_callback(input);
}

void repl_callback_enable()
{
    ios_printf(ios_stdout, prompt_string);
    ios_flush(ios_stdout);
}

void repl_callback_disable()
{
}

void repl_stdin_callback()
{
    char *input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);
    jl_input_line_callback(input);
}

void repl_print_prompt()
{
    ios_printf(ios_stdout, prompt_string);
}
