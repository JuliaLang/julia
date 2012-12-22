#include "repl.h"

char jl_prompt_color[] = "\033[1m\033[32mjulia> \033[0m\033[1m";
char *prompt_string = "julia> ";

void init_repl_environment(int argc, char *argv[])
{
}

DLLEXPORT void jl_enable_color(void)
{
    prompt_string = jl_prompt_color;
}

void jl_input_line_callback(char *input)
{
    jl_value_t *ast;
    int end=0, doprint=1;

    if (!input || ios_eof(ios_stdin)) {
        end = 1;
        ast = jl_nothing;
    }
    else {
        ast = jl_parse_input_line(input);
        // TODO
        //if (jl_is_expr(ast) && ((jl_expr_t*)ast)->head == jl_continue_sym)
        doprint = !ends_with_semicolon(input);
    }
    handle_input(ast, end, doprint);
}

char *read_expr(char *prompt)
{
    char *input;
    ios_printf(ios_stdout, prompt);
    ios_flush(ios_stdout);
    input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);
    return input;
}

void repl_callback_enable()
{
    ios_printf(ios_stdout, prompt_string);
    ios_flush(ios_stdout);
}

void jl_stdin_callback(void)
{
    char *input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);
    jl_input_line_callback(input);
}
