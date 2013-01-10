#include "repl.h"
#include <unistd.h>

char jl_prompt_color[] = "\033[1m\033[32mjulia> \033[0m\033[1m";
char *prompt_string = "julia> ";

char *stdin_buf = NULL;
unsigned long stdin_buf_len = 0;
unsigned long stdin_buf_maxlen = 128;

void repl_print(const char *buf, size_t n)
{
    if(jl_uv_stdin->type == UV_TTY) {
       jl_write(jl_uv_stdin, buf, n);
    }
}

void init_repl_environment(int argc, char *argv[])
{
    stdin_buf = malloc(stdin_buf_maxlen);
}

DLLEXPORT void jl_enable_color(void)
{
    prompt_string = jl_prompt_color;
}

void jl_prep_terminal(int meta_flag)
{   //order must be 2,1,0
    uv_tty_set_mode((uv_tty_t*)JL_STDERR,1);
    uv_tty_set_mode((uv_tty_t*)JL_STDOUT,1);
    uv_tty_set_mode((uv_tty_t*)JL_STDIN,1);
}

/* Restore the terminal's normal settings and modes. */
void jl_deprep_terminal()
{   //order must be 0,1,2
    uv_tty_set_mode((uv_tty_t*)JL_STDIN,0);
    uv_tty_set_mode((uv_tty_t*)JL_STDOUT,0);
    uv_tty_set_mode((uv_tty_t*)JL_STDERR,0);
}

void jl_input_line_callback(char *input)
{
    if (input) {
        jl_value_t *ast = jl_parse_input_line(input);
        int line_done = !ast || !jl_is_expr(ast) ||
            (((jl_expr_t*)ast)->head != jl_continue_sym);
        if (line_done) {
            jl_deprep_terminal();
            int doprint = !ends_with_semicolon(input);
            stdin_buf[0] = 0; //also sets input[0] == 0
            stdin_buf_len = 0;
            handle_input(ast, 0, doprint);
        }
    }
}

void repl_callback_enable()
{
    jl_printf(jl_uv_stdout, prompt_string);
    jl_prep_terminal(1);
}

static void stdin_buf_pushc(char c)
{
    if (stdin_buf_len >= stdin_buf_maxlen) {
        stdin_buf_maxlen *= 2;
        stdin_buf = realloc(stdin_buf, stdin_buf_maxlen);
        if (!stdin_buf) {
            // we can safely ignore this error and continue, if that is preferred
            perror("realloc");
            exit(EXIT_FAILURE);
        }
    }
    stdin_buf[stdin_buf_len] = c;
    stdin_buf_len++;
}

static void basic_stdin_callback(void)
{
    stdin_buf_pushc('\0');
    stdin_buf_len--;
    jl_input_line_callback(stdin_buf);
}

void jl_readBuffer(char* base, ssize_t nread)
{
    char *start = base;
    int esc = 0;
    int newline = 0;
    while (*start != 0 && nread > 0) {
        if (*start < 32 || *start == 127) {
            switch (*start) {
            case '\n':
            case '\r':
                jl_putc('\n', jl_uv_stdout);
                stdin_buf_pushc('\n');
                newline = 1;
                break;
            case '\x03':
                repl_print("^C\n", 3);
                jl_clear_input();
                break;
            case '\x04':
                raise(SIGTERM);
                break;
            case '\x1A':
#ifndef __WIN32__
                raise(SIGTSTP);
#endif
                break;
            case '\e':
                esc = 1;
                break;
            case 127:
            case '\b':
                if (stdin_buf_len > 0 && stdin_buf[stdin_buf_len-1] != '\n') {
                    stdin_buf_len--;
                    repl_print("\b \b",3);
                }
            }
        }
        else if (esc == 1) {
            if (*start == '[') {
                esc = 2;
            }
            else {
                esc = 0;
            }
        }
        else if (esc == 2) {
            // for now, we just block all 3 character ctrl signals that I know about
            // this misses delete ^[[3~ and possibly others?
            esc = 0;
        }
        else {
            if(jl_uv_stdin->type == UV_TTY)
                jl_putc(*start, jl_uv_stdin);
            stdin_buf_pushc(*start);
        }
        start++;
        nread--;
    }
    if (newline) basic_stdin_callback();
}

void jl_clear_input(void)
{
    stdin_buf_len = 0;
    stdin_buf[0] = 0;
    repl_print("\n",1);
    repl_callback_enable();
}
