#include "repl.h"

#ifdef __cplusplus
extern "C" {
#endif

static char *stdin_buf = NULL;
static size_t stdin_buf_len = 0;
static size_t stdin_buf_maxlen = 128;
static char *given_prompt=NULL, *prompt_to_use=NULL;
static int callback_en=0;

#ifndef _WIN32
void sigcont_handler(int arg)
{
    if (callback_en) {
        jl_write(jl_uv_stdout, prompt_to_use, strlen(prompt_to_use));
        //jl_write(jl_uv_stdout, stdin_buf, stdin_buf_len); //disabled because the current line was still in the system buffer
    }
}
#endif

void jl_init_repl(int history)
{
    stdin_buf = (char *) malloc(stdin_buf_maxlen);
    stdin_buf_len = 0;
#ifndef _WIN32
    signal(SIGCONT, sigcont_handler);
#endif
}

void jl_prep_terminal(int meta_flag)
{
}

/* Restore the terminal's normal settings and modes. */
void jl_deprep_terminal()
{
    callback_en = 0;
}

void jl_input_line_callback(char *input)
{
    if (input) {
        jl_value_t *ast = jl_parse_input_line(input);
        int line_done = !ast || !jl_is_expr(ast) ||
            (((jl_expr_t*)ast)->head != jl_incomplete_sym);
        if (line_done) {
            jl_deprep_terminal();
            int doprint = !ends_with_semicolon(input);
            stdin_buf[0] = 0; //also sets input[0] == 0
            stdin_buf_len = 0;
            handle_input(ast, 0, doprint);
        }
    }
}

void repl_callback_enable(char *prompt)
{
    callback_en = 1;
    if (given_prompt == NULL || strcmp(prompt, given_prompt)) {
        if (given_prompt) free(given_prompt);
        given_prompt = strdup(prompt);
        if (prompt_to_use) free(prompt_to_use);
        // remove \001 and \002
        size_t n = strlen(prompt);
        prompt_to_use = (char *) malloc(n+1);
        size_t o=0;
        for(size_t i=0; i < n; i++) {
            if (prompt[i] > 2)
                prompt_to_use[o++] = prompt[i];
        }
        prompt_to_use[o] = '\0';
    }
    jl_write(jl_uv_stdout, prompt_to_use, strlen(prompt_to_use));
    jl_prep_terminal(1);
}

static void stdin_buf_pushc(char c)
{
    if (stdin_buf_len >= stdin_buf_maxlen) {
        stdin_buf_maxlen *= 2;
        stdin_buf = (char *) realloc(stdin_buf, stdin_buf_maxlen);
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

void jl_read_buffer(unsigned char* base, ssize_t nread)
{
    unsigned char *start = base;
    int esc = 0;
    int newline = 0;
    while (*start != 0 && nread > 0) {
        if (*start < 32 || *start == 127) {
            switch (*start) {
            case '\n':
            case '\r':
                //jl_putc('\n', jl_uv_stdout);
                stdin_buf_pushc('\n');
                newline = 1;
                break;
            case '\x03':
                raise(SIGINT);
                break;
            case '\x04':
                raise(SIGTERM);
                break;
            case '\x1A':
#ifndef _WIN32
                raise(SIGTSTP);
#endif
                break;
            case '\x1B': // MSVC does not understand \e
                esc = 1;
                break;
            case 127:
            case '\b':
                if (stdin_buf_len > 0 && stdin_buf[stdin_buf_len-1] != '\n') {
                    stdin_buf_len--;
                    JL_WRITE(jl_uv_stdout,"\b \b",3);
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
            stdin_buf_pushc(*start);
        }
        start++;
        nread--;
    }
    if (newline) basic_stdin_callback();
}

void jl_reset_input(void)
{
    stdin_buf_len = 0;
    stdin_buf[0] = 0;
    JL_WRITE(jl_uv_stdout,"\n",1);
}

#ifdef __cplusplus
}
#endif
