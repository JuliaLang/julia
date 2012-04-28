#include "repl.h"
#include <unistd.h>

char jl_prompt_color[] = "\033[1m\033[32mjulia> \033[0m\033[1m";
char *prompt_string = "julia> ";

char *stdin_buf = NULL;
unsigned long stdin_buf_len = 0;
unsigned long stdin_buf_maxlen = 128;


void init_repl_environment(void)
{
	stdin_buf = LLT_ALLOC(stdin_buf_maxlen);
	int pipefd[2];
    if (pipe(pipefd) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }}

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
		LLT_FREE(input);
        // TODO
        //if (jl_is_expr(ast) && ((jl_expr_t*)ast)->head == jl_continue_sym)
        doprint = !ends_with_semicolon(input);
    }
    handle_input(ast, end, doprint);
}

char *read_expr(char *prompt)
{
	perror("read_expr not supported");
	exit(EXIT_FAILURE);
}

void repl_callback_enable()
{
    ios_printf(ios_stdout, prompt_string);
    ios_flush(ios_stdout);
}

void basic_stdin_callback(void)
{
	char *start = stdin_buf;
	int linelen = 0;
	if (stdin_buf_len == 0) return;
	while (*start != '\n') {
		if (*start == 0) return;
		linelen++;
		start++;
		if (linelen > stdin_buf_len) return;
	}
	char *input = LLT_ALLOC(linelen);
	memcpy(input, stdin_buf, linelen);
	input[linelen] = '\0';
	stdin_buf_len = 0;
	stdin_buf[0] = 0;
    jl_input_line_callback(input);
}

void stdin_buf_pushc(char c) {
	if (stdin_buf_len >= stdin_buf_maxlen) {
		stdin_buf_maxlen *= 2;
		stdin_buf = LLT_REALLOC(stdin_buf, stdin_buf_maxlen);
		if (!stdin_buf) {
			perror("realloc");
        	exit(EXIT_FAILURE);
		}
	}
	stdin_buf[stdin_buf_len] = c;
	stdin_buf_len++;
}

void jl_readBuffer(uv_stream_t* stream, ssize_t nread, char *base, int buflen)
{
	char *start = base;
	int esc = 0;
    while(*start != 0 && nread > 0) {
		if (*start < 32 || *start == 127) {
			switch (*start) {
			case '\n':
			case '\r':
				ios_putc('\n', ios_stdout);
				ios_flush(ios_stdout);
				stdin_buf_pushc('\n');
				basic_stdin_callback();
				break;
			case '\x03':
				raise(SIGINT);
				break;
			case '\x04':
				raise(SIGTERM);
				break;
			case '\x1A':
				raise(SIGTSTP);
				break;
			case '\e':
				esc = 1;
				break;
			case 127:
			case '\b':
				if (stdin_buf_len > 0) {
					stdin_buf_len--;
					ios_write(ios_stdout,"\e[D \e[D",7);
				}
			}
		} else if (esc == 1) {
			if (*start == '[') {
				esc = 2;
			} else {
				esc = 0;
			}
		} else if (esc == 2) {
			//for now, we just block all ctrl signals
			esc = 0;
		} else {
			ios_putc(*start, ios_stdout);
			stdin_buf_pushc(*start);
    	}
        start++;
        nread--;
	}
	ios_flush(ios_stdout);
}

void rl_clear_input(void)
{
	stdin_buf_len = 0;
	stdin_buf[0] = 0;
	ios_printf(ios_stdout, "\n");
	repl_callback_enable();
}

