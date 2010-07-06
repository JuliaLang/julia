/*
  repl.c
  system startup, main(), and console interaction
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <libgen.h>
#include <getopt.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#else
#include <ctype.h>
#endif
#include "llt.h"
#include "julia.h"

static char jl_banner_plain[] =
    "               _      \n"
    "   _       _ _(_)_     |\n"
    "  (_)     | (_) (_)    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\n\n";

static char jl_banner_color[] =
    "\033[1m               \033[32m_\033[37m      \n"
    "   \033[34m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |\n"
    "  \033[34m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  pre-release version\n"
    "   _ _   _| |_  __ _   |\n"
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2010 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\033[0m\n\n";

static char jl_prompt_plain[] = "julia> ";
static char jl_prompt_color[] = "\001\033[1m\033[32m\002julia> \001\033[37m\002";
static char jl_answer_color[] = "\033[0m\033[37m";
static char jl_input_color[]  = "\033[1m\033[37m";
static char jl_color_normal[] = "\033[0m\033[37m";

char *julia_home = NULL; // load is relative to here
static char *history_file = NULL;
static int print_banner = 1;
static int no_readline = 0;
static int tab_width = 2;
static int load_start_j = 1;
static int lisp_prompt = 0;
static char *program = NULL;

int num_evals = 0;
char **eval_exprs = NULL;
int *print_exprs = NULL;

static const char *usage = "julia [options] [program] [args...]\n";
static const char *opts =
    " -q --quiet         Quiet startup without banner\n"
    " -R --no-readline   Disable readline functionality\n"
    " -e --eval=<expr>   Evaluate <expr>, don't print\n"
    " -E --print=<expr>  Evaluate and print <expr>\n"
    " -H --home=<dir>    Load files relative to <dir>\n"
    " -T --tab=<size>    Set REPL tab width to <size>\n"
    " -b --bare          Bare REPL: don't load start.j\n"
    " -L --lisp          Start with Lisp prompt not Julia\n"
    " -h --help          Print this message\n";

void parse_opts(int *argcp, char ***argvp) {
    static struct option longopts[] = {
        { "quiet",       no_argument,       0, 'q' },
        { "no-readline", no_argument,       0, 'R' },
        { "eval",        required_argument, 0, 'e' },
        { "print",       required_argument, 0, 'E' },
        { "home",        required_argument, 0, 'H' },
        { "tab",         required_argument, 0, 'T' },
        { "bare",        no_argument,       0, 'b' },
        { "lisp",        no_argument,       0, 'L' },
        { "help",        no_argument,       0, 'h' },
        { 0, 0, 0, 0 }
    };
    int c;
    while ((c = getopt_long(*argcp,*argvp,"qRe:E:H:T:bLh",longopts,0)) != -1) {
        switch (c) {
        case 'q':
            print_banner = 0;
            break;
        case 'R':
            no_readline = 1;
            break;
        case 'e':
        case 'E':
            no_readline = 1;
            num_evals++;
            eval_exprs = (char**)realloc(eval_exprs, num_evals*sizeof(char*));
            print_exprs = (int*)realloc(print_exprs, num_evals*sizeof(int));
            eval_exprs[num_evals-1] = optarg;
            print_exprs[num_evals-1] = (c == 'E');
            break;
        case 'H':
            julia_home = strdup(optarg);
            break;
        case 'T':
            // TODO: more robust error checking.
            tab_width = atoi(optarg);
            break;
        case 'b':
            load_start_j = 0;
            break;
        case 'L':
            lisp_prompt = 1;
            break;
        case 'h':
            printf("%s%s", usage, opts);
            exit(0);
        case '?':
            ios_printf(ios_stderr, "options:\n%s", opts);
            exit(1);
        default:
            ios_printf(ios_stderr, "julia: unhandled option -- %c\n",  c);
            ios_printf(ios_stderr, "This is a bug, please report it.\n");
            exit(1);
        }
    }
    if (!julia_home) {
        julia_home = getenv("JULIA_HOME");
        if (julia_home) {
            julia_home = strdup(julia_home);
        } else {
            char *julia_path = (char*)malloc(PATH_MAX);
            get_exename(julia_path, PATH_MAX);
            julia_home = strdup(dirname(julia_path));
            free(julia_path);
        }
    }
    char *pwd = getenv("PWD");
    if (julia_home && pwd) {
        int i, prefix = 1;
        for (i=0; pwd[i]; i++) {
            if (pwd[i] != julia_home[i]) {
                prefix = 0;
                break;
            }
        }
        if (prefix && (julia_home[i] == '/' || julia_home[i] == '\0')) {
            while (julia_home[i] == '/') i++;
            if (julia_home[i]) {
                char *p = strdup(julia_home + i);
                free(julia_home);
                julia_home = p;
            } else {
                julia_home = NULL;
            }
        }
    }
    *argvp += optind;
    *argcp -= optind;
    if (!num_evals && *argcp > 0) {
        if (strcmp((*argvp)[0], "-")) {
            no_readline = 1;
            program = (*argvp)[0];
        }
        ++*argvp; --*argcp;
    }
}

void fpe_handler(int arg)
{
    (void)arg;
    jl_error("error: integer divide by zero");
}

void julia_init()
{
#ifdef BOEHM_GC
    GC_expand_hp(12000000);  //shaves a tiny bit off startup time
#endif
    jl_init_frontend();
    jl_init_types();
    jl_init_builtin_types();
    jl_init_modules();
    jl_init_builtins();
    jl_init_codegen();
    signal(SIGFPE, fpe_handler);
}

static int detect_color()
{
#ifdef WIN32
    return 0;
#else
    int tput = system("tput setaf 0 >&/dev/null");
    if (tput == 0) return 1;
    if (tput == 1) return 0;
    char *term = getenv("TERM");
    if (term == NULL) return 0;
    return (!strcmp(term,"xterm") || !strcmp(term,"xterm-color"));
#endif
}

jl_value_t *jl_new_closure_internal(jl_lambda_info_t *li, jl_value_t *env);

// heuristic for whether a top-level input should be evaluated with
// the compiler or the interpreter.
static int eval_with_compiler_p(jl_array_t *body)
{
    size_t i;
    for(i=0; i < body->length; i++) {
        jl_value_t *stmt = jl_cellref(body,i);
        if (jl_is_expr(stmt) && (((jl_expr_t*)stmt)->head == goto_sym ||
                                 ((jl_expr_t*)stmt)->head == goto_ifnot_sym)) {
            return 1;
        }
    }
    return 0;
}

jl_value_t *jl_toplevel_eval_thunk(jl_lambda_info_t *thk)
{
    //jl_print(thk);
    //ios_printf(ios_stdout, "\n");
    assert(jl_typeof(thk) == (jl_type_t*)jl_lambda_info_type);
    assert(jl_is_expr(thk->ast));
    if (eval_with_compiler_p(jl_lam_body((jl_expr_t*)thk->ast))) {
        jl_value_t *thunk = jl_new_closure_internal(thk, (jl_value_t*)jl_null);
        return jl_apply((jl_function_t*)thunk, NULL, 0);
    }
    return jl_interpret_toplevel_thunk(thk);
}

static int have_color;
static int prompt_length;

jmp_buf ExceptionHandler;
jmp_buf *CurrentExceptionHandler = &ExceptionHandler;

static int ends_with_semicolon(const char *input)
{
    char *p = strrchr(input, ';');
    if (p++) {
        while (isspace(*p)) p++;
        if (*p == '\0' || *p == '#')
            return 1;
    }
    return 0;
}

#ifdef USE_READLINE

static jl_value_t *rl_ast;
static int history_offset = -1;

// yes, readline uses inconsistent indexing internally.
#define history_rem(n) remove_history(n-history_base)

static void init_history() {
    using_history();
    char *home = getenv("HOME");
    if (!home) return;
    asprintf(&history_file, "%s/.julia_history", home);
    struct stat stat_info;
    if (!stat(history_file, &stat_info)) {
        read_history(history_file);
        for (;;) {
            HIST_ENTRY *entry = history_get(history_base);
            if (entry && isspace(entry->line[0]))
                free_history_entry(history_rem(history_base));
            else break;
        }
        int i, j, k;
        for (i=1 ;; i++) {
            HIST_ENTRY *first = history_get(i);
            if (!first) break;
            int length = strlen(first->line)+1;
            for (j = i+1 ;; j++) {
                HIST_ENTRY *child = history_get(j);
                if (!child || !isspace(child->line[0])) break;
                length += strlen(child->line)+1;
            }
            if (j == i+1) continue;
            first->line = (char*)realloc(first->line, length);
            char *p = strchr(first->line, '\0');
            for (k = i+1; k < j; k++) {
                *p = '\n';
                p = stpcpy(p+1, history_get(i+1)->line);
                free_history_entry(history_rem(i+1));
            }
        }
    } else if (errno == ENOENT) {
        write_history(history_file);
    } else {
        jl_errorf("history file error: %s", strerror(errno));
    }
}

static int line_start(int point) {
    if (!point) return 0;
    int i = point-1;
    for (; i && rl_line_buffer[i] != '\n'; i--) ;
    return i ? i+1 : 0;
}

static int line_end(int point) {
    char *nl = strchr(rl_line_buffer + point, '\n');
    if (!nl) return rl_end;
    return nl - rl_line_buffer;
}

static int newline_callback(int count, int key) {
    rl_insert_text("\n");
    int i;
    for (i = 0; i < prompt_length; i++)
        rl_insert_text(" ");
    return 0;
}

static int return_callback(int count, int key) {
    if (have_color) {
        ios_printf(ios_stdout, jl_answer_color);
        ios_flush(ios_stdout);
    }
    rl_ast = jl_parse_input_line(rl_line_buffer);
    rl_done = !rl_ast || !jl_is_expr(rl_ast) ||
        (((jl_expr_t*)rl_ast)->head != continue_sym);
    if (!rl_done && have_color) {
        ios_printf(ios_stdout, jl_input_color);
        ios_flush(ios_stdout);
    }
    if (!rl_done) {
        newline_callback(count, key);
    } else {
        rl_point = rl_end;
        rl_redisplay();
    }
    return 0;
}

static int space_callback(int count, int key) {
    if (rl_point > 0)
        rl_insert_text(" ");
    return 0;
}

static int tab_callback(int count, int key) {
    if (rl_point > 0) {
        int i;
        for (i=0; i < tab_width; i++)
            rl_insert_text(" ");
    }
    return 0;
}

static int line_start_callback(int count, int key) {
    int start = line_start(rl_point);
    int flush_left = rl_point == 0 || rl_point == start + prompt_length;
    rl_point = flush_left ? 0 : (!start ? start : start + prompt_length);
    return 0;
}

static int line_end_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    rl_point = flush_right ? rl_end : end;
    return 0;
}

static int line_kill_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    int kill = flush_right ? end + prompt_length + 1 : end;
    if (kill > rl_end) kill = rl_end;
    rl_kill_text(rl_point, kill);
    return 0;
}

static int left_callback(int count, int key) {
    if (rl_point > 0) {
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
    }
    return 0;
}

static int right_callback(int count, int key) {
    rl_point += (rl_line_buffer[rl_point] == '\n') ? prompt_length+1 : 1;
    if (rl_end < rl_point) rl_point = rl_end;
    return 0;
}

static int up_callback(int count, int key) {
    int i = line_start(rl_point);
    if (i > 0) {
        int j = line_start(i-1);
        if (j == 0) rl_point -= prompt_length;
        rl_point += j - i;
        if (rl_point >= i) rl_point = i - 1;
    } else {
        if (history_offset >= 0) {
            history_set_pos(history_offset+1);
            history_offset = -1;
        }
        return rl_get_previous_history(count, key);
    }
    return 0;
}

static int down_callback(int count, int key) {
    int j = line_end(rl_point);
    if (j < rl_end) {
        int i = line_start(rl_point);
        if (i == 0) rl_point += prompt_length;
        rl_point += j - i + 1;
        int k = line_end(j+1);
        if (rl_point > k) rl_point = k;
    } else {
        if (history_offset >= 0) {
            history_set_pos(history_offset);
            history_offset = -1;
        }
        return rl_get_next_history(count, key);
    }
    return 0;
}

/*
static int backspace_callback(int count, int key) {
    return 0;
    if (rl_point > 0) {
        int i = line_start(rl_point);
        int j = (i == 0 || rl_point-i > prompt_length) ? rl_point-1 : i-1;
        rl_delete_text(j, i);
    }
    return 0;
}
*/

static jl_value_t *read_expr_ast_readline(char *prompt, int *end, int *doprint)
{
    rl_ast = NULL;
    char *input = readline(prompt);
    if (!input || ios_eof(ios_stdin)) {
        *end = 1;
        return NULL;
    }
    if (rl_ast == NULL) return NULL;

    *doprint = !ends_with_semicolon(input);
    if (input && *input) {
        HIST_ENTRY *entry = current_history();
        if (!entry || strcmp(input, entry->line)) {
            add_history(input);
            history_offset = -1;
            if (history_file)
                append_history(1, history_file);
        } else {
            history_offset = where_history();
        }
    }

    ios_printf(ios_stdout, "\n");

    free(input);
    return rl_ast;
}

#endif

static char *ios_readline(ios_t *s)
{
    ios_t dest;
    ios_mem(&dest, 0);
    ios_copyuntil(&dest, s, '\n');
    size_t n;
    return ios_takebuf(&dest, &n);
}

static jl_value_t *read_expr_ast_no_readline(char *prompt, int *end, int *doprint)
{
    char *input;
    ios_printf(ios_stdout, prompt);
    ios_flush(ios_stdout);
    input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);

    if (!input || ios_eof(ios_stdin)) {
        *end = 1;
        return NULL;
    }

    jl_value_t *ast = jl_parse_input_line(input);
    if (ast == NULL) return NULL;
    if (jl_is_expr(ast) && ((jl_expr_t*)ast)->head == continue_sym)
        return read_expr_ast_no_readline(prompt, end, doprint);

    *doprint = !ends_with_semicolon(input);
    return ast;
}

static jl_value_t *read_expr_ast(char *prompt, int *end, int *doprint) {
#ifdef USE_READLINE
    return no_readline ?
        read_expr_ast_no_readline(prompt, end, doprint) :
        read_expr_ast_readline(prompt, end, doprint);
#else
    return read_expr_ast_no_readline(prompt, end, doprint);
#endif
}

void jl_lisp_prompt();

jl_function_t *jl_typeinf_func=NULL;

int jl_load_startup_file()
{
    if (!setjmp(ExceptionHandler)) {
        jl_load("start.j");
        if (jl_boundp(jl_system_module, jl_symbol("typeinf"))) {
            jl_typeinf_func =
                (jl_function_t*)*(jl_get_bindingp(jl_system_module,
                                                  jl_symbol("typeinf")));
        }
    } else {
        ios_printf(ios_stderr, "error during startup.\n");
        return 1;
    }
    return 0;
}

int main(int argc, char *argv[])
{
    llt_init();
    parse_opts(&argc, &argv);
    julia_init();

    jl_value_t *dims = (jl_value_t*)jl_box_int32(argc);
    jl_value_t *args = (jl_value_t*)jl_new_array(jl_array_any_type, &dims, 1);
    int i;
    for (i=0; i < argc; i++) {
        jl_value_t *idx = (jl_value_t*)jl_box_int32(i+1);
        jl_value_t *val = (jl_value_t*)jl_cstr_to_array(argv[i]);
        jl_value_t *dat[] = {args, idx, val};
        jl_f_arrayset(NULL, dat, 3);
    }
    jl_set_const(jl_system_module, jl_symbol("ARGS"), args);

#ifdef USE_READLINE
    if (!no_readline) {
        init_history();
        rl_bind_key(' ', space_callback);
        rl_bind_key('\t', tab_callback);
        rl_bind_key('\r', return_callback);
        rl_bind_key('\n', return_callback);
        rl_bind_key('\v', line_kill_callback);
        rl_bind_key('\001', line_start_callback);
        rl_bind_key('\005', line_end_callback);
        rl_bind_key('\002', left_callback);
        rl_bind_key('\006', right_callback);
        rl_bind_keyseq("\033[A", up_callback);
        rl_bind_keyseq("\033[B", down_callback);
        rl_bind_keyseq("\033[D", left_callback);
        rl_bind_keyseq("\033[C", right_callback);
    }
#endif

    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }
    if (load_start_j) {
        if (jl_load_startup_file())
            return 1;
    }
    if (num_evals) {
        int i;
        for (i=0; i < num_evals; i++) {
            jl_value_t *ast = jl_parse_input_line(eval_exprs[i]);
            jl_value_t *value = jl_toplevel_eval_thunk((jl_lambda_info_t*)ast);
            if (print_exprs[i]) {
                jl_print(value);
                ios_printf(ios_stdout, "\n");
            }
        }
        jl_shutdown_frontend();
        return 0;
    }
    if (program) {
        if (!setjmp(ExceptionHandler)) {
            jl_load(program);
        } else {
            return 1;
        }
        return 0;
    }

    have_color = detect_color();
    char *banner = have_color ? jl_banner_color : jl_banner_plain;
    char *prompt = have_color ? jl_prompt_color : jl_prompt_plain;
    prompt_length = strlen(jl_prompt_plain);

    if (print_banner)
        ios_printf(ios_stdout, "%s", banner);

    while (1) {
        ios_flush(ios_stdout);

        if (!setjmp(ExceptionHandler)) {
            int end = 0;
            int print_value = 1;
            jl_value_t *ast = read_expr_ast(prompt, &end, &print_value);
            if (end) {
                ios_printf(ios_stdout, "\n");
                break;
            }
            if (have_color) {
                ios_printf(ios_stdout, jl_answer_color);
                ios_flush(ios_stdout);
            }
            if (ast != NULL) {
                jl_value_t *value =
                    jl_toplevel_eval_thunk((jl_lambda_info_t*)ast);
                if (print_value) {
                    jl_print(value);
                    ios_printf(ios_stdout, "\n");
                }
            }
        }

        ios_printf(ios_stdout, "\n");
    }
    if (have_color) {
        ios_printf(ios_stdout, jl_color_normal);
        ios_flush(ios_stdout);
    }

    return 0;
}
