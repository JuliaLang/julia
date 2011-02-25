/*
  repl.c
  system startup, main(), and console interaction
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <assert.h>
#include <time.h>
#if defined(LINUX) || defined(MACOSX)
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
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
#ifdef DEBUG
    "   _ _   _| |_  __ _   |  debug build\n"
#else
    "   _ _   _| |_  __ _   |\n"
#endif
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2011 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\n\n";

static char jl_banner_color[] =
    "\033[1m               \033[32m_\033[37m      \n"
    "   \033[34m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |\n"
    "  \033[34m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  pre-release version\n"
#ifdef DEBUG
    "   _ _   _| |_  __ _   |  debug build\n"
#else
    "   _ _   _| |_  __ _   |\n"
#endif
    "  | | | | | | |/ _` |  |\n"
    "  | | |_| | | | (_| |  |  \302\2512009-2011 contributors\n"
    " _/ |\\__'_|_|_|\\__'_|  |  \n"
    "|__/                   |\033[0m\n\n";

static char jl_prompt_plain[] = "julia> ";
static char jl_prompt_color[] = "\001\033[1m\033[32m\002julia> \001\033[37m\002";
static char jl_prompt_color_no_readline[] = "\033[1m\033[32mjulia> \033[37m";
static char *jl_answer_color  = "\033[1m\033[34m";
static char jl_input_color[]  = "\033[1m\033[37m";
static char jl_color_normal[] = "\033[0m\033[37m";

char *julia_home = NULL; // load is relative to here
static char *history_file = NULL;
static int print_banner = 1;
#ifdef USE_READLINE
static int no_readline = 0;
#else
static int no_readline = 1;
#endif
static int tab_width = 2;
static char *post_boot = NULL;
static int lisp_prompt = 0;
static int load_start_j = 1;
static char *program = NULL;

int num_evals = 0;
char **eval_exprs = NULL;
int *print_exprs = NULL;

static const char *usage = "julia [options] [program] [args...]\n";
static const char *opts =
    " -q --quiet               Quiet startup without banner\n"
    " -R --no-readline         Disable readline functionality\n"
    " -e --eval=<expr>         Evaluate <expr> and don't print\n"
    " -E --print=<expr>        Evaluate and print <expr>\n"
    " -P --post-boot=<expr>    Evaluate <expr> right after boot\n"
    " -H --home=<dir>          Load files relative to <dir>\n"
    " -T --tab=<size>          Set REPL tab width to <size>\n"
    " -L --lisp                Start with Lisp prompt not Julia\n"
    " -b --bare                Bare REPL: don't load start.j\n"
    " -h --help                Print this message\n";

void parse_opts(int *argcp, char ***argvp) {
    static char* shortopts = "qRe:E:P:H:T:bLh";
    static struct option longopts[] = {
        { "quiet",       no_argument,       0, 'q' },
        { "no-readline", no_argument,       0, 'R' },
        { "eval",        required_argument, 0, 'e' },
        { "print",       required_argument, 0, 'E' },
        { "post-boot",   required_argument, 0, 'P' },
        { "home",        required_argument, 0, 'H' },
        { "tab",         required_argument, 0, 'T' },
        { "bare",        no_argument,       0, 'b' },
        { "lisp",        no_argument,       0, 'L' },
        { "help",        no_argument,       0, 'h' },
        { 0, 0, 0, 0 }
    };
    int c;
    while ((c = getopt_long(*argcp,*argvp,shortopts,longopts,0)) != -1) {
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
        case 'P':
            post_boot = strdup(optarg);
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
    if (!post_boot) {
        post_boot = getenv("JL_POST_BOOT");
        if (post_boot) post_boot = strdup(post_boot);
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
    char *answer_color = getenv("JL_ANSWER_COLOR");
    if (answer_color) {
        if (!strcmp(answer_color,"black"))
            jl_answer_color  = "\033[1m\033[30m";
        else if (!strcmp(answer_color,"red"))
            jl_answer_color  = "\033[1m\033[31m";
        else if (!strcmp(answer_color,"green"))
            jl_answer_color  = "\033[1m\033[32m";
        else if (!strcmp(answer_color,"yellow"))
            jl_answer_color  = "\033[1m\033[33m";
        else if (!strcmp(answer_color,"blue"))
            jl_answer_color  = "\033[1m\033[34m";
        else if (!strcmp(answer_color,"magenta"))
            jl_answer_color  = "\033[1m\033[35m";
        else if (!strcmp(answer_color,"cyan"))
            jl_answer_color  = "\033[1m\033[36m";
        else if (!strcmp(answer_color,"white"))
            jl_answer_color  = "\033[1m\033[37m";
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

static int have_color;
static int prompt_length;
static char *prompt_string;

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

static void handle_input(jl_value_t *ast, int end, int show_value);

#ifdef USE_READLINE

static jl_value_t *rl_ast;

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
        ios_printf(ios_stderr, "history file error: %s\n", strerror(errno));
        exit(1);
    }
}

static int last_hist_is_temp = 0;
static int last_hist_offset = -1;

static void add_history_temporary(char *input) {
    if (!input || !*input) return;
    if (last_hist_is_temp) {
        history_rem(history_length);
        last_hist_is_temp = 0;
    }
    last_hist_offset = -1;
    add_history(input);
    last_hist_is_temp = 1;
}

static void add_history_permanent(char *input) {
    if (!input || !*input) return;
    if (last_hist_is_temp) {
        history_rem(history_length);
        last_hist_is_temp = 0;
    }
    last_hist_offset = -1;
    HIST_ENTRY *entry = history_get(history_length);
    if (entry && !strcmp(input, entry->line)) return;
    last_hist_offset = where_history();
    add_history(input);
    if (history_file)
        append_history(1, history_file);
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
    add_history_temporary(rl_line_buffer);
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

static int backspace_callback(int count, int key) {
    if (rl_point > 0) {
        int j = rl_point;
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
        rl_delete_text(rl_point, j);
    }
    return 0;
}

static int delete_callback(int count, int key) {
    int j = rl_point;
    j += (rl_line_buffer[j] == '\n') ? prompt_length+1 : 1;
    if (rl_end < j) j = rl_end;
    rl_delete_text(rl_point, j);
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
        last_hist_offset = -1;
        rl_get_previous_history(count, key);
        rl_point = line_end(0);
        return 0;
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
        if (last_hist_offset >= 0) {
            history_set_pos(last_hist_offset);
            last_hist_offset = -1;
        }
        return rl_get_next_history(count, key);
    }
    return 0;
}

DLLEXPORT void jl_input_line_callback_readline(char *input)
{
    int end=0, doprint=1;
    if (!input || ios_eof(ios_stdin)) {
        end = 1;
        rl_ast = NULL;
    }

    if (rl_ast != NULL) {
        doprint = !ends_with_semicolon(input);
        add_history_permanent(input);
        ios_printf(ios_stdout, "\n");
        free(input);
    }

    handle_input(rl_ast, end, doprint);
}

static void read_expr_readline(char *prompt)
{
    rl_ast = NULL;
    char *input = readline(prompt);
    jl_input_line_callback_readline(input);
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

DLLEXPORT void jl_input_line_callback_no_readline(char *input)
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
        //if (jl_is_expr(ast) && ((jl_expr_t*)ast)->head == continue_sym)
        //return read_expr_ast_no_readline(prompt, end, doprint);
        doprint = !ends_with_semicolon(input);
    }
    handle_input(ast, end, doprint);
}

static void read_expr_no_readline(char *prompt)
{
    char *input;
    //ios_printf(ios_stdout, prompt);
    //ios_flush(ios_stdout);
    input = ios_readline(ios_stdin);
    ios_purge(ios_stdin);
    jl_input_line_callback_no_readline(input);
}

static void block_for_input(char *prompt)
{
    if (no_readline) {
        read_expr_no_readline(prompt);
    }
    else {
#ifdef USE_READLINE
        read_expr_readline(prompt);
#endif
    }
}

DLLEXPORT void jl_stdin_callback()
{
    if (no_readline) {
        char *input = ios_readline(ios_stdin);
        ios_purge(ios_stdin);
        jl_input_line_callback_no_readline(input);
    }
    else {
#ifdef USE_READLINE
        rl_callback_read_char();
#endif
    }
}

static int exec_program()
{
    int err = 0;
 again: ;
    JL_TRY {
        if (err) {
            jl_show(jl_exception_in_transit);
            JL_EH_POP();
            return 1;
        }
        jl_load(program);
    }
    JL_CATCH {
        err = 1;
        goto again;
    }
    return 0;
}

static void exit_repl(int code)
{
#ifdef USE_READLINE
    if (!no_readline) {
        rl_callback_handler_remove();
    }
#endif
    if (have_color) {
        ios_printf(ios_stdout, jl_color_normal);
        ios_flush(ios_stdout);
    }
#ifdef JL_GF_PROFILE
    print_profile();
#endif
    exit(code);
}

void jl_show_full_function(jl_value_t *v);

static void repl_show_value(jl_value_t *v)
{
    if (jl_is_function(v) && !jl_is_struct_type(v)) {
        // show method table when a function is shown at the top level.
        jl_show_full_function(v);
        return;
    }
    jl_show(v);
    if (jl_is_struct_type(v)) {
        ios_t *s = jl_current_output_stream();
        // for convenience, show constructor methods when
        // a type is shown at the top level.
        jl_struct_type_t *tt = (jl_struct_type_t*)v;
        if (tt->name->primary==v && jl_is_gf(v)) {
            ios_putc('\n', s);
            jl_show_full_function(v);
        }
    }
}

DLLEXPORT void jl_handle_user_input(jl_value_t *ast, int show_value)
{
#ifdef USE_READLINE
    if (!no_readline) {
        if (load_start_j) {
            // with multi.j loaded the readline callback can return
            // before the command finishes running, so we have to
            // disable rl to prevent the prompt from reappearing too soon.
            rl_callback_handler_remove();
        }
    }
#endif
    JL_GC_PUSH(&ast);
    assert(ast != NULL);
    int iserr = 0;
 again:
    ;
    JL_TRY {
        if (have_color) {
            ios_printf(ios_stdout, jl_color_normal);
            ios_flush(ios_stdout);
        }
        if (iserr) {
            jl_show(jl_exception_in_transit);
            ios_printf(ios_stdout, "\n");
            JL_EH_POP();
            break;  // leave JL_TRY
        }
        jl_value_t *value = jl_toplevel_eval(ast);
        jl_set_global(jl_system_module, jl_symbol("ans"), value);
        if (show_value) {
            if (have_color) {
                ios_printf(ios_stdout, jl_answer_color);
                ios_flush(ios_stdout);
            }
            repl_show_value(value);
            ios_printf(ios_stdout, "\n");
        }
    }
    JL_CATCH {
        iserr = 1;
        goto again;
    }
    ios_printf(ios_stdout, "\n");
    ios_flush(ios_stdout);
    JL_GC_POP();
    if (no_readline) {
        ios_printf(ios_stdout, prompt_string);
        ios_flush(ios_stdout);
    }
    else {
#ifdef USE_READLINE
        if (load_start_j) {
            rl_callback_handler_install(prompt_string,
                                        jl_input_line_callback_readline);
        }
#endif
    }
}

// handle a command line input event
static void handle_input(jl_value_t *ast, int end, int show_value)
{
    if (end) {
        ios_printf(ios_stdout, "\n");
        exit_repl(0);
    }
    if (ast == NULL) {
        ios_printf(ios_stdout, "\n");
        if (no_readline) {
            ios_printf(ios_stdout, prompt_string);
        }
        ios_flush(ios_stdout);
        return;
    }
    jl_value_t *f=jl_get_global(jl_system_module,jl_symbol("repl_callback"));
    if (f == NULL) {
        jl_handle_user_input(ast, show_value);
    }
    else {
        jl_value_t *fargs[] = { ast, jl_box_int32(show_value) };
        jl_apply((jl_function_t*)f, fargs, 2);
    }
}

// TODO: sigfpe hack
static void awful_sigfpe_hack()
{
    JL_TRY {
        kill(getpid(), SIGFPE);
    }
    JL_CATCH {
    }
}

void jl_lisp_prompt();

#ifdef JL_GF_PROFILE
static void print_profile()
{
    size_t i;
    void **table = jl_system_module->bindings.table;
    for(i=1; i < jl_system_module->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value != NULL && jl_is_function(b->value) &&
                jl_is_gf(b->value)) {
                ios_printf(ios_stdout, "%d\t%s\n",
                           jl_gf_mtable(b->value)->ncalls,
                           jl_gf_name(b->value)->name);
            }
        }
    }
}
#endif

int main(int argc, char *argv[])
{
    double julia_launch_tic = clock_now();

#ifdef BOEHM_GC
    GC_init();
#endif
    llt_init();
    parse_opts(&argc, &argv);
    julia_init();
    if (post_boot) {
        jl_value_t *ast = jl_parse_input_line(post_boot);
        jl_toplevel_eval(ast);
    }

    jl_array_t *args = jl_alloc_cell_1d(argc);
    jl_set_const(jl_system_module, jl_symbol("ARGS"), (jl_value_t*)args);
    int i;
    for (i=0; i < argc; i++) {
        jl_arrayset(args, i, (jl_value_t*)jl_cstr_to_string(argv[i]));
    }

#ifdef USE_READLINE
    if (!no_readline) {
        init_history();
        rl_bind_key(' ', space_callback);
        rl_bind_key('\t', tab_callback);
        rl_bind_key('\r', return_callback);
        rl_bind_key('\n', return_callback);
        rl_bind_key('\v', line_kill_callback);
        rl_bind_key('\b', backspace_callback);
        rl_bind_key('\001', line_start_callback);
        rl_bind_key('\005', line_end_callback);
        rl_bind_key('\002', left_callback);
        rl_bind_key('\006', right_callback);
        rl_bind_keyseq("\e[A", up_callback);
        rl_bind_keyseq("\e[B", down_callback);
        rl_bind_keyseq("\e[D", left_callback);
        rl_bind_keyseq("\e[C", right_callback);
        rl_bind_keyseq("\\C-d", delete_callback);
    }
#endif

    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }

    awful_sigfpe_hack();

    if (load_start_j) {
        if (jl_load_startup_file())
            return 1;
    }
    if (num_evals) {
        int i, iserr=0;
        jl_value_t *ast=NULL, *value=NULL;
        JL_GC_PUSH(&ast, &value);
        for (i=0; i < num_evals; i++) {
        try_again: ;
            JL_TRY {
                if (iserr) {
                    jl_show(jl_exception_in_transit);
                    ios_printf(ios_stdout, "\n");
                    iserr = 0;
                }
                if (i < num_evals) {
                    ast = jl_parse_input_line(eval_exprs[i]);
                    if (ast != NULL) {
                        value = jl_toplevel_eval(ast);
                        if (print_exprs[i]) {
                            jl_show(value);
                            ios_printf(ios_stdout, "\n");
                        }
                    }
                }
            }
            JL_CATCH {
                iserr = 1;
                i++;
                goto try_again;
            }
        }
        jl_shutdown_frontend();
        JL_GC_POP();
        return 0;
    }
    if (program) {
        return exec_program();
    }

    have_color = detect_color();
    char *banner = have_color ? jl_banner_color : jl_banner_plain;
    char *prompt = have_color ?
        (no_readline ? jl_prompt_color_no_readline : jl_prompt_color) :
        jl_prompt_plain;
    prompt_length = strlen(jl_prompt_plain);
    prompt_string = prompt;

    if (print_banner) {
        ios_printf(ios_stdout, "%s", banner);
	ios_printf(ios_stdout, "Startup time: %.2f seconds\n\n", 
		   (clock_now()-julia_launch_tic));
    }

    jl_function_t *start_client =
        (jl_function_t*)
        jl_get_global(jl_system_module, jl_symbol("start_client"));

    if (no_readline) {
        ios_printf(ios_stdout, prompt_string);
        ios_flush(ios_stdout);
    }
    if (start_client == NULL) {
        // client event loop not available; use fallback blocking version
        int iserr = 0;
    again:
        ;
        JL_TRY {
            if (iserr) {
                if (have_color) {
                    ios_printf(ios_stdout, jl_color_normal);
                    ios_flush(ios_stdout);
                }
                jl_show(jl_exception_in_transit);
                ios_printf(ios_stdout, "\n\n");
                ios_flush(ios_stdout);
                iserr = 0;
            }
            while (1) {
                block_for_input(prompt);
            }
        }
        JL_CATCH {
            iserr = 1;
            goto again;
        }
    }
    else {
        if (!no_readline) {
#ifdef USE_READLINE
            rl_callback_handler_install(prompt_string,
                                        jl_input_line_callback_readline);
#endif
        }
        jl_apply(start_client, NULL, 0);
    }

    exit_repl(0);
    return 0;
}
