/*
  repl.c
  system startup, main(), and console interaction
*/

#include "repl.h"

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


char *jl_answer_color  = "\033[1m\033[34m";
char *julia_home = NULL; // load is relative to here
char *prompt_string;

static char jl_prompt_plain[] = "julia> ";
static char jl_color_normal[] = "\033[0m\033[37m";
static int print_banner = 1;
static char *post_boot = NULL;
static int lisp_prompt = 0;
static int have_event_loop = 0;
static char *program = NULL;

int num_evals = 0;
char **eval_exprs = NULL;
int *print_exprs = NULL;
char *image_file = "sys.ji";
char *load_file = NULL;

jl_value_t *rl_ast = NULL;
int tab_width = 2;
int prompt_length = 0;
int have_color = 1;
int no_readline = 1;

static const char *usage = "julia [options] [program] [args...]\n";
static const char *opts =
    " -q --quiet               Quiet startup without banner\n"
    " -R --no-readline         Disable readline functionality\n"
    " -H --home=<dir>          Load files relative to <dir>\n"
    " -T --tab=<size>          Set REPL tab width to <size>\n\n"

    " -e --eval=<expr>         Evaluate <expr> and don't print\n"
    " -E --print=<expr>        Evaluate and print <expr>\n"
    " -P --post-boot=<expr>    Evaluate <expr> right after boot\n"
    " -L --load=file           Load <file> right after boot\n"
    " -b --bare                Bare: don't load default startup files\n"
    " -J --sysimage=file       Start up with the given system image file\n"
    "    --lisp                Start with Lisp prompt not Julia\n\n"

    " -h --help                Print this message\n";

void parse_opts(int *argcp, char ***argvp) {
    static char* shortopts = "qRe:E:P:H:T:bL:hJ:";
    static struct option longopts[] = {
        { "quiet",       no_argument,       0, 'q' },
        { "no-readline", no_argument,       0, 'R' },
        { "eval",        required_argument, 0, 'e' },
        { "print",       required_argument, 0, 'E' },
        { "post-boot",   required_argument, 0, 'P' },
        { "home",        required_argument, 0, 'H' },
        { "tab",         required_argument, 0, 'T' },
        { "bare",        no_argument,       0, 'b' },
        { "lisp",        no_argument,       &lisp_prompt, 1 },
        { "load",        required_argument, 0, 'L' },
        { "help",        no_argument,       0, 'h' },
        { "sysimage",    required_argument, 0, 'J' },
        { 0, 0, 0, 0 }
    };
    int c;
    while ((c = getopt_long(*argcp,*argvp,shortopts,longopts,0)) != -1) {
        switch (c) {
        case 0:
            break;
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
            image_file = NULL;
            break;
        case 'L':
            load_file = optarg;
            break;
        case 'J':
            image_file = optarg;
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
    /*
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
    */
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

int ends_with_semicolon(const char *input)
{
    char *p = strrchr(input, ';');
    if (p++) {
        while (isspace(*p)) p++;
        if (*p == '\0' || *p == '#')
            return 1;
    }
    return 0;
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

char *ios_readline(ios_t *s)
{
    ios_t dest;
    ios_mem(&dest, 0);
    ios_copyuntil(&dest, s, '\n');
    size_t n;
    return ios_takebuf(&dest, &n);
}

// called when we detect an event on stdin
DLLEXPORT void jl_stdin_callback()
{
    if (no_readline) {
        char *input = ios_readline(ios_stdin);
        ios_purge(ios_stdin);
        jl_input_line_callback(input);
    } else {
        repl_stdin_callback();
    }
}

static int exec_program()
{
    int err = 0;
 again: ;
    JL_TRY {
        if (err) {
            jl_show(jl_exception_in_transit);
            ios_printf(ios_stdout, "\n");
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

// load a file at startup before proceeding with REPL or program
int jl_load_startup_file(char *fname)
{
    JL_TRY {
        jl_load(fname);
    }
    JL_CATCH {
        ios_printf(ios_stderr, "error during startup:\n");
        //jl_typeinf_func = NULL;
        jl_show(jl_exception_in_transit);
        ios_printf(ios_stdout, "\n");
        return 1;
    }
#ifdef JL_GC_MARKSWEEP
    jl_gc_collect();
#endif
    return 0;
}

static void exit_repl(int code)
{
    exit_repl_environment();

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

DLLEXPORT void jl_eval_user_input(jl_value_t *ast, int show_value)
{
    if (!no_readline) {
        if (have_event_loop) {
            // with multi.j loaded the readline callback can return
            // before the command finishes running, so we have to
            // disable rl to prevent the prompt from reappearing too soon.
	    repl_callback_disable();
        }
    }
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
        if (have_event_loop) {
            repl_callback_enable();
        }
    }
}

// handle a command line input event
void handle_input(jl_value_t *ast, int end, int show_value)
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
        jl_eval_user_input(ast, show_value);
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

#ifdef COPY_STACKS
void jl_switch_stack(jl_task_t *t, jmp_buf *where);
extern jmp_buf * volatile jl_jmp_target;
#endif

int main(int argc, char *argv[])
{
    llt_init();
    parse_opts(&argc, &argv);
    julia_init(lisp_prompt ? NULL : image_file);
#ifdef COPY_STACKS
    // initialize base context of root task
    jl_root_task->stackbase = (char*)&argc;
    if (setjmp(jl_root_task->base_ctx)) {
        jl_switch_stack(jl_current_task, jl_jmp_target);
    }
#endif

    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }

    jl_array_t *args = jl_alloc_cell_1d(argc);
    jl_set_const(jl_system_module, jl_symbol("ARGS"), (jl_value_t*)args);
    int i;
    for (i=0; i < argc; i++) {
        jl_arrayset(args, i, (jl_value_t*)jl_cstr_to_string(argv[i]));
    }
    jl_set_const(jl_system_module, jl_symbol("JULIA_HOME"),
                 jl_cstr_to_string(julia_home));

    awful_sigfpe_hack();

    // post boot phase: do -P and -L actions
    if (post_boot) {
        jl_value_t *ast = jl_parse_input_line(post_boot);
        jl_toplevel_eval(ast);
    }

    if (load_file) {
        if (jl_load_startup_file(load_file))
            return 1;
    }

    // handle -e and -E
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

    // run program if specified, otherwise enter REPL
    if (program) {
        return exec_program();
    }

    init_repl_environment();

    have_color = detect_color();
    char *banner = have_color ? jl_banner_color : jl_banner_plain;
    char *prompt = have_color ? jl_prompt_color : jl_prompt_plain;
    prompt_length = strlen(jl_prompt_plain);
    prompt_string = prompt;

    if (print_banner) {
        ios_printf(ios_stdout, "%s", banner);
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
                read_expr(prompt);
            }
        }
        JL_CATCH {
            iserr = 1;
            goto again;
        }
    }
    else {
        have_event_loop = 1;
        if (!no_readline) {
            repl_callback_enable();
        }
        jl_apply(start_client, NULL, 0);
    }

    exit_repl(0);
    return 0;
}
