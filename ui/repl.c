/*
  repl.c
  system startup, main(), and console interaction
*/

#include "repl.h"

char *prompt_string;
static char jl_prompt_plain[] = "julia> ";
static char jl_color_normal[] = "\033[0m";
static int lisp_prompt = 0;
int jl_have_event_loop = 0;
static char *program = NULL;
char *image_file = "sys.ji";
jl_value_t *rl_ast = NULL;
int tab_width = 2;
int prompt_length = 0;
int have_color = 1;

static const char *usage = "julia [options] [program] [args...]\n";
static const char *opts =
    " -q --quiet               Quiet startup without banner\n"
    " -H --home=<dir>          Load files relative to <dir>\n"
    " -T --tab=<size>          Set REPL tab width to <size>\n\n"

    " -e --eval=<expr>         Evaluate <expr> and don't print\n"
    " -E --print=<expr>        Evaluate and print <expr>\n"
    " -P --post-boot=<expr>    Evaluate <expr> right after boot\n"
    " -L --load=file           Load <file> right after boot\n"
    " -b --bare                Bare: don't load default startup files\n"
    " -J --sysimage=file       Start up with the given system image file\n\n"

    " -p n                     Run n local processes\n\n"

    " -h --help                Print this message\n";

void parse_opts(int *argcp, char ***argvp) {
    static char* shortopts = "+H:T:bhJ:";
    static struct option longopts[] = {
        { "home",        required_argument, 0, 'H' },
        { "tab",         required_argument, 0, 'T' },
        { "bare",        no_argument,       0, 'b' },
        { "lisp",        no_argument,       &lisp_prompt, 1 },
        { "help",        no_argument,       0, 'h' },
        { "sysimage",    required_argument, 0, 'J' },
        { 0, 0, 0, 0 }
    };
    int c;
    opterr = 0;
    while ((c = getopt_long(*argcp,*argvp,shortopts,longopts,0)) != -1) {
        if (c == '?') {
            optind--;
            break;
        }
        switch (c) {
        case 0:
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
        case 'J':
            image_file = optarg;
            break;
        case 'h':
            printf("%s%s", usage, opts);
            exit(0);
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
    *argvp += optind;
    *argcp -= optind;
    if (image_file==NULL && *argcp > 0) {
        if (strcmp((*argvp)[0], "-")) {
            program = (*argvp)[0];
        }
        ++*argvp; --*argcp;
    }
}

char *jl_answer_color() {
    char *answer_color = getenv("JL_ANSWER_COLOR");
    if (answer_color) {
        switch (answer_color[0]) {
        case 'b': if (!strcmp(answer_color,"black"))   return "\033[1m\033[30m"; break;
        case 'r': if (!strcmp(answer_color,"red"))     return "\033[1m\033[31m"; break;
        case 'g': if (!strcmp(answer_color,"green"))   return "\033[1m\033[32m"; break;
        case 'y': if (!strcmp(answer_color,"yellow"))  return "\033[1m\033[33m"; break;
        case 'm': if (!strcmp(answer_color,"magenta")) return "\033[1m\033[35m"; break;
        case 'c': if (!strcmp(answer_color,"cyan"))    return "\033[1m\033[36m"; break;
        case 'w': if (!strcmp(answer_color,"white"))   return "\033[1m\033[37m"; break;
        }
    }
    return "\033[1m\033[34m";
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
    int tput = system("tput setaf 0 >/dev/null");
    if (tput == 0) return 1;
    if (tput == 1) return 0;
    char *term = getenv("TERM");
    if (term == NULL) return 0;
    return (!strcmp(term,"xterm") || !strcmp(term,"xterm-color"));
#endif
}

// called when we detect an event on stdin
DLLEXPORT void jl_stdin_callback()
{
    repl_stdin_callback();
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
        if (jl_is_gf(v)) {
            ios_putc('\n', s);
            jl_show_full_function(v);
        }
    }
}

DLLEXPORT void jl_eval_user_input(jl_value_t *ast, int show_value)
{
    if (jl_have_event_loop) {
        // with multi.j loaded the command line input callback can return
        // before the command finishes running, so we have to
        // disable rl to prevent the prompt from reappearing too soon.
        repl_callback_disable();
    }
    JL_GC_PUSH(&ast);
    assert(ast != NULL);
    int iserr = 0;

 again:
    ;
    JL_TRY {
        jl_register_toplevel_eh();
        if (have_color) {
            ios_printf(ios_stdout, jl_color_normal);
            ios_flush(ios_stdout);
        }
        if (iserr) {
            jl_show(jl_exception_in_transit);
            ios_printf(ios_stdout, "\n");
            JL_EH_POP();
            break; // leave JL_TRY
        }
        jl_value_t *value = jl_toplevel_eval(ast);
        jl_set_global(jl_system_module, jl_symbol("ans"), value);
        if (value != (jl_value_t*)jl_nothing && show_value) {
            if (have_color) {
                ios_printf(ios_stdout, jl_answer_color());
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
    repl_callback_enable();
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
        repl_print_prompt();
        ios_flush(ios_stdout);
        return;
    }
    if (!jl_have_event_loop) {
        jl_eval_user_input(ast, show_value);
    }
    else {
        jl_value_t *f = 
            jl_get_global(jl_system_module,jl_symbol("repl_callback"));
        assert(f != NULL);
        jl_value_t *fargs[] = { ast, jl_box_int32(show_value) };
        jl_apply((jl_function_t*)f, fargs, 2);
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

int true_main(int argc, char *argv[])
{
    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }

    jl_array_t *args = jl_alloc_cell_1d(argc);
    jl_set_global(jl_system_module, jl_symbol("ARGS"), (jl_value_t*)args);
    int i;
    for (i=0; i < argc; i++) {
        jl_arrayset(args, i, (jl_value_t*)jl_cstr_to_string(argv[i]));
    }
    jl_set_global(jl_system_module, jl_symbol("JULIA_HOME"),
                 jl_cstr_to_string(julia_home));

    // run program if specified, otherwise enter REPL
    if (program) {
        return exec_program();
    }

    init_repl_environment();

    have_color = detect_color();
    char *prompt = have_color ? jl_prompt_color : jl_prompt_plain;
    prompt_length = strlen(jl_prompt_plain);
    prompt_string = prompt;

    jl_function_t *start_client =
        (jl_function_t*)
        jl_get_global(jl_system_module, jl_symbol("_start"));

    if (start_client == NULL) {
        repl_print_prompt();
        ios_flush(ios_stdout);
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
        jl_have_event_loop = 1;
        jl_apply(start_client, NULL, 0);
    }

    exit_repl(0);
    return 0;
}

int main(int argc, char *argv[])
{
    libsupport_init();
    parse_opts(&argc, &argv);
    julia_init(lisp_prompt ? NULL : image_file);
    return julia_trampoline(argc, argv, true_main);
}
