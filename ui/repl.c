/*
  repl.c
  system startup, main(), and console interaction
*/

#include "repl.h"

static int lisp_prompt = 0;
static char *program = NULL;
char *image_file = "sys.ji";
int tab_width = 2;

static const char *usage = "julia [options] [program] [args...]\n";
static const char *opts =
    " -v --version             Display version information\n"
    " -q --quiet               Quiet startup without banner\n"
    " -H --home=<dir>          Load files relative to <dir>\n"
    " -T --tab=<size>          Set REPL tab width to <size>\n\n"

    " -e --eval=<expr>         Evaluate <expr>\n"
    " -E --print=<expr>        Evaluate and show <expr>\n"
    " -P --post-boot=<expr>    Evaluate <expr> right after boot\n"
    " -L --load=file           Load <file> right after boot\n"
    " -J --sysimage=file       Start up with the given system image file\n\n"

    " -p n                     Run n local processes\n"
    " --machinefile file       Run processes on hosts listed in file\n\n"

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
    int ind = 1;
#ifdef JL_SYSTEM_IMAGE_PATH
    int imagepathspecified=0;
#endif
    while ((c = getopt_long(*argcp,*argvp,shortopts,longopts,0)) != -1) {
        switch (c) {
        case 0:
            break;
        case '?':
            break;
        case 'H':
            julia_home = strdup(optarg);
            ind+=2;
            break;
        case 'T':
            // TODO: more robust error checking.
            tab_width = atoi(optarg);
            ind+=2;
            break;
        case 'b':
            image_file = NULL;
            ind+=1;
            break;
        case 'J':
            image_file = optarg;
#ifdef JL_SYSTEM_IMAGE_PATH
            imagepathspecified = 1;
#endif
            ind+=2;
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
    *argvp += ind;
    *argcp -= ind;
    if (image_file==NULL && *argcp > 0) {
        if (strcmp((*argvp)[0], "-")) {
            program = (*argvp)[0];
        }
    }
#ifdef JL_SYSTEM_IMAGE_PATH
    if (image_file && !imagepathspecified) {
        image_file = JL_SYSTEM_IMAGE_PATH;
    }
#endif
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

static int exec_program(void)
{
    int err = 0;
 again: ;
    JL_TRY {
        jl_register_toplevel_eh();
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

// handle a command line input event
void handle_input(jl_value_t *ast, int end, int show_value)
{
    if (end) {
        show_value = -1;
        ast = jl_nothing;
    }
    jl_value_t *f = jl_get_global(jl_base_module,jl_symbol("repl_callback"));
    assert(f);
    jl_value_t *fargs[] = { ast, jl_box_long(show_value) };
    jl_apply((jl_function_t*)f, fargs, 2);
}

void jl_lisp_prompt();

#ifdef JL_GF_PROFILE
static void print_profile(void)
{
    size_t i;
    void **table = jl_base_module->bindings.table;
    for(i=1; i < jl_base_module->bindings.size; i+=2) {
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
    jl_set_global(jl_current_module, jl_symbol("ARGS"), (jl_value_t*)args);
    int i;
    for (i=0; i < argc; i++) {
        jl_arrayset(args, i, (jl_value_t*)jl_cstr_to_string(argv[i]));
    }
    jl_set_const(jl_current_module, jl_symbol("JULIA_HOME"),
                 jl_cstr_to_string(julia_home));

    // run program if specified, otherwise enter REPL
    if (program) {
        return exec_program();
    }

    init_repl_environment(argc, argv);

    jl_function_t *start_client =
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_start"));

    if (start_client) {
        jl_apply(start_client, NULL, 0);
        return 0;
    }

    // client event loop not available; use fallback blocking version
    int iserr = 0;
 again:
    ;
    JL_TRY {
        if (iserr) {
            jl_show(jl_exception_in_transit);
            ios_printf(ios_stdout, "\n\n");
            iserr = 0;
        }
        while (1) {
            char *input = read_expr("julia> ");
            if (!input || ios_eof(ios_stdin)) {
                ios_printf(ios_stdout, "\n");
                break;
            }
            jl_value_t *ast = jl_parse_input_line(input);
            jl_value_t *value = jl_toplevel_eval(ast);
            jl_show(value);
            ios_printf(ios_stdout, "\n\n");
        }
    }
    JL_CATCH {
        iserr = 1;
        goto again;
    }

    return 0;
}

int main(int argc, char *argv[])
{
    libsupport_init();
    parse_opts(&argc, &argv);
    julia_init(lisp_prompt ? NULL : image_file);
    return julia_trampoline(argc, argv, true_main);
}
