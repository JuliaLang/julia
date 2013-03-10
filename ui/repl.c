/*
  repl.c
  system startup, main(), and console interaction
*/

#include "repl.h"
#include "uv.h"
#define WHOLE_ARCHIVE
#include "../src/julia.h"

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

    " --no-history             Don't load or save history\n"
    " -f --no-startup          Don't load ~/.juliarc.jl\n"
    " -F                       Load ~/.juliarc.jl, then handle remaining inputs\n\n"

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
            size_t path_size = PATH_MAX;
            uv_exepath(julia_path, &path_size);
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
    if (image_file) {
        int build_time_path = 0;
#ifdef JL_SYSTEM_IMAGE_PATH
        if (!imagepathspecified) {
            image_file = JL_SYSTEM_IMAGE_PATH;
            build_time_path = 1;
        }
#endif
        if (image_file[0] != PATHSEP) {
            struct stat stbuf;
            char path[512];
            if (build_time_path) {
                // build time path relative to JULIA_HOME
                snprintf(path, sizeof(path), "%s%s%s",
                         julia_home, PATHSEPSTRING, JL_SYSTEM_IMAGE_PATH);
                image_file = strdup(path);
            }
            else if (jl_stat(image_file, (char*)&stbuf) != 0) {
                // otherwise try julia_home/../lib/julia/%s
                snprintf(path, sizeof(path), "%s%s..%slib%sjulia%s%s",
                         julia_home, PATHSEPSTRING, PATHSEPSTRING,
                         PATHSEPSTRING, PATHSEPSTRING, image_file);
                image_file = strdup(path);
            }
        }
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

static int exec_program(void)
{
    int err = 0;
 again: ;
    JL_TRY {
        if (err) {
            //jl_lisp_prompt();
            //return 1;
            jl_value_t *errs = jl_stderr_obj();
            jl_value_t *e = jl_exception_in_transit;
            if (errs != NULL) {
                jl_show(jl_stderr_obj(), e);
            }
            else {
                while (1) {
                    if (jl_typeof(e) == (jl_value_t*)jl_loaderror_type) {
                        e = jl_fieldref(e, 2);
                        // TODO: show file and line
                    }
                    else break;
                }
                if (jl_typeof(e) == (jl_value_t*)jl_errorexception_type) {
                    jl_printf(JL_STDERR, "error during bootstrap: %s\n",
                               jl_string_data(jl_fieldref(e,0)));
                }
                else {
                    jl_printf(JL_STDERR, "error during bootstrap\n");
                }
            }
            jl_printf(JL_STDERR, "\n");
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

uv_buf_t *jl_alloc_read_buffer(uv_handle_t* handle, size_t suggested_size)
{
    if(suggested_size>512) suggested_size = 512; //Readline has a max buffer of 512
    char *buf = malloc(suggested_size);
    uv_buf_t *ret = malloc(sizeof(uv_buf_t));
    *ret = uv_buf_init(buf,suggested_size);
    return ret;
}


int true_main(int argc, char *argv[])
{
    if (jl_base_module != NULL) {
        jl_array_t *args = jl_alloc_cell_1d(argc);
        jl_set_global(jl_base_module, jl_symbol("ARGS"), (jl_value_t*)args);
        int i;
        for (i=0; i < argc; i++) {
            jl_arrayset(args, (jl_value_t*)jl_cstr_to_string(argv[i]), i);
        }
    }
    jl_set_const(jl_core_module, jl_symbol("JULIA_HOME"),
                 jl_cstr_to_string(julia_home));
    jl_module_export(jl_core_module, jl_symbol("JULIA_HOME"));

    // run program if specified, otherwise enter REPL
    if (program) {
        int ret = exec_program();
        uv_tty_reset_mode();
        return ret;
    }

    init_repl_environment(argc, argv);

    jl_function_t *start_client =
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_start"));

    //uv_read_start(jl_stdin_tty,jl_alloc_read_buffer,&readBuffer);

    if (start_client) {
        jl_apply(start_client, NULL, 0);
        //rl_cleanup_after_signal();
        return 0;
    }

    // client event loop not available; use fallback blocking version
    //install_read_event_handler(&echoBack);
    int iserr = 0;

 again:
    ;
    JL_TRY {
        if (iserr) {
            //jl_show(jl_exception_in_transit);# What if the error was in show?
            jl_printf(JL_STDERR, "\n\n");
            iserr = 0;
        }
    uv_run(jl_global_event_loop(),UV_RUN_DEFAULT);
    }
    JL_CATCH {
        iserr = 1;
        JL_PUTS("error during run:\n",JL_STDERR);
        jl_show(jl_stderr_obj(),jl_exception_in_transit);
        JL_PUTS("\n",JL_STDOUT);
        goto again;
    }
    uv_tty_reset_mode();
    return iserr;
}

int main(int argc, char *argv[])
{
    libsupport_init();
    parse_opts(&argc, &argv);
    if (lisp_prompt) {
        jl_init_frontend();
        jl_lisp_prompt();
        return 0;
    }
    julia_init(lisp_prompt ? NULL : image_file);
    return julia_trampoline(argc, argv, true_main);
}
