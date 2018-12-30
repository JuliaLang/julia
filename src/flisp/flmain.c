#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "flisp.h"
#include "opcodes.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
const char* __asan_default_options() {
    return "detect_leaks=0";
}
#endif
#endif

static value_t argv_list(fl_context_t *fl_ctx, int argc, char *argv[])
{
    int i;
    value_t lst=fl_ctx->NIL, temp;
    fl_gc_handle(fl_ctx, &lst);
    fl_gc_handle(fl_ctx, &temp);
    for(i=argc-1; i >= 0; i--) {
        temp = cvalue_static_cstring(fl_ctx, argv[i]);
        lst = fl_cons(fl_ctx, temp, lst);
    }
    fl_free_gc_handles(fl_ctx, 2);
    return lst;
}

extern value_t fl_file(fl_context_t *fl_ctx, value_t *args, uint32_t nargs);

static fl_context_t fl_global_ctx;

int main(int argc, char *argv[])
{
    char fname_buf[1024];
    fl_context_t *fl_ctx = &fl_global_ctx;

    fl_init(fl_ctx, 512*1024);

    fname_buf[0] = '\0';
    if (argc >= 3 && strcmp(argv[1], "--boot") == 0) {
        strncpy(fname_buf, argv[2], sizeof(fname_buf));
        fname_buf[sizeof(fname_buf)-1] = '\0';
        argc -= 2; argv[2] = argv[0]; argv += 2;
    } else {
        value_t str = symbol_value(symbol(fl_ctx, "*install-dir*"));
        char *exedir = (char*)(str == UNBOUND ? NULL : cvalue_data(str));
        if (exedir != NULL) {
            strcat(fname_buf, exedir);
            strcat(fname_buf, PATHSEPSTRING);
        }
        strcat(fname_buf, "flisp.boot");
    }

    value_t args[2];
    fl_gc_handle(fl_ctx, &args[0]);
    fl_gc_handle(fl_ctx, &args[1]);
    FL_TRY_EXTERN(fl_ctx) {
        args[0] = cvalue_static_cstring(fl_ctx, fname_buf);
        args[1] = symbol(fl_ctx, ":read");
        value_t f = fl_file(fl_ctx, &args[0], 2);
        fl_free_gc_handles(fl_ctx, 2);

        if (fl_load_system_image(fl_ctx, f))
            return 1;

        (void)fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "__start")),
                        argv_list(fl_ctx, argc, argv));
    }
    FL_CATCH_EXTERN(fl_ctx) {
        ios_puts("fatal error:\n", ios_stderr);
        fl_print(fl_ctx, ios_stderr, fl_ctx->lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }
    return 0;
}

#ifdef __cplusplus
}
#endif
