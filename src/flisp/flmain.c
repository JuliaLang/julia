#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "flisp.h"
#include "opcodes.h"

static value_t argv_list(int argc, char *argv[])
{
    int i;
    value_t lst=FL_NIL, temp;
    fl_gc_handle(&lst);
    fl_gc_handle(&temp);
    for(i=argc-1; i >= 0; i--) {
        temp = cvalue_static_cstring(argv[i]);
        lst = fl_cons(temp, lst);
    }
    fl_free_gc_handles(2);
    return lst;
}

extern value_t fl_file(value_t *args, uint32_t nargs);

int main(int argc, char *argv[])
{
    char fname_buf[1024];

    fl_init(512*1024);

    fname_buf[0] = '\0';
    value_t str = symbol_value(symbol("*install-dir*"));
    char *exedir = (str == UNBOUND ? NULL : cvalue_data(str));
    if (exedir != NULL) {
        strcat(fname_buf, exedir);
        strcat(fname_buf, PATHSEPSTRING);
    }
    strcat(fname_buf, "flisp.boot");

    value_t args[2];
    fl_gc_handle(&args[0]);
    fl_gc_handle(&args[1]);
    FL_TRY_EXTERN {
        args[0] = cvalue_static_cstring(fname_buf);
        args[1] = symbol(":read");
        value_t f = fl_file(&args[0], 2);
        fl_free_gc_handles(2);

        if (fl_load_system_image(f))
            return 1;

        (void)fl_applyn(1, symbol_value(symbol("__start")),
                        argv_list(argc, argv));
    }
    FL_CATCH_EXTERN {
        ios_puts("fatal error:\n", ios_stderr);
        fl_print(ios_stderr, fl_lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }
    return 0;
}
