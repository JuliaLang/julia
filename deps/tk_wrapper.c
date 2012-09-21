#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include "julia.h"

int jl_tcl_callback(ClientData clientData, Tcl_Interp *interp,
                    int argc, char *argv[])
{
    jl_function_t *f = (jl_function_t*)clientData;
    jl_value_t **jlargs = alloca(argc * sizeof(void*));
    memset(jlargs, 0, argc * sizeof(void*));
    JL_GC_PUSHARGS(jlargs, argc);
    int i;
    for(i=0; i < argc; i++) {
        jlargs[i] = jl_cstr_to_string(argv[i]);
    }
    jl_value_t *result = NULL;
    JL_TRY {
        result = jl_apply(f, jlargs, argc);
    }
    JL_CATCH {
        JL_GC_POP();
        return TCL_ERROR;
    }
    Tcl_SetResult(interp, jl_string_data(result), TCL_VOLATILE);
    JL_GC_POP();
    return TCL_OK;
}

void *jl_tkwin_display(Tk_Window tkwin)
{
    return Tk_Display(tkwin);
}

void *jl_tkwin_visual(Tk_Window tkwin)
{
    return Tk_Visual(tkwin);
}

int jl_tkwin_id(Tk_Window tkwin)
{
    return Tk_WindowId(tkwin);
}
