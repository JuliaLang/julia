#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>

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
