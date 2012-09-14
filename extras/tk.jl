# julia tk interface
# TODO:
# - callbacks (possibly via C wrapper)
# - portable event handling, probably using Tcl_CreateEventSource
# - types: may not make sense to have one for each widget, maybe one TkWidget
# - state-interrogating functions
# - expose constants from tcl.h like TCL_OK, TCL_ERROR, etc.
# - Cairo drawing surfaces
# - more widgets

require("cairo.jl")

module Tk
import Base.*
import Cairo.*

export Window, Button, TkCanvas, Canvas, pack, place, tcl_eval, TclError,
    cairo_surface_for, width, height, reveal, cairo_context, cairo_surface

libtcl = dlopen("libtcl8.5")
libtk = dlopen("libtk8.5")
libX = dlopen("libX11")
tk_wrapper = dlopen("libtk_wrapper")

function tcl_doevent(fd)
    while (ccall(dlsym(libtcl,:Tcl_DoOneEvent), Int32, (Int32,), (1<<1))!=0)
    end
end

# fetch first word from struct
tk_display(w) = pointer_to_array(convert(Ptr{Ptr{Void}},w), (1,), false)[1]

function init()
    ccall(dlsym(libtcl,:Tcl_FindExecutable), Void, (Ptr{Uint8},),
          file_path(JULIA_HOME, "julia"))
    tcl_interp = ccall(dlsym(libtcl,:Tcl_CreateInterp), Ptr{Void}, ())
    ccall(dlsym(libtcl,:Tcl_Init), Int32, (Ptr{Void},), tcl_interp)
    ccall(dlsym(libtk,:Tk_Init), Int32, (Ptr{Void},), tcl_interp)
    # TODO: for now cheat and use X-specific hack for events
    mainwin = mainwindow(tcl_interp)
    disp = tk_display(mainwin)
    fd = ccall(dlsym(libX,:XConnectionNumber), Int32, (Ptr{Void},), disp)
    add_fd_handler(fd, tcl_doevent)
    tcl_interp
end

mainwindow(interp) =
    ccall(dlsym(libtk,:Tk_MainWindow), Ptr{Void}, (Ptr{Void},), interp)

type TclError <: Exception
    msg::String
end

function tcl_result()
    bytestring(ccall(dlsym(libtcl,:Tcl_GetStringResult),
                     Ptr{Uint8}, (Ptr{Void},), tcl_interp))
end

function tcl_evalfile(name)
    if ccall(dlsym(libtcl,:Tcl_EvalFile), Int32, (Ptr{Void}, Ptr{Uint8}),
             tcl_interp, name) != 0
        throw(TclError(tcl_result()))
    end
    nothing
end

function tcl_eval(cmd)
    code = ccall(dlsym(libtcl,:Tcl_Eval), Int32, (Ptr{Void}, Ptr{Uint8}),
                 tcl_interp, cmd)
    result = tcl_result()
    if code != 0
        throw(TclError(result))
    else
        result
    end
end

type TkWidget
    path::ByteString
    kind::ByteString
    parent::Union(TkWidget,Nothing)

    ID::Int = 0
    function TkWidget(parent::TkWidget, kind)
        path = "$(parent.path).jl_$(kind)$(ID)"; ID += 1
        new(path, kind, parent)
    end
    global Window
    function Window(title, w, h)
        wpath = ".jl_win$ID"; ID += 1
        tcl_eval("frame $wpath -width $w -height $h")
        tcl_eval("wm manage $wpath")
        tcl_eval("wm title $wpath \"$title\"")
        tcl_doevent(0)
        new(wpath, "frame", nothing)
    end
end

Window(title) = Window(title, 200, 200)

Button(parent, text) = Button(parent, text, nothing)

function Button(parent, text, command)
    b = TkWidget(parent, "ttk::button")
    cmd = "ttk::button $(b.path) -text \"$text\""
    if isa(command,Function)
        cmd = cmd * " -command $(tcl_callback(command))"
    end
    tcl_eval(cmd)
    b
end

function TkCanvas(parent, w, h)
    c = TkWidget(parent, "canvas")
    tcl_eval("canvas $(c.path) -width $w -height $h")
    c
end

pack(widget::TkWidget) = tcl_eval("pack $(widget.path)")

place(widget::TkWidget, x::Int, y::Int) = tcl_eval("place $(widget.path) -x $x -y $y")

function nametowindow(name)
    ccall(dlsym(libtk, :Tk_NameToWindow), Ptr{Void},
          (Ptr{Void}, Ptr{Uint8}, Ptr{Void}),
          tcl_interp, name, mainwindow(tcl_interp))
end

const _callbacks = ObjectIdDict()

function tcl_callback(f)
    cname = string("jl_cb", repr(uint32(object_id(f))))
    # TODO: use Tcl_CreateObjCommand instead
    ccall(dlsym(libtcl,:Tcl_CreateCommand), Ptr{Void},
          (Ptr{Void}, Ptr{Uint8}, Ptr{Void}, Any, Ptr{Void}),
          tcl_interp, cname, dlsym(tk_wrapper,:jl_tcl_callback), f, C_NULL)
    # TODO: use a delete proc (last arg) to remove this
    _callbacks[f] = true
    cname
end

width(w::TkWidget) = int(tcl_eval("$(w.path) cget -width"))
height(w::TkWidget) = int(tcl_eval("$(w.path) cget -height"))

# NOTE: This has to be ported to each window environment.
# But, this should be the only such function needed.
function cairo_surface_for(w::TkWidget)
    win = nametowindow(w.path)
    disp = ccall(dlsym(tk_wrapper,:jl_tkwin_display), Ptr{Void}, (Ptr{Void},),
                 win)
    d = ccall(dlsym(tk_wrapper,:jl_tkwin_id), Int32, (Ptr{Void},), win)
    vis = ccall(dlsym(tk_wrapper,:jl_tkwin_visual), Ptr{Void}, (Ptr{Void},),
                win)
    if disp==C_NULL || d==0 || vis==C_NULL
        error("invalid window")
    end
    CairoXlibSurface(disp, d, vis, width(w), height(w))
end

# TkCanvas is the plain Tk canvas widget. This one is double-buffered
# and built on Cairo.
type Canvas
    c::TkWidget
    front::CairoSurface  # surface for window
    back::CairoSurface   # backing store
    frontcc::CairoContext
    backcc::CairoContext

    function Canvas(parent, x, y, w, h)
        c = TkWidget(parent, "frame")
        # frame supports empty background, allowing us to control drawing
        tcl_eval("frame $(c.path) -width $w -height $h -background \"\"")
        place(c, x, y)
        tcl_doevent(0)  # make sure window resources are assigned
        front = cairo_surface_for(c)
        back = surface_create_similar(front, w, h)
        can = new(c, front, back, CairoContext(front), CairoContext(back))
        cb = tcl_callback((x...)->reveal(can))
        tcl_eval("bind $(c.path) <Expose> \"$(cb)\"")
        can
    end
end

function reveal(c::Canvas)
    set_source_surface(c.frontcc, c.back, 0, 0)
    paint(c.frontcc)
end

cairo_context(c::Canvas) = c.backcc
cairo_surface(c::Canvas) = c.back

# Example:
#    w = Window("test", 640, 400)
#    c = Canvas(w, 10, 10, 320, 200)
#    cr = cairo_context(c)
#    set_source_rgb(cr,1,1,1)
#    paint(cr)
#    set_source_rgb(cr,1,0,0)
#    move_to(cr,320,0)
#    line_to(cr,0,200)
#    stroke(cr)
#    reveal(c)


tcl_interp = init()
tcl_eval("wm withdraw .")

end  # module
