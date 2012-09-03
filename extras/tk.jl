# julia tk interface
# TODO:
# - callbacks (possibly via C wrapper)
# - portable event handling, probably using Tcl_CreateEventSource
# - types: may not make sense to have one for each widget, maybe one TkWidget
# - state-interrogating functions
# - expose constants from tcl.h like TCL_OK, TCL_ERROR, etc.
# - Cairo drawing surfaces
# - more widgets

module Tk
import Base.*

export Window, Button, pack, place, tcl_eval

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
    mainwin = ccall(dlsym(libtk,:Tk_MainWindow), Ptr{Void}, (Ptr{Void},),
                    tcl_interp)
    disp = tk_display(mainwin)
    fd = ccall(dlsym(libX,:XConnectionNumber), Int32, (Ptr{Void},), disp)
    add_fd_handler(fd, tcl_doevent)
    tcl_interp
end

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

type Window
    path::ByteString

    WIN_ID::Int = 0
    Window(title) = Window(title, 200, 200)
    function Window(title, w, h)
        wpath = ".jl_fra$WIN_ID"; WIN_ID += 1
        tcl_eval("frame $wpath -width $w -height $h")
        tcl_eval("wm manage $wpath")
        tcl_eval("wm title $wpath \"$title\"")
        new(wpath)
    end
end

type Button
    path::ByteString
    parent

    BUTTON_ID::Int = 0
    Button(parent, text) = Button(parent, text, nothing)
    function Button(parent, text, command)
        bpath = "jl_but$BUTTON_ID"; BUTTON_ID += 1
        path = "$(parent.path).$bpath"
        cmd = "ttk::button $path -text \"$text\""
        if isa(command,Function)
            cmd = cmd * " -command $(tcl_callback(command))"
        end
        tcl_eval(cmd)
        new(path, parent)
    end
end

pack(widget) = tcl_eval("pack $(widget.path)")

place(widget, x::Int, y::Int) = tcl_eval("place $(widget.path) -x $x -y $y")

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

tcl_interp = init()
tcl_eval("wm withdraw .")

end  # module
