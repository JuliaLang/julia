module Tk
import Base.*

export Window, Button, pack, place, tcl_eval

libtcl = dlopen("libtcl8.5")
libtk = dlopen("libtk8.5")
libX = dlopen("libX11")

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

function tcl_result()
    bytestring(ccall(dlsym(libtcl,:Tcl_GetStringResult),
                     Ptr{Uint8}, (Ptr{Void},), tcl_interp))
end

function tcl_evalfile(name)
    if ccall(dlsym(libtcl,:Tcl_EvalFile), Int32, (Ptr{Void}, Ptr{Uint8}),
             tcl_interp, name) != 0
        println("tcl error: ", tcl_result())
    end
    nothing
end

function tcl_eval(cmd)
    if ccall(dlsym(libtcl,:Tcl_Eval), Int32, (Ptr{Void}, Ptr{Uint8}),
             tcl_interp, cmd) != 0
        println("tcl error: ", tcl_result())
    end
    nothing
end

type Window
    path::ByteString

    WIN_ID::Int = 0
    Window(title) = Window(title, 200, 200)
    function Window(title, w, h)
        wpath = ".jl_frame_$WIN_ID"; WIN_ID += 1
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
    function Button(parent, text)
        bpath = "jl_button_$BUTTON_ID"; BUTTON_ID += 1
        path = "$(parent.path).$bpath"
        tcl_eval("ttk::button $path -text \"$text\"")
        new(path, parent)
    end
end

pack(widget) = tcl_eval("pack $(widget.path)")

place(widget, x::Int, y::Int) = tcl_eval("place $(widget.path) -x $x -y $y")

tcl_interp = init()
tcl_eval("wm withdraw .")

end  # module
