# This file is a part of Julia. License is MIT: https://julialang.org/license

# clipboard copy and paste

if Sys.isapple()
    function clipboard(x)
        pbcopy_cmd = `pbcopy`

        # OSX shells, especially when run within `tmux` or `screen`, can be
        # disconnected from the global shell namespace, which causes problems
        # with clipboards.  Luckily, the `reattach-to-user-namespace` utility
        # dodges these issues quite nicely, so we automatically utilize it if
        # it is installed.
        if Sys.which("reattach-to-user-namespace") !== nothing
            pbcopy_cmd = `reattach-to-user-namespace pbcopy`
        end

        open(pipeline(pbcopy_cmd, stderr=stderr), "w") do io
            print(io, x)
        end
        nothing
    end
    function clipboard()
        pbpaste_cmd = `pbpaste`

        # See above comment in `clipboard(x)`
        if Sys.which("reattach-to-user-namespace") !== nothing
            pbcopy_cmd = `reattach-to-user-namespace pbpaste`
        end
        return read(pbpaste_cmd, String)
    end

elseif Sys.islinux() || Sys.KERNEL === :FreeBSD
    _clipboardcmd = nothing
    const _clipboard_copy = Dict(
            :xsel  => Sys.islinux() ?
                `xsel --input --clipboard` :
                `xsel -c`,
            :xclip => `xclip -silent -in -selection clipboard`,
        )
    const _clipboard_paste = Dict(
            :xsel  => Sys.islinux() ?
                `xsel --nodetach --output --clipboard` :
                `xsel -p`,
            :xclip => `xclip -quiet -out -selection clipboard`,
        )
    function clipboardcmd()
        global _clipboardcmd
        _clipboardcmd !== nothing && return _clipboardcmd
        for cmd in (:xclip, :xsel)
            success(pipeline(`which $cmd`, devnull)) && return _clipboardcmd = cmd
        end
        pkgs = @static if Sys.KERNEL === :FreeBSD
            "x11/xsel or x11/xclip"
        else
            "xsel or xclip"
        end
        error("no clipboard command found, please install $pkgs")
    end
    function clipboard(x)
        c = clipboardcmd()
        cmd = _clipboard_copy[c]
        open(pipeline(cmd, stderr=stderr), "w") do io
            print(io, x)
        end
        nothing
    end
    function clipboard()
        c = clipboardcmd()
        cmd = _clipboardcmds_paste[c]
        return read(pipeline(cmd, stderr=stderr), String)
    end

elseif Sys.iswindows()
    function clipboard(x::AbstractString)
        if Base.containsnul(x)
            throw(ArgumentError("Windows clipboard strings cannot contain NUL character"))
        end
        x_u16 = Base.cwstring(x)
        pdata = Ptr{UInt16}(C_NULL)
        function cleanup(cause)
            errno = cause == :success ? UInt32(0) : Libc.GetLastError()
            if cause !== :OpenClipboard
                if cause !== :success && pdata != C_NULL
                    ccall((:GlobalFree, "kernel32"), stdcall, Cint, (Ptr{UInt16},), pdata)
                end
                ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror(:CloseClipboard) # this should never fail
            end
            cause == :success || Base.windowserror(cause, errno)
            nothing
        end
        ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL) == 0 && return Base.windowserror(:OpenClipboard)
        ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()) == 0 && return cleanup(:EmptyClipboard)
        # copy data to locked, allocated space
        pdata = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{UInt16}, (Cuint, Csize_t), 2 #=GMEM_MOVEABLE=#, sizeof(x_u16))
        pdata == C_NULL && return cleanup(:GlobalAlloc)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        plock == C_NULL && return cleanup(:GlobalLock)
        ccall(:memcpy, Ptr{UInt16}, (Ptr{UInt16}, Ptr{UInt16}, Csize_t), plock, x_u16, sizeof(x_u16))
        unlock = ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), pdata)
        (unlock == 0 && Libc.GetLastError() == 0) || return cleanup(:GlobalUnlock) # this should never fail
        pset = ccall((:SetClipboardData, "user32"), stdcall, Ptr{UInt16}, (Cuint, Ptr{UInt16}), 13, pdata)
        pdata != pset && return cleanup(:SetClipboardData)
        cleanup(:success)
    end
    clipboard(x) = clipboard(sprint(print, x)::String)
    function clipboard()
        function cleanup(cause)
            errno = cause == :success ? UInt32(0) : Libc.GetLastError()
            if cause !== :OpenClipboard
                ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror(:CloseClipboard) # this should never fail
            end
            if cause !== :success && (cause !== :GetClipboardData || errno != 0)
                Base.windowserror(cause, errno)
            end
            ""
        end
        ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL) == 0 && return Base.windowserror(:OpenClipboard)
        ccall(:SetLastError, stdcall, Cvoid, (UInt32,), 0) # allow distinguishing if the clipboard simply didn't have text
        pdata = ccall((:GetClipboardData, "user32"), stdcall, Ptr{UInt16}, (Cuint,), 13)
        pdata == C_NULL && return cleanup(:GetClipboardData)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        plock == C_NULL && return cleanup(:GlobalLock)
        s = try
            # find NUL terminator (0x0000 16-bit code unit)
            len = 0
            while unsafe_load(plock, len + 1) != 0
                len += 1
            end
            # get Vector{UInt16}, transcode data to UTF-8, make a String of it
            transcode(String, unsafe_wrap(Array, plock, len))
        finally
           unlock = ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), pdata)
           (unlock != 0 || Libc.GetLastError() == 0) || return cleanup(:GlobalUnlock) # this should never fail
           cleanup(:success)
        end
        return s
    end

else
    clipboard(x="") = error("`clipboard` function not implemented for $(Sys.KERNEL)")
end


"""
    clipboard(x)

Send a printed form of `x` to the operating system clipboard ("copy").
"""
clipboard(x)

"""
    clipboard() -> AbstractString

Return a string with the contents of the operating system clipboard ("paste").
"""
clipboard()
