# This file is a part of Julia. License is MIT: https://julialang.org/license

# clipboard copy and paste

"""
    _detect_osc52_support(term::Base.Terminals.TTYTerminal) -> Bool

Detect if the terminal supports OSC52 clipboard operations using XTGETTCAP.

Queries the terminal for the 'Ms' (clipboard) capability using the DCS + q sequence.
Returns true if the terminal responds positively or times out (assume support),
false if the terminal explicitly reports no support.
"""
function _detect_osc52_support(term::Base.Terminals.TTYTerminal)
    # Check cache first
    cached = get(ENV, "JULIA_OSC52_SUPPORTED", nothing)
    cached === "1" && return true
    cached === "0" && return false

    io_in = term.in_stream
    io_out = term.out_stream

    # XTGETTCAP query for "Ms" capability (clipboard)
    # Format: DCS + q Pt ST, where Pt is hex-encoded capability name
    # "Ms" in hex is "4d73"
    query = "\033P+q4d73\033\\"

    # Set up for reading response
    old_raw = nothing
    try
        # Save terminal mode and enter raw mode
        old_raw = Base.Terminals.raw!(term, true)

        # Send query
        print(io_out, query)
        flush(io_out)

        # Read response with timeout
        # Expected: DCS 1 + r 4d73=<value> ST (support) or DCS 0 + r ST (no support)
        response = ""
        timeout = time() + 0.1  # 100ms timeout

        while time() < timeout
            if bytesavailable(io_in) > 0
                c = read(io_in, Char)
                response *= c
                # Check if we have a complete response
                # DCS ends with ST (ESC \)
                if endswith(response, "\033\\")
                    break
                end
            else
                sleep(0.001)
            end
        end

        # Parse response
        # DCS 1 + r means capability is supported
        # DCS 0 + r means not supported
        if occursin("\033P1+r", response)
            ENV["JULIA_OSC52_SUPPORTED"] = "1"
            return true
        elseif occursin("\033P0+r", response)
            ENV["JULIA_OSC52_SUPPORTED"] = "0"
            return false
        else
            # No response or unclear - assume support (many terminals
            # support OSC52 but don't respond to XTGETTCAP)
            ENV["JULIA_OSC52_SUPPORTED"] = "1"
            return true
        end
    catch
        # On error, assume OSC52 is supported
        return true
    finally
        # Restore terminal mode
        if old_raw !== nothing
            Base.Terminals.raw!(term, old_raw)
        end
    end
end

"""
    clipboard(io::IO, x; osc52::Bool=true)

Copy content `x` to the system clipboard using OSC52 escape sequences via `io`.

OSC52 (Operating System Command 52) is a terminal escape sequence that allows
applications to set the clipboard content even when running over SSH or in
environments where traditional clipboard tools (xsel, xclip, pbcopy) aren't available.

This works with most modern terminal emulators including:
- iTerm2, Terminal.app (macOS)
- Windows Terminal, ConEmu (Windows)
- Alacritty, kitty, wezterm, foot (cross-platform)

# Arguments
- `io::IO`: The terminal output stream (typically a TTY)
- `x`: Content to copy to clipboard
- `osc52::Bool=true`: Whether to use OSC52 (set to false to disable)

# Example
```julia
clipboard(stdout, "Hello, World!")
```
"""
function clipboard(io::IO, x; osc52::Bool=true)
    osc52 || error("OSC52 disabled and no alternative method specified")

    # Convert content to string
    content = sprint(print, x)

    # Encode as base64
    b64 = base64encode(content)

    # Standard OSC52 sequence
    # Format: ESC ] 52 ; <clipboard> ; <base64 data> BEL
    # Using 'c' for clipboard (some terminals also support 'p' for primary selection)
    print(io, "\033]52;c;", b64, "\007")

    nothing
end

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
            pbpaste_cmd = `reattach-to-user-namespace pbpaste`
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
            :wlclipboard => `wl-copy`
        )
    const _clipboard_paste = Dict(
            :xsel  => Sys.islinux() ?
                `xsel --nodetach --output --clipboard` :
                `xsel -p`,
            :xclip => `xclip -quiet -out -selection clipboard`,
            :wlclipboard => `wl-paste`
        )
    function clipboardcmd()
        global _clipboardcmd
        _clipboardcmd !== nothing && return _clipboardcmd
        for cmd in (:xclip, :xsel, :wlclipboard)
            # wl-clipboard ships wl-copy/paste individually
            c = cmd === :wlclipboard ? Symbol("wl-copy") : cmd
            success(pipeline(`which $c`, devnull)) && return _clipboardcmd = cmd
        end
        pkgs = @static if Sys.KERNEL === :FreeBSD
            "x11/xsel or x11/xclip"
        else
            "xsel or xclip or wl-clipboard"
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
        cmd = _clipboard_paste[c]
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
            errno = cause === :success ? UInt32(0) : Libc.GetLastError()
            if cause !== :OpenClipboard
                if cause !== :success && pdata != C_NULL
                    ccall((:GlobalFree, "kernel32"), stdcall, Cint, (Ptr{UInt16},), pdata)
                end
                ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror(:CloseClipboard) # this should never fail
            end
            cause === :success || Base.windowserror(cause, errno)
            nothing
        end
        ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL) == 0 && return Base.windowserror(:OpenClipboard)
        ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()) == 0 && return cleanup(:EmptyClipboard)
        # copy data to locked, allocated space
        pdata = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{UInt16}, (Cuint, Csize_t), 2 #=GMEM_MOVEABLE=#, sizeof(x_u16))
        pdata == C_NULL && return cleanup(:GlobalAlloc)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        plock == C_NULL && return cleanup(:GlobalLock)
        GC.@preserve x_u16 memcpy(plock, Base.unsafe_convert(Ptr{UInt16}, Base.cconvert(Ptr{UInt16}, x_u16)), sizeof(x_u16))
        unlock = ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), pdata)
        (unlock == 0 && Libc.GetLastError() == 0) || return cleanup(:GlobalUnlock) # this should never fail
        pset = ccall((:SetClipboardData, "user32"), stdcall, Ptr{UInt16}, (Cuint, Ptr{UInt16}), 13, pdata) # CF_UNICODETEXT
        pdata != pset && return cleanup(:SetClipboardData)
        cleanup(:success)
    end
    clipboard(x) = clipboard(sprint(print, x)::String)
    function clipboard()
        function cleanup(cause)
            errno = cause === :success ? UInt32(0) : Libc.GetLastError()
            if cause !== :OpenClipboard
                ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror(:CloseClipboard) # this should never fail
            end
            if cause !== :success && !(cause === :GetClipboardData && (errno == 0x8004006A || errno == 0x800401D3)) # ignore DV_E_CLIPFORMAT and CLIPBRD_E_BAD_DATA from GetClipboardData
                Base.windowserror(cause, errno)
            end
            ""
        end
        ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL) == 0 && return Base.windowserror(:OpenClipboard)
        ccall(:SetLastError, stdcall, Cvoid, (UInt32,), 0) # allow distinguishing if the clipboard simply didn't have text
        pdata = ccall((:GetClipboardData, "user32"), stdcall, Ptr{UInt16}, (Cuint,), 13) # CF_UNICODETEXT
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
    clipboard()::String

Return a string with the contents of the operating system clipboard ("paste").
"""
clipboard()
