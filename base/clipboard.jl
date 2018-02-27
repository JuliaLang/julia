# This file is a part of Julia. License is MIT: https://julialang.org/license

# clipboard copy and paste

if Sys.isapple()
    function clipboard(x)
        open(pipeline(`pbcopy`, stderr=stderr), "w") do io
            print(io, x)
        end
    end
    clipboard() = read(`pbpaste`, String)

elseif Sys.islinux() || Sys.KERNEL === :FreeBSD
    _clipboardcmd = nothing
    const _clipboardcmds = Dict(
        :copy => Dict(
            :xsel  => Sys.islinux() ?
                `xsel --nodetach --input --clipboard` : `xsel -c`,
            :xclip => `xclip -silent -in -selection clipboard`,
        ),
        :paste => Dict(
            :xsel  => Sys.islinux() ?
                `xsel --nodetach --output --clipboard` : `xsel -p`,
            :xclip => `xclip -quiet -out -selection clipboard`,
        )
    )
    function clipboardcmd()
        global _clipboardcmd
        _clipboardcmd !== nothing && return _clipboardcmd
        for cmd in (:xclip, :xsel)
            success(pipeline(`which $cmd`, devnull)) && return _clipboardcmd = cmd
        end
        pkgs = @static if Sys.islinux()
            "xsel or xclip"
        elseif Sys.KERNEL === :FreeBSD
            "x11/xsel or x11/xclip"
        end
        error("no clipboard command found, please install $pkgs")
    end
    function clipboard(x)
        c = clipboardcmd()
        cmd = get(_clipboardcmds[:copy], c, nothing)
        if cmd === nothing
            error("unexpected clipboard command: $c")
        end
        open(pipeline(cmd, stderr=stderr), "w") do io
            print(io, x)
        end
    end
    function clipboard()
        c = clipboardcmd()
        cmd = get(_clipboardcmds[:paste], c, nothing)
        if cmd === nothing
            error("unexpected clipboard command: $c")
        end
        read(pipeline(cmd, stderr=stderr), String)
    end

elseif Sys.iswindows()
    # TODO: these functions leak memory and memory locks if they throw an error
    function clipboard(x::AbstractString)
        if containsnul(x)
            throw(ArgumentError("Windows clipboard strings cannot contain NUL character"))
        end
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL))
        systemerror(:EmptyClipboard, 0==ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()))
        x_u16 = cwstring(x)
        # copy data to locked, allocated space
        p = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{UInt16}, (UInt16, Int32), 2, sizeof(x_u16))
        systemerror(:GlobalAlloc, p==C_NULL)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), p)
        systemerror(:GlobalLock, plock==C_NULL)
        ccall(:memcpy, Ptr{UInt16}, (Ptr{UInt16},Ptr{UInt16},Int), plock, x_u16, sizeof(x_u16))
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{Cvoid},), plock))
        pdata = ccall((:SetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32, Ptr{UInt16}), 13, p)
        systemerror(:SetClipboardData, pdata!=p)
        ccall((:CloseClipboard, "user32"), stdcall, Cvoid, ())
    end
    clipboard(x) = clipboard(sprint(print, x)::String)
    function clipboard()
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL))
        pdata = ccall((:GetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32,), 13)
        systemerror(:SetClipboardData, pdata==C_NULL)
        systemerror(:CloseClipboard, 0==ccall((:CloseClipboard, "user32"), stdcall, Cint, ()))
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        systemerror(:GlobalLock, plock==C_NULL)
        # find NUL terminator (0x0000 16-bit code unit)
        len = 0
        while unsafe_load(plock, len+1) != 0; len += 1; end
        # get Vector{UInt16}, transcode data to UTF-8, make a String of it
        s = transcode(String, unsafe_wrap(Array, plock, len))
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), plock))
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
