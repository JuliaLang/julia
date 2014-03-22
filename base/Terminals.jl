module Terminals
    import Base.size, Base.write, Base.flush
    abstract TextTerminal <: Base.IO
    export TextTerminal, NCurses, writepos, cmove, pos, getX, getY, hascolor

    # Stuff that really should be in a Geometry package
    immutable Rect
        top
        left
        width
        height
    end

    immutable Size
        width
        height
    end 


    # INTERFACE
    size(::TextTerminal) = error("Unimplemented")
    writepos(t::TextTerminal,x,y,s::Array{Uint8,1}) = error("Unimplemented")
    cmove(t::TextTerminal,x,y) = error("Unimplemented")
    getX(t::TextTerminal) = error("Unimplemented")
    getY(t::TextTerminal) = error("Unimplemented")
    pos(t::TextTerminal) = (getX(t),getY(t))

    # Relative moves (Absolute position fallbacks)
    export cmove_up, cmove_down, cmove_left, cmove_right, cmove_line_up, cmove_line_down, cmove_col

    cmove_up(t::TextTerminal,n) = cmove(getX(),max(1,getY()-n))
    cmove_up(t) = cmove_up(t,1)

    cmove_down(t::TextTerminal,n) = cmove(getX(),max(height(t),getY()+n))
    cmove_down(t) = cmove_down(t,1)

    cmove_left(t::TextTerminal,n) = cmove(max(1,getX()-n),getY())
    cmove_left(t) = cmove_left(t,1)

    cmove_right(t::TextTerminal,n) = cmove(max(width(t),getX()+n),getY())
    cmove_right(t) = cmove_right(t,1)

    cmove_line_up(t::TextTerminal,n) = cmove(1,max(1,getY()-n))
    cmove_line_up(t) = cmove_line_up(t,1)

    cmove_line_down(t::TextTerminal,n) = cmove(1,max(height(t),getY()+n))
    cmove_line_down(t) = cmove_line_down(t,1)

    cmove_col(t::TextTerminal,c) = comve(c,getY())

    # Defaults
    hascolor(::TextTerminal) = false

    # Utility Functions
    function writepos{T}(t::TextTerminal, x, y, b::Array{T})
        if isbits(T)
            writepos(t,x,y,reinterpret(Uint8,b))
        else
            cmove(t,x,y)
            invoke(write, (IO, Array), s, a)
        end
    end
    function writepos(t::TextTerminal,x,y,args...)
        cmove(t,x,y)
        write(t,args...)
    end 
    width(t::TextTerminal) = size(t).width
    height(t::TextTerminal) = size(t).height

    # For terminals with buffers
    flush(t::TextTerminal) = nothing

    clear(t::TextTerminal) = error("Unimplemented")
    clear_line(t::TextTerminal,row) = error("Unimplemented")
    clear_line(t::TextTerminal) = error("Unimplemented")

    raw!(t::TextTerminal,raw::Bool) = error("Unimplemented")

    beep(t::TextTerminal) = nothing

    abstract TextAttribute

    module Attributes
        # This is just to get started and will have to be revised

        import Terminals.TextAttribute, Terminals.TextTerminal

        export Standout, Underline, Reverse, Blink, Dim, Bold, AltCharset, Invisible, Protect, Left, Right, Top,
                Vertical, Horizontal, Low

        macro flag_attribute(name)
            quote
                immutable $name <: TextAttribute
                end
            end
        end

        @flag_attribute Standout
        @flag_attribute Underline
        @flag_attribute Reverse
        @flag_attribute Blink
        @flag_attribute Dim
        @flag_attribute Bold
        @flag_attribute AltCharset
        @flag_attribute Invisible
        @flag_attribute Protect
        @flag_attribute Left
        @flag_attribute Right
        @flag_attribute Top
        @flag_attribute Vertical
        @flag_attribute Horizontal
        @flag_attribute Low

        attr_simplify(::TextTerminal, x::TextAttribute) = x
        attr_simplify{T<:TextAttribute}(::TextTerminal, ::Type{T}) = T()
        function attr_simplify(::TextTerminal, s::Symbol)
            if s == :standout 
                return Standout()
            elseif s == :underline 
                return Underline()
            elseif s == :reverse 
                return Reverse()
            elseif s == :blink
                return Blink()
            end
        end


    end

    module Colors 
        import Terminals.TextAttribute, Terminals.TextTerminal, Terminals.Attributes.attr_simplify
        using Color

        export TerminalColor, TextColor, BackgroundColor, ForegroundColor, approximate,
                lookup_color, terminal_color, maxcolors, maxcolorpairs, palette, numcolors

        # Represents a color actually displayable by the current terminal
        abstract TerminalColor

        immutable TextColor <: TextAttribute
            c::TerminalColor
        end
        immutable BackgroundColor <: TextAttribute
            c::TerminalColor
        end

        function approximate(t::TextTerminal, c::ColorValue)
            x = keys(palette(t))
            lookup_color(t,x[indmin(map(x->colordiff(c,x),x))])
        end

        attr_simplify(t::TextTerminal, c::ColorValue) = TextColor(lookup_color(t,c))

        # Terminals should implement this
        lookup_color(t::TextTerminal) = error("Unimplemented")
        maxcolors(t::TextTerminal) = error("Unimplemented")
        maxcolorpairs(t::TextTerminal) = error("Unimplemented")
        palette(t::TextTerminal) = error("Unimplemented")
        numcolors(t::TextTerminal) = error("Unimplemented")
    end

    module Unix
        importall Terminals

        import Terminals: width, height, cmove, Rect, Size, getX, 
                          getY, raw!, clear, clear_line, beep, hascolor
        import Base: size, read, write, flush, TTY, writemime, readuntil, start_reading, stop_reading

        export UnixTerminal

        type UnixTerminal <: TextTerminal
            term_type
            in_stream::TTY
            out_stream::TTY
            err_stream::TTY
        end

        const CSI = "\x1b["

        cmove_up(t::UnixTerminal,n) = write(t.out_stream,"$(CSI)$(n)A")
        cmove_down(t::UnixTerminal,n) = write(t.out_stream,"$(CSI)$(n)B")
        cmove_right(t::UnixTerminal,n) = write(t.out_stream,"$(CSI)$(n)C")
        cmove_left(t::UnixTerminal,n) = write(t.out_stream,"$(CSI)$(n)D")
        cmove_line_up(t::UnixTerminal,n) = (cmove_up(t,n);cmove_col(t,0))
        cmove_line_down(t::UnixTerminal,n) = (cmove_down(t,n);cmove_col(t,0))
        cmove_col(t::UnixTerminal,n) = write(t.out_stream,"$(CSI)$(n)G")

        raw!(t::UnixTerminal,raw::Bool) = ccall(:uv_tty_set_mode,Int32,(Ptr{Void},Int32),t.in_stream.handle,raw?1:0)!=-1
        enable_bracketed_paste(t::UnixTerminal) = write(t.out_stream,"$(CSI)?2004h")
        disable_bracketed_paste(t::UnixTerminal) = write(t.out_stream,"$(CSI)?2004l")

        function size(t::UnixTerminal)
            s = Array(Int32,2)
            Base.uv_error("size (TTY)",ccall(:uv_tty_get_winsize,Int32,(Ptr{Void},Ptr{Int32},Ptr{Int32}),t.out_stream.handle,pointer(s,1),pointer(s,2))!=0)
            Size(s[1],s[2])
        end

        clear(t::UnixTerminal) = write(t.out_stream,"\x1b[H\x1b[2J")
        clear_line(t::UnixTerminal) = write(t.out_stream,"\x1b[0G\x1b[0K")
        beep(t::UnixTerminal) = write(t.err_stream,"\x7")

        write{T,N}(t::UnixTerminal,a::Array{T,N}) = write(t.out_stream,a)
        write(t::UnixTerminal,p::Ptr{Uint8}) = write(t.out_stream,p)
        write(t::UnixTerminal,p::Ptr{Uint8},x::Integer) = write(t.out_stream,p,x)
        write(t::UnixTerminal,x::Uint8) = write(t.out_stream,x)
        read{T,N}(t::UnixTerminal,x::Array{T,N}) = read(t.in_stream,x)
        readuntil(t::UnixTerminal,s::String) = readuntil(t.in_stream,s)
        readuntil(t::UnixTerminal,c::Char) = readuntil(t.in_stream,c) 
        readuntil(t::UnixTerminal,s) = readuntil(t.in_stream,s)
        read(t::UnixTerminal,::Type{Uint8}) = read(t.in_stream,Uint8)
        start_reading(t::UnixTerminal) = start_reading(t.in_stream)
        stop_reading(t::UnixTerminal) = stop_reading(t.in_stream)


        hascolor(t::UnixTerminal) = (beginswith(t.term_type,"xterm") || success(`tput setaf 0`))
        #writemime(t::UnixTerminal, ::MIME"text/plain", x) = writemime(t.out_stream, MIME("text/plain"), x)
    end
    importall .Unix
    export UnixTerminal
end