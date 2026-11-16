# This file is a part of Julia. License is MIT: https://julialang.org/license

# Drive the browser through its views so the first `help?>` Enter opens it without compiling.
# Only links that stay inside the browser are followed: no editor, web browser or loading.
function precompile_workload()
    function terminal(input::String)
        out = IOContext(IOBuffer(), :displaysize => (32, 120), :color => true)
        return REPL.Terminals.TTYTerminal("xterm", IOBuffer(input), out, IOBuffer())
    end
    b = Browser(terminal(""))
    frame(b)
    keys = Any['j', 'j', 'k', :right, 'j', 'j', :enter, :tab, :tab, :backtab, :down, :pagedown,
               :up, :left, :left, 'a', 'i', 'i', '}', '{', 'w', 'w', 'w', 'w', '?', :escape, '[', ']',
               :backspace, :end, :home]
    for k in keys
        handle!(b, k isa Char ? KeyEvent(k) : KeyEvent(k))
        frame(b)
    end
    follow!(b, NameTarget(Base, :sort!))
    frame(b)
    toggle_flag!(b, :extended)
    frame(b)
    follow!(b, NameTarget(Base, :AbstractVector))
    frame(b)
    follow!(b, ModuleTarget(Base.Iterators))
    follow!(b, AnchorTarget(1))
    frame(b)
    follow!(b, SectionTarget(:julia))
    frame(b)
    if !isempty(b.cat.manual)
        follow!(b, ManualTarget(first(b.cat.manual).path, ""))
        frame(b)
        b.focus = :names
        handle!(b, KeyEvent(:down))
        frame(b)
    end
    for ev in ("/", "s", "o", "r", "t", "\x14", "\x7f", "\e[B", "\e[A", "\t", "\e")
        e = read_event(IOBuffer(ev))
        e === nothing || handle!(b, e)
        frame(b)
    end
    for ev in ("\e[<35;40;8M", "\e[<0;40;8M", "\e[<0;40;8m", "\e[<0;5;5M", "\e[<0;5;5m",
               "\e[<65;90;9M", "\e[<64;90;9M", "\e[<0;100;1M", "\e[200~iter\e[201~", "\e[1;3D",
               "\e]11;rgb:0000/0000/0000\e\\", "\e[?62c")
        e = read_event(IOBuffer(ev))
        e === nothing || handle!(b, e)
        frame(b)
    end
    # Selecting text in the documentation, without touching the clipboard.
    copy = COPY[]
    COPY[] = (b, text) -> ""
    try
        follow!(b, NameTarget(Base, :sort!))
        for ev in ("\e[<0;70;6M", "\e[<32;90;9M", "\e[<0;90;9m", "\e[<0;70;7M", "\e[<0;70;7m",
                   "\e[<0;70;7M", "\e[<0;70;7m", "\e[<0;70;7M", "\e[<0;70;7m")
            handle!(b, read_event(IOBuffer(ev))::MouseEvent)
            frame(b)
        end
        handle!(b, KeyEvent(:escape))
    finally
        COPY[] = copy
    end
    # The card that previews a name the pointer rests on.
    follow!(b, NameTarget(Base, :sort!))
    frame(b)
    for h in b.hits
        h.action isa Tuple && first(h.action) === :names || continue
        handle!(b, MouseEvent(:move, 0, h.row, first(h.cols), 0))
        b.hover_since = 0.0
        show_card!(b)
        frame(b)
        break
    end
    hide_card!(b)
    for tip in eachindex(TIPS)
        start_page(b.cat, 80, tip)
    end
    unloaded_page(PkgId(UUID(UInt128(1)), "Example"), 80)
    # The event loop, the screen set-up and the entry points.
    browse(terminal("jjlj\ty"); query = "sum")
    browse(terminal("q"), Base.Iterators)
    browse(terminal("q"))
    LAST[] = nothing
    LAYOUT[] = :auto
    FIRST_OPEN[] = true
    nothing
end

if Base.generating_output()
    ccall(:jl_tag_newly_inferred_enable, Cvoid, ())
    try
        precompile_workload()
    finally
        ccall(:jl_tag_newly_inferred_disable, Cvoid, ())
    end
end
