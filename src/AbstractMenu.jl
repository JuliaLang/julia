abstract type AbstractMenu end

function request(msg::AbstractString, m::AbstractMenu)
    println(msg)
    request(m)
end

header(m::AbstractMenu) = ""
keypress(m::AbstractMenu, i::UInt32) = false
pick(m::AbstractMenu, cursor::Int) = error("unimplemented")
cancel(m::AbstractMenu) = error("unimplemented")
options(m::AbstractMenu) = error("unimplemented")
function writeLine(buf::IOBuffer, menu::AbstractMenu, idx::Int, cursor::Bool)
    error("unimplemented")
end

function request(m::AbstractMenu)
    cursor = 1

    menu_header = header(m)
    if menu_header != ""
        println(header(m))
    end

    printMenu(m, cursor, init=true)

    enableRawMode()
    print("\x1b[?25l") # hide the cursor
    try
        while true
            c = readKey()
            if c == Int(ARROW_UP)
                # move selection
                cursor -= cursor > 1 ? 1 : 0
                # scroll the page
                if cursor < (2+m.pageoffset) && m.pageoffset > 0
                    m.pageoffset -= 1
                end
            elseif c == Int(ARROW_DOWN)
                # move selection
                cursor += cursor < length(options(m)) ? 1 : 0
                # scroll page
                if cursor >= m.pagesize + m.pageoffset && m.pagesize + m.pageoffset < length(options(m))
                    m.pageoffset += 1
                end
            elseif c == 13 # <enter>
                # will break if pick returns true
                pick(m, cursor) && break
            elseif c == UInt32('q') || c == 3 # ctrl-c (cancel)
                cancel(m)
                break
            else
                # will break if keypress returns true
                keypress(m, c) && break
            end

            printMenu(m, cursor)
        end
    finally
        # always disable raw mode even even if there is an
        #  exception in the above loop
        print("\x1b[?25h") #unhide cursor
        disableRawMode()
    end
    println()

    return m.selected
end

function printMenu(m::AbstractMenu, cursor::Int; init=false)
    buf = IOBuffer()

    # Move the cursor to the begining of where it should print
    # Don't do this on the initial print
    lines = m.pagesize-1
    !init && print(buf, "\x1b[999D\x1b[$(lines)A")

    for i in (m.pageoffset+1):(m.pageoffset + m.pagesize)
        print(buf, "\x1b[2K")

        if i ==  m.pageoffset+1 && m.pageoffset > 0
            # first line && scrolled past first entry
            print(buf, CONFIG[:up_arrow])
        elseif i == m.pagesize+m.pageoffset && i != length(options(m))
            # last line && not last option
            print(buf, CONFIG[:down_arrow])
        else
            # non special line
            print(buf, " ")
        end

        writeLine(buf, m, i, i == cursor)

        # dont print an \r\n on the last line
        i != (m.pagesize+m.pageoffset) && print(buf, "\r\n")
    end

    print(String(take!(buf)))
end
