include("util.jl")

type RadioMenu <: AbstractMenu
    options::Array{String,1}
    default::Int
    pagesize::Int
    pageoffset::Int
end

function RadioMenu(options::Array{String,1}; default=1, pagesize=10)
    length(options) < 2 && error("RadioMenu must have at least two options")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 2
    pagesize < 2 && error("pagesize must be >= 2")

    # default is out of range
    default = clamp(default, 1, length(options))

    pageoffset = min(default, 1+length(options)-pagesize) - 1

    RadioMenu(options, default, pagesize, pageoffset)
end

function request(m::RadioMenu)
    selected = m.default

    printMenu(m, selected, init=true)

    enableRawMode()
    print("\x1b[?25l") # hide the cursor
    try
        while true
            c = readKey()
            if c == Int(ARROW_UP)
                # move selection
                selected -= selected > 1 ? 1 : 0
                # scroll the page
                if selected < (2+m.pageoffset) && m.pageoffset > 0
                    m.pageoffset -= 1
                end
            elseif c == Int(ARROW_DOWN)
                # move selection
                selected += selected < length(m.options) ? 1 : 0
                # scroll page
                if selected >= m.pagesize + m.pageoffset && m.pagesize + m.pageoffset < length(m.options)
                    m.pageoffset += 1
                end
            elseif c == 13 # <enter>
                break
            elseif c == UInt32('q') || c == 3 # ctrl-c (cancel)
                selected = -1
                break
            end

            printMenu(m, selected)
        end
    finally
        # always disable raw mode even even if there is an
        #  exception in the above loop
        print("\x1b[?25h") #unhide cursor
        disableRawMode()
    end
    println()

    return selected

end

function printMenu(m::RadioMenu, selected::Int; init=false)
    buf = IOBuffer()

    # Move the cursor to the begining of where it should print
    # Don't do this on the initial print
    lines = m.pagesize-1
    !init && print(buf, "\x1b[999D\x1b[$(lines)A")

    for i in (m.pageoffset+1):(m.pageoffset + m.pagesize)
        print(buf, "\x1b[2K")

        if i ==  m.pageoffset+1 && m.pageoffset > 0
            # first line && scrolled past first entry
            print(buf, "^")
        elseif i == m.pagesize+m.pageoffset && i != length(m.options)
            # last line && not last option
            print(buf, "v")
        else
            # non special line
            print(buf, " ")
        end

        # print a ">" on the selected entry
        i == selected ? print(buf, "> ") : print(buf, "  ")

        print(buf, clean(m.options[i]))

        # dont print an \r\n on the last line
        i != (m.pagesize+m.pageoffset) && print(buf, "\r\n")
    end

    print(takebuf_string(buf))
end
