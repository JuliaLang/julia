# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type CompletionMenu end

# Vector-like operations
Base.length(menu::CompletionMenu) = length(menu.completions)
Base.firstindex(menu::CompletionMenu) = firstindex(menu.completions)
Base.lastindex(menu::CompletionMenu) = lastindex(menu.completions)
Base.getindex(menu::CompletionMenu, i::Integer) = menu.completions[i]

struct BashCompletionMenu <: CompletionMenu
    # partial string for completion
    partial::String
    # position to splice `partial` into the buffer
    position::Int

    # candidates of completion
    completions::Vector{String}

    # dimensions of the completion table
    colwidth::Int

    function BashCompletionMenu(partial, position, completions)
        colwidth = maximum(textwidth, completions)
        return new(partial, position, completions, colwidth)
    end
end

isselectable(menu::BashCompletionMenu) = false
hasselected(menu::BashCompletionMenu) = false

# `height` is not used
function show_completion_menu(terminal::TextTerminal, menu::BashCompletionMenu, width::Integer, height::Int)
    # calculate dimensions for menu display
    margin = 2
    nrows, ncols = calc_dimensions(width, menu.colwidth + margin, length(menu))

    # list completion candidates as a table
    for r in 1:nrows
        for c in 1:ncols
            i = (c - 1) * nrows + r
            i > lastindex(menu) && break
            completion = menu[i]
            text = string(menu[i], ' '^(menu.colwidth - textwidth(completion)))
            print(terminal, text, ' '^margin)
        end
        if r != nrows
            println(terminal)
        end
    end
    println(terminal)

    return nrows
end

mutable struct FishCompletionMenu <: CompletionMenu
    # partial string for completion
    partial::String
    # position to splice `partial` into the buffer
    position::Int

    # candidates of completion
    completions::Vector{String}
    # selected completion candidate (index of the `completions` field);
    # the value is zero if none is selected
    selected::Int

    # dimensions of the completion table
    colwidth::Int
    nrows::Int
    ncols::Int

    function FishCompletionMenu(partial, position, completions)
        colwidth = maximum(textwidth, completions)
        return new(partial, position, completions, 0, colwidth, 0, 0)
    end
end

isselectable(menu::FishCompletionMenu) = true
hasselected(menu::FishCompletionMenu) = menu.selected != 0
selected(menu::FishCompletionMenu) = menu.completions[menu.selected]

dimensions(menu::FishCompletionMenu) = (menu.nrows, menu.ncols)

function set_dimensions!(menu::FishCompletionMenu, (nrows, ncols)::Tuple{Int,Int})
    menu.nrows = nrows
    menu.ncols = ncols
    return menu
end

function select_next!(menu::FishCompletionMenu)
    menu.selected = mod1(menu.selected + 1, length(menu.completions))
    return menu
end

function select_previous!(menu::FishCompletionMenu)
    menu.selected = mod1(menu.selected - 1, length(menu.completions))
    return menu
end

function select_right!(menu::FishCompletionMenu)
    if !hasselected(menu)
        menu.selected = firstindex(menu.completions)
    else
        nrows, ncols = dimensions(menu)
        n_items_per_page = nrows * ncols
        npages = fld1(length(menu), n_items_per_page)
        page, offset = divrem(menu.selected - 1, n_items_per_page)
        col, row = divrem(offset, nrows)  # in-page col/row (0-based)
        if col == ncols - 1
            col = 0
            page = mod(page + 1, npages)
        else
            col += 1
        end
        right = page * n_items_per_page + col * nrows + row + 1
        if right > lastindex(menu)
            right = firstindex(menu)
        end
        menu.selected = right
    end
    return menu
end

function select_left!(menu::FishCompletionMenu)
    if !hasselected(menu)
        menu.selected = lastindex(menu.completions)
    else
        nrows, ncols = dimensions(menu)
        n_items_per_page = nrows * ncols
        npages = fld1(length(menu), n_items_per_page)
        page, offset = divrem(menu.selected - 1, n_items_per_page)
        col, row = divrem(offset, nrows)  # in-page col/row (0-based)
        if col == 0
            col = ncols - 1
            page = mod(page - 1, npages)
        else
            col -= 1
        end
        left = page * n_items_per_page + col * nrows + row + 1
        if left > lastindex(menu)
            left = lastindex(menu)
        end
        menu.selected = left
    end
    return menu
end

function show_completion_menu(terminal::TextTerminal, menu::FishCompletionMenu, width::Integer, height::Integer)
    # calculate dimensions for menu display
    margin = 2
    nrows, ncols = calc_dimensions(width, menu.colwidth + margin, length(menu))
    statusbar = false
    if nrows > height
        # completion menu won't fit in a page
        nrows = height - 1
        statusbar = true
    end
    n_items_per_page = nrows * ncols

    # list completion candidates as a table
    selected = menu.selected
    first = hasselected(menu) ? (selected - 1) ÷ n_items_per_page * n_items_per_page : 0
    print(terminal, "\x1b[0J")
    for r in 1:nrows
        for c in 1:ncols
            i = (c - 1) * nrows + r + first
            i > lastindex(menu) && break
            completion = menu[i]
            prefix = menu.partial
            rest = chopprefix(completion, prefix)
            # highlight prefix (partial)
            text = string(
                "\x1b[1m\x1b[4m", prefix, "\x1b[22m\x1b[24m",
                rest,
                ' '^(menu.colwidth - textwidth(completion))
            )
            if i == selected
                # highlight selected candidate
                text = string("\x1b[48;5;240m", text, "\x1b[0m")
            end
            print(terminal, text, ' '^margin)
        end
        if r != nrows
            println(terminal)
        end
    end

    # update nrows and ncols (these may change if the terminal is resized)
    set_dimensions!(menu, (nrows, ncols))

    # show status bar if needed
    if statusbar
        lo = first + 1
        hi = min(first + n_items_per_page, length(menu))
        println(terminal)
        printstyled(terminal, "$(lo)-$(hi) out of $(length(menu))", reverse = true)
    end

    # return the number of written lines
    return nrows + statusbar
end

# calculate the number of rows and columns
function calc_dimensions(tablewidth, colwidth, len)
    n_max_cols = max(tablewidth ÷ colwidth, 1)
    nrows = (len - 1) ÷ n_max_cols + 1
    ncols = (len - 1) ÷ nrows + 1
    return nrows, ncols
end
