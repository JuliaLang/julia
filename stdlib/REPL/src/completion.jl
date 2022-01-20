# This file is a part of Julia. License is MIT: https://julialang.org/license

# Stores the state of a running completion menu
mutable struct CompletionMenu
    # partial string for completion
    partial::String
    # position to splice partial to the buffer
    position::Int

    # candidates of completion
    completions::Vector{String}
    # selected completion candidate (index of the `completions` field);
    # the value is zero if none is selected
    selected::Int

    # dimensions of the completion table
    nrows::Int
    ncols::Int

    CompletionMenu(partial, position, completions) =
        new(partial, position, completions, 0, 0, 0)
end

Base.length(menu::CompletionMenu) = length(menu.completions)
Base.firstindex(menu::CompletionMenu) = firstindex(menu.completions)
Base.lastindex(menu::CompletionMenu) = lastindex(menu.completions)
Base.getindex(menu::CompletionMenu, i::Integer) = menu.completions[i]

hasselected(menu::CompletionMenu) = menu.selected != 0
selected(menu::CompletionMenu) = menu.completions[menu.selected]

dimensions(menu::CompletionMenu) = (menu.nrows, menu.ncols)

function set_dimensions!(menu::CompletionMenu, (nrows, ncols)::Tuple{Int,Int})
    menu.nrows = nrows
    menu.ncols = ncols
    return menu
end

function select_next!(menu::CompletionMenu)
    menu.selected = mod1(menu.selected + 1, length(menu.completions))
    return menu
end

function select_previous!(menu::CompletionMenu)
    menu.selected = mod1(menu.selected - 1, length(menu.completions))
    return menu
end

function select_right!(menu::CompletionMenu)
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

function select_left!(menu::CompletionMenu)
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

# show completion menu as a table to terminal
function show_completion_menu(terminal::TextTerminal, menu::CompletionMenu, height::Integer, width::Integer)
    # calculate dimensions for menu display
    colwidth = maximum(textwidth, menu.completions)
    margin = 2
    nrows, ncols = calc_dimensions(width, colwidth + margin, length(menu))
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
            rest = chopprefix(completion, menu.partial)
            # highlight prefix (partial)
            text = string(
                "\x1b[1m\x1b[4m", menu.partial, "\x1b[22m\x1b[24m",
                rest,
                ' '^(colwidth - textwidth(completion))
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
