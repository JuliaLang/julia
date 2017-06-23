CONFIG = Dict()

function config(;charset::Symbol = :na,
                cursor::Char = '\0',
                up_arrow::Char = '\0',
                down_arrow::Char = '\0',
                checked::String = "",
                unchecked::String = "")

    if !(charset in [:na, :ascii, :unicode])
        error("charset should be :ascii or :unicode, recieved $charset")
    end

    if charset == :ascii
        cursor = '>'
        up_arrow = '^'
        down_arrow = 'v'
        checked = "[X]"
        unchecked = "[ ]"
    elseif charset == :unicode
        cursor = '→'
        up_arrow = '↑'
        down_arrow = '↓'
        checked = "✓"
        unchecked = "⬚"
    end

    cursor != '\0'      && (CONFIG[:cursor] = cursor)
    up_arrow != '\0'    && (CONFIG[:up_arrow] = up_arrow)
    down_arrow != '\0'  && (CONFIG[:down_arrow] = down_arrow)
    checked != ""       && (CONFIG[:checked] = checked)
    unchecked != ""     && (CONFIG[:unchecked] = unchecked)

    # Don't return anything
    nothing
end

# Set up defaults
config(charset=:ascii)
