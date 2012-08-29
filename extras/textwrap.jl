# Text wrapping
# Julia port of Python's TextWrap module

# Copyright (C) 1999-2001 Gregory P. Ward.
# Copyright (C) 2002, 2003 Python Software Foundation.
# Written by Greg Ward <gward@python.net>
# Julia port by Carlo Baldassi <carlobaldassi@gmail.com>

# Differences to Python version:
# 
# 1) wrapper functions take a String as a first argument
#    and an (optional) Options as second argument. Options
#    names are the same as Python's.
# 2) function 'fill' renamed to 'wrapped_string'
# 3) added convenience functions 'print_wrapped' and
#    'println_wrapped' which just print the result of
#    'wrapped_string' and take and optional io::IO as
#    their first argument

require("options.jl")

module TextWrap
import Base.*
import OptionsMod.*

export
    wrap,
    wrapped_string,
    print_wrapped,
    println_wrapped,
    dedent

# unlike Julia's native split, this keeps the separators in the
# resulting Vector (but unlike Python's it discards empty strings)
_pylike_split(str::String, splitter) = _pylike_split(utf8(str), splitter)
function _pylike_split(str::UTF8String, splitter)
    strs = String[]
    i = start(str)
    n = length(str)
    j, k = search(str,splitter,i)
    while 0 < j <= n
        if i < k
            if i < j
                push(strs, str[i:j-1])
            end
            i = k
        end
        push(strs, str[j:k-1])
        if k <= j; k = nextind(str,j) end
        j, k = search(str,splitter,k)
    end
    if !done(str,i)
        push(strs, str[i:])
    end
    return strs
end

function _expand_tabs(text::String)
    out_buf = memio(0, false)
    i = 0
    for c in text
        if c == '\t'
            while i < 8
                print(out_buf, ' ')
                i += 1
            end
            i = 0
        elseif c == '\n' || c == '\r'
            print(out_buf, c)
            i = 0
        else
            print(out_buf, c)
            i = (i + 1) % 8
        end
    end
    return takebuf_string(out_buf)
end

function _translate_whitespace(text::String)
    # Hardcode the recognized whitespace characters to the US-ASCII
    # whitespace characters.  The main reason for doing this is that in
    # ISO-8859-1, 0xa0 is non-breaking whitespace, so in certain locales
    # that character winds up in string.whitespace.  Respecting
    # string.whitespace in those cases would 1) make textwrap treat 0xa0 the
    # same as any other whitespace char, which is clearly wrong (it's a
    # *non-breaking* space), 2) possibly cause problems with Unicode,
    # since 0xa0 is not in range(128).
    whitespace = ['\t', '\n', '\x0b', '\x0c', '\r']
    out_buf = memio(0, false)
    for c in text
        found = false
        for w in whitespace
            if c == w
                print(out_buf, ' ')
                found = true
            end
        end
        if !found
            print(out_buf, c)
        end
    end
    return takebuf_string(out_buf)
end


function _munge_whitespace(text::String, expand_tabs::Bool, replace_whitespace::Bool)
    # Munge whitespace in text: expand tabs and convert all other
    # whitespace characters to spaces.  Eg. " foo\tbar\n\nbaz"
    # becomes " foo    bar  baz".
    if expand_tabs
        text = _expand_tabs(text) # TODO
    end
    if replace_whitespace
        text = _translate_whitespace(text)
    end
    return text
end

function _split(text::String, break_on_hyphens::Bool)
    # Split the text to wrap into indivisible chunks.  Chunks are
    # not quite the same as words; see _wrap_chunks() for full
    # details.  As an example, the text
    #   Look, goof-ball -- use the -b option!
    # breaks into the following chunks:
    #   "Look,", " ", "goof-", "ball", " ", "--", " ",
    #   "use", " ", "the", " ", "-b", " ", "option!"
    # if break_on_hyphens is True, or in:
    #   "Look,", " ", "goof-ball", " ", "--", " ",
    #   "use", " ", "the", " ", "-b", " ", option!"
    # otherwise.

    _wordsep_re = 
        r"(\s+|
           [^\s\w]*\w+[^0-9\W]-(?=\w+[^0-9\W])|
           (?<=[\w\!\"\'\&\.\,\?])-{2,}(?=\w))"x

    _wordsep_simple_re = r"(\s+)"

    if break_on_hyphens
        chunks = _pylike_split(text, _wordsep_re)
    else
        chunks = _pylike_split(text, _wordsep_simple_re)
    end
    return chunks
end

function _fix_sentence_endings{T<:String}(chunks::Vector{T})
    # Correct for sentence endings buried in 'chunks'.  Eg. when the
    # original text contains "... foo.\nBar ...", _munge_whitespace()
    # and _split() will convert that to [..., "foo.", " ", "Bar", ...]
    # which has one too few spaces; this method simply changes the one
    # space to two.

    _sentence_end_re = r"[a-z][\.\!\?][\"\']?\Z"

    i = 1
    while i < length(chunks)
        if chunks[i+1] == " " && ismatch(_sentence_end_re, chunks[i])
            chunks[i+1] = "  "
            i += 2
        else
            i += 1
        end
    end
    return chunks
end

function _handle_long_word{T<:String}(reversed_chunks::Vector{T}, cur_line::Vector{T}, cur_len::Integer, width::Integer, break_long_words::Bool)
    # Handle a chunk of text (most likely a word, not whitespace) that
    # is too long to fit in any line.
    
    # Figure out when indent is larger than the specified width, and make
    # sure at least one character is stripped off on every pass
    if width < 1
        space_left = 1
    else
        space_left = width - cur_len
    end

    # If we're allowed to break long words, then do so: put as much
    # of the next chunk onto the current line as will fit.
    if break_long_words
        push(cur_line, reversed_chunks[end][1:space_left])
        reversed_chunks[end] = reversed_chunks[end][space_left+1:end]

    # Otherwise, we have to preserve the long word intact.  Only add
    # it to the current line if there's nothing already there --
    # that minimizes how much we violate the width constraint.
    elseif isempty(cur_line)
        push(cur_line, pop(reversed_chunks))

    # If we're not allowed to break long words, and there's already
    # text on the current line, do nothing.  Next time through the
    # main loop of _wrap_chunks(), we'll wind up here again, but
    # cur_len will be zero, so the next line will be entirely
    # devoted to the long word that we can't handle right now.
    end
    return reversed_chunks, cur_line
end

function _wrap_chunks{T<:String}(chunks::Vector{T}, width::Integer,
    initial_indent::String, subsequent_indent::String,
    drop_whitespace::Bool, break_long_words::Bool)
    # Wrap a sequence of text chunks and return a list of lines of
    # length 'opts.width' or less.  (If 'break_long_words' is false,
    # some lines may be longer than this.)  Chunks correspond roughly
    # to words and the whitespace between them: each chunk is
    # indivisible (modulo 'break_long_words'), but a line break can
    # come between any two chunks.  Chunks should not have internal
    # whitespace; i.e. a chunk is either all whitespace or a "word".
    # Whitespace chunks will be removed from the beginning and end of
    # lines, but apart from that whitespace is preserved.
    lines = String[]
    if width <= 0
        error("invalid width $width (must be > 0)")
    end

    # Arrange in reverse order so items can be efficiently popped
    # from a stack of chucks.
    reverse!(chunks)

    while length(chunks) > 0

        # Start the list of chunks that will make up the current line.
        # cur_len is just the length of all the chunks in cur_line.
        cur_line = String[]
        cur_len = 0

        # Figure out which static string will prefix this line.
        if !isempty(lines)
            indent = subsequent_indent
        else
            indent = initial_indent
        end

        # Maximum width for this line.
        lwidth = width - length(indent)

        # First chunk on line is whitespace -- drop it, unless this
        # is the very beginning of the text (ie. no lines started yet).
        if drop_whitespace && isempty(strip(chunks[end])) && !isempty(lines)
            pop(chunks)
        end

        while !isempty(chunks)
            l = strlen(chunks[end])

            # Can at least squeeze this chunk onto the current line.
            if cur_len + l <= lwidth
                push(cur_line, pop(chunks))
                cur_len += l
            else
            # Nope, this line is full.
                break
            end
        end

        # The current line is full, and the next chunk is too big to
        # fit on *any* line (not just this one).
        if !isempty(chunks) && strlen(chunks[end]) > lwidth
            chunks, cur_line = _handle_long_word(chunks, cur_line, cur_len, lwidth, break_long_words)
        end

        # If the last chunk on this line is all whitespace, drop it.
        if drop_whitespace && !isempty(cur_line) && isempty(strip(cur_line[end]))
            pop(cur_line)
        end

        # Convert current line back to a string and store it in list
        # of all lines (return value).
        if !isempty(cur_line)
            push(lines, indent * join(cur_line))
        end
    end

    return lines
end

function wrap(text::String, opts::Options)
    # Reformat the single paragraph in 'text' so it fits in lines of
    # no more than 'opts.width' columns, and return a Vector of wrapped
    # lines.  Tabs in 'text' are expanded with _expand_tabs(),
    # and all other whitespace characters (including newline) are
    # converted to space.
    @defaults(opts,
        width=>70,
        initial_indent=>"",
        subsequent_indent=>"",
        expand_tabs=>true,
        replace_whitespace=>true,
        fix_sentence_endings=>false,
        break_long_words=>true,
        drop_whitespace=>true,
        break_on_hyphens=>true)
    @check_used(opts)

    text = _munge_whitespace(text, expand_tabs, replace_whitespace)
    chunks = _split(text, break_on_hyphens)
    if fix_sentence_endings
        chunks = _fix_sentence_endings(chunks)
    end
    return _wrap_chunks(chunks, width, initial_indent,
            subsequent_indent, drop_whitespace, break_long_words)
end
wrap(text::String) = wrap(text, Options())

function wrapped_string(text::String, opts::Options)
    # Reformat the single paragraph in 'text' to fit in lines of no
    # more than 'self.width' columns, and output it to a string
    return join(wrap(text, opts), '\n')
end
wrapped_string(text::String) = wrapped_string(text, Options())

print_wrapped(io::IO, text::String, opts::Options) = print(io, wrapped_string(text, opts))
print_wrapped(text::String) = print_wrapped(stdout_stream, text, Options())
print_wrapped(text::String, opts::Options) = print_wrapped(stdout_stream, text, opts)
print_wrapped(io::IO, text::String) = print_wrapped(io, text, Options())

println_wrapped(io::IO, text::String, opts::Options) = println(io, wrapped_string(text, opts))
println_wrapped(text::String) = println_wrapped(stdout_stream, text, Options())
println_wrapped(text::String, opts::Options) = println_wrapped(stdout_stream, text, opts)
println_wrapped(io::IO, text::String) = println_wrapped(io, text, Options())

function dedent(text::String)
    # Remove any common leading whitespace from every line in `text`.

    # Note that tabs and spaces are both treated as whitespace, but they
    # are not equal: the lines "  hello" and "\thello" are
    # considered to have no common leading whitespace.
    
    _whitespace_only_re = r"^[ \t]+$"m
    _leading_whitespace_re = r"(^[ \t]*)(?=[^ \t\n])"m

    # Look for the longest leading string of spaces and tabs common to
    # all lines.
    margin = nothing
    text = replace(text, _whitespace_only_re, "")
    for ii in each_search(text, _leading_whitespace_re)
        indent = text[ii[1]:ii[2]-1]
        if is(margin, nothing)
            margin = indent

        # Current line more deeply indented than previous winner:
        # no change (previous winner is still on top).
        elseif begins_with(indent, margin)
            # no-op

        # Current line consistent with and no deeper than previous winner:
        # it's the new winner.
        elseif begins_with(margin, indent)
            margin = indent

        # Current line and previous winner have no common whitespace:
        # there is no margin.
        else
            margin = ""
            break
        end
    end

    # sanity check (testing/debugging only)
    if false && !is(margin, nothing)
        for line in split(text, ['\n', '\r'])
            @assert isempty(line) || begins_with(line, margin)
        end
    end

    if !is(margin, nothing) && !isempty(margin)
        text = replace(text, Regex("(?m)^" * margin, PCRE_UTF8), "")
    end
    return text
end

end # module TextWrap
