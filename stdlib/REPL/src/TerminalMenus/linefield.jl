# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LineField(text = "")

One line of text being typed, with a cursor, for a program that draws the field itself.
[`edit!`](@ref) gives it the keys readline has, and [`paste!`](@ref) a paste.

`text` is the text, `cursor` how many characters of it are before the cursor, and `killed`
what the last key that cuts text took, which `^Y` puts back.
"""
mutable struct LineField
    text::String
    cursor::Int
    killed::String
end
LineField(text::AbstractString="") = LineField(String(text), length(text), "")

"""
    edit!(f::LineField, ev::KeyEvent) -> Bool

Edit `f` with the key `ev`, and return whether it was a key that edits. A program's own keys,
such as Enter, Escape or the vertical arrows, return `false`; a program that gives some keys
of its own meaning, such as `^A`, tests for them before calling this.

  * a character is typed at the cursor
  * `←` `→` `^B` `^F` move a character, and `Alt-←` `Alt-→` `Alt-B` `Alt-F` a word
  * `Home` `^A` and `End` `^E` move to the start and the end
  * `⌫` and `Delete` `^D` delete a character, before and after the cursor
  * `^W` cuts back to a space, and `Alt-⌫` back to the start of a word of letters and digits;
    `Alt-D` cuts forward to the end of one
  * `^U` and `^K` cut to the start and to the end
  * `^Y` puts back what was cut last
  * `^T` swaps the characters around the cursor
"""
function edit!(f::LineField, ev::KeyEvent)
    q = collect(f.text)
    n = clamp(f.cursor, 0, length(q))
    k, alt, ctrl, plain = ev.key, ev.mods == ALT, ev.mods == CTRL, ev.mods == 0
    if k isa Char && (plain || ev.mods == SHIFT) && isvalid(k) && !iscntrl(k)
        insert!(q, n += 1, k)
    elseif k === :left && plain || ctrl && k == 'b'
        n = max(0, n - 1)
    elseif k === :right && plain || ctrl && k == 'f'
        n = min(length(q), n + 1)
    elseif k === :left && alt || alt && k == 'b'
        n = word_back(q, n, isalnum)
    elseif k === :right && alt || alt && k == 'f'
        n = word_forward(q, n, isalnum)
    elseif k === :home && plain || ctrl && k == 'a'
        n = 0
    elseif k === :end && plain || ctrl && k == 'e'
        n = length(q)
    elseif k === :backspace && plain
        n > 0 && deleteat!(q, n)
        n = max(0, n - 1)
    elseif k === :delete && plain || ctrl && k == 'd'
        n < length(q) && deleteat!(q, n + 1)
    elseif k === :backspace && alt || ctrl && k == 'w'
        m = word_back(q, n, alt ? isalnum : !isspace)
        cut!(f, q, m+1:n)
        n = m
    elseif alt && k == 'd'
        cut!(f, q, n+1:word_forward(q, n, isalnum))
    elseif ctrl && k == 'u'
        cut!(f, q, 1:n)
        n = 0
    elseif ctrl && k == 'k'
        cut!(f, q, n+1:length(q))
    elseif ctrl && k == 'y'
        for c in f.killed
            insert!(q, n += 1, c)
        end
    elseif ctrl && k == 't'
        # at the end, the last two swap, as in readline
        if length(q) >= 2 && n > 0
            n == length(q) && (n -= 1)
            q[n], q[n+1] = q[n+1], q[n]
            n += 1
        end
    else
        return false
    end
    f.text = String(q)
    f.cursor = n
    return true
end

"""
    paste!(f::LineField, text) -> LineField

Put a paste into `f` at the cursor, as one line: each line break becomes a space, and other
control characters are left out.
"""
function paste!(f::LineField, text::AbstractString)
    s = filter(!iscntrl, replace(String(text), r"\r\n|\r|\n" => ' '))
    f.text = string(first(f.text, f.cursor), s, chop(f.text; head=f.cursor, tail=0))
    f.cursor += length(s)
    return f
end

isalnum(c::Char) = isletter(c) || isdigit(c)

# the start of the word before `n`, or the end of the one after it, skipping what is not a word
function word_back(q, n, inword)
    while n > 0 && !inword(q[n]); n -= 1; end
    while n > 0 && inword(q[n]); n -= 1; end
    return n
end
function word_forward(q, n, inword)
    while n < length(q) && !inword(q[n+1]); n += 1; end
    while n < length(q) && inword(q[n+1]); n += 1; end
    return n
end

function cut!(f::LineField, q::Vector{Char}, r::UnitRange{Int})
    isempty(r) && return
    f.killed = String(q[r])
    deleteat!(q, r)
    return
end
