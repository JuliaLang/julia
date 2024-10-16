module AnnotatedDisplay

using ..Base: IO, SubString, IOBuffer, AnnotatedString, AnnotatedChar, AnnotatedIOBuffer
using ..Base: eachregion, invoke_in_world, tls_world_age

import ..Base: write, print, show, escape_string # implemented methods

# This is the "display interface" for printing an AnnotatedString
termreset(io::IO, ::Nothing) = nothing # no-op
termstyle(io::IO, ::Nothing, laststyle::Any) = termreset(io, laststyle)
# termstyle(io::IO, face::Symbol, laststyle::Any) = error("Unresolved `:face` annotation encountered: $face")

htmlreset(io::IO, ::Nothing) = nothing # no-op
htmlstyle(io::IO, ::Nothing, laststyle::Any) = htmlreset(io, laststyle)
# htmlstyle(io::IO, face::Symbol, laststyle::Any) = error("Unresolved `:face` annotation encountered: $face")

mergestyle(::Nothing, @nospecialize(style::Any)) = (style, true)
# mergestyle(style::Symbol, @nospecialize(::Any)) = (style, false)

# call mergestyle(...) w/ invalidation barrier
mergestyle_(@nospecialize(merged::Any), @nospecialize(style::Any)) =
    invoke_in_world(tls_world_age(), mergestyle, merged, style)
# call termreset(...) w/ invalidation barrier
termreset_(io::IO, @nospecialize(laststyle::Any)) =
    invoke_in_world(tls_world_age(), termreset, io, laststyle)
# call termstyle(...) w/ invalidation barrier
termstyle_(io::IO, @nospecialize(style::Any), @nospecialize(laststyle::Any)) =
    invoke_in_world(tls_world_age(), termstyle, io, style, laststyle)
# call htmlreset(...) w/ invalidation barrier
htmlreset_(io::IO, @nospecialize(laststyle::Any)) =
    invoke_in_world(tls_world_age(), htmlreset, io, laststyle)
# call htmlstyle(...) w/ invalidation barrier
htmlstyle_(io::IO, @nospecialize(style::Any), @nospecialize(laststyle::Any)) =
    invoke_in_world(tls_world_age(), htmlstyle, io, style, laststyle)

function _ansi_writer(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}},
                      string_writer::F) where {F <: Function}
    # We need to make sure that the customisations are loaded
    # before we start outputting any styled content.
    if get(io, :color, false)::Bool
        buf = IOBuffer() # Avoid the overhead in repeatedly printing to `stdout`
        active_style::Any = nothing # Represents the currently-applied style
        for (str, styles) in eachregion(s)
            link = nothing
            merged_style = nothing
            for (key, style) in reverse(styles)
                if key === :link
                    link = style::String
                end
                key !== :face && continue
                # Merge as many of these as we can into a single style before
                # we print to the terminal
                (merged_style, successful) = mergestyle_(merged_style, style)
                if !successful
                    active_style = termstyle_(buf, merged_style, active_style)
                    merged_style = style
                end
            end

            active_style = termstyle_(buf, merged_style, active_style)
            !isnothing(link) && write(buf, "\e]8;;", link, "\e\\")
            string_writer(buf, str)
            !isnothing(link) && write(buf, "\e]8;;\e\\")
        end
        # Reset the terminal state (whoever last wrote has the responsibility)
        termreset_(buf, active_style)
        write(io, take!(buf))
    elseif s isa AnnotatedString
        string_writer(io, s.string)
    elseif s isa SubString
        string_writer(
            io, SubString(s.string.string, s.offset, s.ncodeunits, Val(:noshift)))
    end
end

write(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    _ansi_writer(io, s, write)::Int

print(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (_ansi_writer(io, s, print); nothing)

# We need to make sure that printing to an `AnnotatedIOBuffer` calls `write` not `print`
# so we get the specialised handling that `_ansi_writer` doesn't provide.
print(io::AnnotatedIOBuffer, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (write(io, s); nothing)

escape_string(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}},
              esc = ""; keep = ()) =
    (_ansi_writer(io, s, (io, s) -> escape_string(io, s, esc; keep)); nothing)

function write(io::IO, c::AnnotatedChar)
    if get(io, :color, false) == true
        active_style::Any = nothing # Represents the currently-applied style

        # TODO: re-factor into separate (shared) function
        link = nothing
        merged_style = nothing
        for (key, style) in reverse(c.annotations)
            if key === :link
                link = style::String
            end
            key !== :face && continue
            # Merge as many of these as we can into a single style before
            # we print to the terminal
            (merged_style, successful) = mergestyle_(merged_style, style)
            if !successful
                active_style = termstyle_(buf, merged_style, active_style)
                merged_style = style
            end
        end

        active_style = termstyle_(io, merged_style, active_style)
        bytes = write(io, c.char)
        termreset_(io, active_style)
        bytes
    else
        write(io, c.char)
    end
end

print(io::IO, c::AnnotatedChar) = (write(io, c); nothing)

function show(io::IO, c::AnnotatedChar)
    if get(io, :color, false) == true
        out = IOBuffer()
        show(out, c.char)
        cstr = AnnotatedString(
            String(take!(out)[2:end-1]),
            [(1:ncodeunits(c), a...) for a in c.annotations])
        print(io, ''', cstr, ''')
    else
        show(io, c.char)
    end
end

function write(io::IO, aio::AnnotatedIOBuffer)
    if get(io, :color, false) == true
        # This does introduce an overhead that technically
        # could be avoided, but I'm not sure that it's currently
        # worth the effort to implement an efficient version of
        # writing from a AnnotatedIOBuffer with style.
        # In the meantime, by converting to an `AnnotatedString` we can just
        # reuse all the work done to make that work.
        write(io, read(aio, AnnotatedString))
    else
        write(io, aio.io)
    end
end

function show(io::IO, ::MIME"text/html", s::Union{<:AnnotatedString, SubString{<:AnnotatedString}})
    htmlescape(str) = replace(str, '&' => "&amp;", '<' => "&lt;", '>' => "&gt;")

    buf = IOBuffer() # Avoid potential overhead in repeatedly printing a more complex IO
    active_style::Any = nothing # Represents the currently-applied style
    for (str, styles) in eachregion(s)
        link = nothing
        merged_style = nothing
        for (key, style) in reverse(styles)
            if key === :link
                link = style::String
            end
            key !== :face && continue

            # Merge as many of these as we can into a single style before
            # we print to the terminal
            (merged_style, successful) = mergestyle_(merged_style, style)
            if !successful
                active_style = htmlstyle_(buf, merged_style, active_style)
                merged_style = style
            end
        end

        active_style = htmlstyle_(buf, merged_style, active_style)
        !isnothing(link) && print(buf, "<a href=\"", link, "\">")
        print(buf, htmlescape(str))
        !isnothing(link) && print(buf, "</a>")
    end

    # Reset the terminal state (whoever last wrote has the responsibility)
    htmlreset_(buf, active_style)
    write(io, take!(buf))
    nothing
end

end
