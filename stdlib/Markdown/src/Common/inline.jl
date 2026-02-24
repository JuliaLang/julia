# This file is a part of Julia. License is MIT: https://julialang.org/license

# ––––––––
# Emphasis
# ––––––––

mutable struct Italic <: MarkdownElement
    text
end

@trigger '*' ->
function asterisk_italic(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "*")
    return result === nothing ? nothing : Italic(parseinline(result, md))
end

@trigger '_' ->
function underscore_italic(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "_")
    return result === nothing ? nothing : Italic(parseinline(result, md))
end

mutable struct Bold <: MarkdownElement
    text
end

@trigger '*' ->
function asterisk_bold(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "**")
    return result === nothing ? nothing : Bold(parseinline(result, md))
end

@trigger '_' ->
function underscore_bold(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "__")
    return result === nothing ? nothing : Bold(parseinline(result, md))
end

mutable struct Strikethrough <: MarkdownElement
    text
end

@trigger '~' ->
function tilde_strikethrough(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "~")
    return result === nothing ? nothing : Strikethrough(parseinline(result, md))
end

@trigger '~' ->
function double_tilde_strikethrough(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "~~")
    return result === nothing ? nothing : Strikethrough(parseinline(result, md))
end

# ––––
# Code
# ––––

@trigger '`' ->
function inline_code(stream::IO, md::MD)
    withstream(stream) do
        ticks = matchstart(stream, r"^(`+)").match
        result = readuntil(stream, ticks; newlines=true)
        if result === nothing
            nothing
        else
            result = strip(result)
            # in code spans, newlines are replaced by spaces
            result = replace(result, '\n' => ' ')
            # An odd number of backticks wrapping the text will produce a `Code` node, while
            # an even number will result in a `LaTeX` node. This allows for arbitrary
            # backtick combinations to be embedded inside the resulting node, i.e.
            #
            # `a`, ``a``, `` `a` ``, ``` ``a`` ```, ``` `a` ```, etc.
            #  ^     ^        ^            ^             ^
            #  C     L        L            C             C       with C=Code and L=LaTeX.
            #
            isodd(length(ticks)) ? Code(result) : LaTeX(result)
        end
    end
end

# ––––––––––––––
# Images & Links
# ––––––––––––––

mutable struct Image <: MarkdownElement
    url::String
    alt::String
end

@trigger '!' ->
function image(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, "![") || return
        alt = readuntil(stream, ']', match = '[')
        alt ≡ nothing && return
        skipwhitespace(stream)
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        return Image(url, alt)
    end
end

mutable struct Link <: MarkdownElement
    text::Vector
    url::String
end

@trigger '[' ->
function link(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, '[') || return
        text = readuntil(stream, ']', match = '[')
        text ≡ nothing && return
        startswith(stream, '(') || return
        url = readuntil(stream, ')', match = '(')
        url ≡ nothing && return
        url = replace_escapes_and_entities(url)
        return Link(parseinline(text, md), url)
    end
end

@trigger '[' ->
function footnote_link(stream::IO, md::MD)
    withstream(stream) do
        regex = r"^\[\^(\w+)\]"
        m = matchstart(stream, regex)
        if m === nothing
            return
        else
            ref = m.captures[1]
            return Footnote(ref, nothing)
        end
    end
end

@trigger '<' ->
function autolink(stream::IO, md::MD)
    withstream(stream) do
        startswith(stream, '<') || return
        url = readuntil(stream, '>')
        url ≡ nothing && return
        _is_link(url) && return Link([url], url)
        _is_mailto(url) && return Link([url], url)
        _is_email(url) && return Link([url], "mailto:" * url)
        return
    end
end

# This list is taken from the commonmark spec
# http://spec.commonmark.org/0.19/#absolute-uri
const _allowable_schemes = Set(split("coap doi javascript aaa aaas about acap cap cid crid data dav dict dns file ftp geo go gopher h323 http https iax icap im imap info ipp iris iris.beep iris.xpc iris.xpcs iris.lwz ldap mailto mid msrp msrps mtqp mupdate news nfs ni nih nntp opaquelocktoken pop pres rtsp service session shttp sieve sip sips sms snmp,soap.beep soap.beeps tag tel telnet tftp thismessage tn3270 tip tv urn vemmi ws wss xcon xcon-userid xmlrpc.beep xmlrpc.beeps xmpp z39.50r z39.50s
adiumxtra afp afs aim apt,attachment aw beshare bitcoin bolo callto chrome,chrome-extension com-eventbrite-attendee content cvs,dlna-playsingle dlna-playcontainer dtn dvb ed2k facetime feed finger fish gg git gizmoproject gtalk hcp icon ipn irc irc6 ircs itms jar jms keyparc lastfm ldaps magnet maps market,message mms ms-help msnim mumble mvn notes oid palm paparazzi platform proxy psyc query res resource rmi rsync rtmp secondlife sftp sgn skype smb soldat spotify ssh steam svn teamspeak
things udp unreal ut2004 ventrilo view-source webcal wtai wyciwyg xfire xri ymsgr"))

function _is_link(s::AbstractString)
    '<' in s && return false

    m = match(r"^(.*)://(\S+?)(:\S*)?$", s)
    m ≡ nothing && return false
    scheme = lowercase(m.captures[1])
    return scheme in _allowable_schemes
end

# non-normative regex from the HTML5 spec
const _email_regex_str = raw"""[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""
const _mailto_regex = Regex("^mailto\\:" * _email_regex_str)
const _email_regex = Regex("^" * _email_regex_str)

function _is_mailto(s::AbstractString)
    return occursin(_mailto_regex, s)
end

function _is_email(s::AbstractString)
    return occursin(_email_regex, s)
end

# –––––––––––
# Punctuation
# –––––––––––

mutable struct LineBreak <: MarkdownElement end

@trigger '\\' ->
function linebreak(stream::IO, md::MD)
    if startswith(stream, "\\\n") || startswith(stream, "\\\r\n") || startswith(stream, "\\\r")
        return LineBreak()
    end
end

@trigger '-' ->
function en_or_em_dash(stream::IO, md::MD)
    if startswith(stream, "---")
        return "—"
    end
    if startswith(stream, "--")
        return "–"
    end
end

# –––––––––––
# Backslash escapes
# –––––––––––

const escape_chars = """!"#\$%&'()*+,-./:;<=>?@[\\]^_`{|}~"""

@trigger '\\' ->
function escapes(stream::IO, md::MD)
    withstream(stream) do
        if startswith(stream, "\\") && !eof(stream) && (c = read(stream, Char)) in escape_chars
            return string(c)
        end
    end
end

# –––––––––––
# Entity and numeric character references
# –––––––––––

const DEC_OR_HEX_REGEX = r"^&#(([0-9]{1,7})|[xX]([0-9a-fA-F]{1,6}));"

@trigger '&' ->
function entity(stream::IO, md::MD)
    entity(stream)
end

function entity(stream::IO)
    # named entity?
    m = matchstart(stream, r"^&([a-zA-Z][a-zA-Z0-9]*);")
    if m !== nothing
        chars = get(entities, m.match, nothing)
        return chars !== nothing ? chars : string(m.match)
    end
    # decimal or hexadecimal entity?
    m = matchstart(stream, DEC_OR_HEX_REGEX)
    if m !== nothing
        val = if m.captures[2] !== nothing
            Base.parse(UInt, m.captures[2]; base=10)
        else
            Base.parse(UInt, m.captures[3]; base=16)
        end
        c = (val != 0 && isvalid(Char, val)) ? Char(val) : Char(0xFFFD)
        #return c
        return string(c)
    end
end


function replace_escapes_and_entities(s::AbstractString)
    # TODO: performance??? ugh
    stream = IOBuffer(s)
    out = IOBuffer()
    while !eof(stream)
        c = read(stream, Char)
        if c == '\\'
            if !eof(stream)
                c = read(stream, Char)
                c in escape_chars || write(out, '\\')
                write(out, c)
            end
        elseif c == '&'
            skip(stream, -1)
            ent = entity(stream)
            if ent === nothing
                skip(stream, 1)
                write(out, '&')
            else
                write(out, ent)
            end
        else
            write(out, c)
        end
    end
    return takestring!(out)
end

# ––––––––
# Raw HTML
# ––––––––

mutable struct HTMLInline <: MarkdownElement
    content::String
end

# skip \n, \r\n or \r
function skip_line_end(io::IO)
    eof(io) && return false
    c = peek(io, Char)
    if c == '\n'
        read(io, Char)
        return true
    elseif c == '\r'
        read(io, Char)
        if !eof(io) && peek(io, Char) == '\n'
            read(io, Char)
        end
        return true
    end
    return false
end

# skip spaces, tabs, and up to one line ending;
# return true if at least one whitespace was skipped
function skip_spaces_and_up_to_one_line_ending(io::IO)
    pos = position(io)
    skipwhitespace(io)
    if skip_line_end(io)
        skipwhitespace(io)
    end
    return position(io) > pos
end

function skip_attribute_value_spec(io::IO)
    withstream(io) do
        # An attribute value specification consists of optional spaces, tabs, and up
        # to one line ending, ...
        skip_spaces_and_up_to_one_line_ending(io)
        # ... a = character, ...
        startswith(io, '=') || return false
        #... optional spaces, tabs, and up to one line ending, ...
        skip_spaces_and_up_to_one_line_ending(io)
        #... and an attribute value.
        startswith(io, ATTRIBUTE_VALUE_REGEX)
    end
end

function skip_attribute(io::IO)
    withstream(io) do
        # An attribute consists of spaces, tabs, and up to one line ending, ...
        skip_spaces_and_up_to_one_line_ending(io) || return false
        # ... an attribute name, ...
        startswith(io, ATTRIBUTE_NAME_REGEX) || return false
        # ... and an optional attribute value specification.
        skip_attribute_value_spec(io)
        return true
    end
end

function skip_open_tag(io::IO)
    withstream(io) do
        # An open tag consists of a < character, ...
        startswith(io, '<') || return false
        # ... a tag name, ...
        startswith(io, TAG_NAME_REGEX) || return false
        # ... zero or more attributes, ...
        while skip_attribute(io)
        end
        # ... optional spaces, tabs, and up to one line ending, ...
        skip_spaces_and_up_to_one_line_ending(io)
        # ... an optional / character, ...
        res = startswith(io, '/')
        # ... and a > character.
        startswith(io, '>') || return false
        return true
    end
end

function skip_closing_tag(io::IO)
    withstream(io) do
        # A closing tag consists of the string </, ...
        startswith(io, '<') || return false
        startswith(io, '/') || return false
        # ... a tag name,...
        startswith(io, TAG_NAME_REGEX) || return false
        # ... optional spaces, tabs, and up to one line ending,...
        skip_spaces_and_up_to_one_line_ending(io)
        # ... and the character >.
        startswith(io, '>') || return false
        return true
    end
end

function skip_html_comment(io::IO)
    withstream(io) do
        # An HTML comment consists of <!-->, <!--->, or <!--, a string of characters
        # not including the string -->, and --> (see the HTML spec).
        startswith(io, "<!--") || return false
        seek(io, position(io) - 2)
        count = 0
        while !eof(io)
            c = read(io, Char)
            if c == '-'
                count += 1
            elseif c == '>' && count >= 2
                return true
            else
                count = 0
            end
        end
        return false
    end
end

function skip_processing_instruction(io::IO)
    withstream(io) do
        # A processing instruction consists of the string <?, ...
        startswith(io, "<?") || return false
        # ... a string of characters not including the string ?>, and the string ?>.
        prev = ' '
        while !eof(io)
            c = read(io, Char)
            prev == '?' && c == '>' && return true
            prev = c
        end
        return false
    end
end

function skip_declaration(io::IO)
    withstream(io) do
        # A declaration consists of the string <!, ...
        startswith(io, "<!") || return false
        # ... an ASCII letter, ...
        !eof(io) || return false
        c = read(io, Char)
        (isascii(c) && isletter(c)) || return false
        # ... zero or more characters not including the character >, and the character >.
        while !eof(io)
            read(io, Char) == '>' && return true
        end
        return false
    end
end

function skip_cdata_section(io::IO)
    withstream(io) do
        # A CDATA section consists of the string <![CDATA[, ...
        startswith(io, "<![CDATA[") || return false
        # ... a string of characters not including the string ]]>, and the string ]]>.
        count = 0
        while !eof(io)
            c = read(io, Char)
            if c == ']'
                count += 1
            elseif c == '>' && count >= 2
                return true
            else
                count = 0
            end
        end
        return false
    end
end

const HTML_BREAK_REGEX = r"^<br\s*/?>"i

@trigger '<' ->
function html_inline(stream::IO, md::MD)
    pos = position(stream)

    # special case for <br>: this is of course not part of CommonMark,
    # but it is the only way to get linebreaks into tables, and by handling
    # this here, it will also work in LaTeX / PDF output.
    startswith(stream, HTML_BREAK_REGEX) && return LineBreak()

    # An HTML tag consists of an open tag, a closing tag, an HTML comment, a
    # processing instruction, a declaration, or a CDATA section.
    skip_open_tag(stream) ||
        skip_closing_tag(stream) ||
        skip_html_comment(stream) ||
        skip_processing_instruction(stream) ||
        skip_declaration(stream) ||
        skip_cdata_section(stream) ||
        return nothing

    endpos = position(stream)
    seek(stream, pos)
    data = read(stream, endpos - pos)
    return HTMLInline(String(data))
end
