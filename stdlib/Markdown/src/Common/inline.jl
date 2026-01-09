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
    text
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
        _is_link(url) && return Link(url, url)
        _is_mailto(url) && return Link(url, url)
        _is_email(url) && return Link(url, "mailto:" * url)
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
    if startswith(stream, "\\\n")
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

const escape_chars = """!"#\$%&'()*+,-./:;<=>?@[\\]^_`{|}~"""

@trigger '\\' ->
function escapes(stream::IO, md::MD)
    withstream(stream) do
        if startswith(stream, "\\") && !eof(stream) && (c = read(stream, Char)) in escape_chars
            return string(c)
        end
    end
end
