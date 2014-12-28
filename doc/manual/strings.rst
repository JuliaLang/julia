.. _man-strings:

.. currentmodule:: Base

*********
 Strings
*********

Strings are finite sequences of characters. Of course, the real trouble
comes when one asks what a character is. The characters that English
speakers are familiar with are the letters ``A``, ``B``, ``C``, etc.,
together with numerals and common punctuation symbols. These characters
are standardized together with a mapping to integer values between 0 and
127 by the `ASCII <http://en.wikipedia.org/wiki/ASCII>`_ standard. There
are, of course, many other characters used in non-English languages,
including variants of the ASCII characters with accents and other
modifications, related scripts such as Cyrillic and Greek, and scripts
completely unrelated to ASCII and English, including Arabic, Chinese,
Hebrew, Hindi, Japanese, and Korean. The
`Unicode <http://en.wikipedia.org/wiki/Unicode>`_ standard tackles the
complexities of what exactly a character is, and is generally accepted
as the definitive standard addressing this problem. Depending on your
needs, you can either ignore these complexities entirely and just
pretend that only ASCII characters exist, or you can write code that can
handle any of the characters or encodings that one may encounter when
handling non-ASCII text. Julia makes dealing with plain ASCII text
simple and efficient, and handling Unicode is as simple and efficient as
possible. In particular, you can write C-style string code to process
ASCII strings, and they will work as expected, both in terms of
performance and semantics. If such code encounters non-ASCII text, it
will gracefully fail with a clear error message, rather than silently
introducing corrupt results. When this happens, modifying the code to
handle non-ASCII data is straightforward.

There are a few noteworthy high-level features about Julia's strings:

-  :obj:`AbstractString` is an abstraction, not a concrete type — many different
   representations can implement the :obj:`AbstractString` interface, but they can
   easily be used together and interact transparently. Any string type
   can be used in any function expecting a :obj:`AbstractString`.
-  Like C and Java, but unlike most dynamic languages, Julia has a
   first-class type representing a single character, called :obj:`Char`.
   This is just a special kind of 32-bit bitstype whose numeric value
   represents a Unicode code point.
-  As in Java, strings are immutable: the value of a :obj:`AbstractString` object
   cannot be changed. To construct a different string value, you
   construct a new string from parts of other strings.
-  Conceptually, a string is a *partial function* from indices to
   characters — for some index values, no character value is returned,
   and instead an exception is thrown. This allows for efficient
   indexing into strings by the byte index of an encoded representation
   rather than by a character index, which cannot be implemented both
   efficiently and simply for variable-width encodings of Unicode
   strings.
-  Julia supports the full range of
   `Unicode <http://en.wikipedia.org/wiki/Unicode>`_ characters: literal
   strings are always `ASCII <http://en.wikipedia.org/wiki/ASCII>`_ or
   `UTF-8 <http://en.wikipedia.org/wiki/UTF-8>`_ but other encodings for
   strings from external sources can be supported.

.. _man-characters:

Characters
----------

A :obj:`Char` value represents a single character: it is just a 32-bit
bitstype with a special literal representation and appropriate arithmetic
behaviors, whose numeric value is interpreted as a `Unicode code
point <http://en.wikipedia.org/wiki/Code_point>`_. Here is how :obj:`Char`
values are input and shown:

.. doctest::

    julia> 'x'
    'x'

    julia> typeof(ans)
    Char

You can convert a :obj:`Char` to its integer value, i.e. code point,
easily:

.. doctest::

    julia> int('x')
    120

    julia> typeof(ans)
    Int64

On 32-bit architectures, :func:`typeof(ans) <typeof>` will be ``Int32``. You can
convert an integer value back to a :obj:`Char` just as easily:

.. doctest::

    julia> char(120)
    'x'

Not all integer values are valid Unicode code points, but for
performance, the :func:`char` conversion does not check that every character
value is valid. If you want to check that each converted value is a
valid code point, use the :func:`is_valid_char` function:

.. doctest::

    julia> char(0x110000)
    '\U110000'

    julia> is_valid_char(0x110000)
    false

As of this writing, the valid Unicode code points are ``U+00`` through
``U+d7ff`` and ``U+e000`` through ``U+10ffff``. These have not all been
assigned intelligible meanings yet, nor are they necessarily
interpretable by applications, but all of these values are considered to
be valid Unicode characters.

You can input any Unicode character in single quotes using ``\u``
followed by up to four hexadecimal digits or ``\U`` followed by up to
eight hexadecimal digits (the longest valid value only requires six):

.. doctest::

    julia> '\u0'
    '\0'

    julia> '\u78'
    'x'

    julia> '\u2200'
    '∀'

    julia> '\U10ffff'
    '\U10ffff'

Julia uses your system's locale and language settings to determine which
characters can be printed as-is and which must be output using the
generic, escaped ``\u`` or ``\U`` input forms. In addition to these
Unicode escape forms, all of `C's traditional escaped input
forms <http://en.wikipedia.org/wiki/C_syntax#Backslash_escapes>`_ can
also be used:

.. doctest::

    julia> int('\0')
    0

    julia> int('\t')
    9

    julia> int('\n')
    10

    julia> int('\e')
    27

    julia> int('\x7f')
    127

    julia> int('\177')
    127

    julia> int('\xff')
    255

You can do comparisons and a limited amount of arithmetic with
:obj:`Char` values:

.. doctest::

    julia> 'A' < 'a'
    true

    julia> 'A' <= 'a' <= 'Z'
    false

    julia> 'A' <= 'X' <= 'Z'
    true

    julia> 'x' - 'a'
    23

    julia> 'A' + 1
    'B'

:obj:`AbstractString` Basics
----------------------------

:obj:`AbstractString` literals are delimited by double quotes or triple double quotes:

.. doctest::

    julia> str = "Hello, world.\n"
    "Hello, world.\n"

    julia> """Contains "quote" characters"""
    "Contains \"quote\" characters"

If you want to extract a character from a string, you index into it:

.. doctest::

    julia> str[1]
    'H'

    julia> str[6]
    ','

    julia> str[end]
    '\n'

All indexing in Julia is 1-based: the first element of any
integer-indexed object is found at index 1, and the last
element is found at index ``n``, when the string has
a length of ``n``.

In any indexing expression, the keyword ``end`` can be used as a
shorthand for the last index (computed by :func:`endof(str) <endof>`).
You can perform arithmetic and other operations with ``end``, just like
a normal value:

.. doctest::

    julia> str[end-1]
    '.'

    julia> str[end/2]
    ' '

    julia> str[end/3]
    ERROR: InexactError()
     in getindex at string.jl:59

    julia> str[end/4]
    ERROR: InexactError()
     in getindex at string.jl:59

Using an index less than 1 or greater than ``end`` raises an error::

    julia> str[0]
    ERROR: BoundsError()
     in getindex at /Users/sabae/src/julia/usr/lib/julia/sys.dylib (repeats 2 times)

    julia> str[end+1]
    ERROR: BoundsError()
     in getindex at /Users/sabae/src/julia/usr/lib/julia/sys.dylib (repeats 2 times)

You can also extract a substring using range indexing:

.. doctest::

    julia> str[4:9]
    "lo, wo"

Notice that the expressions ``str[k]`` and ``str[k:k]`` do not give the same result:

.. doctest::

    julia> str[6]
    ','

    julia> str[6:6]
    ","

The former is a single character value of type :obj:`Char`, while the
latter is a string value that happens to contain only a single
character. In Julia these are very different things.

Unicode and UTF-8
-----------------

Julia fully supports Unicode characters and strings. As `discussed
above <#characters>`_, in character literals, Unicode code points can be
represented using Unicode ``\u`` and ``\U`` escape sequences, as well as
all the standard C escape sequences. These can likewise be used to write
string literals:

.. doctest::

    julia> s = "\u2200 x \u2203 y"
    "∀ x ∃ y"

Whether these Unicode characters are displayed as escapes or shown as
special characters depends on your terminal's locale settings and its
support for Unicode. Non-ASCII string literals are encoded using the
UTF-8 encoding. UTF-8 is a variable-width encoding, meaning that not all
characters are encoded in the same number of bytes. In UTF-8, ASCII
characters — i.e. those with code points less than 0x80 (128) — are
encoded as they are in ASCII, using a single byte, while code points
0x80 and above are encoded using multiple bytes — up to four per
character. This means that not every byte index into a UTF-8 string is
necessarily a valid index for a character. If you index into a string at
such an invalid byte index, an error is thrown:

.. doctest::

    julia> s[1]
    '∀'

    julia> s[2]
    ERROR: invalid UTF-8 character index
     in next at ./utf8.jl:68
     in getindex at string.jl:57

    julia> s[3]
    ERROR: invalid UTF-8 character index
     in next at ./utf8.jl:68
     in getindex at string.jl:57

    julia> s[4]
    ' '

In this case, the character ``∀`` is a three-byte character, so the
indices 2 and 3 are invalid and the next character's index is 4.

Because of variable-length encodings, the number of characters in a
string (given by :func:`length(s) <length>`) is not always the same as the last index.
If you iterate through the indices 1 through :func:`endof(s) <endof>` and index
into ``s``, the sequence of characters returned when errors aren't
thrown is the sequence of characters comprising the string ``s``.
Thus we have the identity that ``length(s) <= endof(s)``, since each
character in a string must have its own index. The following is an
inefficient and verbose way to iterate through the characters of ``s``:

.. doctest::

    julia> for i = 1:endof(s)
             try
               println(s[i])
             catch
               # ignore the index error
             end
           end
    ∀
    <BLANKLINE>
    x
    <BLANKLINE>
    ∃
    <BLANKLINE>
    y

The blank lines actually have spaces on them. Fortunately, the above
awkward idiom is unnecessary for iterating through the characters in a
string, since you can just use the string as an iterable object, no
exception handling required:

.. doctest::

    julia> for c in s
             println(c)
           end
    ∀
    <BLANKLINE>
    x
    <BLANKLINE>
    ∃
    <BLANKLINE>
    y

UTF-8 is not the only encoding that Julia supports, and adding support
for new encodings is quite easy.  In particular, Julia also provides
:obj:`UTF16String` and :obj:`UTF32String` types, constructed by
:func:`utf16` and :func:`utf32` respectively, for UTF-16 and
UTF-32 encodings.  It also provides aliases :obj:`WString` and
:func:`wstring` for either UTF-16 or UTF-32 strings, depending on the
size of ``Cwchar_t``. Additional discussion of other encodings and how to
implement support for them is beyond the scope of this document for
the time being. For further discussion of UTF-8 encoding issues, see
the section below on `byte array literals <#Byte+Array+Literals>`_,
which goes into some greater detail.

.. _man-string-interpolation:

Interpolation
-------------

One of the most common and useful string operations is concatenation:

.. doctest::

    julia> greet = "Hello"
    "Hello"

    julia> whom = "world"
    "world"

    julia> string(greet, ", ", whom, ".\n")
    "Hello, world.\n"

Constructing strings like this can become a bit cumbersome, however. To
reduce the need for these verbose calls to :func:`string`, Julia allows
interpolation into string literals using ``$``, as in Perl:

.. doctest::

    julia> "$greet, $whom.\n"
    "Hello, world.\n"

This is more readable and convenient and equivalent to the above string
concatenation — the system rewrites this apparent single string literal
into a concatenation of string literals with variables.

The shortest complete expression after the ``$`` is taken as the
expression whose value is to be interpolated into the string. Thus, you
can interpolate any expression into a string using parentheses:

.. doctest::

    julia> "1 + 2 = $(1 + 2)"
    "1 + 2 = 3"

Both concatenation and string interpolation call 
:func:`string` to convert objects into string form. Most
non-:obj:`AbstractString` objects are converted to strings closely
corresponding to how they are entered as literal expressions:

.. doctest::

    julia> v = [1,2,3]
    3-element Array{Int64,1}:
     1
     2
     3

    julia> "v: $v"
    "v: [1,2,3]"

:func:`string` is the identity for :obj:`AbstractString` and :obj:`Char`
values, so these are interpolated into strings as themselves, unquoted
and unescaped:

.. doctest::

    julia> c = 'x'
    'x'

    julia> "hi, $c"
    "hi, x"

To include a literal ``$`` in a string literal, escape it with a
backslash:

.. doctest::

    julia> print("I have \$100 in my account.\n")
    I have $100 in my account.

Common Operations
-----------------

You can lexicographically compare strings using the standard comparison
operators:

.. doctest::

    julia> "abracadabra" < "xylophone"
    true

    julia> "abracadabra" == "xylophone"
    false

    julia> "Hello, world." != "Goodbye, world."
    true

    julia> "1 + 2 = 3" == "1 + 2 = $(1 + 2)"
    true

You can search for the index of a particular character using the
:func:`search` function:

.. doctest::

    julia> search("xylophone", 'x')
    1

    julia> search("xylophone", 'p')
    5

    julia> search("xylophone", 'z')
    0

You can start the search for a character at a given offset by providing
a third argument:

.. doctest::

    julia> search("xylophone", 'o')
    4

    julia> search("xylophone", 'o', 5)
    7

    julia> search("xylophone", 'o', 8)
    0

You can use the :func:`contains` function to check if a substring is
contained in a string:

.. doctest::

    julia> contains("Hello, world.", "world")
    true

    julia> contains("Xylophon", "o")
    true

    julia> contains("Xylophon", "a")
    false

    julia> contains("Xylophon", 'o')
    ERROR: `contains` has no method matching contains(::ASCIIString, ::Char)

The last error is because ``'o'`` is a character literal, and :func:`contains`
is a generic function that looks for subsequences. To look for an element in a
sequence, you must use :func:`in` instead.

Two other handy string functions are :func:`repeat` and :func:`join`:

.. doctest::

    julia> repeat(".:Z:.", 10)
    ".:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:."

    julia> join(["apples", "bananas", "pineapples"], ", ", " and ")
    "apples, bananas and pineapples"

Some other useful functions include:

-  :func:`endof(str) <endof>` gives the maximal (byte) index that can be used to
   index into ``str``.
-  :func:`length(str) <length>` the number of characters in ``str``.
-  :func:`i = start(str) <start>` gives the first valid index at which a character
   can be found in ``str`` (typically 1).
-  :func:`c, j = next(str,i) <next>` returns next character at or after the index
   ``i`` and the next valid character index following that. With
   :func:`start` and :func:`endof`, can be used to iterate through the
   characters in ``str``.
-  :func:`ind2chr(str,i) <ind2chr>` gives the number of characters in ``str`` up to
   and including any at index ``i``.
-  :func:`chr2ind(str,j) <chr2ind>` gives the index at which the ``j``\ th character
   in ``str`` occurs.

.. _man-non-standard-string-literals:

Non-Standard AbstractString Literals
------------------------------------

There are situations when you want to construct a string or use string
semantics, but the behavior of the standard string construct is not
quite what is needed. For these kinds of situations, Julia provides
:ref:`non-standard string literals <man-non-standard-string-literals2>`.
A non-standard string literal looks like
a regular double-quoted string literal, but is immediately prefixed by
an identifier, and doesn't behave quite like a normal string literal. Regular
expressions, byte array literals and version number literals, as described
below, are some examples of non-standard string literals. Other examples are
given in the :ref:`metaprogramming <man-non-standard-string-literals2>`
section.

Regular Expressions
-------------------

Julia has Perl-compatible regular expressions (regexes), as provided by
the `PCRE <http://www.pcre.org/>`_ library. Regular expressions are
related to strings in two ways: the obvious connection is that regular
expressions are used to find regular patterns in strings; the other
connection is that regular expressions are themselves input as strings,
which are parsed into a state machine that can be used to efficiently
search for patterns in strings. In Julia, regular expressions are input
using non-standard string literals prefixed with various identifiers
beginning with ``r``. The most basic regular expression literal without
any options turned on just uses ``r"..."``:

.. doctest::

    julia> r"^\s*(?:#|$)"
    r"^\s*(?:#|$)"

    julia> typeof(ans)
    Regex (constructor with 3 methods)

To check if a regex matches a string, use :func:`ismatch`:

.. doctest::

    julia> ismatch(r"^\s*(?:#|$)", "not a comment")
    false

    julia> ismatch(r"^\s*(?:#|$)", "# a comment")
    true

As one can see here, :func:`ismatch` simply returns true or false,
indicating whether the given regex matches the string or not. Commonly,
however, one wants to know not just whether a string matched, but also
*how* it matched. To capture this information about a match, use the
:func:`match` function instead:

.. doctest::

    julia> match(r"^\s*(?:#|$)", "not a comment")

    julia> match(r"^\s*(?:#|$)", "# a comment")
    RegexMatch("#")

If the regular expression does not match the given string, :func:`match`
returns ``nothing`` — a special value that does not print anything at
the interactive prompt. Other than not printing, it is a completely
normal value and you can test for it programmatically::

    m = match(r"^\s*(?:#|$)", line)
    if m == nothing
      println("not a comment")
    else
      println("blank or comment")
    end

If a regular expression does match, the value returned by :func:`match` is a
:obj:`RegexMatch` object. These objects record how the expression matches,
including the substring that the pattern matches and any captured
substrings, if there are any. This example only captures the portion of
the substring that matches, but perhaps we want to capture any non-blank
text after the comment character. We could do the following:

.. doctest::

    julia> m = match(r"^\s*(?:#\s*(.*?)\s*$|$)", "# a comment ")
    RegexMatch("# a comment ", 1="a comment")

When calling :func:`match`, you have the option to specify an index at
which to start the search. For example:

.. doctest::

   julia> m = match(r"[0-9]","aaaa1aaaa2aaaa3",1)
   RegexMatch("1")

   julia> m = match(r"[0-9]","aaaa1aaaa2aaaa3",6)
   RegexMatch("2")

   julia> m = match(r"[0-9]","aaaa1aaaa2aaaa3",11)
   RegexMatch("3")

You can extract the following info from a :obj:`RegexMatch` object:

-  the entire substring matched: ``m.match``
-  the captured substrings as a tuple of strings: ``m.captures``
-  the offset at which the whole match begins: ``m.offset``
-  the offsets of the captured substrings as a vector: ``m.offsets``

For when a capture doesn't match, instead of a substring, ``m.captures``
contains ``nothing`` in that position, and ``m.offsets`` has a zero
offset (recall that indices in Julia are 1-based, so a zero offset into
a string is invalid). Here's is a pair of somewhat contrived examples::

    julia> m = match(r"(a|b)(c)?(d)", "acd")
    RegexMatch("acd", 1="a", 2="c", 3="d")

    julia> m.match
    "acd"

    julia> m.captures
    3-element Array{Union(SubString{UTF8String},Nothing),1}:
     "a"
     "c"
     "d"

    julia> m.offset
    1

    julia> m.offsets
    3-element Array{Int64,1}:
     1
     2
     3

    julia> m = match(r"(a|b)(c)?(d)", "ad")
    RegexMatch("ad", 1="a", 2=nothing, 3="d")

    julia> m.match
    "ad"

    julia> m.captures
    3-element Array{Union(SubString{UTF8String},Nothing),1}:
     "a"
     nothing
     "d"

    julia> m.offset
    1

    julia> m.offsets
    3-element Array{Int64,1}:
     1
     0
     2

It is convenient to have captures returned as a tuple so that one can
use tuple destructuring syntax to bind them to local variables::

    julia> first, second, third = m.captures; first
    "a"

You can modify the behavior of regular expressions by some combination
of the flags ``i``, ``m``, ``s``, and ``x`` after the closing double
quote mark. These flags have the same meaning as they do in Perl, as
explained in this excerpt from the `perlre
manpage <http://perldoc.perl.org/perlre.html#Modifiers>`_::

    i   Do case-insensitive pattern matching.

        If locale matching rules are in effect, the case map is taken
        from the current locale for code points less than 255, and
        from Unicode rules for larger code points. However, matches
        that would cross the Unicode rules/non-Unicode rules boundary
        (ords 255/256) will not succeed.

    m   Treat string as multiple lines.  That is, change "^" and "$"
        from matching the start or end of the string to matching the
        start or end of any line anywhere within the string.

    s   Treat string as single line.  That is, change "." to match any
        character whatsoever, even a newline, which normally it would
        not match.

        Used together, as r""ms, they let the "." match any character
        whatsoever, while still allowing "^" and "$" to match,
        respectively, just after and just before newlines within the
        string.

    x   Tells the regular expression parser to ignore most whitespace
        that is neither backslashed nor within a character class. You
        can use this to break up your regular expression into
        (slightly) more readable parts. The '#' character is also
        treated as a metacharacter introducing a comment, just as in
        ordinary code.

For example, the following regex has all three flags turned on:

.. doctest::

    julia> r"a+.*b+.*?d$"ism
    r"a+.*b+.*?d$"ims

    julia> match(r"a+.*b+.*?d$"ism, "Goodbye,\nOh, angry,\nBad world\n")
    RegexMatch("angry,\nBad world")

Triple-quoted regex strings, of the form ``r"""..."""``, are also
supported (and may be convenient for regular expressions containing
quotation marks or newlines).

Byte Array Literals
-------------------

Another useful non-standard string literal is the byte-array string
literal: ``b"..."``. This form lets you use string notation to express
literal byte arrays — i.e. arrays of ``UInt8`` values. The convention is
that non-standard literals with uppercase prefixes produce actual string
objects, while those with lowercase prefixes produce non-string objects
like byte arrays or compiled regular expressions. The rules for byte
array literals are the following:

-  ASCII characters and ASCII escapes produce a single byte.
-  ``\x`` and octal escape sequences produce the *byte* corresponding to
   the escape value.
-  Unicode escape sequences produce a sequence of bytes encoding that
   code point in UTF-8.

There is some overlap between these rules since the behavior of ``\x``
and octal escapes less than 0x80 (128) are covered by both of the first
two rules, but here these rules agree. Together, these rules allow one
to easily use ASCII characters, arbitrary byte values, and UTF-8
sequences to produce arrays of bytes. Here is an example using all
three:

.. doctest::

    julia> b"DATA\xff\u2200"
    8-element Array{UInt8,1}:
     0x44
     0x41
     0x54
     0x41
     0xff
     0xe2
     0x88
     0x80

The ASCII string "DATA" corresponds to the bytes 68, 65, 84, 65.
``\xff`` produces the single byte 255. The Unicode escape ``\u2200`` is
encoded in UTF-8 as the three bytes 226, 136, 128. Note that the
resulting byte array does not correspond to a valid UTF-8 string — if
you try to use this as a regular string literal, you will get a syntax
error:

.. doctest::

    julia> "DATA\xff\u2200"
    ERROR: syntax: invalid UTF-8 sequence

Also observe the significant distinction between ``\xff`` and ``\uff``:
the former escape sequence encodes the *byte 255*, whereas the latter
escape sequence represents the *code point 255*, which is encoded as two
bytes in UTF-8:

.. doctest::

    julia> b"\xff"
    1-element Array{UInt8,1}:
     0xff

    julia> b"\uff"
    2-element Array{UInt8,1}:
     0xc3
     0xbf

In character literals, this distinction is glossed over and ``\xff`` is
allowed to represent the code point 255, because characters *always*
represent code points. In strings, however, ``\x`` escapes always
represent bytes, not code points, whereas ``\u`` and ``\U`` escapes
always represent code points, which are encoded in one or more bytes.
For code points less than ``\u80``, it happens that the UTF-8
encoding of each code point is just the single byte produced by the
corresponding ``\x`` escape, so the distinction can safely be ignored.
For the escapes ``\x80`` through ``\xff`` as compared to ``\u80``
through ``\uff``, however, there is a major difference: the former
escapes all encode single bytes, which — unless followed by very
specific continuation bytes — do not form valid UTF-8 data, whereas the
latter escapes all represent Unicode code points with two-byte
encodings.

If this is all extremely confusing, try reading `"The Absolute Minimum
Every Software Developer Absolutely, Positively Must Know About Unicode
and Character
Sets" <http://www.joelonsoftware.com/articles/Unicode.html>`_. It's an
excellent introduction to Unicode and UTF-8, and may help alleviate some
confusion regarding the matter.

.. _man-version-number-literals:

Version Number Literals
-----------------------

Version numbers can easily be expressed with non-standard string literals of
the form ``v"..."``. Version number literals create :obj:`VersionNumber` objects
which follow the specifications of `semantic versioning <http://semver.org>`_,
and therefore are composed of major, minor and patch numeric values, followed
by pre-release and build alpha-numeric annotations. For example,
``v"0.2.1-rc1+win64"`` is broken into major version ``0``, minor version ``2``,
patch version ``1``, pre-release ``rc1`` and build ``win64``. When entering a
version literal, everything except the major version number is optional,
therefore e.g.  ``v"0.2"`` is equivalent to ``v"0.2.0"`` (with empty
pre-release/build annotations), ``v"2"`` is equivalent to ``v"2.0.0"``, and so
on.

:obj:`VersionNumber` objects are mostly useful to easily and correctly compare two
(or more) versions. For example, the constant ``VERSION`` holds Julia version
number as a :obj:`VersionNumber` object, and therefore one can define some
version-specific behavior using simple statements as::

    if v"0.2" <= VERSION < v"0.3-"
        # do something specific to 0.2 release series
    end

Note that in the above example the non-standard version number ``v"0.3-"`` is
used, with a trailing ``-``: this notation is a Julia extension of the
standard, and it's used to indicate a version which is lower than any ``0.3``
release, including all of its pre-releases. So in the above example the code
would only run with stable ``0.2`` versions, and exclude such versions as
``v"0.3.0-rc1"``. In order to also allow for unstable (i.e. pre-release)
``0.2`` versions, the lower bound check should be modified like this: ``v"0.2-"
<= VERSION``.

Another non-standard version specification extension allows to use a trailing
``+`` to express an upper limit on build versions, e.g.  ``VERSION >
"v"0.2-rc1+"`` can be used to mean any version above ``0.2-rc1`` and any of its
builds: it will return ``false`` for version ``v"0.2-rc1+win64"`` and ``true``
for ``v"0.2-rc2"``.

It is good practice to use such special versions in comparisons (particularly,
the trailing ``-`` should always be used on upper bounds unless there's a good
reason not to), but they must not be used as the actual version number of
anything, as they are invalid in the semantic versioning scheme.

Besides being used for the :const:`VERSION` constant, :obj:`VersionNumber` objects are
widely used in the :mod:`Pkg <Base.Pkg>` module, to specify packages versions and their
dependencies.
