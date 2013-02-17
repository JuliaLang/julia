.. _man-strings:

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

-  ``String`` is an abstraction, not a concrete type — many different
   representations can implement the ``String`` interface, but they can
   easily be used together and interact transparently. Any string type
   can be used in any function expecting a ``String``.
-  Like C and Java, but unlike most dynamic languages, Julia has a
   first-class type representing a single character, called ``Char``.
   This is just a special kind of 32-bit integer whose numeric value
   represents a Unicode code point.
-  As in Java, strings are immutable: the value of a ``String`` object
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

A ``Char`` value represents a single character: it is just a 32-bit
integer with a special literal representation and appropriate arithmetic
behaviors, whose numeric value is interpreted as a `Unicode code
point <http://en.wikipedia.org/wiki/Code_point>`_. Here is how ``Char``
values are input and shown::

    julia> 'x'
    'x'

    julia> typeof(ans)
    Char

You can convert a ``Char`` to its integer value, i.e. code point,
easily::

    julia> int('x')
    120

    julia> typeof(ans)
    Int64

On 32-bit architectures, ``typeof(ans)`` will be Int32. You can convert an integer 
value back to a ``Char`` just as easily::

    julia> char(120)
    'x'

Not all integer values are valid Unicode code points, but for
performance, the ``char`` conversion does not check that every character
value is valid. If you want to check that each converted value is a
valid code point, use the ``safe_char`` conversion instead::

    julia> char(0x110000)
    '\U110000'

    julia> safe_char(0x110000)
    invalid Unicode code point: U+110000

As of this writing, the valid Unicode code points are ``U+00`` through
``U+d7ff`` and ``U+e000`` through ``U+10ffff``. These have not all been
assigned intelligible meanings yet, nor are they necessarily
interpretable by applications, but all of these values are considered to
be valid Unicode characters.

You can input any Unicode character in single quotes using ``\u``
followed by up to four hexadecimal digits or ``\U`` followed by up to
eight hexadecimal digits (the longest valid value only requires six)::

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
also be used::

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
``Char`` values::

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

String Basics
-------------

Here a variable is initialized with a simple string literal::

    julia> str = "Hello, world.\n"
    "Hello, world.\n"

If you want to extract a character from a string, you index into it::

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
shorthand for the last index (computed by ``endof(str)``).
You can perform arithmetic and other operations with ``end``, just like
a normal value::

    julia> str[end-1]
    '.'

    julia> str[end/2]
    ' '

    julia> str[end/3]
    'o'

    julia> str[end/4]
    'l'

Using an index less than 1 or greater than ``end`` raises an error::

    julia> str[0]
    BoundsError()

    julia> str[end+1]
    BoundsError()

You can also extract a substring using range indexing::

    julia> str[4:9]
    "lo, wo"

Note the distinction between ``str[k]`` and ``str[k:k]``::

    julia> str[6]
    ','

    julia> str[6:6]
    ","

The former is a single character value of type ``Char``, while the
latter is a string value that happens to contain only a single
character. In Julia these are very different things.

Unicode and UTF-8
-----------------

Julia fully supports Unicode characters and strings. As `discussed
above <#characters>`_, in character literals, Unicode code points can be
represented using unicode ``\u`` and ``\U`` escape sequences, as well as
all the standard C escape sequences. These can likewise be used to write
string literals::

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
such an invalid byte index, an error is thrown::

    julia> s[1]
    '∀'

    julia> s[2]
    invalid UTF-8 character index

    julia> s[3]
    invalid UTF-8 character index

    julia> s[4]
    ' '

In this case, the character ``∀`` is a three-byte character, so the
indices 2 and 3 are invalid and the next character's index is 4.

Because of variable-length encodings, the number of character in a
string (given by ``length(s)``) is not always the same as the last index.
If you iterate through the indices 1 through ``endof(s)`` and index
into ``s``, the sequence of characters returned, when errors aren't
thrown, is the sequence of characters comprising the string ``s``.
Thus, we do have the identity that ``length(s) <= endof(s)`` since each
character in a string must have its own index. The following is an
inefficient and verbose way to iterate through the characters of ``s``::

    julia> for i = 1:endof(s)
             try
               println(s[i])
             catch
               # ignore the index error
             end
           end
    ∀

    x

    ∃

    y

The blank lines actually have spaces on them. Fortunately, the above
awkward idiom is unnecessary for iterating through the characters in a
string, since you can just use the string as an iterable object, no
exception handling required::

    julia> for c in s
             println(c)
           end
    ∀

    x

    ∃

    y

UTF-8 is not the only encoding that Julia supports, and adding support
for new encodings is quite easy, but discussion of other encodings and
how to implement support for them is beyond the scope of this document
for the time being. For further discussion of UTF-8 encoding issues, see
the section below on `byte array literals <#Byte+Array+Literals>`_,
which goes into some greater detail.

.. _man-string-interpolation:

Interpolation
-------------

One of the most common and useful string operations is concatenation::

    julia> greet = "Hello"
    "Hello"

    julia> whom = "world"
    "world"

    julia> strcat(greet, ", ", whom, ".\n")
    "Hello, world.\n"

Constructing strings like this can become a bit cumbersome, however. To
reduce the need for these verbose calls to ``strcat``, Julia allows
interpolation into string literals using ``$``, as in Perl::

    julia> "$greet, $whom.\n"
    "Hello, world.\n"

This is more readable and convenient and equivalent to the above string
concatenation — the system rewrites this apparent single string literal
into a concatenation of string literals with variables.

The shortest complete expression after the ``$`` is taken as the
expression whose value is to be interpolated into the string. Thus, you
can interpolate any expression into a string using parentheses::

    julia> "1 + 2 = $(1 + 2)"
    "1 + 2 = 3"

The expression need not be contained in parentheses, however. For
example, since a literal array expression is not complete until the
opening ``[`` is closed by a matching ``]``, you can interpolate an
array like this::

    julia> x = 2; y = 3; z = 5;

    julia> "x,y,z: $[x,y,z]."
    "x,y,z: [2,3,5]."

Both concatenation and string interpolation call the generic ``string``
function to convert objects into ``String`` form. Most non-``String``
objects are converted to strings as they are shown in interactive
sessions::

    julia> v = [1,2,3]
    [1,2,3]

    julia> "v: $v"
    "v: [1,2,3]"

The ``string`` function is the identity for ``String`` and ``Char``
values, so these are interpolated into strings as themselves, unquoted
and unescaped::

    julia> c = 'x'
    'x'

    julia> "hi, $c"
    "hi, x"

To include a literal ``$`` in a string literal, escape it with a
backslash::

    julia> print("I have \$100 in my account.\n")
    I have $100 in my account.

Common Operations
-----------------

You can lexicographically compare strings using the standard comparison
operators::

    julia> "abracadabra" < "xylophone"
    true

    julia> "abracadabra" == "xylophone"
    false

    julia> "Hello, world." != "Goodbye, world."
    true

    julia> "1 + 2 = 3" == "1 + 2 = $(1 + 2)"
    true

You can search for the index of a particular character using the
``strchr`` function::

    julia> strchr("xylophone", 'x')
    1

    julia> strchr("xylophone", 'p')
    5

    julia> strchr("xylophone", 'z')
    0

You can start the search for a character at a given offset by providing
a third argument::

    julia> strchr("xylophone", 'o')
    4

    julia> strchr("xylophone", 'o', 5)
    7

    julia> strchr("xylophone", 'o', 8)
    0

Another handy string function is ``repeat``::

    julia> repeat(".:Z:.", 10)
    ".:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:..:Z:."

Some other useful functions include:

-  ``endof(str)`` gives the maximal (byte) index that can be used to
   index into ``str``.
-  ``length(str)`` the number of characters in ``str``.
-  ``i = start(str)`` gives the first valid index at which a character
   can be found in ``str`` (typically 1).
-  ``c, j = next(str,i)`` returns next character at or after the index
   ``i`` and the next valid character index following that. With
   ``start`` and ``endof``, can be used to iterate through the
   characters in ``str``.
-  ``ind2chr(str,i)`` gives the number of characters in ``str`` up to
   and including any at index ``i``.
-  ``chr2ind(str,j)`` gives the index at which the ``j``\ th character
   in ``str`` occurs.

.. _man-non-standard-string-literals:

Non-Standard String Literals
----------------------------

There are situations when you want to construct a string or use string
semantics, but the behavior of the standard string construct is not
quite what is needed. For these kinds of situations, Julia provides
*non-standard string literals*. A non-standard string literal looks like
a regular double-quoted string literal, but is immediately prefixed by
an identifier, and doesn't behave quite like a normal string literal.

Two types of interpretation are performed on normal Julia string
literals: interpolation and unescaping (escaping is the act of
expressing a non-standard character with a sequence like ``\n``, whereas
unescaping is the process of interpreting such escape sequences as
actual characters). There are cases where its convenient to disable
either or both of these behaviors. For such situations, Julia provides
three types of non-standard string literals:

-  ``E"..."`` interpret escape sequences but do not interpolate, thereby
   rendering ``$`` a harmless, normal character.
-  ``I"..."`` perform interpolation but do not interpret escape
   sequences specially.
-  ``L"..."`` perform neither unescaping nor interpolation.

Suppose, for example, you would like to write strings that will contain
many ``$`` characters without interpolation. You can, as described
above, escape the ``$`` characters with a preceding backslash. This can
become tedious, however. Non-standard string literals prefixed with
``E`` do not perform string interpolation::

    julia> E"I have $100 in my account.\n"
    "I have \$100 in my account.\n"

This allows you to have ``$`` characters inside of string literals
without triggering interpolation and without needing to escape those
``$``\ s by preceding them with a ``\``. Escape sequences, such as the
``\n`` above, still behave as usual, so '' becomes a newline character.

On the other hand, ``I"..."`` string literals perform interpolation but
no unescaping::

    julia> I"I have $100 in my account.\n"
    "I have 100 in my account.\\n"

The value of the expression ``100`` is interpolated into the string,
yielding the decimal string representation of the value 100 — namely
``"100"`` (sorry, that might be a bit confusing). The trailing ``\n``
sequence is taken as literal backslash and ``n`` characters, rather than
being interpreted as a single newline character.

The third non-standard string form interprets all the characters between
the opening and closing quotes literally: the ``L"..."`` form. Here is
an example usage::

    julia> L"I have $100 in my account.\n"
    "I have \$100 in my account.\\n"

Neither the ``$`` nor the ``\n`` sequence are specially interpreted.

Byte Array Literals
~~~~~~~~~~~~~~~~~~~

Some string literal forms don't create strings at all. In the `next
section <#regular-expressions>`_, we will see that regular expressions
are written as non-standard string literals. Another useful non-standard
string literal, however, is the byte-array string literal: ``b"..."``.
This form lets you use string notation to express literal byte arrays —
i.e. arrays of ``Uint8`` values. The convention is that non-standard
literals with uppercase prefixes produce actual string objects, while
those with lowercase prefixes produce non-string objects like byte
arrays or compiled regular expressions. The rules for byte array
literals are the following:

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
three::

    julia> b"DATA\xff\u2200"
    [68,65,84,65,255,226,136,128]

The ASCII string "DATA" corresponds to the bytes 68, 65, 84, 65.
``\xff`` produces the single byte 255. The Unicode escape ``\u2200`` is
encoded in UTF-8 as the three bytes 226, 136, 128. Note that the
resulting byte array does not correspond to a valid UTF-8 string — if
you try to use this as a regular string literal, you will get a syntax
error::

    julia> "DATA\xff\u2200"
    syntax error: invalid UTF-8 sequence

Also observe the significant distinction between ``\xff`` and ``\uff``:
the former escape sequence encodes the *byte 255*, whereas the latter
escape sequence represents the *code point 255*, which is encoded as two
bytes in UTF-8::

    julia> b"\xff"
    1-element Uint8 Array:
     0xff

    julia> b"\uff"
    2-element Uint8 Array:
     0xc3
     0xbf

In character literals, this distinction is glossed over and ``\xff`` is
allowed to represent the code point 255, because characters *always*
represent code points. In strings, however, ``\x`` escapes always
represent bytes, not code points, whereas ``\u`` and ``\U`` escapes
always represent code points, which are encoded in one or more bytes.
For code points less than ``\u80``, it happens that the the UTF-8
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

In byte array literals, objects interpolate as their binary
representation rather than as their string representation::

    julia> msg = "Hello."
    "Hello."

    julia> len = uint16(length(msg))
    6

    julia> b"$len$msg"
    [6,0,72,101,108,108,111,46]

Here the first two bytes are the native (little-endian on x86) binary
representation of the length of the string "Hello.", encoded as a
unsigned 16-bit integer, while the following bytes are the ASCII bytes
of the string "Hello." itself.

Regular Expressions
-------------------

Julia has Perl-compatible regular expressions, as provided by the
`PCRE <http://www.pcre.org/>`_ library. Regular expressions are related
to strings in two ways: the obvious connection is that regular
expressions are used to find regular patterns in strings; the other
connection is that regular expressions are themselves input as strings,
which are parsed into a state machine that can be used to efficiently
search for patterns in strings. In Julia, regular expressions are input
using non-standard string literals prefixed with various identifiers
beginning with ``r``. The most basic regular expression literal without
any options turned on just uses ``r"..."``::

    julia> r"^\s*(?:#|$)"
    r"^\s*(?:#|$)"

    julia> typeof(ans)
    Regex

To check if a regex matches a string, use the ``ismatch`` function::

    julia> ismatch(r"^\s*(?:#|$)", "not a comment")
    false

    julia> ismatch(r"^\s*(?:#|$)", "# a comment")
    true

As one can see here, ``ismatch`` simply returns true or false,
indicating whether the given regex matches the string or not. Commonly,
however, one wants to know not just whether a string matched, but also
*how* it matched. To capture this information about a match, use the
``match`` function instead::

    julia> match(r"^\s*(?:#|$)", "not a comment")

    julia> match(r"^\s*(?:#|$)", "# a comment")
    RegexMatch("#")

If the regular expression does not match the given string, ``match``
returns ``nothing`` — a special value that does not print anything at
the interactive prompt. Other than not printing, it is a completely
normal value and you can test for it programmatically::

    m = match(r"^\s*(?:#|$)", line)
    if m == nothing
      println("not a comment")
    else
      println("blank or comment")
    end

If a regular expression does match, the value returned by ``match`` is a
``RegexMatch`` object. These objects record how the expression matches,
including the substring that the pattern matches and any captured
substrings, if there are any. This example only captures the portion of
the substring that matches, but perhaps we want to capture any non-blank
text after the comment character. We could do the following::

    julia> m = match(r"^\s*(?:#\s*(.*?)\s*$|$)", "# a comment ")
    RegexMatch("# a comment ", 1="a comment")

You can extract the following info from a ``RegexMatch`` object:

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
    3-element Union(UTF8String,ASCIIString,Nothing) Array:
     "a"
     "c"
     "d"

    julia> m.offset
    1

    julia> m.offsets
    3-element Int64 Array:
     1
     2
     3

    julia> m = match(r"(a|b)(c)?(d)", "ad")
    RegexMatch("ad", 1="a", 2=nothing, 3="d")

    julia> m.match
    "ad"

    julia> m.captures
    3-element Union(UTF8String,ASCIIString,Nothing) Array:
     "a"
     nothing
     "d"

    julia> m.offset
    1

    julia> m.offsets
    3-element Int64 Array:
     1
     0
     2

It is convenient to have captures returned as a tuple so that one can
use tuple destructuring syntax to bind them to local variables::

    julia> first, second, third = m.captures; first
    "a"

You can modify the behavior of regular expressions by some combination of
the flags ``i``, ``m``, ``s``, and ``x`` after the closing double quote
mark. These flags have the same meaning as they do in Perl, as explained
in this excerpt from the `perlre
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

For example, the following regex has all three flags turned on::

    julia> r"a+.*b+.*?d$"ism
    r"a+.*b+.*?d$"ims

    julia> match(r"a+.*b+.*?d$"ism, "Goodbye,\nOh, angry,\nBad world\n")
    RegexMatch("angry,\nBad world")
