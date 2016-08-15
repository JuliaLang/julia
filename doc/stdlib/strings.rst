.. currentmodule:: Base

*********
 Strings
*********

.. function:: length(s::AbstractString)

   .. Docstring generated from Julia source

   The number of characters in string ``s``\ .

.. function:: sizeof(s::AbstractString)

   .. Docstring generated from Julia source

   The number of bytes in string ``s``\ .

   .. doctest::

       julia> sizeof("❤")
       3

.. function:: *(s::AbstractString, t::AbstractString)

   .. Docstring generated from Julia source

   Concatenate strings. The ``*`` operator is an alias to this function.

   .. doctest::

       julia> "Hello " * "world"
       "Hello world"

.. function:: ^(s::AbstractString, n::Integer)

   .. Docstring generated from Julia source

   Repeat ``n`` times the string ``s``\ . The :func:`repeat` function is an alias to this operator.

   .. doctest::

       julia> "Test "^3
       "Test Test Test "

.. function:: string(xs...)

   .. Docstring generated from Julia source

   Create a string from any values using the :func:`print` function.

.. function:: repr(x)

   .. Docstring generated from Julia source

   Create a string from any value using the :func:`showall` function.

.. function:: String(s::AbstractString)

   .. Docstring generated from Julia source

   Convert a string to a contiguous byte array representation encoded as UTF-8 bytes. This representation is often appropriate for passing strings to C.

.. function:: transcode(T, src)

   .. Docstring generated from Julia source

   Convert string data between Unicode encodings. ``src`` is either a ``String`` or a ``Vector{UIntXX}`` of UTF-XX code units, where ``XX`` is 8, 16, or 32. ``T`` indicates the encoding of the return value: ``String`` to return a (UTF-8 encoded) ``String`` or ``UIntXX`` to return a ``Vector{UIntXX}`` of UTF-``XX`` data.   (The alias ``Cwchar_t`` can also be used as the integer type, for converting ``wchar_t*`` strings used by external C libraries.)

   The ``transcode`` function succeeds as long as the input data can be reasonably represented in the target encoding; it always succeeds for conversions between UTF-XX encodings, even for invalid Unicode data.

   Only conversion to/from UTF-8 is currently supported.

.. function:: unsafe_string(p::Ptr{UInt8}, [length::Integer])

   .. Docstring generated from Julia source

   Copy a string from the address of a C-style (NUL-terminated) string encoded as UTF-8. (The pointer can be safely freed afterwards.) If ``length`` is specified (the length of the data in bytes), the string does not have to be NUL-terminated.

   This function is labelled "unsafe" because it will crash if ``p`` is not a valid memory address to data of the requested length.

   See also :func:`unsafe_wrap`\ , which takes a pointer and wraps a string object around it without making a copy.

.. function:: unsafe_wrap(String, p::Ptr{UInt8}, [length,] own=false)

   .. Docstring generated from Julia source

   Wrap a pointer ``p`` to an array of bytes in a ``String`` object, interpreting the bytes as UTF-8 encoded characters *without making a copy*. The optional ``length`` argument indicates the length in bytes of the pointer's data; if it is omitted, the data is assumed to be NUL-terminated.  The ``own`` argument optionally specifies whether Julia should take ownership of the memory, calling ``free`` on the pointer when the array is no longer referenced.

   This function is labelled "unsafe" because it will crash if ``p`` is not a valid memory address to data of the requested length.

   See also :func:`unsafe_string`\ , which takes a pointer and makes a copy of the data.

.. function:: ascii(s::AbstractString)

   .. Docstring generated from Julia source

   Convert a string to ``String`` type and check that it contains only ASCII data, otherwise throwing an ``ArgumentError`` indicating the position of the first non-ASCII byte.

.. function:: @r_str -> Regex

   .. Docstring generated from Julia source

   Construct a regex, such as ``r"^[a-z]*$"``\ . The regex also accepts one or more flags, listed after the ending quote, to change its behaviour:

   * ``i`` enables case-insensitive matching
   * ``m`` treats the ``^`` and ``$`` tokens as matching the start and end of individual lines, as opposed to the whole string.
   * ``s`` allows the ``.`` modifier to match newlines.
   * ``x`` enables "comment mode": whitespace is enabled except when escaped with ``\``\ , and ``#`` is treated as starting a comment.

   For example, this regex has all three flags enabled:

   .. code-block:: julia

       julia> match(r"a+.*b+.*?d$"ism, "Goodbye,\nOh, angry,\nBad world\n")
       RegexMatch("angry,\nBad world")

.. function:: @html_str -> Docs.HTML

   .. Docstring generated from Julia source

   Create an ``HTML`` object from a literal string.

.. function:: @text_str -> Docs.Text

   .. Docstring generated from Julia source

   Create a ``Text`` object from a literal string.

.. function:: normalize_string(s::AbstractString, normalform::Symbol)

   .. Docstring generated from Julia source

   Normalize the string ``s`` according to one of the four "normal forms" of the Unicode standard: ``normalform`` can be ``:NFC``\ , ``:NFD``\ , ``:NFKC``\ , or ``:NFKD``\ .  Normal forms C (canonical composition) and D (canonical decomposition) convert different visually identical representations of the same abstract string into a single canonical form, with form C being more compact.  Normal forms KC and KD additionally canonicalize "compatibility equivalents": they convert characters that are abstractly similar but visually distinct into a single canonical choice (e.g. they expand ligatures into the individual characters), with form KC being more compact.

   Alternatively, finer control and additional transformations may be be obtained by calling ``normalize_string(s; keywords...)``\ , where any number of the following boolean keywords options (which all default to ``false`` except for ``compose``\ ) are specified:

   * ``compose=false``\ : do not perform canonical composition
   * ``decompose=true``\ : do canonical decomposition instead of canonical composition (``compose=true`` is ignored if present)
   * ``compat=true``\ : compatibility equivalents are canonicalized
   * ``casefold=true``\ : perform Unicode case folding, e.g. for case-insensitive string comparison
   * ``newline2lf=true``\ , ``newline2ls=true``\ , or ``newline2ps=true``\ : convert various newline sequences (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or paragraph-separation (PS) character, respectively
   * ``stripmark=true``\ : strip diacritical marks (e.g. accents)
   * ``stripignore=true``\ : strip Unicode's "default ignorable" characters (e.g. the soft hyphen or the left-to-right marker)
   * ``stripcc=true``\ : strip control characters; horizontal tabs and form feeds are converted to spaces; newlines are also converted to spaces unless a newline-conversion flag was specified
   * ``rejectna=true``\ : throw an error if unassigned code points are found
   * ``stable=true``\ : enforce Unicode Versioning Stability

   For example, NFKC corresponds to the options ``compose=true, compat=true, stable=true``\ .

.. function:: graphemes(s::AbstractString) -> GraphemeIterator

   .. Docstring generated from Julia source

   Returns an iterator over substrings of ``s`` that correspond to the extended graphemes in the string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as single characters, even though they may contain more than one codepoint; for example a letter combined with an accent mark is a single grapheme.)

.. function:: isvalid(value) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if the given value is valid for its type, which currently can be either ``Char`` or ``String``\ .

.. function:: isvalid(T, value) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if the given value is valid for that type. Types currently can be either ``Char`` or ``String``\ . Values for ``Char`` can be of type ``Char`` or ``UInt32``\ . Values for ``String`` can be of that type, or ``Vector{UInt8}``\ .

.. function:: isvalid(str::AbstractString, i::Integer)

   .. Docstring generated from Julia source

   Tells whether index ``i`` is valid for the given string.

.. function:: is_assigned_char(c) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if the given char or integer is an assigned Unicode code point.

.. function:: ismatch(r::Regex, s::AbstractString) -> Bool

   .. Docstring generated from Julia source

   Test whether a string contains a match of the given regular expression.

.. function:: match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   .. Docstring generated from Julia source

   Search for the first match of the regular expression ``r`` in ``s`` and return a ``RegexMatch`` object containing the match, or nothing if the match failed. The matching substring can be retrieved by accessing ``m.match`` and the captured sequences can be retrieved by accessing ``m.captures`` The optional ``idx`` argument specifies an index at which to start the search.

.. function:: eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   .. Docstring generated from Julia source

   Search for all matches of a the regular expression ``r`` in ``s`` and return a iterator over the matches. If overlap is ``true``\ , the matching sequences are allowed to overlap indices in the original string, otherwise they must be from distinct character ranges.

.. function:: matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   .. Docstring generated from Julia source

   Return a vector of the matching substrings from :func:`eachmatch`\ .

.. function:: lpad(s, n::Integer, p::AbstractString=" ")

   .. Docstring generated from Julia source

   Make a string at least ``n`` columns wide when printed by padding ``s`` on the left with copies of ``p``\ .

   .. doctest::

       julia> lpad("March",10)
       "     March"

.. function:: rpad(s, n::Integer, p::AbstractString=" ")

   .. Docstring generated from Julia source

   Make a string at least ``n`` columns wide when printed by padding ``s`` on the right with copies of ``p``\ .

   .. doctest::

       julia> rpad("March",20)
       "March               "

.. function:: search(string::AbstractString, chars::Chars, [start::Integer])

   .. Docstring generated from Julia source

   Search for the first occurrence of the given characters within the given string. The second argument may be a single character, a vector or a set of characters, a string, or a regular expression (though regular expressions are only allowed on contiguous strings, such as ASCII or UTF-8 strings). The third argument optionally specifies a starting index. The return value is a range of indexes where the matching sequence is found, such that ``s[search(s,x)] == x``\ :

   ``search(string, "substring")`` = ``start:end`` such that ``string[start:end] == "substring"``\ , or ``0:-1`` if unmatched.

   ``search(string, 'c')`` = ``index`` such that ``string[index] == 'c'``\ , or ``0`` if unmatched.

   .. doctest::

       julia> search("Hello to the world", "z")
       0:-1

       julia> search("JuliaLang","Julia")
       1:5

.. function:: rsearch(s::AbstractString, chars::Chars, [start::Integer])

   .. Docstring generated from Julia source

   Similar to :func:`search`\ , but returning the last occurrence of the given characters within the given string, searching in reverse from ``start``\ .

   .. doctest::

       julia> rsearch("aaabbb","b")
       6:6

.. function:: searchindex(s::AbstractString, substring, [start::Integer])

   .. Docstring generated from Julia source

   Similar to :func:`search`\ , but return only the start index at which the substring is found, or ``0`` if it is not.

.. function:: rsearchindex(s::AbstractString, substring, [start::Integer])

   .. Docstring generated from Julia source

   Similar to :func:`rsearch`\ , but return only the start index at which the substring is found, or ``0`` if it is not.

.. function:: contains(haystack::AbstractString, needle::AbstractString)

   .. Docstring generated from Julia source

   Determine whether the second argument is a substring of the first.

   .. doctest::

       julia> contains("JuliaLang is pretty cool!", "Julia")
       true

.. function:: reverse(s::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   Reverses a string.

   .. doctest::

       julia> reverse("JuliaLang")
       "gnaLailuJ"

.. function:: replace(string::AbstractString, pat, r[, n::Integer=0])

   .. Docstring generated from Julia source

   Search for the given pattern ``pat``\ , and replace each occurrence with ``r``\ . If ``n`` is provided, replace at most ``n`` occurrences. As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression. If ``r`` is a function, each occurrence is replaced with ``r(s)`` where ``s`` is the matched substring. If ``pat`` is a regular expression and ``r`` is a ``SubstitutionString``\ , then capture group references in ``r`` are replaced with the corresponding matched text.

.. function:: split(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

   .. Docstring generated from Julia source

   Return an array of substrings by splitting the given string on occurrences of the given character delimiters, which may be specified in any of the formats allowed by ``search``\ 's second argument (i.e. a single character, collection of characters, string, or regular expression). If ``chars`` is omitted, it defaults to the set of all space characters, and ``keep`` is taken to be ``false``\ . The two keyword arguments are optional: they are a maximum size for the result and a flag determining whether empty fields should be kept in the result.

   .. doctest::

       julia> a = "Ma.rch"
       "Ma.rch"

       julia> split(a,".")
       2-element Array{SubString{String},1}:
        "Ma"
        "rch"

.. function:: rsplit(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

   .. Docstring generated from Julia source

   Similar to :func:`split`\ , but starting from the end of the string.

   .. doctest::

       julia> a = "M.a.r.c.h"
       "M.a.r.c.h"

       julia> rsplit(a,".")
       5-element Array{SubString{String},1}:
        "M"
        "a"
        "r"
        "c"
        "h"

       julia> rsplit(a,".";limit=1)
       1-element Array{SubString{String},1}:
        "M.a.r.c.h"

       julia> rsplit(a,".";limit=2)
       2-element Array{SubString{String},1}:
        "M.a.r.c"
        "h"

.. function:: strip(s::AbstractString, [chars::Chars])

   .. Docstring generated from Julia source

   Return ``s`` with any leading and trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: lstrip(s::AbstractString[, chars::Chars])

   .. Docstring generated from Julia source

   Return ``s`` with any leading whitespace and delimiters removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: rstrip(s::AbstractString[, chars::Chars])

   .. Docstring generated from Julia source

   Return ``s`` with any trailing whitespace and delimiters removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

   .. doctest::

       julia> a = rpad("March",20)
       "March               "

       julia> rstrip(a)
       "March"

.. function:: startswith(s::AbstractString, prefix::AbstractString)

   .. Docstring generated from Julia source

   Returns ``true`` if ``s`` starts with ``prefix``\ . If ``prefix`` is a vector or set of characters, tests whether the first character of ``s`` belongs to that set.

   .. doctest::

       julia> startswith("JuliaLang", "Julia")
       true

.. function:: endswith(s::AbstractString, suffix::AbstractString)

   .. Docstring generated from Julia source

   Returns ``true`` if ``s`` ends with ``suffix``\ . If ``suffix`` is a vector or set of characters, tests whether the last character of ``s`` belongs to that set.

   .. doctest::

       julia> endswith("Sunday", "day")
       true

.. function:: uppercase(s::AbstractString)

   .. Docstring generated from Julia source

   Returns ``s`` with all characters converted to uppercase.

   .. doctest::

       julia> uppercase("Julia")
       "JULIA"

.. function:: lowercase(s::AbstractString)

   .. Docstring generated from Julia source

   Returns ``s`` with all characters converted to lowercase.

   .. doctest::

       julia> lowercase("STRINGS AND THINGS")
       "strings and things"

.. function:: ucfirst(s::AbstractString)

   .. Docstring generated from Julia source

   Returns ``string`` with the first character converted to uppercase.

   .. doctest::

       julia> ucfirst("python")
       "Python"

.. function:: lcfirst(s::AbstractString)

   .. Docstring generated from Julia source

   Returns ``string`` with the first character converted to lowercase.

   .. doctest::

       julia> lcfirst("Julia")
       "julia"

.. function:: join(io::IO, strings, delim, [last])

   .. Docstring generated from Julia source

   Join an array of ``strings`` into a single string, inserting the given delimiter between adjacent strings. If ``last`` is given, it will be used instead of ``delim`` between the last two strings. For example,

   .. doctest::

       julia> join(["apples", "bananas", "pineapples"], ", ", " and ")
       "apples, bananas and pineapples"

   ``strings`` can be any iterable over elements ``x`` which are convertible to strings via ``print(io::IOBuffer, x)``\ .

.. function:: chop(s::AbstractString)

   .. Docstring generated from Julia source

   Remove the last character from a string.

   .. doctest::

       julia> a = string("March")
       "March"

       julia> chop(a)
       "Marc"

.. function:: chomp(s::AbstractString)

   .. Docstring generated from Julia source

   Remove a single trailing newline from a string.

.. function:: ind2chr(s::AbstractString, i::Integer)

   .. Docstring generated from Julia source

   Convert a byte index ``i`` to a character index.

.. function:: chr2ind(s::AbstractString, i::Integer)

   .. Docstring generated from Julia source

   Convert a character index ``i`` to a byte index.

.. function:: nextind(str::AbstractString, i::Integer)

   .. Docstring generated from Julia source

   Get the next valid string index after ``i``\ . Returns a value greater than ``endof(str)`` at or after the end of the string.

.. function:: prevind(str::AbstractString, i::Integer)

   .. Docstring generated from Julia source

   Get the previous valid string index before ``i``\ . Returns a value less than ``1`` at the beginning of the string.

.. function:: randstring([rng,] len=8)

   .. Docstring generated from Julia source

   Create a random ASCII string of length ``len``\ , consisting of upper- and lower-case letters and the digits 0-9. The optional ``rng`` argument specifies a random number generator, see :ref:`Random Numbers <random-numbers>`\ .

.. function:: charwidth(c)

   .. Docstring generated from Julia source

   Gives the number of columns needed to print a character.

.. function:: strwidth(s::AbstractString)

   .. Docstring generated from Julia source

   Gives the number of columns needed to print a string.

   .. doctest::

       julia> strwidth("March")
       5

.. function:: isalnum(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is alphanumeric, or whether this is true for all elements of a string. A character is classified as alphabetic if it belongs to the Unicode general category Letter or Number, i.e. a character whose category code begins with 'L' or 'N'.

.. function:: isalpha(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is alphabetic, or whether this is true for all elements of a string. A character is classified as alphabetic if it belongs to the Unicode general category Letter, i.e. a character whose category code begins with 'L'.

.. function:: isascii(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character belongs to the ASCII character set, or whether this is true for all elements of a string.

.. function:: iscntrl(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is a control character, or whether this is true for all elements of a string. Control characters are the non-printing characters of the Latin-1 subset of Unicode.

.. function:: isdigit(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is a numeric digit (0-9), or whether this is true for all elements of a string.

.. function:: isgraph(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is printable, and not a space, or whether this is true for all elements of a string. Any character that would cause a printer to use ink should be classified with ``isgraph(c)==true``\ .

.. function:: islower(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is a lowercase letter, or whether this is true for all elements of a string. A character is classified as lowercase if it belongs to Unicode category Ll, Letter: Lowercase.

.. function:: isnumber(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is numeric, or whether this is true for all elements of a string. A character is classified as numeric if it belongs to the Unicode general category Number, i.e. a character whose category code begins with 'N'.

.. function:: isprint(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is printable, including spaces, but not a control character. For strings, tests whether this is true for all elements of the string.

.. function:: ispunct(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character belongs to the Unicode general category Punctuation, i.e. a character whose category code begins with 'P'. For strings, tests whether this is true for all elements of the string.

.. function:: isspace(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is any whitespace character. Includes ASCII characters '\\t', '\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode category Zs. For strings, tests whether this is true for all elements of the string.

.. function:: isupper(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is an uppercase letter, or whether this is true for all elements of a string. A character is classified as uppercase if it belongs to Unicode category Lu, Letter: Uppercase, or Lt, Letter: Titlecase.

.. function:: isxdigit(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   Tests whether a character is a valid hexadecimal digit, or whether this is true for all elements of a string.

   .. doctest::

       julia> isxdigit("abc")
       true

       julia> isxdigit("0x9")
       false

.. function:: Symbol(x...) -> Symbol

   .. Docstring generated from Julia source

   Create a ``Symbol`` by concatenating the string representations of the arguments together.

.. function:: escape_string([io,] str::AbstractString[, esc::AbstractString]) -> AbstractString

   .. Docstring generated from Julia source

   General escaping of traditional C and Unicode escape sequences. Any characters in ``esc`` are also escaped (with a backslash). See also :func:`unescape_string`\ .

.. function:: unescape_string([io,] s::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   General unescaping of traditional C and Unicode escape sequences. Reverse of :func:`escape_string`\ .

