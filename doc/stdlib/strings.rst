.. currentmodule:: Base


*********
 Strings
*********

.. function:: length(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       length(A) -> Integer

   Returns the number of elements in A

   .. code-block:: julia

       length(collection) -> Integer

   For ordered, indexable collections, the maximum index ``i`` for which ``getindex(collection, i)`` is valid. For unordered collections, the number of elements.

   .. code-block:: julia

       length(s)

   The number of characters in string ``s``\ .

.. function:: sizeof(s::AbstractString)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sizeof(type)

   Size, in bytes, of the canonical binary representation of the given type, if any.

   .. code-block:: julia

       sizeof(s::AbstractString)

   The number of bytes in string ``s``\ .

.. function:: *(s, t)

   .. Docstring generated from Julia source

   ::

              *(A, B)

   Matrix multiplication

   ::

              *(x, y...)

   Multiplication operator. ``x*y*z*...`` calls this function with all arguments, i.e.
   ``*(x, y, z, ...)``.

   ::

              *(s, t)

   Concatenate strings. The ``*`` operator is an alias to this function.


	   julia> "Hello " * "world"
	   "Hello world"

.. function:: ^(s, n)

   .. Docstring generated from Julia source

   ::

              ^(x, y)

   Exponentiation operator.

   ::

              ^(s, n)

   Repeat ``n`` times the string ``s``. The ``^`` operator is an alias to this function.

   .. doctest::

   	julia> "Test "^3
   	"Test Test Test "

.. function:: string(xs...)

   .. Docstring generated from Julia source

   .. code-block:: julia

       string(xs...)

   Create a string from any values using the ``print`` function.

.. function:: repr(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       repr(x)

   Create a string from any value using the ``showall`` function.

.. function:: bytestring(::Ptr{UInt8}, [length])

   .. Docstring generated from Julia source

   .. code-block:: julia

       bytestring(::Ptr{UInt8}, [length])

   Create a string from the address of a C (0-terminated) string encoded in ASCII or UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

   .. code-block:: julia

       bytestring(s)

   Convert a string to a contiguous byte array representation appropriate for passing it to C functions. The string will be encoded as either ASCII or UTF-8.

.. function:: bytestring(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       bytestring(::Ptr{UInt8}, [length])

   Create a string from the address of a C (0-terminated) string encoded in ASCII or UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

   .. code-block:: julia

       bytestring(s)

   Convert a string to a contiguous byte array representation appropriate for passing it to C functions. The string will be encoded as either ASCII or UTF-8.

.. function:: ascii(::Array{UInt8,1})

   .. Docstring generated from Julia source

   .. code-block:: julia

       ascii(::Array{UInt8,1})

   Create an ASCII string from a byte array.

   .. code-block:: julia

       ascii(s)

   Convert a string to a contiguous ASCII string (all characters must be valid ASCII characters).

   .. code-block:: julia

       ascii(::Ptr{UInt8}, [length])

   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

.. function:: ascii(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ascii(::Array{UInt8,1})

   Create an ASCII string from a byte array.

   .. code-block:: julia

       ascii(s)

   Convert a string to a contiguous ASCII string (all characters must be valid ASCII characters).

   .. code-block:: julia

       ascii(::Ptr{UInt8}, [length])

   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

.. function:: ascii(::Ptr{UInt8}, [length])

   .. Docstring generated from Julia source

   .. code-block:: julia

       ascii(::Array{UInt8,1})

   Create an ASCII string from a byte array.

   .. code-block:: julia

       ascii(s)

   Convert a string to a contiguous ASCII string (all characters must be valid ASCII characters).

   .. code-block:: julia

       ascii(::Ptr{UInt8}, [length])

   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

.. function:: utf8(::Array{UInt8,1})

   .. Docstring generated from Julia source

   .. code-block:: julia

       utf8(::Array{UInt8,1})

   Create a UTF-8 string from a byte array.

   .. code-block:: julia

       utf8(::Ptr{UInt8}, [length])

   Create a UTF-8 string from the address of a C (0-terminated) string encoded in UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

   .. code-block:: julia

       utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).

.. function:: utf8(::Ptr{UInt8}, [length])

   .. Docstring generated from Julia source

   .. code-block:: julia

       utf8(::Array{UInt8,1})

   Create a UTF-8 string from a byte array.

   .. code-block:: julia

       utf8(::Ptr{UInt8}, [length])

   Create a UTF-8 string from the address of a C (0-terminated) string encoded in UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

   .. code-block:: julia

       utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).

.. function:: utf8(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       utf8(::Array{UInt8,1})

   Create a UTF-8 string from a byte array.

   .. code-block:: julia

       utf8(::Ptr{UInt8}, [length])

   Create a UTF-8 string from the address of a C (0-terminated) string encoded in UTF-8. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.

   .. code-block:: julia

       utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).

.. function:: normalize_string(s, normalform::Symbol)

   .. Docstring generated from Julia source

   ::

              normalize_string(s, normalform::Symbol)

   Normalize the string ``s`` according to one of the four "normal
   forms" of the Unicode standard: ``normalform`` can be ``:NFC``,
   ``:NFD``, ``:NFKC``, or ``:NFKD``.  Normal forms C (canonical
   composition) and D (canonical decomposition) convert different
   visually identical representations of the same abstract string into
   a single canonical form, with form C being more compact.  Normal
   forms KC and KD additionally canonicalize "compatibility
   equivalents": they convert characters that are abstractly similar
   but visually distinct into a single canonical choice (e.g. they expand
   ligatures into the individual characters), with form KC being more compact.

   Alternatively, finer control and additional transformations may be
   be obtained by calling `normalize_string(s; keywords...)`, where
   any number of the following boolean keywords options (which all default
   to ``false`` except for ``compose``) are specified:

   * ``compose=false``: do not perform canonical composition
   * ``decompose=true``: do canonical decomposition instead of canonical composition (``compose=true`` is ignored if present)
   * ``compat=true``: compatibility equivalents are canonicalized
   * ``casefold=true``: perform Unicode case folding, e.g. for case-insensitive string comparison
   * ``newline2lf=true``, ``newline2ls=true``, or ``newline2ps=true``: convert various newline sequences (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or paragraph-separation (PS) character, respectively
   * ``stripmark=true``: strip diacritical marks (e.g. accents)
   * ``stripignore=true``: strip Unicode's "default ignorable" characters (e.g. the soft hyphen or the left-to-right marker)
   * ``stripcc=true``: strip control characters; horizontal tabs and form feeds are converted to spaces; newlines are also converted to spaces unless a newline-conversion flag was specified
   * ``rejectna=true``: throw an error if unassigned code points are found
   * ``stable=true``: enforce Unicode Versioning Stability

   For example, NFKC corresponds to the options ``compose=true, compat=true, stable=true``.

.. function:: graphemes(s) -> iterator over substrings of s

   .. Docstring generated from Julia source

   .. code-block:: julia

       graphemes(s) -> iterator over substrings of s

   Returns an iterator over substrings of ``s`` that correspond to the extended graphemes in the string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as single characters, even though they may contain more than one codepoint; for example a letter combined with an accent mark is a single grapheme.)

.. function:: isvalid(value) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isvalid(value) -> Bool

   Returns true if the given value is valid for its type, which currently can be one of ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String``

   .. code-block:: julia

       isvalid(T, value) -> Bool

   Returns true if the given value is valid for that type. Types currently can be ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String`` Values for ``Char`` can be of type ``Char`` or ``UInt32`` Values for ``ASCIIString`` and ``UTF8String`` can be of that type, or ``Vector{UInt8}`` Values for ``UTF16String`` can be ``UTF16String`` or ``Vector{UInt16}`` Values for ``UTF32String`` can be ``UTF32String``\ , ``Vector{Char}`` or ``Vector{UInt32}``

   .. code-block:: julia

       isvalid(str, i)

   Tells whether index ``i`` is valid for the given string

.. function:: isvalid(T, value) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isvalid(value) -> Bool

   Returns true if the given value is valid for its type, which currently can be one of ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String``

   .. code-block:: julia

       isvalid(T, value) -> Bool

   Returns true if the given value is valid for that type. Types currently can be ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String`` Values for ``Char`` can be of type ``Char`` or ``UInt32`` Values for ``ASCIIString`` and ``UTF8String`` can be of that type, or ``Vector{UInt8}`` Values for ``UTF16String`` can be ``UTF16String`` or ``Vector{UInt16}`` Values for ``UTF32String`` can be ``UTF32String``\ , ``Vector{Char}`` or ``Vector{UInt32}``

   .. code-block:: julia

       isvalid(str, i)

   Tells whether index ``i`` is valid for the given string

.. function:: is_assigned_char(c) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       is_assigned_char(c) -> Bool

   Returns true if the given char or integer is an assigned Unicode code point.

.. function:: ismatch(r::Regex, s::AbstractString) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       ismatch(r::Regex, s::AbstractString) -> Bool

   Test whether a string contains a match of the given regular expression.

.. function:: match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   .. Docstring generated from Julia source

   .. code-block:: julia

       match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   Search for the first match of the regular expression ``r`` in ``s`` and return a RegexMatch object containing the match, or nothing if the match failed. The matching substring can be retrieved by accessing ``m.match`` and the captured sequences can be retrieved by accessing ``m.captures`` The optional ``idx`` argument specifies an index at which to start the search.

.. function:: eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   .. Docstring generated from Julia source

   .. code-block:: julia

       eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   Search for all matches of a the regular expression ``r`` in ``s`` and return a iterator over the matches. If overlap is true, the matching sequences are allowed to overlap indices in the original string, otherwise they must be from distinct character ranges.

.. function:: matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   .. Docstring generated from Julia source

   .. code-block:: julia

       matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   Return a vector of the matching substrings from eachmatch.

.. function:: lpad(string, n, p)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lpad(string, n, p)

   Make a string at least ``n`` columns wide when printed, by padding on the left with copies of ``p``\ .

.. function:: rpad(string, n, p)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rpad(string, n, p)

   Make a string at least ``n`` columns wide when printed, by padding on the right with copies of ``p``\ .

.. function:: search(string, chars, [start])

   .. Docstring generated from Julia source

   .. code-block:: julia

       search(string, chars, [start])

   Search for the first occurrence of the given characters within the given string. The second argument may be a single character, a vector or a set of characters, a string, or a regular expression (though regular expressions are only allowed on contiguous strings, such as ASCII or UTF-8 strings). The third argument optionally specifies a starting index. The return value is a range of indexes where the matching sequence is found, such that ``s[search(s,x)] == x``\ :

   ``search(string, "substring")`` = ``start:end`` such that ``string[start:end] == "substring"``\ , or ``0:-1`` if unmatched.

   ``search(string, 'c')`` = ``index`` such that ``string[index] == 'c'``\ , or ``0`` if unmatched.

.. function:: rsearch(string, chars, [start])

   .. Docstring generated from Julia source

   .. code-block:: julia

       rsearch(string, chars, [start])

   Similar to ``search``\ , but returning the last occurrence of the given characters within the given string, searching in reverse from ``start``\ .

.. function:: searchindex(string, substring, [start])

   .. Docstring generated from Julia source

   .. code-block:: julia

       searchindex(string, substring, [start])

   Similar to ``search``\ , but return only the start index at which the substring is found, or 0 if it is not.

.. function:: rsearchindex(string, substring, [start])

   .. Docstring generated from Julia source

   .. code-block:: julia

       rsearchindex(string, substring, [start])

   Similar to ``rsearch``\ , but return only the start index at which the substring is found, or 0 if it is not.

.. function:: contains(haystack, needle)

   .. Docstring generated from Julia source

   .. code-block:: julia

       contains(haystack, needle)

   Determine whether the second argument is a substring of the first.

.. function:: replace(string, pat, r[, n])

   .. Docstring generated from Julia source

   .. code-block:: julia

       replace(string, pat, r[, n])

   Search for the given pattern ``pat``\ , and replace each occurrence with ``r``\ . If ``n`` is provided, replace at most ``n`` occurrences. As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression. If ``r`` is a function, each occurrence is replaced with ``r(s)`` where ``s`` is the matched substring. If ``pat`` is a regular expression and ``r`` is a ``SubstitutionString``\ , then capture group references in ``r`` are replaced with the corresponding matched text.

.. function:: split(string, [chars]; limit=0, keep=true)

   .. Docstring generated from Julia source

   .. code-block:: julia

       split(string, [chars]; limit=0, keep=true)

   Return an array of substrings by splitting the given string on occurrences of the given character delimiters, which may be specified in any of the formats allowed by ``search``\ 's second argument (i.e. a single character, collection of characters, string, or regular expression). If ``chars`` is omitted, it defaults to the set of all space characters, and ``keep`` is taken to be false. The two keyword arguments are optional: they are are a maximum size for the result and a flag determining whether empty fields should be kept in the result.

.. function:: rsplit(string, [chars]; limit=0, keep=true)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rsplit(string, [chars]; limit=0, keep=true)

   Similar to ``split``\ , but starting from the end of the string.

.. function:: strip(string, [chars])

   .. Docstring generated from Julia source

   .. code-block:: julia

       strip(string, [chars])

   Return ``string`` with any leading and trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: lstrip(string, [chars])

   .. Docstring generated from Julia source

   .. code-block:: julia

       lstrip(string, [chars])

   Return ``string`` with any leading whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: rstrip(string, [chars])

   .. Docstring generated from Julia source

   .. code-block:: julia

       rstrip(string, [chars])

   Return ``string`` with any trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.

.. function:: startswith(string, prefix | chars)

   .. Docstring generated from Julia source

   .. code-block:: julia

       startswith(string, prefix | chars)

   Returns ``true`` if ``string`` starts with ``prefix``\ . If the second argument is a vector or set of characters, tests whether the first character of ``string`` belongs to that set.

.. function:: endswith(string, suffix | chars)

   .. Docstring generated from Julia source

   .. code-block:: julia

       endswith(string, suffix | chars)

   Returns ``true`` if ``string`` ends with ``suffix``\ . If the second argument is a vector or set of characters, tests whether the last character of ``string`` belongs to that set.

.. function:: uppercase(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       uppercase(string)

   Returns ``string`` with all characters converted to uppercase.

.. function:: lowercase(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lowercase(string)

   Returns ``string`` with all characters converted to lowercase.

.. function:: ucfirst(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ucfirst(string)

   Returns ``string`` with the first character converted to uppercase.

.. function:: lcfirst(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lcfirst(string)

   Returns ``string`` with the first character converted to lowercase.

.. function:: join(strings, delim, [last])

   .. Docstring generated from Julia source

   .. code-block:: julia

       join(strings, delim, [last])

   Join an array of ``strings`` into a single string, inserting the given delimiter between adjacent strings. If ``last`` is given, it will be used instead of ``delim`` between the last two strings. For example, ``join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"``\ .

   ``strings`` can be any iterable over elements ``x`` which are convertible to strings via ``print(io::IOBuffer, x)``\ .

.. function:: chop(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       chop(string)

   Remove the last character from a string

.. function:: chomp(string)

   .. Docstring generated from Julia source

   .. code-block:: julia

       chomp(string)

   Remove a trailing newline from a string

.. function:: ind2chr(string, i)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ind2chr(string, i)

   Convert a byte index to a character index

.. function:: chr2ind(string, i)

   .. Docstring generated from Julia source

   .. code-block:: julia

       chr2ind(string, i)

   Convert a character index to a byte index

.. function:: isvalid(str, i)

   .. Docstring generated from Julia source

   .. code-block:: julia

       isvalid(value) -> Bool

   Returns true if the given value is valid for its type, which currently can be one of ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String``

   .. code-block:: julia

       isvalid(T, value) -> Bool

   Returns true if the given value is valid for that type. Types currently can be ``Char``\ , ``ASCIIString``\ , ``UTF8String``\ , ``UTF16String``\ , or ``UTF32String`` Values for ``Char`` can be of type ``Char`` or ``UInt32`` Values for ``ASCIIString`` and ``UTF8String`` can be of that type, or ``Vector{UInt8}`` Values for ``UTF16String`` can be ``UTF16String`` or ``Vector{UInt16}`` Values for ``UTF32String`` can be ``UTF32String``\ , ``Vector{Char}`` or ``Vector{UInt32}``

   .. code-block:: julia

       isvalid(str, i)

   Tells whether index ``i`` is valid for the given string

.. function:: nextind(str, i)

   .. Docstring generated from Julia source

   .. code-block:: julia

       nextind(str, i)

   Get the next valid string index after ``i``\ . Returns a value greater than ``endof(str)`` at or after the end of the string.

.. function:: prevind(str, i)

   .. Docstring generated from Julia source

   .. code-block:: julia

       prevind(str, i)

   Get the previous valid string index before ``i``\ . Returns a value less than ``1`` at the beginning of the string.

.. function:: randstring([rng,] len=8)

   .. Docstring generated from Julia source

   ::

              randstring([rng,] len=8)

   Create a random ASCII string of length ``len``, consisting of upper- and
   lower-case letters and the digits 0-9. The optional ``rng`` argument
   specifies a random number generator, see :ref:`Random Numbers <random-numbers>`.

.. function:: charwidth(c)

   .. Docstring generated from Julia source

   .. code-block:: julia

       charwidth(c)

   Gives the number of columns needed to print a character.

.. function:: strwidth(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       strwidth(s)

   Gives the number of columns needed to print a string.

.. function:: isalnum(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isalnum(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is alphanumeric, or whether this is true for all elements of a string. A character is classified as alphabetic if it belongs to the Unicode general category Letter or Number, i.e. a character whose category code begins with 'L' or 'N'.

.. function:: isalpha(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isalpha(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is alphabetic, or whether this is true for all elements of a string. A character is classified as alphabetic if it belongs to the Unicode general category Letter, i.e. a character whose category code begins with 'L'.

.. function:: isascii(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isascii(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character belongs to the ASCII character set, or whether this is true for all elements of a string.

.. function:: iscntrl(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       iscntrl(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is a control character, or whether this is true for all elements of a string. Control characters are the non-printing characters of the Latin-1 subset of Unicode.

.. function:: isdigit(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isdigit(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is a numeric digit (0-9), or whether this is true for all elements of a string.

.. function:: isgraph(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isgraph(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is printable, and not a space, or whether this is true for all elements of a string. Any character that would cause a printer to use ink should be classified with isgraph(c)==true.

.. function:: islower(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       islower(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is a lowercase letter, or whether this is true for all elements of a string. A character is classified as lowercase if it belongs to Unicode category Ll, Letter: Lowercase.

.. function:: isnumber(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isnumber(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is numeric, or whether this is true for all elements of a string. A character is classified as numeric if it belongs to the Unicode general category Number, i.e. a character whose category code begins with 'N'.

.. function:: isprint(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isprint(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is printable, including spaces, but not a control character. For strings, tests whether this is true for all elements of the string.

.. function:: ispunct(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       ispunct(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character belongs to the Unicode general category Punctuation, i.e. a character whose category code begins with 'P'. For strings, tests whether this is true for all elements of the string.

.. function:: isspace(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isspace(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is any whitespace character. Includes ASCII characters '\\t', '\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode category Zs. For strings, tests whether this is true for all elements of the string.

.. function:: isupper(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isupper(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is an uppercase letter, or whether this is true for all elements of a string. A character is classified as uppercase if it belongs to Unicode category Lu, Letter: Uppercase, or Lt, Letter: Titlecase.

.. function:: isxdigit(c::Union{Char,AbstractString}) -> Bool

   .. Docstring generated from Julia source

   .. code-block:: julia

       isxdigit(c::Union{Char,AbstractString}) -> Bool

   Tests whether a character is a valid hexadecimal digit, or whether this is true for all elements of a string.

.. function:: symbol(x...) -> Symbol

   .. Docstring generated from Julia source

   .. code-block:: julia

       symbol(x...) -> Symbol

   Create a ``Symbol`` by concatenating the string representations of the arguments together.

.. function:: escape_string(str::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   ::

              escape_string(str::AbstractString) -> AbstractString

   General escaping of traditional C and Unicode escape sequences. See :func:`print_escaped` for more general escaping.

.. function:: unescape_string(s::AbstractString) -> AbstractString

   .. Docstring generated from Julia source

   ::

              unescape_string(s::AbstractString) -> AbstractString

   General unescaping of traditional C and Unicode escape sequences. Reverse of :func:`escape_string`. See also :func:`print_unescaped`.

.. function:: utf16(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       utf16(s)

   Create a UTF-16 string from a byte array, array of ``UInt16``\ , or any other string type. (Data must be valid UTF-16. Conversions of byte arrays check for a byte-order marker in the first two bytes, and do not include it in the resulting string.)

   Note that the resulting ``UTF16String`` data is terminated by the NUL codepoint (16-bit zero), which is not treated as a character in the string (so that it is mostly invisible in Julia); this allows the string to be passed directly to external functions requiring NUL-terminated data. This NUL is appended automatically by the utf16(s) conversion function. If you have a ``UInt16`` array ``A`` that is already NUL-terminated valid UTF-16 data, then you can instead use UTF16String(A)` to construct the string without making a copy of the data and treating the NUL as a terminator rather than as part of the string.

   .. code-block:: julia

       utf16(::Union{Ptr{UInt16},Ptr{Int16}} [, length])

   Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.

.. function:: utf16(::Union{Ptr{UInt16},Ptr{Int16}} [, length])

   .. Docstring generated from Julia source

   .. code-block:: julia

       utf16(s)

   Create a UTF-16 string from a byte array, array of ``UInt16``\ , or any other string type. (Data must be valid UTF-16. Conversions of byte arrays check for a byte-order marker in the first two bytes, and do not include it in the resulting string.)

   Note that the resulting ``UTF16String`` data is terminated by the NUL codepoint (16-bit zero), which is not treated as a character in the string (so that it is mostly invisible in Julia); this allows the string to be passed directly to external functions requiring NUL-terminated data. This NUL is appended automatically by the utf16(s) conversion function. If you have a ``UInt16`` array ``A`` that is already NUL-terminated valid UTF-16 data, then you can instead use UTF16String(A)` to construct the string without making a copy of the data and treating the NUL as a terminator rather than as part of the string.

   .. code-block:: julia

       utf16(::Union{Ptr{UInt16},Ptr{Int16}} [, length])

   Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.

.. function:: utf32(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       wstring(s)

   This is a synonym for either ``utf32(s)`` or ``utf16(s)``\ , depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.

.. function:: utf32(::Union{Ptr{Char},Ptr{UInt32},Ptr{Int32}} [, length])

   .. Docstring generated from Julia source

   .. code-block:: julia

       wstring(s)

   This is a synonym for either ``utf32(s)`` or ``utf16(s)``\ , depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.

.. function:: wstring(s)

   .. Docstring generated from Julia source

   .. code-block:: julia

       wstring(s)

   This is a synonym for either ``utf32(s)`` or ``utf16(s)``\ , depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.

