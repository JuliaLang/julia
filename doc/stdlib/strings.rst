.. currentmodule:: Base

*********
 Strings
*********

.. function:: length(s)

   ::
   
       length(s)
   
   The number of characters in string ``s``.
   

.. function:: sizeof(s::AbstractString)

   ::
   
       sizeof(s::AbstractString)
   
   The number of bytes in string ``s``.
   

.. function:: *(s, t)

   ::
   
       *(s, t)
   
   Concatenate strings. The ``*`` operator is an alias to this function.
   

	   julia> "Hello " * "world"
	   "Hello world"

.. function:: ^(s, n)

   ::
   
       ^(s, n)
   
   Repeat ``n`` times the string ``s``. The ``^`` operator is an alias to this function.
   

.. function:: string(xs...)

   ::
   
       string(xs...)
   
   Create a string from any values using the ``print`` function.
   

.. function:: repr(x)

   ::
   
       repr(x)
   
   Create a string from any value using the ``showall`` function.
   

.. function:: bytestring(::Ptr{UInt8}, [length])

   ::
   
       bytestring(s)
   
   Convert a string to a contiguous byte array representation appropriate for passing it to C functions. The string will be encoded as either ASCII or UTF-8.
   

.. function:: bytestring(s)

   ::
   
       bytestring(s)
   
   Convert a string to a contiguous byte array representation appropriate for passing it to C functions. The string will be encoded as either ASCII or UTF-8.
   

.. function:: ascii(::Array{UInt8,1})

   ::
   
       ascii(::Ptr{UInt8}[, length])
   
   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.
   

.. function:: ascii(s)

   ::
   
       ascii(::Ptr{UInt8}[, length])
   
   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.
   

.. function:: ascii(::Ptr{UInt8}, [length])

   ::
   
       ascii(::Ptr{UInt8}[, length])
   
   Create an ASCII string from the address of a C (0-terminated) string encoded in ASCII. A copy is made; the ptr can be safely freed. If ``length`` is specified, the string does not have to be 0-terminated.
   

.. function:: utf8(::Array{UInt8,1})

   ::
   
       utf8(s)
   
   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).
   

.. function:: utf8(::Ptr{UInt8}, [length])

   ::
   
       utf8(s)
   
   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).
   

.. function:: utf8(s)

   ::
   
       utf8(s)
   
   Convert a string to a contiguous UTF-8 string (all characters must be valid UTF-8 characters).
   

.. function:: normalize_string(s, normalform::Symbol)

   ::
   
       normalize_string(s, normalform::Symbol)
   
   Normalize the string ``s`` according to one of the four ``normal forms`` of the Unicode standard: ``normalform`` can be ``:NFC``, composition) and D (canonical decomposition) convert different visually identical representations of the same abstract string into a single canonical form, with form C being more compact.  Normal forms KC and KD additionally canonicalize ``compatibility equivalents``: they convert characters that are abstractly similar but visually distinct into a single canonical choice (e.g. they expand ligatures into the individual characters), with form KC being more compact. Alternatively, finer control and additional transformations may be be obtained by calling *normalize_string(s; keywords...)*, where any number of the following boolean keywords options (which all default to ``false`` except for ``compose``) are specified: For example, NFKC corresponds to the options ``compose=true, compat=true, stable=true``.
   

.. function:: graphemes(s) -> iterator over substrings of s

   ::
   
       graphemes(s) -> iterator over substrings of s
   
   Returns an iterator over substrings of ``s`` that correspond to the extended graphemes in the string, as defined by Unicode UAX #29. even though they may contain more than one codepoint; for example a letter combined with an accent mark is a single grapheme.)
   

.. function:: isvalid(value) -> Bool

   ::
   
       isvalid(str, i)
   
   Tells whether index ``i`` is valid for the given string
   

.. function:: isvalid(T, value) -> Bool

   ::
   
       isvalid(str, i)
   
   Tells whether index ``i`` is valid for the given string
   

.. function:: is_assigned_char(c) -> Bool

   ::
   
       is_assigned_char(c) -> Bool
   
   Returns true if the given char or integer is an assigned Unicode code point.
   

.. function:: ismatch(r::Regex, s::AbstractString) -> Bool

   ::
   
       ismatch(r::Regex, s::AbstractString) -> Bool
   
   Test whether a string contains a match of the given regular expression.
   

.. function:: match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   ::
   
       match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])
   
   Search for the first match of the regular expression ``r`` in ``s`` and return a RegexMatch object containing the match, or nothing if the match failed. The matching substring can be retrieved by accessing ``m.match`` and the captured sequences can be retrieved by accessing ``m.captures`` The optional ``idx`` argument specifies an index at which to start the search.
   

.. function:: eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   ::
   
       eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])
   
   Search for all matches of a the regular expression ``r`` in ``s`` and return a iterator over the matches. If overlap is true, the matching sequences are allowed to overlap indices in the original string, otherwise they must be from distinct character ranges.
   

.. function:: matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   ::
   
       matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}
   
   Return a vector of the matching substrings from eachmatch.
   

.. function:: lpad(string, n, p)

   ::
   
       lpad(string, n, p)
   
   Make a string at least ``n`` columns wide when printed, by padding on the left with copies of ``p``.
   

.. function:: rpad(string, n, p)

   ::
   
       rpad(string, n, p)
   
   Make a string at least ``n`` columns wide when printed, by padding on the right with copies of ``p``.
   

.. function:: search(string, chars, [start])

   ::
   
       search(string, chars[, start])
   
   Search for the first occurrence of the given characters within the given string. The second argument may be a single character, a vector or a set of characters, a string, or a regular expression such as ASCII or UTF-8 strings). The third argument optionally specifies a starting index. The return value is a range of indexes where the matching sequence is found, such that ``s[search(s,x)] == x``:
   

.. function:: rsearch(string, chars, [start])

   ::
   
       rsearch(string, chars[, start])
   
   Similar to ``search``, but returning the last occurrence of the given characters within the given string, searching in reverse from
   

.. function:: searchindex(string, substring, [start])

   ::
   
       searchindex(string, substring[, start])
   
   Similar to ``search``, but return only the start index at which the substring is found, or 0 if it is not.
   

.. function:: rsearchindex(string, substring, [start])

   ::
   
       rsearchindex(string, substring[, start])
   
   Similar to ``rsearch``, but return only the start index at which the substring is found, or 0 if it is not.
   

.. function:: contains(haystack, needle)

   ::
   
       contains(haystack, needle)
   
   Determine whether the second argument is a substring of the first.
   

.. function:: replace(string, pat, r[, n])

   ::
   
       replace(string, pat, r[, n])
   
   Search for the given pattern ``pat``, and replace each occurrence with ``r``. If ``n`` is provided, replace at most ``n`` occurrences.  As with search, the second argument may be a single character, a vector or a set of characters, a string, or a regular expression. If ``r`` is a function, each occurrence is replaced with ``r(s)`` where ``s`` is the matched substring.
   

.. function:: split(string, [chars]; limit=0, keep=true)

   ::
   
       split(string, [chars]; limit=0, keep=true)
   
   Return an array of substrings by splitting the given string on occurrences of the given character delimiters, which may be specified in any of the formats allowed by ``search``'s second argument (i.e. a single character, collection of characters, string, or regular expression). If ``chars`` is omitted, it defaults to the set of all space characters, and ``keep`` is taken to be false. The two keyword arguments are optional: they are are a maximum size for the result and a flag determining whether empty fields should be kept in the result.
   

.. function:: rsplit(string, [chars]; limit=0, keep=true)

   ::
   
       rsplit(string, [chars]; limit=0, keep=true)
   
   Similar to ``split``, but starting from the end of the string.
   

.. function:: strip(string, [chars])

   ::
   
       strip(string[, chars])
   
   Return ``string`` with any leading and trailing whitespace removed. If ``chars`` (a character, or vector or set of characters) is provided, instead remove characters contained in it.
   

.. function:: lstrip(string, [chars])

   ::
   
       lstrip(string[, chars])
   
   Return ``string`` with any leading whitespace removed. If ``chars`` remove characters contained in it.
   

.. function:: rstrip(string, [chars])

   ::
   
       rstrip(string[, chars])
   
   Return ``string`` with any trailing whitespace removed. If provided, instead remove characters contained in it.
   

.. function:: startswith(string, prefix | chars)

   ::
   
       startswith(string, prefix | chars)
   
   Returns ``true`` if ``string`` starts with ``prefix``. If the second argument is a vector or set of characters, tests whether the first character of ``string`` belongs to that set.
   

.. function:: endswith(string, suffix | chars)

   ::
   
       endswith(string, suffix | chars)
   
   Returns ``true`` if ``string`` ends with ``suffix``. If the second argument is a vector or set of characters, tests whether the last character of ``string`` belongs to that set.
   

.. function:: uppercase(string)

   ::
   
       uppercase(string)
   
   Returns ``string`` with all characters converted to uppercase.
   

.. function:: lowercase(string)

   ::
   
       lowercase(string)
   
   Returns ``string`` with all characters converted to lowercase.
   

.. function:: ucfirst(string)

   ::
   
       ucfirst(string)
   
   Returns ``string`` with the first character converted to uppercase.
   

.. function:: lcfirst(string)

   ::
   
       lcfirst(string)
   
   Returns ``string`` with the first character converted to lowercase.
   

.. function:: join(strings, delim, [last])

   ::
   
       join(strings, delim[, last])
   
   Join an array of ``strings`` into a single string, inserting the given delimiter between adjacent strings. If ``last`` is given, it will be used instead of ``delim`` between the last two strings. For example, ``join([``apples``, `bananas``, ``pineapples``], ``, `, convertible to strings via `print(io::IOBuffer, x)``.
   

.. function:: chop(string)

   ::
   
       chop(string)
   
   Remove the last character from a string
   

.. function:: chomp(string)

   ::
   
       chomp(string)
   
   Remove a trailing newline from a string
   

.. function:: ind2chr(string, i)

   ::
   
       ind2chr(string, i)
   
   Convert a byte index to a character index
   

.. function:: chr2ind(string, i)

   ::
   
       chr2ind(string, i)
   
   Convert a character index to a byte index
   

.. function:: isvalid(str, i)

   ::
   
       isvalid(str, i)
   
   Tells whether index ``i`` is valid for the given string
   

.. function:: nextind(str, i)

   ::
   
       nextind(str, i)
   
   Get the next valid string index after ``i``. Returns a value greater than ``endof(str)`` at or after the end of the string.
   

.. function:: prevind(str, i)

   ::
   
       prevind(str, i)
   
   Get the previous valid string index before ``i``. Returns a value less than ``1`` at the beginning of the string.
   

.. function:: randstring([rng,] len=8)

   ::
   
       randstring([rng], len=8)
   
   Create a random ASCII string of length ``len``, consisting of upper- and lower-case letters and the digits 0-9. The optional Numbers*.
   

.. function:: charwidth(c)

   ::
   
       charwidth(c)
   
   Gives the number of columns needed to print a character.
   

.. function:: strwidth(s)

   ::
   
       strwidth(s)
   
   Gives the number of columns needed to print a string.
   

.. function:: isalnum(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isalnum(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is alphanumeric, or whether this is true for all elements of a string.  A character is classified as alphabetic if it belongs to the Unicode general category Letter or Number, i.e. a character whose category code begins with 'L' or
   

.. function:: isalpha(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isalpha(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is alphabetic, or whether this is true for all elements of a string. A character is classified as alphabetic if it belongs to the Unicode general category Letter, i.e. a character whose category code begins with 'L'.
   

.. function:: isascii(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isascii(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character belongs to the ASCII character set, or whether this is true for all elements of a string.
   

.. function:: iscntrl(c::Union{Char,AbstractString}) -> Bool

   ::
   
       iscntrl(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is a control character, or whether this is true for all elements of a string.  Control characters are the non-printing characters of the Latin-1 subset of Unicode.
   

.. function:: isdigit(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isdigit(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is a numeric digit (0-9), or whether this is true for all elements of a string.
   

.. function:: isgraph(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isgraph(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is printable, and not a space, or whether this is true for all elements of a string.  Any character that would cause a printer to use ink should be classified with isgraph(c)==true.
   

.. function:: islower(c::Union{Char,AbstractString}) -> Bool

   ::
   
       islower(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is a lowercase letter, or whether this is true for all elements of a string.  A character is classified as lowercase if it belongs to Unicode category Ll, Letter: Lowercase.
   

.. function:: isnumber(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isnumber(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is numeric, or whether this is true for all elements of a string.   A character is classified as numeric if it belongs to the Unicode general category Number, i.e. a character whose category code begins with 'N'.
   

.. function:: isprint(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isprint(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is printable, including spaces, but not a control character. For strings, tests whether this is true for all elements of the string.
   

.. function:: ispunct(c::Union{Char,AbstractString}) -> Bool

   ::
   
       ispunct(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character belongs to the Unicode general category Punctuation, i.e. a character whose category code begins with 'P'. For strings, tests whether this is true for all elements of the string.
   

.. function:: isspace(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isspace(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is any whitespace character.  Includes ASCII characters '\t', '\n', '\v', '\f', '\r', and ' ', Latin-1 character U+0085, and characters in Unicode category Zs. For strings, tests whether this    is true for all elements of the string.
   

.. function:: isupper(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isupper(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is an uppercase letter, or whether this is true for all elements of a string.    A character is classified as uppercase if it belongs to Unicode category Lu, Letter: Uppercase, or Lt, Letter: Titlecase.
   

.. function:: isxdigit(c::Union{Char,AbstractString}) -> Bool

   ::
   
       isxdigit(c::Union{Char, AbstractString}) -> Bool
   
   Tests whether a character is a valid hexadecimal digit, or whether this is true for all elements of a string.
   

.. function:: symbol(x...) -> Symbol

   ::
   
       symbol(x...) -> Symbol
   
   Create a ``Symbol`` by concatenating the string representations of the arguments together.
   

.. function:: escape_string(str::AbstractString) -> AbstractString

   ::
   
       escape_string(str::AbstractString) -> AbstractString
   
   General escaping of traditional C and Unicode escape sequences. See
   

.. function:: unescape_string(s::AbstractString) -> AbstractString

   ::
   
       unescape_string(s::AbstractString) -> AbstractString
   
   General unescaping of traditional C and Unicode escape sequences. Reverse of ``escape_string()``. See also ``print_unescaped()``.
   

.. function:: utf16(s)

   ::
   
       utf16(::Union{Ptr{UInt16}, Ptr{Int16}}[, length])
   
   Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.
   

.. function:: utf16(::Union{Ptr{UInt16},Ptr{Int16}} [, length])

   ::
   
       utf16(::Union{Ptr{UInt16}, Ptr{Int16}}[, length])
   
   Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the pointer can be safely freed. If ``length`` is specified, the string does not have to be NUL-terminated.
   

.. function:: utf32(s)

   ::
   
       wstring(s)
   
   This is a synonym for either ``utf32(s)`` or ``utf16(s)``, depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.
   

.. function:: utf32(::Union{Ptr{Char},Ptr{UInt32},Ptr{Int32}} [, length])

   ::
   
       wstring(s)
   
   This is a synonym for either ``utf32(s)`` or ``utf16(s)``, depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.
   

.. function:: wstring(s)

   ::
   
       wstring(s)
   
   This is a synonym for either ``utf32(s)`` or ``utf16(s)``, depending on whether ``Cwchar_t`` is 32 or 16 bits, respectively. The synonym ``WString`` for ``UTF32String`` or ``UTF16String`` is also provided.
   

