# This file is a part of Julia. License is MIT: http://julialang.org/license

# lower and upper
@test uppercase("aBc") == "ABC"
@test uppercase('A') == 'A'
@test uppercase('a') == 'A'
@test lowercase("AbC") == "abc"
@test lowercase('A') == 'a'
@test lowercase('a') == 'a'
@test uppercase('α') == '\u0391'
@test lowercase('Δ') == 'δ'
@test lowercase('\U118bf') == '\U118df'
@test uppercase('\U1044d') == '\U10425'
@test ucfirst("Abc") == "Abc"
@test ucfirst("abc") == "Abc"
@test lcfirst("ABC") == "aBC"
@test lcfirst("aBC") == "aBC"

# issue # 11464: uppercase/lowercase of UTF16String becomes a UTF8String
str = "abcdef\uff\uffff\u10ffffABCDEF"
@test typeof(uppercase("abcdef")) == ASCIIString
@test typeof(uppercase(utf8(str))) == UTF8String
@test typeof(uppercase(utf16(str))) == UTF16String
@test typeof(uppercase(utf32(str))) == UTF32String
@test typeof(lowercase("ABCDEF")) == ASCIIString
@test typeof(lowercase(utf8(str))) == UTF8String
@test typeof(lowercase(utf16(str))) == UTF16String
@test typeof(lowercase(utf32(str))) == UTF32String

foomap(ch) = (ch > 65)
foobar(ch) = Char(0xd800)
foobaz(ch) = Char(0x200000)
@test_throws UnicodeError map(foomap, utf16(str))
@test_throws UnicodeError map(foobar, utf16(str))
@test_throws UnicodeError map(foobaz, utf16(str))
