# This file is a part of Julia. License is MIT: http://julialang.org/license

# issue #4586
@test rsplit(RevString("ailuj"),'l') == ["ju","ia"]
@test parse(Float64,RevString("64")) === 46.0

# reverseind
for T in (ASCIIString, UTF8String, UTF16String, UTF32String)
    for prefix in ("", "abcd", "\U0001d6a4\U0001d4c1", "\U0001d6a4\U0001d4c1c", " \U0001d6a4\U0001d4c1")
        for suffix in ("", "abcde", "\U0001d4c1β\U0001d6a4", "\U0001d4c1β\U0001d6a4c", " \U0001d4c1β\U0001d6a4")
            for c in ('X', 'δ', '\U0001d6a5')
                T != ASCIIString || (isascii(prefix) && isascii(suffix) && isascii(c)) || continue
                s = convert(T, string(prefix, c, suffix))
                ri = search(reverse(s), c)
                @test reverse(s) == RevString(s)
                @test c == s[reverseind(s, ri)] == reverse(s)[ri]
                s = RevString(s)
                ri = search(reverse(s), c)
                @test c == s[reverseind(s, ri)] == reverse(s)[ri]
                s = convert(T, string(prefix, prefix, c, suffix, suffix))
                pre = convert(T, prefix)
                sb = SubString(s, nextind(pre, endof(pre)), endof(convert(T, string(prefix, prefix, c, suffix))))
                ri = search(reverse(sb), c)
                @test c == sb[reverseind(sb, ri)] == reverse(sb)[ri]
            end
        end
    end
end
