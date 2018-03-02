# This file is a part of Julia. License is MIT: https://julialang.org/license

function collect_eachmatch(re, str; overlap=false)
    [m.match for m in collect(eachmatch(re, str, overlap = overlap))]
end

@test collect_eachmatch(r"a?b?", "asbd") == ["a","","b","",""] ==
    collect_eachmatch(r"""a?b?""", "asbd")
@test collect_eachmatch(r"a?b?", "asbd", overlap=true) == ["a","","b","",""]
@test collect_eachmatch(r"\w+", "hello", overlap=true) == ["hello","ello","llo","lo","o"]
@test collect_eachmatch(r".\s", "x \u2200 x \u2203 y") == ["x ", "∀ ", "x ", "∃ "]
@test collect_eachmatch(r"(\w+)(\s*)", "The dark side of the moon") ==
    ["The ", "dark ", "side ", "of ", "the ", "moon"]
@test collect_eachmatch(r"", "") == [""]
@test collect_eachmatch(r"", "", overlap=true) == [""]
@test collect_eachmatch(r"aa", "aaaa") == ["aa", "aa"]
@test collect_eachmatch(r"aa", "aaaa", overlap=true) == ["aa", "aa", "aa"]
@test collect_eachmatch(r"", "aaa") == ["", "", "", ""]
@test collect_eachmatch(r"", "aaa", overlap=true) == ["", "", "", ""]
@test collect_eachmatch(r"GCG","GCGCG") == ["GCG"]
@test collect_eachmatch(r"GCG","GCGCG",overlap=true) == ["GCG","GCG"]

# Issue 8278
target = """71.163.72.113 - - [30/Jul/2014:16:40:55 -0700] "GET emptymind.org/thevacantwall/wp-content/uploads/2013/02/DSC_006421.jpg HTTP/1.1" 200 492513 "http://images.search.yahoo.com/images/view;_ylt=AwrB8py9gdlTGEwADcSjzbkF;_ylu=X3oDMTI2cGZrZTA5BHNlYwNmcC1leHAEc2xrA2V4cARvaWQDNTA3NTRiMzYzY2E5OTEwNjBiMjc2YWJhMjkxMTEzY2MEZ3BvcwM0BGl0A2Jpbmc-?back=http%3A%2F%2Fus.yhs4.search.yahoo.com%2Fyhs%2Fsearch%3Fei%3DUTF-8%26p%3Dapartheid%2Bwall%2Bin%2Bpalestine%26type%3Dgrvydef%26param1%3D1%26param2%3Dsid%253Db01676f9c26355f014f8a9db87545d61%2526b%253DChrome%2526ip%253D71.163.72.113%2526p%253Dgroovorio%2526x%253DAC811262A746D3CD%2526dt%253DS940%2526f%253D7%2526a%253Dgrv_tuto1_14_30%26hsimp%3Dyhs-fullyhosted_003%26hspart%3Dironsource&w=588&h=387&imgurl=occupiedpalestine.files.wordpress.com%2F2012%2F08%2F5-peeking-through-the-wall.jpg%3Fw%3D588%26h%3D387&rurl=http%3A%2F%2Fwww.stopdebezetting.com%2Fwereldpers%2Fcompare-the-berlin-wall-vs-israel-s-apartheid-wall-in-palestine.html&size=49.0KB&name=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&p=apartheid+wall+in+palestine&oid=50754b363ca991060b276aba291113cc&fr2=&fr=&tt=...+%3Cb%3EApartheid+wall+in+Palestine%3C%2Fb%3E...+%7C+Or+you+go+peeking+through+the+%3Cb%3Ewall%3C%2Fb%3E&b=0&ni=21&no=4&ts=&tab=organic&sigr=13evdtqdq&sigb=19k7nsjvb&sigi=12o2la1db&sigt=12lia2m0j&sign=12lia2m0j&.crumb=.yUtKgFI6DE&hsimp=yhs-fullyhosted_003&hspart=ironsource" "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.125 Safari/537.36"""
pat = r"""([\d\.]+) ([\w.-]+) ([\w.-]+) (\[.+\]) "([^"\r\n]*|[^"\r\n\[]*\[.+\][^"]+|[^"\r\n]+.[^"]+)" (\d{3}) (\d+|-) ("(?:[^"]|\")+)"? ("(?:[^"]|\")+)"?"""
match(pat, target)

# Issue 9545 (32 bit)
buf = PipeBuffer()
show(buf, r"")
@test read(buf, String) == "r\"\""

# see #10994, #11447: PCRE2 allows NUL chars in the pattern
@test occursin(Regex("^a\0b\$"), "a\0b")

# regex match / search string must be a String
@test_throws ArgumentError match(r"test", GenericString("this is a test"))
@test_throws ArgumentError findfirst(r"test", GenericString("this is a test"))

# Named subpatterns
let m = match(r"(?<a>.)(.)(?<b>.)", "xyz")
    @test (m[:a], m[2], m["b"]) == ("x", "y", "z")
    @test sprint(show, m) == "RegexMatch(\"xyz\", a=\"x\", 2=\"y\", b=\"z\")"
end

# Backcapture reference in substitution string
@test replace("abcde", r"(..)(?P<byname>d)" => s"\g<byname>xy\\\1") == "adxy\\bce"
@test_throws ErrorException replace("a", r"(?P<x>)" => s"\g<y>")

# Proper unicode handling
@test  match(r"∀∀", "∀x∀∀∀").match == "∀∀"
