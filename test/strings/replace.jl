# This file is a part of Julia. License is MIT: https://julialang.org/license

using Unicode

@testset "replace" begin
    @test replace("\u2202", '*' => '\0', ""=>"") == "\u2202"

    @test replace("foobar", 'o' => '0', ""=>"") == "f00bar"
    @test replace("foobar", 'o' => '0', count=1, ""=>"") == "f0obar"
    @test replace("foobar", 'o' => "", ""=>"") == "fbar"
    @test replace("foobar", 'o' => "", count=1, ""=>"") == "fobar"
    @test replace("foobar", 'f' => 'F', ""=>"") == "Foobar"
    @test replace("foobar", 'r' => 'R', ""=>"") == "foobaR"

    @test replace("foofoofoo", "foo" => "bar", ""=>"") == "barbarbar"
    @test replace("foobarfoo", "foo" => "baz", ""=>"") == "bazbarbaz"
    @test replace("barfoofoo", "foo" => "baz", ""=>"") == "barbazbaz"

    @test replace("", "" => "", ""=>"") == ""
    @test replace("", "" => "x", ""=>"") == "x"
    @test replace("", "x" => "y", ""=>"") == ""

    @test replace("abcd", "" => "^", ""=>"") == "^a^b^c^d^"
    @test replace("abcd", "b" => "^", ""=>"") == "a^cd"
    @test replace("abcd", r"b?" => "^", ""=>"") == "^a^c^d^"
    @test replace("abcd", r"b+" => "^", ""=>"") == "a^cd"
    @test replace("abcd", r"b?c?" => "^", ""=>"") == "^a^d^"
    @test replace("abcd", r"[bc]?" => "^", ""=>"") == "^a^^d^"

    @test replace("foobarfoo", r"(fo|ba)" => "xx", ""=>"") == "xxoxxrxxo"
    @test replace("foobarfoo", r"(foo|ba)" => "bar", ""=>"") == "barbarrbar"

    @test replace("foobar", 'o' => 'ø', ""=>"") == "føøbar"
    @test replace("foobar", 'o' => 'ø', count=1, ""=>"") == "føobar"
    @test replace("føøbar", 'ø' => 'o', ""=>"") == "foobar"
    @test replace("føøbar", 'ø' => 'o', count=1, ""=>"") == "foøbar"
    @test replace("føøbar", 'ø' => 'ö', ""=>"") == "fööbar"
    @test replace("føøbar", 'ø' => 'ö', count=1, ""=>"") == "föøbar"
    @test replace("føøbar", 'ø' => "", ""=>"") == "fbar"
    @test replace("føøbar", 'ø' => "", count=1, ""=>"") == "føbar"
    @test replace("føøbar", 'f' => 'F', ""=>"") == "Føøbar"
    @test replace("ḟøøbar", 'ḟ' => 'F', ""=>"") == "Føøbar"
    @test replace("føøbar", 'f' => 'Ḟ', ""=>"") == "Ḟøøbar"
    @test replace("ḟøøbar", 'ḟ' => 'Ḟ', ""=>"") == "Ḟøøbar"
    @test replace("føøbar", 'r' => 'R', ""=>"") == "føøbaR"
    @test replace("føøbaṙ", 'ṙ' => 'R', ""=>"") == "føøbaR"
    @test replace("føøbar", 'r' => 'Ṙ', ""=>"") == "føøbaṘ"
    @test replace("føøbaṙ", 'ṙ' => 'Ṙ', ""=>"") == "føøbaṘ"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar", ""=>"") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "baz", ""=>"") == "bazbarbaz"
    @test replace("barḟøøḟøø", "ḟøø" => "baz", ""=>"") == "barbazbaz"

    @test replace("foofoofoo", "foo" => "ƀäṙ", ""=>"") == "ƀäṙƀäṙƀäṙ"
    @test replace("fooƀäṙfoo", "foo" => "baz", ""=>"") == "bazƀäṙbaz"
    @test replace("ƀäṙfoofoo", "foo" => "baz", ""=>"") == "ƀäṙbazbaz"

    @test replace("foofoofoo", "foo" => "bar", ""=>"") == "barbarbar"
    @test replace("foobarfoo", "foo" => "ƀäż", ""=>"") == "ƀäżbarƀäż"
    @test replace("barfoofoo", "foo" => "ƀäż", ""=>"") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ", ""=>"") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "baz", ""=>"") == "bazƀäṙbaz"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "baz", ""=>"") == "ƀäṙbazbaz"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "bar", ""=>"") == "barbarbar"
    @test replace("ḟøøbarḟøø", "ḟøø" => "ƀäż", ""=>"") == "ƀäżbarƀäż"
    @test replace("barḟøøḟøø", "ḟøø" => "ƀäż", ""=>"") == "barƀäżƀäż"

    @test replace("ḟøøḟøøḟøø", "ḟøø" => "ƀäṙ", ""=>"") == "ƀäṙƀäṙƀäṙ"
    @test replace("ḟøøƀäṙḟøø", "ḟøø" => "ƀäż", ""=>"") == "ƀäżƀäṙƀäż"
    @test replace("ƀäṙḟøøḟøø", "ḟøø" => "ƀäż", ""=>"") == "ƀäṙƀäżƀäż"

    @test replace("", "" => "ẍ", ""=>"") == "ẍ"
    @test replace("", "ẍ" => "ÿ", ""=>"") == ""

    @test replace("äƀçđ", "" => "π", ""=>"") == "πäπƀπçπđπ"
    @test replace("äƀçđ", "ƀ" => "π", ""=>"") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?" => "π", ""=>"") == "πäπçπđπ"
    @test replace("äƀçđ", r"ƀ+" => "π", ""=>"") == "äπçđ"
    @test replace("äƀçđ", r"ƀ?ç?" => "π", ""=>"") == "πäπđπ"
    @test replace("äƀçđ", r"[ƀç]?" => "π", ""=>"") == "πäππđπ"

    @test replace("foobarfoo", r"(fo|ba)" => "ẍẍ", ""=>"") == "ẍẍoẍẍrẍẍo"

    @test replace("ḟøøbarḟøø", r"(ḟø|ba)" => "xx", ""=>"") == "xxøxxrxxø"
    @test replace("ḟøøbarḟøø", r"(ḟøø|ba)" => "bar", ""=>"") == "barbarrbar"

    @test replace("fooƀäṙfoo", r"(fo|ƀä)" => "xx", ""=>"") == "xxoxxṙxxo"
    @test replace("fooƀäṙfoo", r"(foo|ƀä)" => "ƀäṙ", ""=>"") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("ḟøøƀäṙḟøø", r"(ḟø|ƀä)" => "xx", ""=>"") == "xxøxxṙxxø"
    @test replace("ḟøøƀäṙḟøø", r"(ḟøø|ƀä)" => "ƀäṙ", ""=>"") == "ƀäṙƀäṙṙƀäṙ"

    @test replace("foo", "oo" => uppercase, ""=>"") == "fOO"

    # Issue 13332
    @test replace("abc", 'b' => 2.1, ""=>"") == "a2.1c"

    # test replace with a count for String and GenericString
    # check that replace is a no-op if count==0
    for s in ["aaa", Test.GenericString("aaa")]
        @test_throws DomainError replace(s, 'a' => "", count = -1, ""=>"")
        @test replace(s, 'a' => 'z', count=0, ""=>"") === s
        @test replace(s, 'a' => 'z', count=1, ""=>"") == "zaa"
        @test replace(s, 'a' => 'z', count=2, ""=>"") == "zza"
        @test replace(s, 'a' => 'z', count=3, ""=>"") == "zzz"
        @test replace(s, 'a' => 'z', count=4, ""=>"") == "zzz"
        @test replace(s, 'a' => 'z', count=typemax(Int), ""=>"") == "zzz"
        @test replace(s, 'a' => 'z', ""=>"")    == "zzz"
    end

    for s in ["abc"]
        @test replace(s) === s
        @test replace(s, 'a' => 'z', ""=>"") === "zbc"
        @test replace(s, 'a' => 'z', 'b' => 'y') == "zyc"
        @test replace(s, 'a' => 'z', 'c' => 'x', "b" => 'y') == "zyx"
        @test replace(s, '1' => 'z', ""=>"") == s
        @test replace(s, 'b' => "BbB", ""=>"", count=1) == "aBbBc"
    end

    for s in ["quick quicker quickest"]
        @test replace(s) === s
        @test replace(s, "quick" => 'a', "quicker" => uppercase, "quickest" => 'z') == "a QUICKER z"
        @test replace(s, "quick"=>"Duck", "quicker"=>"is", "quickest"=>"lame", count=2) == "Duck is quickest"
        @test replace(s, "" => '1', ""=>"") == "1q1u1i1c1k1 1q1u1i1c1k1e1r1 1q1u1i1c1k1e1s1t1"
        @test replace(s, "qu" => "QU", "qu" => "never happens", "ick" => "") == "QU QUer QUest"
        @test replace(s, " " => '_', "r " => "r-") == "quick_quicker-quickest"
        @test replace(s, r"[aeiou]" => "ä", "ui" => "ki", "i" => "I") == "qkick qkickär qkickäst"
        @test replace(s, r"[^ ]+" => "word", "quicker " => "X", count=big"99") == "word Xword"

        @test replace(s, r"(quick)(e)"=>s"\2-\1", "x"=>"X") == "quick e-quickr e-quickst"

        @test replace(s, 'q'=>'Q', 'u'=>'U') == "QUick QUicker QUickest"
        @test replace(s, 'q'=>'Q', r"u"=>'U') == "QUick QUicker QUickest"
        @test replace(s, 'q'=>'Q', equalto('u')=>uppercase) == "QUick QUicker QUickest"
        @test replace(s, 'q'=>'Q', islower=>'-') == "Q---- Q------ Q-------"
        @test replace(s, ['q', 'u']=>'K') == "KKick KKicker KKickest"
        @test replace(s, occursin("uq")=>'K') == "KKick KKicker KKickest"
        @test replace(s, equalto('q')=>"B") == "Buick Buicker Buickest"

        @test replace(s, "qui"=>"A", 'r'=>'R') == "Ack AckeR Ackest"
        @test replace(s, 'r'=>'x', islower=>uppercase) == "QUICK QUICKEx QUICKEST"
        @test replace(s, islower=>uppercase, 'r'=>'x') == "QUICK QUICKER QUICKEST"
        @test replace(s, "q"=>"z", islower=>uppercase, 'r'=>'x') == "zUICK zUICKER zUICKEST"
        @test replace(s, "qui"=>"A", 'r'=>'x', islower=>uppercase) == "ACK ACKEx ACKEST"
        @test replace(s, "qui"=>"A", 'r'=>'x', islower=>uppercase) == "ACK ACKEx ACKEST"
        @test replace(s, r"q"=>"z", islower=>uppercase, 'r'=>'x') == "zUICK zUICKER zUICKEST"
        @test_throws ErrorException("type String has no field match_data") replace(s, "q"=>s"a\1b")
        @test_throws ErrorException("PCRE error: unknown substring") replace(s, r"q"=>s"a\1b")
    end

end
