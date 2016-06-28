# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "types" begin
## SubString, RevString, RepString and Cstring tests ##

## SubString tests ##
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
u8str2 = u8str^2
len_u8str = length(u8str)
slen_u8str = length(u8str)
len_u8str2 = length(u8str2)
slen_u8str2 = length(u8str2)

@test len_u8str2 == 2 * len_u8str
@test slen_u8str2 == 2 * slen_u8str

u8str2plain = String(u8str2)

for i1 = 1:length(u8str2)
    if !isvalid(u8str2, i1); continue; end
    for i2 = i1:length(u8str2)
        if !isvalid(u8str2, i2); continue; end
        @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
        @test length(u8str2[i1:i2]) == length(u8str2plain[i1:i2])
        @test u8str2[i1:i2] == u8str2plain[i1:i2]
    end
end

str="tempus fugit"              #length(str)==12
ss=SubString(str,1,length(str)) #match source string
@test length(ss)==length(str)

ss=SubString(str,1,0)    #empty SubString
@test length(ss)==0

ss=SubString(str,14,20)  #start indexed beyond source string length
@test length(ss)==0

ss=SubString(str,10,16)  #end indexed beyond source string length
@test length(ss)==3

str2=""
ss=SubString(str2,1,4)  #empty source string
@test length(ss)==0

ss=SubString(str2,1,1)  #empty source string, identical start and end index
@test length(ss)==0

@test SubString("foobar",big(1),big(3)) == "foo"

str = "aa\u2200\u2222bb"
u = SubString(str, 3, 6)
@test length(u)==2
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == "\u2200\u2222"

@test_throws ArgumentError SubString(str, 4, 5)
@test_throws BoundsError next(u, 0)
@test_throws BoundsError next(u, 7)
@test_throws BoundsError getindex(u, 0)
@test_throws BoundsError getindex(u, 7)
@test_throws BoundsError getindex(u, 0:1)
@test_throws BoundsError getindex(u, 7:7)
@test reverseind(u, 1) == 4
@test typeof(Base.cconvert(Ptr{Int8},u)) == SubString{String}
@test Base.cconvert(Ptr{Int8},u) == u

str = "føøbar"
u = SubString(str, 4, 3)
@test length(u)==0
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == ""

str = "føøbar"
u = SubString(str, 10, 10)
@test length(u)==0
b = IOBuffer()
write(b, u)
@test takebuf_string(b) == ""

# search and SubString (issue #5679)
str = "Hello, world!"
u = SubString(str, 1, 5)
@test rsearch(u, "World") == 0:-1
@test rsearch(u, 'z') == 0
@test rsearch(u, "ll") == 3:4

# sizeof
@test sizeof(SubString("abc\u2222def",4,4)) == 3

# issue #3710
@test prevind(SubString("{var}",2,4),4) == 3

# issue #4183
@test split(SubString("x", 2, 0), "y") == AbstractString[""]

# issue #6772
@test float(SubString("10",1,1)) === 1.0
@test float(SubString("1 0",1,1)) === 1.0
@test parse(Float32,SubString("10",1,1)) === 1.0f0

# issue #5870
@test !ismatch(Regex("aa"), SubString("",1,0))
@test ismatch(Regex(""), SubString("",1,0))

# isvalid(), chr2ind() and ind2chr() for SubString{DirectIndexString}
let s="lorem ipsum",
    sdict=Dict(SubString(s,1,11)=>s,
               SubString(s,1,6)=>"lorem ",
               SubString(s,1,0)=>"",
               SubString(s,2,4)=>"ore",
               SubString(s,2,16)=>"orem ipsum",
               SubString(s,12,14)=>""
               )
    for (ss,s) in sdict
        for i in -1:12
            @test isvalid(ss,i)==isvalid(s,i)
        end
    end
    for (ss,s) in sdict
        for i in 1:length(ss)
            @test ind2chr(ss,i)==ind2chr(s,i)
        end
    end
    for (ss,s) in sdict
        for i in 1:length(ss)
            @test chr2ind(ss,i)==chr2ind(s,i)
        end
    end
end #let

#for isvalid(SubString{String})
let s = "Σx + βz - 2"
  for i in -1:length(s)+2
      ss=SubString(s,1,i)
      @test isvalid(ss,i)==isvalid(s,i)
  end
end

ss=SubString("hello",1,5)
@test_throws BoundsError ind2chr(ss, -1)
@test_throws BoundsError chr2ind(ss, -1)
@test_throws BoundsError chr2ind(ss, 10)
@test_throws BoundsError ind2chr(ss, 10)

# length(SubString{String}) performance specialization
let s = "|η(α)-ϕ(κ)| < ε"
    @test length(SubString(s,1,0))==length(s[1:0])
    @test length(SubString(s,4,4))==length(s[4:4])
    @test length(SubString(s,1,7))==length(s[1:7])
    @test length(SubString(s,4,11))==length(s[4:11])
end

## Reverse strings ##

rs = RevString("foobar")
@test length(rs) == 6
@test sizeof(rs) == 6
@test isascii(rs)

# issue #4586
@test rsplit(RevString("ailuj"),'l') == ["ju","ia"]
@test parse(Float64,RevString("64")) === 46.0

# reverseind
for T in (String, UTF16String, UTF32String)
    for prefix in ("", "abcd", "\U0001d6a4\U0001d4c1", "\U0001d6a4\U0001d4c1c", " \U0001d6a4\U0001d4c1")
        for suffix in ("", "abcde", "\U0001d4c1β\U0001d6a4", "\U0001d4c1β\U0001d6a4c", " \U0001d4c1β\U0001d6a4")
            for c in ('X', 'δ', '\U0001d6a5')
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

## Repeat strings ##

# issue #7764
let
    rs = RepString("foo", 2)
    @test length(rs) == 6
    @test sizeof(rs) == 6
    @test isascii(rs)
    @test convert(RepString, "foobar") == "foobar"
    @test typeof(convert(RepString, "foobar")) == RepString

    srep = RepString("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,endof(s))

    @test ss^2 == "ΣβΣβ"
    @test RepString(ss,2) == "ΣβΣβ"

    @test endof(srep) == 7

    @test next(srep, 3) == ('β',5)
    @test next(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end


## Cstring tests ##

# issue #13974: comparison against pointers

str = String("foobar")
ptr = pointer(str)
cstring = Cstring(ptr)
@test ptr == cstring
@test cstring == ptr

# convenient NULL string creation from Ptr{Void}
nullstr = Cstring(C_NULL)

# Comparisons against NULL strings
@test ptr != nullstr
@test nullstr != ptr

# Short-hand comparison against C_NULL
@test nullstr == C_NULL
@test C_NULL == nullstr
@test cstring != C_NULL
@test C_NULL != cstring

end
