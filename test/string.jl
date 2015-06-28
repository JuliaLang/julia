# This file is a part of Julia. License is MIT: http://julialang.org/license

# {starts,ends}with
@test startswith("abcd", 'a')
@test startswith("abcd", "a")
@test startswith("abcd", "ab")
@test !startswith("ab", "abcd")
@test !startswith("abcd", "bc")
@test endswith("abcd", 'd')
@test endswith("abcd", "d")
@test endswith("abcd", "cd")
@test !endswith("abcd", "dc")
@test !endswith("cd", "abcd")

@test replace("\u2202", '*', '\0') == "\u2202"

# string iteration, and issue #1454
str = "é"
str_a = vcat(str...)
@test length(str_a)==1
@test str_a[1] == str[1]

str = "s\u2200"
@test str[1:end] == str

# sizeof
@test sizeof("abc") == 3
@test sizeof("\u2222") == 3

# issue #3597
@test string(utf32(['T', 'e', 's', 't'])[1:1], "X") == "TX"

for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
    b = 2:62, _ = 1:10
    n = T != BigInt ? rand(T) : BigInt(rand(Int128))
    @test parse(T,base(b,n),b) == n
end

# issue #6027
let
    # make symbol with invalid char
    sym = symbol(Char(0xdcdb))
    @test string(sym) == string(Char(0xdcdb))
    @test expand(sym) === sym
    res = string(parse(string(Char(0xdcdb)," = 1"),1,raise=false)[1])
    @test res == """\$(Expr(:error, "invalid character \\\"\\udcdb\\\"\"))"""
end

@test symbol("asdf") === :asdf
@test symbol(:abc,"def",'g',"hi",0) === :abcdefghi0
@test startswith(string(gensym("asdf")),"##asdf#")
@test gensym("asdf") != gensym("asdf")
@test gensym() != gensym()
@test startswith(string(gensym()),"##")
@test_throws ArgumentError symbol("ab\0")
@test_throws ArgumentError gensym("ab\0")

# issue #6949
let f =IOBuffer(),
    x = split("1 2 3")
    @test write(f, x) == 3
    @test takebuf_string(f) == "123"
    @test invoke(write, Tuple{IO, AbstractArray}, f, x) == 3
    @test takebuf_string(f) == "123"
end

# issue #7248
@test_throws BoundsError ind2chr("hello", -1)
@test_throws BoundsError chr2ind("hello", -1)
@test_throws BoundsError ind2chr("hellø", -1)
@test_throws BoundsError chr2ind("hellø", -1)
@test_throws BoundsError ind2chr("hello", 10)
@test_throws BoundsError chr2ind("hello", 10)
@test_throws BoundsError ind2chr("hellø", 10)
@test_throws BoundsError chr2ind("hellø", 10)
@test_throws BoundsError checkbounds("hello", 0)
@test_throws BoundsError checkbounds("hello", 6)
@test_throws BoundsError checkbounds("hello", 0:3)
@test_throws BoundsError checkbounds("hello", 4:6)
@test_throws BoundsError checkbounds("hello", [0:3;])
@test_throws BoundsError checkbounds("hello", [4:6;])
@test checkbounds("hello", 2)
@test checkbounds("hello", 1:5)
@test checkbounds("hello", [1:5;])

# This caused JuliaLang/JSON.jl#82
@test first('\x00':'\x7f') === '\x00'
@test last('\x00':'\x7f') === '\x7f'

# make sure substrings handle last code unit even if not start of codepoint
let s = "x\u0302"
    @test s[1:3] == s
end

# issue #9781
# float(SubString) wasn't tolerant of trailing whitespace, which was different
# to "normal" strings. This also checks we aren't being too tolerant and allowing
# any arbitrary trailing characters.
@test parse(Float64,"1\n") == 1.0
@test [parse(Float64,x) for x in split("0,1\n",",")][2] == 1.0
@test_throws ArgumentError parse(Float64,split("0,1 X\n",",")[2])
@test parse(Float32,"1\n") == 1.0
@test [parse(Float32,x) for x in split("0,1\n",",")][2] == 1.0
@test_throws ArgumentError parse(Float32,split("0,1 X\n",",")[2])

#more ascii tests
@test convert(ASCIIString, UInt8[32,107,75], "*") == " kK"
@test convert(ASCIIString, UInt8[132,107,75], "*") == "*kK"
@test convert(ASCIIString, UInt8[], "*") == ""
@test convert(ASCIIString, UInt8[255], "*") == "*"

@test ucfirst("Hola")=="Hola"
@test ucfirst("hola")=="Hola"
@test ucfirst("")==""
@test ucfirst("*")=="*"

@test lcfirst("Hola")=="hola"
@test lcfirst("hola")=="hola"
@test lcfirst("")==""
@test lcfirst("*")=="*"

#more UTF8String tests
@test convert(UTF8String, UInt8[32,107,75], "*") == " kK"
@test convert(UTF8String, UInt8[132,107,75], "*") == "*kK"
@test convert(UTF8String, UInt8[32,107,75], "αβ") == " kK"
@test convert(UTF8String, UInt8[132,107,75], "αβ") == "αβkK"
@test convert(UTF8String, UInt8[], "*") == ""
@test convert(UTF8String, UInt8[255], "αβ") == "αβ"

# test AbstractString functions at beginning of string.jl
immutable tstStringType <: AbstractString
    data::Array{UInt8,1}
end
tstr = tstStringType("12");
@test_throws ErrorException endof(tstr)
@test_throws ErrorException next(tstr, Bool(1))

## generic string uses only endof and next ##

immutable GenericString <: AbstractString
    string::AbstractString
end

Base.endof(s::GenericString) = endof(s.string)
Base.next(s::GenericString, i::Int) = next(s.string, i)

gstr = GenericString("12");
@test typeof(string(gstr))==GenericString
@test bytestring()==""

@test convert(Array{UInt8}, gstr) ==[49;50]
@test convert(Array{Char,1}, gstr) ==['1';'2']
@test convert(Symbol, gstr)==symbol("12")

@test getindex(gstr, Bool(1))=='1'
@test getindex(gstr,Bool(1):Bool(1))=="1"
@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test symbol(gstr)==symbol("12")

@test_throws ErrorException sizeof(gstr)

@test length(GenericString(""))==0

@test getindex(gstr,AbstractVector([Bool(1):Bool(1);]))=="1"

@test nextind(AbstractArray([Bool(1):Bool(1);]),1)==2

@test ind2chr(gstr,2)==2

# issue #10307
@test typeof(map(Int16,String[])) == Vector{Int16}

for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
    for i in [typemax(T), typemin(T)]
        s = "$i"
        @test get(tryparse(T, s)) == i
    end
end

for T in [Int8, Int16, Int32, Int64, Int128]
    for i in [typemax(T), typemin(T)]
        f = "$(i)0"
        @test isnull(tryparse(T, f))
    end
end

# issue #11142
s = "abcdefghij"
sp = pointer(s)
@test ascii(sp) == s
@test ascii(sp,5) == "abcde"
@test typeof(ascii(sp)) == ASCIIString
@test typeof(utf8(sp)) == UTF8String
s = "abcde\uff\u2000\U1f596"
sp = pointer(s)
@test utf8(sp) == s
@test utf8(sp,5) == "abcde"
@test typeof(utf8(sp)) == UTF8String

@test get(tryparse(BigInt, "1234567890")) == BigInt(1234567890)
@test isnull(tryparse(BigInt, "1234567890-"))

@test get(tryparse(Float64, "64")) == 64.0
@test isnull(tryparse(Float64, "64o"))
@test get(tryparse(Float32, "32")) == 32.0f0
@test isnull(tryparse(Float32, "32o"))

# issue #10994: handle embedded NUL chars for string parsing
for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
    @test_throws ArgumentError parse(T, "1\0")
end
for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float64, Float32]
    @test isnull(tryparse(T, "1\0"))
end
let s = normalize_string("tést",:NFKC)
    @test bytestring(Base.unsafe_convert(Cstring, s)) == s
    @test bytestring(convert(Cstring, symbol(s))) == s
    @test wstring(Base.unsafe_convert(Cwstring, wstring(s))) == s
end
let s = "ba\0d"
    @test_throws ArgumentError Base.unsafe_convert(Cstring, s)
    @test_throws ArgumentError Base.unsafe_convert(Cwstring, wstring(s))
end

# issue # 11389: Vector{UInt32} was copied with UTF32String, unlike Vector{Char}
a = UInt32[48,0]
b = UTF32String(a)
@test b=="0"
a[1] = 65
@test b=="A"
c = Char['0','\0']
d = UTF32String(c)
@test d=="0"
c[1] = 'A'
@test d=="A"

# iteration
@test [c for c in "ḟøøƀäṙ"] == ['ḟ', 'ø', 'ø', 'ƀ', 'ä', 'ṙ']
@test [i for i in eachindex("ḟøøƀäṙ")] == [1, 4, 6, 8, 10, 12]
@test [x for x in enumerate("ḟøøƀäṙ")] == [(1, 'ḟ'), (2, 'ø'), (3, 'ø'), (4, 'ƀ'), (5, 'ä'), (6, 'ṙ')]
