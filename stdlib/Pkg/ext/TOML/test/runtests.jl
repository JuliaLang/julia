using TOML
import TOML: linecol, whitespace, comment, newline, expect, lookup, Parser, parse

if Base.isdeprecated(Base, :Test)
    using Test
else
    using Base.Test
end

macro testval(s, v)
    f = "foo = $s"
    :( @test get(parse(Parser($f)))["foo"] == $v )
end

macro fail(s...)
    teststr = s[1]
    errstr = ""
    debug = false
    if length(s) == 2
        if isa(s[2], String)
            errstr = s[2]
        elseif isa(s[2], Expr)
            debug = s[2].args[1] == :debug
        end
    end
    if length(s) == 3
        errstr = s[2]
        debug = s[3].args[1] == :debug
    end

    # macro vars
    pvar = :pv
    ppvar = :ppv

    # debuging report
    dbgexp = if debug
        quote
            println("\nTEST FAIL: ", escape_string($teststr))
            if length($pvar.errors) > 0
                println("ERRORS:")
                for e in $pvar.errors
                    println(e)
                end
            end
            if !isnull($ppvar)
                println("RESULT:")
                println(get($ppvar))
            end
        end
    else
        :()
    end

    # error for comparison
    errtsts = if !isempty(errstr)
        quote
            @test length($pvar.errors)>0
            @test findlast(m->m==$errstr, map(e->e.msg, $pvar.errors)) != 0
        end
    else
        :()
    end

    quote
        local $pvar = Parser($teststr)
        local $ppvar = parse($pvar)
        $dbgexp
        $errtsts
        @test isnull($ppvar)
    end
end

macro success(s...)
    teststr = s[1]
    debug = false
    if length(s) == 2 && isa(s[2], Expr)
        debug = s[2].args[1] == :debug
    end

    # macro vars
    pvar = :pv
    ppvar = :ppv

    # debuging report
    dbgexp = if debug
        quote
            println("\nTEST SUCCESS: ", escape_string($teststr))
            if length($pvar.errors) > 0
                println("ERRORS:")
                for e in $pvar.errors
                    println(e)
                end
            end
            if !isnull($ppvar)
                println("RESULT:")
                println(get($ppvar))
            end
        end
    else
        :()
    end

    quote
        local $pvar = Parser($teststr)
        local $ppvar = parse($pvar)
        $dbgexp
        @test !isnull($ppvar)
    end
end

@testset "TOML parser" begin

    @testset "Parser internal functions" begin
        test = """
   #[test]\r
   [test]
   foo = "bar"
"""
        p = Parser(test)
        @test whitespace(p)
        @test position(p) == 4
        @test comment(p)
        @test position(p) == 13
        @test whitespace(p)
        @test position(p) == 16
        @test !expect(p, 'a')
        @test expect(p, '[')
        @test position(p) == 17
    end


    @testset "Lookups" begin
        p = Parser("""hello."world\\t".a.0.'escaped'.value""")
        @testset for (p, s) in zip(get(lookup(p)), ["hello"; "world\t"; "a"; "0"; "escaped"; "value"])
            @test p == s
        end

        p = Parser("")
        @test get(lookup(p)) == String[]

        p = Parser("value")
        @test get(lookup(p)) == String["value"]

        p = Parser("\"\"")
        #TODO: @test get(lookup(p)) == String[""]

    end

    @testset "New line" begin
        @success("""
[project.A]\r
name = \"splay2\"\r
\r
[project]\r
\r
name = \"splay\"\r
version = \"0.1.0\"\r
authors = [\"alex@crichton.co\"]\r
\r
[[lib]]\r
\r
path = \"lib.rs\"\r
name = \"splay\"\r
description = \"\"\"
A Rust implementation of a TAR file reader and writer. This library does not\r
currently handle compression, but it is abstract over all I/O readers and\r
writers. Additionally, great lengths are taken to ensure that the entire\r
contents are never required to be entirely resident in memory all at once.\r
\"\"\"
[[lib.aaa]]\r
bbb = \"aaa\"\r
""")

        @fail("\r")

        @fail("a = [ \r ]")

        @fail("a = \"\"\"\r\"\"\"")

        @fail("a = \"\"\"\\  \r  \"\"\"")

        @success("a = '''\r'''")

        @success("a = '\r'")

        @fail("a = \"\n\"")

        @fail("a = '\n'")

        p = Parser("
            foo = \"\"\"\\\r\n\"\"\"
            bar = \"\"\"\\\r\n   \r\n   \r\n   a\"\"\"
        ")
        res = get(parse(p))
        @test res["foo"] == ""
        @test res["bar"] == "a"

        @fail("0=0r=false", "expected a newline after a key")

        @fail("""
0=""o=""m=""r=""00="0"q=\"\"\"0\"\"\"e=\"\"\"0\"\"\"
""", "expected a newline after a key")

        @fail("""
[[0000l0]]
0="0"[[0000l0]]
0="0"[[0000l0]]
0="0"l="0"
""", "expected a newline after a key")

        @fail("""
0=[0]00=[0,0,0]t=["0","0","0"]s=[1000-00-00T00:00:00Z,2000-00-00T00:00:00Z]
""", "expected a newline after a key")

        @fail("""
0=0r0=0r=false
""", "expected a newline after a key")

        @fail("""
0=0r0=0r=falsefal=false
""", "expected a newline after a key")

    end

    @testset "Offset->(Line, Col) " begin
        p = Parser("ab\ncde\nf")
        @test linecol(p, 0) == (1,0)
        @test linecol(p, 1) == (1,1)
        @test linecol(p, 4) == (2,1)
        @test linecol(p, 5) == (2,2)
        @test linecol(p, 8) == (3,1)
        @test linecol(p, 9) == (3,0)
    end

    @testset "Strings" begin
        p = Parser("""
bar = "\\U00000000"
key1 = "One\\nTwo"
key2 = \"\"\"One\nTwo\"\"\"
key3 = \"\"\"
One
Two\"\"\"

key4 = "The quick brown fox jumps over the lazy dog."
key5 = \"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\"
key6 = \"\"\"\\
       The quick brown \\
       fox jumps over \\
       the lazy dog.\\
       \"\"\"

# What you see is what you get.
winpath  = 'C:\\Users\\nodejs\\templates'
winpath2 = '\\\\ServerX\\admin\$\\system32\\'
quoted   = 'Tom "Dubs" Preston-Werner'
regex    = '<\\i\\c*\\s*>'
regex2 = '''I [dw]on't need \\d{2} apples'''
lines  = '''
The first newline is
trimmed in raw strings.
    All other whitespace
    is preserved.
'''
""")
        res = get(parse(p))

        @test res["bar"]  == "\0"
        @test res["key1"] == "One\nTwo"
        @test res["key2"] == "One\nTwo"
        @test res["key3"] == "One\nTwo"

        msg = "The quick brown fox jumps over the lazy dog."
        @test res["key4"] == msg
        @test res["key5"] == msg
        @test res["key6"] == msg

        @test res["winpath"] == "C:\\Users\\nodejs\\templates"
        @test res["winpath2"] == "\\\\ServerX\\admin\$\\system32\\"
        @test res["quoted"] == """Tom "Dubs" Preston-Werner"""
        @test res["regex"] == """<\\i\\c*\\s*>"""
        @test res["regex2"] == """I [dw]on't need \\d{2} apples"""

        @test res["lines"] == "The first newline is\ntrimmed in raw strings.\n    All other whitespace\n    is preserved.\n"


        @testval("''", "")
        @testval("\"\"", "")
        @testval("\"\"\"\n\n\n\"\"\"", "\n\n")

        #@fail("foo = \"\\uD800\"")

        @fail("foo = \"\\uxx\"")
        @fail("foo = \"\\u\"")
        @fail("foo = \"\\")
        @fail("foo = '")

    end

    @testset "Numbers" begin
        @fail("a = 00")
        @fail("a = -00")
        @fail("a = +00")
        @fail("a = 00.0")
        @fail("a = -00.0")
        @fail("a = +00.0")
        @fail("a = 9223372036854775808")
        @fail("a = -9223372036854775809")

        @fail("a = 0.")
        @fail("a = 0.e")
        @fail("a = 0.E")
        @fail("a = 0.0E")
        @fail("a = 0.0e")
        @fail("a = 0.0e-")
        @fail("a = 0.0e+")
        @fail("a = 0.0e+00")

        @testval("1.0", 1.0)
        @testval("1.0e0", 1.0)
        @testval("1.0e+0", 1.0)
        @testval("1.0e-0", 1.0)
        @testval("1.001e-0", 1.001)
        @testval("2e10", 2e10)
        @testval("2e+10", 2e10)
        @testval("2e-10", 2e-10)
        @testval("2_0.0", 20.0)
        @testval("2_0.0_0e0_0", 20.0)
        @testval("2_0.1_0e1_0", 20.1e10)

        @fail("4")

        @testval("1_0", 10)
        @testval("1_0_0", 100)
        @testval("1_000", 1000)
        @testval("+1_000", 1000)
        @testval("-1_000", -1000)

        @fail("foo = 0_")
        @fail("foo = 0__0")
        @fail("foo = __0")
        @fail("foo = 1_0_")

    end

    @testset "Booleans" begin

        @testval("true", true)
        @testval("false", false)

        @fail("foo = true2")
        @fail("foo = false2")
        @fail("foo = t1")
        @fail("foo = f1")

    end

    @testset "Datetime" begin

        @testval("2016-09-09T09:09:09Z", DateTime(2016,9,9,9,9,9))
        @testval("2016-09-09T09:09:09.0Z", DateTime(2016,9,9,9,9,9))
        @testval("2016-09-09T09:09:09.0+10:00", DateTime(2016,9,9,19,9,9))
        @testval("2016-09-09T09:09:09.012-02:00", DateTime(2016,9,9,7,9,9,12))

        @fail("foo = 2016-09-09T09:09:09.Z", "malformed date literal")
        @fail("foo = 2016-9-09T09:09:09Z", "malformed date literal")
        @fail("foo = 2016-09-09T09:09:09+2:00", "malformed date literal")
        @fail("foo = 2016-09-09T09:09:09-2:00", "malformed date literal")
        @fail("foo = 2016-09-09T09:09:09Z-2:00", "expected a newline after a key")

    end


    @testset "Keys" begin

        p = Parser("
            foo = 3
            foo_3 = 3
            foo_-2--3--r23f--4-f2-4 = 3
            _ = 3
            - = 3
            8 = 8
            \"a\" = 3
            \"!\" = 3
            \"a^b\" = 3
            \"\\\\\\\"\" = 3
            \"character encoding\" = \"value\"
            'ʎǝʞ' = \"value\"
        ")
        res = get(parse(p))

        @test haskey(res, "foo")
        @test haskey(res, "-")
        @test haskey(res, "_")
        @test haskey(res, "8")
        @test haskey(res, "foo_3")
        @test haskey(res, "foo_-2--3--r23f--4-f2-4")
        @test haskey(res, "a")
        @test haskey(res, "!")
        @test haskey(res, "\\\"")
        @test haskey(res, "character encoding")
        @test haskey(res, "ʎǝʞ")

        @fail("key\n=3")
        @fail("key=\n3")
        @fail("key|=3")
        @fail("\"\"=3")
        @fail("=3")
        @fail("\"\"|=3")
        @fail("\"\n\"|=3")
        @fail("\"\r\"|=3")

    end


    @testset "Tables" begin

        @fail("[]")
        @fail("[.]")
        @fail("[\"\".\"\"]")
        @fail("[a.]")
        @fail("[\"\"]")
        @fail("[!]")
        @fail("[\"\n\"]")
        @fail("[a.b]\n[a.\"b\"]")
        @fail("[']")
        @fail("[''']")
        @fail("['''''']")
        @fail("['\n']")
        @fail("['\r\n']")

        p = Parser("
            [a.\"b\"]
            [\"f f\"]
            [\"f.f\"]
            [\"\\\\\\\"\"]
            ['a.a']
            ['\"\"']
        ")
        res = get(parse(p))
        @test haskey(res, "a.a")
        @test haskey(res, "f f")
        @test haskey(res, "f.f")
        @test haskey(res, "\\\"")
        @test haskey(res, "\"\"")
        @test haskey(res["a"], "b")


        @test haskey(get(parse(Parser("[foo]"))), "foo")


        @testset "Inline Tables" begin

            @success("a = {}")
            @success("a = {b=1}")
            @success("a = {   b   =   1    }")
            @success("a = {a=1,b=2}")
            @success("a = {a=1,b=2,c={}}")
            @fail("a = {a=1,}")
            @fail("a = {,}")
            @fail("a = {a=1,a=1}")
            @fail("a = {\n}")
            @fail("a = {")
            @success("a = {a=[\n]}")
            @success("a = {\"a\"=[\n]}")
            @success("a = [\n{},\n{},\n]")

        end


        @testset "Redefinition" begin
            @fail("
                [a]
                foo=\"bar\"
                [a.b]
                foo=\"bar\"
                [a]
            ", "redefinition of table `a`")
            @fail("
                [a]
                foo=\"bar\"
                b = { foo = \"bar\" }
                [a]
            ", "redefinition of table `a`")
            @success("
                [a.b]
                c = {}
                [a]
                d = 1
            ")
            @fail("
                [a.b]
                c = 1
                [a]
                b = {}
            ", "duplicate key `b`")
            @fail("
                [a]
                b = {}
                [a]
            ", "redefinition of table `a`")
        end

        @testset "Nesting" begin
            @fail("
                a = [2]
                [[a]]
                b = 5
            ", "expected type `TOML.Table`, found type `$(Int)`")
            @fail("
                a = 1
                [a.b]
            ", "key `a` was not previously a table")
            @fail("
                a = []
                [a.b]
            ", "array `a` does not contain tables")
            @success("
                a = [{}]
                [a.b]
            ")
            @fail("
                a = []
                [[a.b]]
            ", "array `a` does not contain tables")
            @fail("
                [a]
                b = { c = 2, d = {} }
                [a.b]
                c = 2
            ", "duplicate key `c`")
        end
    end

    @testset "Arrays" begin

        p = Parser("""
[[foo]]
  #…
  [foo.bar]
    #…
[[foo]] # ...
  #…
  [foo.bar]
    #...
""")
        res = get(parse(p))
        @test haskey(res, "foo")
        arr = res["foo"]
        @test length(arr) == 2
        @test isa(arr[1], TOML.Table)
        @test haskey(arr[1], "bar")
        @test haskey(arr[2], "bar")

        p = Parser("""
[[fruit]]
  name = "apple"
  [fruit.physical]
    color = "red"
    shape = "round"
  [[fruit.variety]]
    name = "red delicious"
  [[fruit.variety]]
    name = "granny smith"
[[fruit]]
  name = "banana"
  [[fruit.variety]]
    name = "plantain"
""")
        res = get(parse(p))
        @test haskey(res, "fruit")
        fruit = res["fruit"]
        @test length(fruit) == 2
        @test isa(fruit[1], TOML.Table)
        apple = fruit[1]
        banana = fruit[2]

        @test haskey(apple, "name")
        @test apple["name"] == "apple"
        @test haskey(apple, "physical")
        @test apple["physical"]["color"] == "red"
        @test apple["physical"]["shape"] == "round"
        @test apple["variety"][1]["name"] == "red delicious"
        @test apple["variety"][2]["name"] == "granny smith"

        @test haskey(banana, "name")
        @test banana["name"] == "banana"
        @test banana["variety"][1]["name"] == "plantain"

        res = TOML.parse("""
[[products]]
name = "Hammer"
sku = 738594937

[[products]]

[[products]]
name = "Nail"
sku = 284758393
color = "gray"
""")
        @test haskey(res, "products")
        products = res["products"]
        @test isa(products, Array)
        @test length(products) == 3
        @test products[1]["name"] == "Hammer"
        @test products[1]["sku"] == 738594937
        @test length(products[2]) == 0
        @test products[3]["name"] == "Nail"
        @test products[3]["sku"] == 284758393
        @test products[3]["color"] == "gray"

        @fail("""array = [
        #         "This might most likely happen in multiline arrays",
        #         Like here,
        #         "or here,
        #         and here"
        #         ]     End of array comment, forgot the #
        """)
    end

end

@testset "TOML printer" begin
    res1 = TOML.parse("""
title = "TOML Example"
[owner]
bio = \"\"\"GitHub Cofounder & CEO
Likes tater tots and beer.\"\"\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"
  country = "中国" # This should be parsed as UTF-8

[clients]
data = [ ["gamma", "delta"], [1, 2] ] # just an update to make sure parsers support it

  [[products]]
  name = "Hammer"
  sku = 738594937

  [[products]]
  name = "Nail"
  sku = 284758393
  color = "gray"
""")
    io = IOBuffer()
    TOML.print(io, res1)
    seekstart(io)
    res2 = TOML.parse(io)
    @test res1 == res2

end
