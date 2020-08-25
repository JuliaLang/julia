# This test stuff in the TOML README at https://github.com/toml-lang/toml
@testset "README" begin

@testset "Example" begin
str = """
# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
"""
@test roundtrip(str)
d = parse(str)
@test d["title"] == "TOML Example"
@test d["owner"]["name"] == "Tom Preston-Werner"
@test d["database"] == Dict(
  "server" => "192.168.1.1",
  "ports" => [ 8001, 8001, 8002 ],
  "connection_max" => 5000,
  "enabled" => true,
)
@test d["servers"] == Dict(
  "alpha" => Dict(
  "ip" => "10.0.0.1",
  "dc" => "eqdc10",
  ),
  "beta" => Dict(
    "ip" => "10.0.0.2",
    "dc" => "eqdc10",
  )
)

@test d["clients"]["data"] == [ ["gamma", "delta"], [1, 2] ]
@test d["clients"]["hosts"] == ["alpha", "omega"]

end # testset

@testset "Comment" begin
str = """
# This is a full-line comment
key = "value"  # This is a comment at the end of a line
another = "# This is not a comment"
"""
@test roundtrip(str)
@test parse(str) == Dict("key" => "value", "another" => "# This is not a comment")

end # testset


@testset "Key/Value Pair" begin

str = """
key = # INVALID
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrUnexpectedStartOfValue

str = """
first = "Tom" last = "Preston-Werner" # INVALID
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrExpectedNewLineKeyValue

end

@testset "Keys" begin

str = """
key = "value"
bare_key = "value"
bare-key = "value"
1234 = "value"
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "key" => "value",
  "bare_key" => "value",
  "bare-key" => "value",
  "1234" => "value",
)

str = """
"127.0.0.1" = "value"
"character encoding" = "value"
"ʎǝʞ" = "value"
'key2' = "value"
'quoted "value"' = "value"
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "127.0.0.1" => "value",
  "character encoding" => "value",
  "ʎǝʞ" => "value",
  "key2" => "value",
  "quoted \"value\"" => "value",
)

str = """
= "no key name"  # INVALID
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrEmptyBareKey

str = """
"" = "blank"     # VALID but discouraged
"""
@test parse(str) == Dict("" => "blank")

str = """
'' = 'blank'     # VALID but discouraged
"""
@test roundtrip(str)
@test parse(str) == Dict("" => "blank")

str = """
name = "Orange"
physical.color = "orange"
physical.shape = "round"
site."google.com" = true
"""

@test roundtrip(str)
@test parse(str) == Dict(
  "name" => "Orange",
  "physical" => Dict("color" => "orange", "shape" => "round"),
  "site" => Dict("google.com" => true)
)

str = """
# DO NOT DO THIS
name = "Tom"
name = "Pradyun"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue

str = """
# THIS WILL NOT WORK
spelling = "favorite"
"spelling" = "favourite"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue

str = """
3.14159 = "pi"
"""
@test roundtrip(str)
@test parse(str) == Dict("3" => Dict("14159" => "pi"))


str = """
# This makes the key "fruit" into a table.
fruit.apple.smooth = true

# So then you can add to the table "fruit" like so:
fruit.orange = 2
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "fruit" => Dict("orange" => 2,
                  "apple" => Dict("smooth" => true)))

str = """
# THE FOLLOWING IS INVALID

# This defines the value of fruit.apple to be an integer.
fruit.apple = 1

# But then this treats fruit.apple like it's a table.
# You can't turn an integer into a table.
fruit.apple.smooth = true
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue

str = """
# VALID BUT DISCOURAGED

apple.type = "fruit"
orange.type = "fruit"

apple.skin = "thin"
orange.skin = "thick"

apple.color = "red"
orange.color = "orange"
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "apple" => Dict("type" => "fruit", "skin" => "thin", "color" => "red"),
  "orange" => Dict("type" => "fruit", "skin" => "thick", "color" => "orange"),
)

str = """
# RECOMMENDED

apple.type = "fruit"
apple.skin = "thin"
apple.color = "red"

orange.type = "fruit"
orange.skin = "thick"
orange.color = "orange"
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "apple" => Dict("type" => "fruit", "skin" => "thin", "color" => "red"),
  "orange" => Dict("type" => "fruit", "skin" => "thick", "color" => "orange"),
)

end #testset


@testset "String" begin

str = """str = "I'm a string. \\"You can quote me\\". Name\\tJos\\u00E9\\nLocation\\tSF." """
@test roundtrip(str)
@test parse(str) == Dict("str" => "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF.")

str = """str1 = \"\"\"
Roses are red
Violets are blue
\"\"\"
"""
@test roundtrip(str)
@test parse(str) == Dict("str1" => """
Roses are red
Violets are blue
""")

str = """
# The following strings are byte-for-byte equivalent:
str1 = "The quick brown fox jumps over the lazy dog."

str2 = \"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\"

str3 = \"\"\"\\
       The quick brown \\
       fox jumps over \\
       the lazy dog.\\
       \"\"\"
"""
@test roundtrip(str)
d = @test parse(str)["str1"] == parse(str)["str2"] == parse(str)["str3"]

str = """
str4 = \"\"\"Here are two quotation marks: \"\". Simple enough.\"\"\"
str5 = \"\"\"Here are three quotation marks: \"\"\\\".\"\"\"
str6 = \"\"\"Here are fifteen quotation marks: \"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\".\"\"\"
"""
@test roundtrip(str)
d = parse(str)
@test d["str4"] == "Here are two quotation marks: \"\". Simple enough."
@test d["str5"] == "Here are three quotation marks: \"\"\"."
@test d["str6"] == "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"."

str = """
# "This," she said, "is just a pointless statement."
str7 = \"\"\"\"This,\" she said, \"is just a pointless statement.\"\"\"\"
"""
@test_broken parse(str)

"""
str5 = \"\"\"Here are three quotation marks: \"\"\".\"\"\"  # INVALID
"""

str = raw"""
# What you see is what you get.
winpath  = 'C:\Users\nodejs\templates'
winpath2 = '\\ServerX\admin$\system32\'
quoted   = 'Tom "Dubs" Preston-Werner'
regex    = '<\i\c*\s*>'
"""
@test roundtrip(str)
d = parse(str)
@test d["winpath"] == raw"C:\Users\nodejs\templates"
@test d["winpath2"] == raw"\\ServerX\admin$\system32\\"
@test d["quoted"] == raw"""Tom "Dubs" Preston-Werner"""
@test d["regex"] == raw"<\i\c*\s*>"

str = raw"""
regex2 = '''I [dw]on't need \d{2} apples'''
lines  = '''
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
'''
"""
@test roundtrip(str)
d = parse(str)
@test d["regex2"] == raw"I [dw]on't need \d{2} apples"
@test d["lines"] == raw"""
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
"""

str = """
quot15 = '''Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"'''

apos15 = "Here are fifteen apostrophes: '''''''''''''''"

# 'That,' she said, 'is still pointless.'
str = ''''That,' she said, 'is still pointless.''''
"""
@test_broken parse(str)

str = """
apos15 = '''Here are fifteen apostrophes: ''''''''''''''''''  # INVALID
"""
@test tryparse(str) isa ParserError

end # testset

@testset "Integer" begin

str = """
int1 = +99
int2 = 42
int3 = 0
int4 = -17
int5 = -0
int6 = +0
"""
@test roundtrip(str)
d = parse(str)
@test d["int1"] === Int64(99)
@test d["int2"] === Int64(42)
@test d["int3"] === Int64(0)
@test d["int4"] === Int64(-17)
@test d["int5"] === Int64(0)
@test d["int6"] === Int64(0)


str = """
int5 = 1_000
int6 = 5_349_221
int7 = 53_49_221  # Indian number system grouping
int8 = 1_2_3_4_5  # VALID but discouraged
"""
@test roundtrip(str)
d = parse(str)
@test d["int5"] == 1_000
@test d["int6"] == 5_349_221
@test d["int7"] == 53_49_221
@test d["int8"] == 1_2_3_4_5

str = """
# hexadecimal with prefix `0x`
hex1 = 0xDEADBEEF
hex2 = 0xdeadbeef
hex3 = 0xdead_beef

# octal with prefix `0o`
oct1 = 0o01234567
oct2 = 0o755 # useful for Unix file permissions

# binary with prefix `0b`
bin1 = 0b11010110
"""
@test roundtrip(str)
d = parse(str)
@test d["hex1"] == 0xDEADBEEF
@test d["hex2"] == 0xdeadbeef
@test d["hex3"] == 0xdead_beef
@test d["oct1"] == 0o01234567
@test d["oct2"] == 0o755
@test d["bin1"] == 0b11010110

#Arbitrary 64-bit signed integers (from −2^63 to 2^63−1) should be accepted and
#handled losslessly. If an integer cannot be represented losslessly, an error
#must be thrown.
str = """
low = -9_223_372_036_854_775_808
high = 9_223_372_036_854_775_807
"""
@test roundtrip(str)
d = parse(str)
@test d["low"] == -9_223_372_036_854_775_808
@test d["high"] == 9_223_372_036_854_775_807

str = """
toolow = -9_223_372_036_854_775_809
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrOverflowError

str = """
toohigh = 9_223_372_036_854_775_808
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrOverflowError

end


@testset "Float" begin

str = """
# fractional
flt1 = +1.0
flt2 = 3.1415
flt3 = -0.01

# exponent
flt4 = 5e+22
flt5 = 1e06
flt6 = -2E-2

# both
flt7 = 6.626e-34
flt8 = 224_617.445_991_228
"""
@test roundtrip(str)
d = parse(str)
@test d["flt1"] == +1.0
@test d["flt2"] == 3.1415
@test d["flt3"] == -0.01
@test d["flt4"] == 5e+22
@test d["flt5"] == 1e+6
@test d["flt6"] == -2E-2
@test d["flt7"] == 6.626e-34
@test d["flt8"] == 224_617.445_991_228

#The decimal point, if used, must be surrounded by at least one digit on each side.
str = """
# INVALID FLOATS
invalid_float_1 = .7
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrLeadingDot

str = """
# INVALID FLOATS
invalid_float_2 = 7.
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrNoTrailingDigitAfterDot

str = """
# INVALID FLOATS
invalid_float_3 = 3.e+20
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrNoTrailingDigitAfterDot

str = """
# infinity
sf1 = inf  # positive infinity
sf2 = +inf # positive infinity
sf3 = -inf # negative infinity

# not a number
sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific
sf5 = +nan # same as `nan`
sf6 = -nan # valid, actual encoding is implementation-specific
"""
@test roundtrip(str)
d = parse(str)
@test d["sf1"] == Inf
@test d["sf2"] == Inf
@test d["sf3"] == -Inf
@test isnan(d["sf4"])
@test isnan(d["sf5"])
@test isnan(d["sf6"])

end


@testset "Boolean" begin
str = """
bool1 = true
bool2 = false
"""
@test roundtrip(str)
d = parse(str)
@test d["bool1"] === true
@test d["bool2"] === false


@testset "Offset Date-Time" begin

# Added some fractional digits here
str = "odt1 = 1979-05-27T07:32:00.99999Z"
# Truncated milliseconds
@test roundtrip(str)
@test parse(str)["odt1"] == Base.parse(DateTime, "1979-05-27T07:32:00.999Z", dateformat"yyyy-mm-ddTHH:MM:SS.sZ")

# Julia doesn't support offset datetimes
str = "odt2 = 1979-05-27T00:32:00-07:00"
err = tryparse(str)
@test_broken err isa Dict
@test err isa Internals.ParserError
@test err.type == Internals.ErrOffsetDateNotSupported

str = "odt3 = 1979-05-27T00:32:00.999999-07:00"
err = tryparse(str)
@test_broken err isa Dict
@test err isa Internals.ParserError
@test err.type == Internals.ErrOffsetDateNotSupported

str = "odt4 = 1979-05-27 07:32:00Z"
@test roundtrip(str)
d = parse(str)
@test d["odt4"] == Base.parse(DateTime, "1979-05-27 07:32:00Z", dateformat"yyyy-mm-dd HH:MM:SSZ")

end


@testset "Local Date-Time" begin

str = """
ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999
"""
@test roundtrip(str)
d = parse(str)
@test d["ldt1"] == Base.parse(DateTime, "1979-05-27T07:32:00")
@test d["ldt2"] == Base.parse(DateTime, "1979-05-27T00:32:00.999")

end


@testset "Local Date" begin

str = """
ld1 = 1979-05-27
"""
@test roundtrip(str)
d = parse(str)
@test d["ld1"] == Base.parse(Date, "1979-05-27")

end


@testset "Local Time" begin

str = """
lt1 = 07:32:00
lt2 = 00:32:00.999999
"""
@test roundtrip(str)
d = parse(str)
@test d["lt1"] == Time(07, 32, 00)
@test d["lt2"] == Time(00, 32, 00, 999)

end

end # testset


@testset "Array" begin

str = """
integers = [ 1, 2, 3 ]
colors = [ \"red\", \"yellow\", \"green\" ]
nested_array_of_int = [ [ 1, 2 ], [3, 4, 5] ]
nested_mixed_array = [ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]
string_array = [ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''' ]

# Mixed-type arrays are allowed
numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
contributors = [
  \"Foo Bar <foo@example.com>\",
  { name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }
]
"""
@test_broken roundtrip(str) # Printer doesn't handle inline tables in arrays?
d = parse(str)
@test d["integers"] == [1,2,3]
@test d["colors"] == ["red", "yellow", "green"]
@test d["nested_array_of_int"] == [[1,2], [3,4,5]]
@test d["nested_mixed_array"] == [[1,2], ["a", "b", "c"]]
@test d["string_array"] == ["all", "strings", "are the same", "type"]
@test all(d["numbers"] .=== Any[0.1, 0.2, 0.5, Int64(1), Int64(2), Int64(5)])
@test d["contributors"] == [
    "Foo Bar <foo@example.com>",
    Dict("name" => "Baz Qux",
         "email" => "bazqux@example.com",
         "url" => "https://example.com/bazqux")
]

str = """
integers2 = [
  1, 2, 3
]

integers3 = [
  1,
  2, # this is ok
]
"""
@test roundtrip(str)
d = parse(str)
@test d["integers3"] == [1, 2]
@test d["integers2"] == [1, 2, 3]

end # testset


@testset "Table" begin

str = """
[table]
key1 = "some string"
key2 = "some other string"
"""
@test roundtrip(str)
d = parse(str)
@test d == Dict("table" => Dict("key2" => "some other string", "key1" => "some string"))

str = """
[dog."tater.man"]
type.name = "pug"
"""
@test roundtrip(str)
d = parse(str)
@test d == Dict("dog" => Dict("tater.man" => Dict("type" => Dict("name" => "pug"))))

str = """
[a.b.c]            # this is best practice
[ d.e.f ]          # same as [d.e.f]
[ g .  h  . i ]    # same as [g.h.i]
[ j . "ʞ" . 'l' ]  # same as [j."ʞ".'l']
"""
@test_broken roundtrip(str) # Printer removes empty tables right now
d = parse(str)
@test d == Dict(
  "a" => Dict("b" => Dict("c" => Dict())),
  "d" => Dict("e" => Dict("f" => Dict())),
  "g" => Dict("h" => Dict("i" => Dict())),
  "j" => Dict("ʞ" => Dict("l" => Dict())),
)


str = """
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work

[x] # defining a super-table afterward is ok
"""
@test_broken roundtrip(str) # Printer removes empty tables right now
d = parse(str)
@test d == Dict("x" => Dict("y" => Dict("z" => Dict("w" => Dict()))))


str = """
# [x] you
# [x.y] don't
# [x.y.z] need these
[x.y.z.w] # for this to work
a = 3

[x] # defining a super-table afterward is ok
b = 2
"""
@test roundtrip(str)
d = parse(str)
@test d == Dict(
  "x" => Dict(
      "b" => 2,
      "y" => Dict("z" => Dict("w" => Dict("a" => 3)))
  )
)


str = """
# DO NOT DO THIS

[fruit]
apple = "red"

[fruit]
orange = "orange"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrDuplicatedKey

str = """
# DO NOT DO THIS EITHER

[fruit]
apple = "red"

[fruit.apple]
texture = "smooth"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue


str = """
# VALID BUT DISCOURAGED
[fruit.apple]
[animal]
[fruit.orange]
"""
@test_broken roundtrip(str) # Printer removes empty tables right now
d = parse(str)
@test d == Dict(
  "fruit" => Dict("apple" => Dict(), "orange" => Dict()),
  "animal" => Dict()
)

str = """
# RECOMMENDED
[fruit.apple]
[fruit.orange]
[animal]
"""
@test_broken roundtrip(str) # Printer removes empty tables right now
@test d == Dict(
  "fruit" => Dict("apple" => Dict(), "orange" => Dict()),
  "animal" => Dict()
)

str = """
[fruit]
apple.color = "red"
apple.taste.sweet = true

# [fruit.apple]  # INVALID
# [fruit.apple.taste]  # INVALID

[fruit.apple.texture]  # you can add sub-tables
smooth = true
"""
@test roundtrip(str)
d = parse(str)
@test d == Dict(
"fruit" => Dict("apple" => Dict("color" => "red",
                                "taste" => Dict("sweet" => true),
                                "texture" => Dict("smooth" => true)))
)

end # testset


@testset "Inline table" begin

str = """
name = { first = "Tom", last = "Preston-Werner" }
point = { x = 1, y = 2 }
animal = { type.name = "pug" }
"""

str2 = """
[name]
first = "Tom"
last = "Preston-Werner"

[point]
x = 1
y = 2

[animal]
type.name = "pug"
"""
@test roundtrip(str)
@test roundtrip(str2)
@test parse(str) == parse(str2)

str = """
[product]
type = { name = "Nail" }
type.edible = false  # INVALID
"""
err =  tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrAddKeyToInlineTable

str = """
[product]
type.name = "Nail"
type = { edible = false }  # INVALID
"""
tryparse(str) isa ParserError
@test err isa ParserError
@test err.type == Internals.ErrAddKeyToInlineTable

@testset "Array of Tables" begin

str = """
[[products]]
name = "Hammer"
sku = 738594937

[[products]]

[[products]]
name = "Nail"
sku = 284758393

color = "gray"
"""
@test roundtrip(str)
@test parse(str) == Dict(
  "products" => [
    Dict("name" => "Hammer",
         "sku" => 738594937),
    Dict(),
    Dict("name" => "Nail",
         "sku" => 284758393,
         "color" => "gray")
  ]
)

str = """
[[fruit]]
  name = "apple"

  [fruit.physical]  # subtable
    color = "red"
    shape = "round"

  [[fruit.variety]]  # nested array of tables
    name = "red delicious"

  [[fruit.variety]]
    name = "granny smith"

[[fruit]]
  name = "banana"

  [[fruit.variety]]
    name = "plantain"
"""
@test roundtrip(str)
d = parse(str)
@test d == Dict(
  "fruit" => [
      Dict("name" => "apple",
           "physical" => Dict("color" => "red", "shape" => "round"),
           "variety" => [
        Dict("name" => "red delicious"),
        Dict("name" => "granny smith"),
      ]),
      Dict("name" => "banana",
           "variety" => [
             Dict("name" => "plantain"),
          ]
      )
])

str = """
# INVALID TOML DOC
[fruit.physical]  # subtable, but to which parent element should it belong?
  color = "red"
  shape = "round"

[[fruit]]  # parser must throw an error upon discovering that "fruit" is
           # an array rather than a table
  name = "apple"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrArrayTreatedAsDictionary

str = """
# INVALID TOML DOC
fruit = []

[[fruit]] # Not allowed
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrAddArrayToStaticArray

str = """
# INVALID TOML DOC
[[fruit]]
  name = "apple"

  [[fruit.variety]]
    name = "red delicious"

  # INVALID: This table conflicts with the previous array of tables
  [fruit.variety]
    name = "granny smith"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrDuplicatedKey

str = """
# INVALID TOML DOC
[[fruit]]
  name = "apple"

  [fruit.physical]
    color = "red"
    shape = "round"

  # INVALID: This array of tables conflicts with the previous table
  [[fruit.physical]]
    color = "green"
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrArrayTreatedAsDictionary

str = """
points = [ { x = 1, y = 2, z = 3 },
           { x = 7, y = 8, z = 9 },
           { x = 2, y = 4, z = 8 } ]
"""
@test roundtrip(str)
d = tryparse(str)
@test d == Dict(
  "points" => [
    Dict("x" => 1, "y" => 2, "z" => 3) ,
    Dict("x" => 7, "y" => 8, "z" => 9) ,
    Dict("x" => 2, "y" => 4, "z" => 8)
  ]
)

end

end

end # testset README
