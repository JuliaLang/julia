# Create unicode test data directory
unicodedir = mktempdir()

# Use perl to generate the primary data
primary_encoding = "UTF-32BE"
primary_path = file_path(unicodedir, primary_encoding*".unicode")
run(`perl -e 'print pack "N*", 0xfeff, 0..0xd7ff, 0xe000..0x10ffff' ` > primary_path)

# Use iconv to generate the other data
for encoding in ["UTF-32LE", "UTF-16BE", "UTF-16LE", "UTF-8"]
    output_path = file_path(unicodedir, encoding*".unicode")
    run(`iconv -f $primary_encoding -t $encoding $primary_path` > output_path)
end

str1 = CharString(reinterpret(Char, read(open(file_path(unicodedir,"UTF-32LE.unicode")), Uint32, 1112065)[2:]))
str2 = UTF8String(read(open(file_path(unicodedir,"UTF-8.unicode")), Uint8, 4382595)[4:])
@test str1 == str2

str1 = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
str2 = CharString(
    8704, 32, 949, 32, 62, 32, 48, 44, 32, 8707, 32,
    948, 32, 62, 32, 48, 58, 32, 124, 120, 45, 121, 124,
    32, 60, 32, 948, 32, 8658, 32, 124, 102, 40, 120,
    41, 45, 102, 40, 121, 41, 124, 32, 60, 32, 949
)
@test str1 == str2

# Cleanup unicode data
for encoding in ["UTF-32BE", "UTF-32LE", "UTF-16BE", "UTF-16LE", "UTF-8"]
    file_remove( file_path(unicodedir,encoding*".unicode"))
end
rmdir(unicodedir)
