str1 = CharString(reinterpret(Char, read(open("unicode/UTF-32LE.txt"), Uint32, 1112065)[2:]))
str2 = UTF8String(read(open("unicode/UTF-8.txt"), Uint8, 4382595)[4:])
@assert str1 == str2

str1 = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
str2 = CharString(
    8704, 32, 949, 32, 62, 32, 48, 44, 32, 8707, 32,
    948, 32, 62, 32, 48, 58, 32, 124, 120, 45, 121, 124,
    32, 60, 32, 948, 32, 8658, 32, 124, 102, 40, 120,
    41, 45, 102, 40, 121, 41, 124, 32, 60, 32, 949
)
@assert str1 == str2
