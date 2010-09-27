load("utf8.j")

fh = open("unicode/UTF-8.txt")
utf8 = UTF8String(read(fh, Uint8, 4382595)[4:]);

fh = open("unicode/UTF-32LE.txt")
utf32 = read(fh, Int32, div(4448260,sizeof(Int32)))[2:end]

for i=1, j=1
    while i <= length(utf8)
        (c,i) = read_char(utf8,i)
        assert(c == utf32[j])
        j += 1
    end
end
