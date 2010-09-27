load("utf8.j")

fh = open("unicode/UTF-32LE.txt")
utf32 = read(fh, Int32, div(4448260,sizeof(Int32)))[2:end]

fh = open("unicode/UTF-8.txt")
data = read(fh, Uint8, 4382595)
utf8 = UTF8String(data[4:]);

for i=1, j=1
    while i <= length(utf8)
        (c,i) = read_char(utf8,i)
        if c != utf32[j]
            print((i,j,uint32(c)),"\n")
            break
        end
        j += 1
    end
end
