# This file is a part of Julia. License is MIT: http://julialang.org/license

# countlines
@test countlines(IOBuffer("\n")) == 1
@test countlines(IOBuffer("\n"),'\r') == 0
@test countlines(IOBuffer("\n\n\n\n\n\n\n\n\n\n")) == 10
@test countlines(IOBuffer("\n \n \n \n \n \n \n \n \n \n")) == 10
@test countlines(IOBuffer("\r\n \r\n \r\n \r\n \r\n")) == 5
file = tempname()
write(file,"Spiffy header\nspectacular first row\neven better 2nd row\nalmost done\n")
@test countlines(file) == 4
@test countlines(file,'\r') == 0
@test countlines(file,'\n') == 4
rm(file)

isequaldlm(m1, m2, t) = isequal(m1, m2) && (eltype(m1) == eltype(m2) == t)

@test isequaldlm(readdlm(IOBuffer("1\t2\n3\t4\n5\t6\n")), [1. 2; 3 4; 5 6], Float64)
@test isequaldlm(readdlm(IOBuffer("1\t2\n3\t4\n5\t6\n"), Int), [1 2; 3 4; 5 6], Int)

@test size(readcsv(IOBuffer("1,2,3,4"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,4\n"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,\n"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3,4"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3,"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,4\n1,2,3"))) == (2,4)

@test size(readcsv(IOBuffer("1,2,3,4\r\n"))) == (1,4)
@test size(readcsv(IOBuffer("1,2,3,4\r\n1,2,3\r\n"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,4\r\n1,2,3,4\r\n"))) == (2,4)
@test size(readcsv(IOBuffer("1,2,3,\"4\"\r\n1,2,3,4\r\n"))) == (2,4)

@test size(readdlm(IOBuffer("1 2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3\n"))) == (2,4)
@test size(readdlm(IOBuffer("1,,2,3,4\n1,2,3\n"), ',')) == (2,5)

let result1 = reshape(Any["", "", "", "", "", "", 1.0, 1.0, "", "", "", "", "", 1.0, 2.0, "", 3.0, "", "", "", "", "", 4.0, "", "", ""], 2, 13),
    result2 = reshape(Any[1.0, 1.0, 2.0, 1.0, 3.0, "", 4.0, ""], 2, 4)

    @test isequaldlm(readdlm(IOBuffer(",,,1,,,,2,3,,,4,\n,,,1,,,1\n"), ','), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("   1    2 3   4 \n   1   1\n")), result2, Any)
    @test isequaldlm(readdlm(IOBuffer("   1    2 3   4 \n   1   1\n"), ' '), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1 2\n3 4 \n")), [[1.0, 3.0] [2.0, 4.0]], Float64)
end

let result1 = reshape(Any["", "", "", "", "", "", "भारत", 1.0, "", "", "", "", "", 1.0, 2.0, "", 3.0, "", "", "", "", "", 4.0, "", "", ""], 2, 13)
    @test isequaldlm(readdlm(IOBuffer(",,,भारत,,,,2,3,,,4,\n,,,1,,,1\n"), ',') , result1, Any)
end

let result1 = reshape(Any[1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, ""], 2, 4)
    @test isequaldlm(readdlm(IOBuffer("1\t 2 3 4\n1 2 3")), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1\t 2 3 4\n1 2 3 ")), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1\t 2 3 4\n1 2 3\n")), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1,2,3,4\n1,2,3\n"), ','), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1,2,3,4\n1,2,3"), ','), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1,2,3,4\r\n1,2,3\r\n"), ','), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("1,2,3,\"4\"\r\n1,2,3\r\n"), ','), result1, Any)
end

let result1 = reshape(Any["abc", "hello", "def,ghi", " \"quote\" ", "new\nline", "world"], 2, 3),
    result2 = reshape(Any["abc", "line\"", "\"hello\"", "\"def", "", "\" \"\"quote\"\" \"", "ghi\"", "", "world", "\"new", "", ""], 3, 4)

    @test isequaldlm(readdlm(IOBuffer("abc,\"def,ghi\",\"new\nline\"\n\"hello\",\" \"\"quote\"\" \",world"), ','), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("abc,\"def,ghi\",\"new\nline\"\n\"hello\",\" \"\"quote\"\" \",world"), ',', quotes=false), result2, Any)
end

let result1 = reshape(Any["t", "c", "", "c"], 2, 2),
    result2 = reshape(Any["t", "\"c", "t", "c"], 2, 2)
    @test isequaldlm(readdlm(IOBuffer("t  \n\"c\" c")), result1, Any)
    @test isequaldlm(readdlm(IOBuffer("t t \n\"\"\"c\" c")), result2, Any)
end

@test isequaldlm(readcsv(IOBuffer("\n1,2,3\n4,5,6\n\n\n"), skipblanks=false),
                 reshape(Any["",1.0,4.0,"","","",2.0,5.0,"","","",3.0,6.0,"",""], 5, 3), Any)
@test isequaldlm(readcsv(IOBuffer("\n1,2,3\n4,5,6\n\n\n"), skipblanks=true), reshape([1.0,4.0,2.0,5.0,3.0,6.0], 2, 3), Float64)
@test isequaldlm(readcsv(IOBuffer("1,2\n\n4,5"), skipblanks=false), reshape(Any[1.0,"",4.0,2.0,"",5.0], 3, 2), Any)
@test isequaldlm(readcsv(IOBuffer("1,2\n\n4,5"), skipblanks=true), reshape([1.0,4.0,2.0,5.0], 2, 2), Float64)

let x = bitrand(5, 10), io = IOBuffer()
    writedlm(io, x)
    seek(io, 0)
    @test readdlm(io, Bool) == x
end

let x = [1,2,3], y = [4,5,6], io = IOBuffer()
    writedlm(io, zip(x,y), ",  ")
    seek(io, 0)
    @test readcsv(io) == [x y]
end

let x = [0.1 0.3 0.5], io = IOBuffer()
    writedlm(io, x, ", ")
    seek(io, 0)
    @test readstring(io) == "0.1, 0.3, 0.5\n"
end

let x = [0.1 0.3 0.5], io = IOBuffer()
    writedlm(io, x, ", ")
    seek(io, 0)
    @test readcsv(io) == [0.1 0.3 0.5]
end

let x = ["abc", "def\"ghi", "jk\nl"], y = [1, ",", "\"quoted\""], io = IOBuffer()
    writedlm(io, zip(x,y), ',')
    seek(io, 0)
    @test readcsv(io) == [x y]
end

let x = ["a" "b"; "d" ""], io = IOBuffer()
    writedlm(io, x)
    seek(io, 0)
    @test readdlm(io) == x
end

let x = ["\"hello\"", "world\""], io = IOBuffer()
    writedlm(io, x, quotes=false)
    @test takebuf_string(io) == "\"hello\"\nworld\"\n"

    writedlm(io, x)
    @test takebuf_string(io) == "\"\"\"hello\"\"\"\n\"world\"\"\"\n"
end

# test comments
@test isequaldlm(readcsv(IOBuffer("#this is comment\n1,2,3\n#one more comment\n4,5,6")), [1. 2. 3.;4. 5. 6.], Float64)
@test isequaldlm(readcsv(IOBuffer("#this is \n#comment\n1,2,3\n#one more \n#comment\n4,5,6")), [1. 2. 3.;4. 5. 6.], Float64)
@test isequaldlm(readcsv(IOBuffer("1,2,#3\n4,5,6")), [1. 2. "";4. 5. 6.], Any)
@test isequaldlm(readcsv(IOBuffer("1#,2,3\n4,5,6")), [1. "" "";4. 5. 6.], Any)
@test isequaldlm(readcsv(IOBuffer("1,2,\"#3\"\n4,5,6")), [1. 2. "#3";4. 5. 6.], Any)
@test isequaldlm(readcsv(IOBuffer("1,2,3\n #with leading whitespace\n4,5,6")), [1. 2. 3.;" " "" "";4. 5. 6.], Any)

# test skipstart
let x = ["a" "b" "c"; "d" "e" "f"; "g" "h" "i"; "A" "B" "C"; 1 2 3; 4 5 6; 7 8 9], io = IOBuffer()
    writedlm(io, x, quotes=false)
    seek(io, 0)
    (data, hdr) = readdlm(io, header=true, skipstart=3)
    @test data == [1 2 3; 4 5 6; 7 8 9]
    @test hdr == ["A" "B" "C"]
end
let x = ["a" "b" "\nc"; "d" "\ne" "f"; "g" "h" "i\n"; "A" "B" "C"; 1 2 3; 4 5 6; 7 8 9]
    io = IOBuffer()
    writedlm(io, x, quotes=true)
    seek(io, 0)
    (data, hdr) = readdlm(io, header=true, skipstart=6)
    @test data == [1 2 3; 4 5 6; 7 8 9]
    @test hdr == ["A" "B" "C"]

    io = IOBuffer()
    writedlm(io, x, quotes=false)
    seek(io, 0)
    (data, hdr) = readdlm(io, header=true, skipstart=6)
    @test data == [1 2 3; 4 5 6; 7 8 9]
    @test hdr == ["A" "B" "C"]
end

# source: http://www.i18nguy.com/unicode/unicode-example-utf8.zip
let i18n_data = ["Origin (English)", "Name (English)", "Origin (Native)", "Name (Native)",
        "Australia", "Nicole Kidman", "Australia", "Nicole Kidman",
        "Austria", "Johann Strauss", "Österreich", "Johann Strauß",
        "Belgium (Flemish)", "Rene Magritte", "België", "René Magritte",
        "Belgium (French)", "Rene Magritte", "Belgique", "René Magritte",
        "Belgium (German)", "Rene Magritte", "Belgien", "René Magritte",
        "Bhutan", "Gonpo Dorji", "འབྲུག་ཡུལ།", "མགོན་པོ་རྡོ་རྗེ།",
        "Canada", "Celine Dion", "Canada", "Céline Dion",
        "Canada - Nunavut (Inuktitut)", "Susan Aglukark", "ᓄᓇᕗᒻᒥᐅᑦ", "ᓱᓴᓐ ᐊᒡᓗᒃᑲᖅ",
        "Democratic People's Rep. of Korea", "LEE Sol-Hee", "조선 민주주의 인민 공화국", "이설희",
        "Denmark", "Soren Hauch-Fausboll", "Danmark", "Søren Hauch-Fausbøll",
        "Denmark", "Soren Kierkegaard", "Danmark", "Søren Kierkegård",
        "Egypt", "Abdel Halim Hafez", "ﻣﺼﺮ", "ﻋﺑﺪﺍﻠﺣﻟﻳﻢ ﺤﺎﻓﻅ",
        "Egypt", "Om Kolthoum", "ﻣﺼﺮ", "ﺃﻡ ﻛﻟﺛﻭﻡ",
        "Eritrea", "Berhane Zeray", "ብርሃነ ዘርኣይ", "ኤርትራ",
        "Ethiopia", "Haile Gebreselassie", "ኃይሌ ገብረሥላሴ", "ኢትዮጵያ",
        "France", "Gerard Depardieu", "France", "Gérard Depardieu",
        "France", "Jean Reno", "France", "Jean Réno",
        "France", "Camille Saint-Saens", "France", "Camille Saint-Saëns",
        "France", "Mylene Demongeot", "France", "Mylène Demongeot",
        "France", "Francois Truffaut", "France", "François Truffaut",
        "France (Braille)", "Louis Braille", "⠋⠗⠁⠝⠉⠑", "⠇⠕⠥⠊⠎⠀<BR>⠃⠗⠁⠊⠇⠇⠑",
        "Georgia", "Eduard Shevardnadze", "საქართველო", "ედუარდ შევარდნაძე",
        "Germany", "Rudi Voeller", "Deutschland", "Rudi Völler",
        "Germany", "Walter Schultheiss", "Deutschland", "Walter Schultheiß",
        "Greece", "Giorgos Dalaras", "Ελλάς", "Γιώργος Νταλάρας",
        "Iceland", "Bjork Gudmundsdottir", "Ísland", "Björk Guðmundsdóttir",
        "India (Hindi)", "Madhuri Dixit", "भारत", "माधुरी दिछित",
        "Ireland", "Sinead O'Connor", "Éire", "Sinéad O'Connor",
        "Israel", "Yehoram Gaon", "ישראל", "יהורם גאון",
        "Italy", "Fabrizio DeAndre", "Italia", "Fabrizio De André",
        "Japan", "KUBOTA Toshinobu", "日本", "久保田    利伸",
        "Japan", "HAYASHIBARA Megumi", "日本", "林原 めぐみ",
        "Japan", "Mori Ogai", "日本", "森鷗外",
        "Japan", "Tex Texin", "日本", "テクス テクサン",
        "Norway", "Tor Age Bringsvaerd", "Noreg", "Tor Åge Bringsværd",
        "Pakistan (Urdu)", "Nusrat Fatah Ali Khan", "پاکستان", "نصرت فتح علی خان",
        "People's Rep. of China", "ZHANG Ziyi", "中国", "章子怡",
        "People's Rep. of China", "WONG Faye", "中国", "王菲",
        "Poland", "Lech Walesa", "Polska", "Lech Wałęsa",
        "Puerto Rico", "Olga Tanon", "Puerto Rico", "Olga Tañón",
        "Rep. of China", "Hsu Chi", "臺灣", "舒淇",
        "Rep. of China", "Ang Lee", "臺灣", "李安",
        "Rep. of Korea", "AHN Sung-Gi", "한민국", "안성기",
        "Rep. of Korea", "SHIM Eun-Ha", "한민국", "심은하",
        "Russia", "Mikhail Gorbachev", "Россия", "Михаил Горбачёв",
        "Russia", "Boris Grebenshchikov", "Россия", "Борис Гребенщиков",
        "Slovenia", "\"Frane \"\"Jezek\"\" Milcinski", "Slovenija", "Frane Milčinski - Ježek",
        "Syracuse (Sicily)", "Archimedes", "Συρακούσα", "Ἀρχιμήδης",
        "Thailand", "Thongchai McIntai", "ประเทศไทย", "ธงไชย แม็คอินไตย์",
        "U.S.A.", "Brad Pitt", "U.S.A.", "Brad Pitt",
        "Yugoslavia (Cyrillic)", "Djordje Balasevic", "Југославија", "Ђорђе Балашевић",
        "Yugoslavia (Latin)", "Djordje Balasevic", "Jugoslavija", "Đorđe Balašević"]

    i18n_arr = transpose(reshape(i18n_data, 4, Int(floor(length(i18n_data)/4))))
    i18n_buff = PipeBuffer()
    writedlm(i18n_buff, i18n_arr, ',')
    @test i18n_arr == readcsv(i18n_buff)

    hdr = i18n_arr[1:1, :]
    data = i18n_arr[2:end, :]
    writedlm(i18n_buff, i18n_arr, ',')
    @test (data, hdr) == readcsv(i18n_buff, header=true)

    writedlm(i18n_buff, i18n_arr, '\t')
    @test (data, hdr) == readdlm(i18n_buff, '\t', header=true)
end

@test isequaldlm(readcsv(IOBuffer("1,22222222222222222222222222222222222222,0x3,10e6\n2000.1,true,false,-10.34"), Any),
    reshape(Any[1,2000.1,Float64(22222222222222222222222222222222222222),true,0x3,false,10e6,-10.34], 2, 4), Any)

@test isequaldlm(readcsv(IOBuffer("-9223355253176920979,9223355253176920979"), Int64), Int64[-9223355253176920979  9223355253176920979], Int64)

# fix #13028
for data in ["A B C", "A B C\n"]
    data,hdr = readdlm(IOBuffer(data), header=true)
    @test hdr == AbstractString["A" "B" "C"]
    @test data == Array{Float64}(0, 3)
end

# fix #13179 parsing unicode lines with default delmiters
@test isequaldlm(readdlm(IOBuffer("# Should ignore this π\n1\tα\n2\tβ\n")), Any[1 "α"; 2 "β"], Any)

# BigInt parser
let data = "1 2 3"
    readdlm(IOBuffer(data), ' ', BigInt) == BigInt[1 2 3]
end

# test show with MIME types
@test sprint(io -> show(io, "text/csv", [1 2; 3 4])) == "1,2\n3,4\n"

for writefunc in ((io,x) -> show(io, "text/csv", x),
                  (io,x) -> invoke(writedlm, (IO, Any, Any), io, x, ","))
    # iterable collections of iterable rows:
    let x = [(1,2), (3,4)], io = IOBuffer()
        writefunc(io, x)
        seek(io, 0)
        @test readcsv(io) == [1 2; 3 4]
    end
    # vectors of strings:
    let x = ["foo", "bar"], io = IOBuffer()
        writefunc(io, x)
        seek(io, 0)
        @test vec(readcsv(io)) == x
    end
end
