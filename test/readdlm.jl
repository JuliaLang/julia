let dlm_data = readdlm(joinpath("perf", "kernel", "imdb-1.tsv"), '\t')
    @test size(dlm_data) == (31383,3)
    @test dlm_data[12345,2] == "Gladiator"
    @test dlm_data[31383,3] == 2005
    @test dlm_data[1,1] == "McClure, Marc (I)"
end

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

@test size(readdlm(IOBuffer("1 2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3"))) == (2,4)
@test size(readdlm(IOBuffer("1\t 2 3 4\n1 2 3\n"))) == (2,4)
@test size(readdlm(IOBuffer("1,,2,3,4\n1,2,3\n"), ',')) == (2,5)

let result1 = reshape({"", "", "", "", "", "", 1.0, 1.0, "", "", "", "", "", 1.0, 2.0, "", 3.0, "", "", "", "", "", 4.0, "", "", ""}, 2, 13),
    result2 = reshape({1.0, 1.0, 2.0, 1.0, 3.0, "", 4.0, ""}, 2, 4)

    @test isequal(readdlm(IOBuffer(",,,1,,,,2,3,,,4,\n,,,1,,,1\n"), ','), result1)
    @test isequal(readdlm(IOBuffer("   1    2 3   4 \n   1   1\n")), result2)
    @test isequal(readdlm(IOBuffer("   1    2 3   4 \n   1   1\n"), ' '), result1)
    @test isequal(readdlm(IOBuffer("1 2\n3 4 \n")), [[1.0, 3.0] [2.0, 4.0]])
end

let result1 = reshape({"", "", "", "", "", "", "भारत", 1.0, "", "", "", "", "", 1.0, 2.0, "", 3.0, "", "", "", "", "", 4.0, "", "", ""}, 2, 13)
    @test isequal(readdlm(IOBuffer(",,,भारत,,,,2,3,,,4,\n,,,1,,,1\n"), ',') , result1)
end

let result1 = reshape({1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, ""}, 2, 4)
    @test isequal(readdlm(IOBuffer("1\t 2 3 4\n1 2 3")), result1)
    @test isequal(readdlm(IOBuffer("1\t 2 3 4\n1 2 3 ")), result1)
    @test isequal(readdlm(IOBuffer("1\t 2 3 4\n1 2 3\n")), result1)
    @test isequal(readdlm(IOBuffer("1,2,3,4\n1,2,3\n"), ','), result1)
    @test isequal(readdlm(IOBuffer("1,2,3,4\n1,2,3"), ','), result1)
    @test isequal(readdlm(IOBuffer("1,2,3,4\r\n1,2,3\r\n"), ','), result1)
end

let result1 = reshape({"abc", "hello", "def,ghi", " \"quote\" ", "new\nline", "world"}, 2, 3),
    result2 = reshape({"abc", "line\"", "\"hello\"", "\"def", "", "\" \"\"quote\"\" \"", "ghi\"", "", "world", "\"new", "", ""}, 3, 4)

    @test isequal(readdlm(IOBuffer("abc,\"def,ghi\",\"new\nline\"\n\"hello\",\" \"\"quote\"\" \",world"), ','), result1)
    @test isequal(readdlm(IOBuffer("abc,\"def,ghi\",\"new\nline\"\n\"hello\",\" \"\"quote\"\" \",world"), ',', quotes=false), result2)
end

let result1 = reshape({"t", "c", "", "c"}, 2, 2),
    result2 = reshape({"t", "\"c", "t", "c"}, 2, 2)
    @test isequal(readdlm(IOBuffer("t  \n\"c\" c")), result1)
    @test isequal(readdlm(IOBuffer("t t \n\"\"\"c\" c")), result2)
end

@test isequal(readcsv(IOBuffer("\n1,2,3\n4,5,6\n\n\n")), reshape({"",1.0,4.0,"","","",2.0,5.0,"","","",3.0,6.0,"",""}, 5, 3))

let x = [1,2,3], y = [4,5,6], io = IOBuffer()
    writedlm(io, zip(x,y), ",  ")
    seek(io, 0)
    @test readcsv(io) == [x y]
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
    print(io, x)
    @assert takebuf_string(io) == "\"hello\"\nworld\"\n"

    writedlm(io, x)
    @assert takebuf_string(io) == "\"\"\"hello\"\"\"\n\"world\"\"\"\n"
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

    i18n_arr = transpose(reshape(i18n_data, 4, int(floor(length(i18n_data)/4))))
    i18n_buff = PipeBuffer()
    writedlm(i18n_buff, i18n_arr, ',')
    @test i18n_arr == readcsv(i18n_buff)
end
