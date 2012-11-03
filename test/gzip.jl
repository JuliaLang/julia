# Testing for gzip
require("../extras/gzip")
#require("../extras/test")

using GZip

##########################
# test_context("GZip tests")
##########################

tmp = mktempdir()

test_infile = "$JULIA_HOME/../../extras/gzip.jl"
test_compressed = "$tmp/gzip.jl.gz"

@windows_only gunzip="gunzip.exe"
@unix_only    gunzip="gunzip"

test_gunzip = true
try
    # TODO: Update for Windows
    run(`which $gunzip` > "/dev/null")
catch
    test_gunzip = false
end

#########################
# test_group("Compress Test1: gzip.jl")
##########################

data = open(readall, test_infile);

gzfile = gzopen(test_compressed, "wb")
@assert write(gzfile, data) == length(data)
@assert close(gzfile) == Z_OK
@assert close(gzfile) != Z_OK

#@assert throws_exception(write(gzfile, data), GZError)
@assert_fails write(gzfile, data)

if test_gunzip
    data2 = readall(`$gunzip -c $test_compressed`)
    @assert data == data2
end

data3 = gzopen(readall, test_compressed)
@assert data == data3

# Test gzfdio
raw_file = open(test_compressed, "r")
gzfile = gzdopen(fd(raw_file), "r")
data4 = readall(gzfile)
close(gzfile)
close(raw_file)
@assert data == data4


# Screw up the file
raw_file = open(test_compressed, "r+")
seek(raw_file, 3) # leave the gzip magic 2-byte header
write(raw_file, zeros(Uint8, 10))
close(raw_file)

#@assert throws_exception(gzopen(readall, test_compressed), GZError)
@assert_fails gzopen(readall, test_compressed)


##########################
# test_group("gzip file function tests (writing)")
##########################
gzfile = gzopen(test_compressed, "wb")
write(gzfile, data) == length(data)
@assert flush(gzfile) == Z_OK

pos = position(gzfile)
@assert_fails seek(gzfile, 100)   # can't seek backwards on write
@assert position(gzfile) == pos
@assert skip(gzfile, 100)
@assert position(gzfile) == pos + 100

#@assert throws_exception(truncate(gzfile, 100), ErrorException)
#@assert throws_exception(seek_end(gzfile), ErrorException)
@assert_fails truncate(gzfile, 100)
@assert_fails seek_end(gzfile)

@assert close(gzfile) == Z_OK


##########################
# test_group("gzip file function tests (strategy read/write)")
##########################

# rewrite the test file
modes = "fhR "
if GZip.ZLIB_VERSION >= (1,2,5,2)
    modes = "fhRFT "
end
for ch in modes
    if ch == ' '
        ch = ""
    end
    for level = 0:9
        gzfile = gzopen(test_compressed, "wb$level$ch")
        @assert write(gzfile, data) == length(data)
        @assert close(gzfile) == Z_OK

        file_size = filesize(test_compressed)

        #println("wb$level$ch: ", file_size)

        if ch == 'T'
            @assert(file_size == length(data))
        elseif ch == 'F'
            @assert(file_size >= length(data))
        elseif level == 0
            @assert(file_size > length(data))
        else
            @assert(file_size < length(data))
        end

        # readline test
        gzf = gzopen(test_compressed)
        s = memio()
        while !eof(gzf)
            write(s, readline(gzf))
        end
        data2 = takebuf_string(s);

        # readuntil test
        seek(gzf, 0)
        while !eof(gzf)
            write(s, readuntil(gzf, 'a'))
        end
        data3 = takebuf_string(s);
        close(gzf)

        @assert(data == data2)
        @assert(data == data3)

    end
end

##########################
# test_group("gzip array/matrix tests (write/read)")
##########################

const BUFSIZE = 65536

for level = 0:3:6
    for T in [Int8,Uint8,Int16,Uint16,Int32,Uint32,Int64,Uint64,Int128,Uint128,
              Float32,Float64,Complex64,Complex128]

        minval = 34567
        try
            minval = min(typemax(T), 34567)
        catch
            # do nothing
        end

        # Ordered array
        b = zeros(T, BUFSIZE)
        if !isa(T, Complex)
            for i = 1:length(b)
                b[i] = (i-1)%minval;
            end
        else
            for i = 1:length(b)
                b[i] = (i-1)%minval - (minval-(i-1))%minval * im
            end
        end

        # Random array
        if isa(T, FloatingPoint)
            r = (T)[rand(BUFSIZE)...];
        elseif isa(T, Complex64)
            r = Int32[rand(BUFSIZE)...] + Int32[rand(BUFSIZE)...] * im
        elseif isa(T, Complex128)
            r = Int64[rand(BUFSIZE)...] + Int64[rand(BUFSIZE)...] * im
        else
            r = b[randi((1,BUFSIZE), BUFSIZE)];
        end

        # Array file
        b_array_fn = "$tmp/b_array.raw.gz"
        r_array_fn = "$tmp/r_array.raw.gz"

        gzaf_b = gzopen(b_array_fn, "w$level")
        write(gzaf_b, b)
        close(gzaf_b)

        #println("$T ($level) ordered: $(filesize(b_array_fn))")

        gzaf_r = gzopen(r_array_fn, "w$level")
        write(gzaf_r, r)
        close(gzaf_r)

        #println("$T ($level) random: $(filesize(r_array_fn))")

        b2 = zeros(T, BUFSIZE)
        r2 = zeros(T, BUFSIZE)

        b2_infile = gzopen(b_array_fn)
        read(b2_infile, b2);
        close(b2_infile)

        r2_infile = gzopen(r_array_fn)
        read(r2_infile, r2);
        close(r2_infile)

        @assert b == b2
        @assert r == r2
    end
end

##########################
# test_group("gzip unicode tests (write/read)")
##########################

unicode_gz_file = "$tmp/unicode_test.gz"

str1 = CharString(reinterpret(Char, read(open("unicode/UTF-32LE.unicode"), Uint32, 1112065)[2:]));
str2 = UTF8String(read(open("unicode/UTF-8.unicode"), Uint8, 4382595)[4:]);

UTF32LE_gz = gzopen(unicode_gz_file, "w")
write(UTF32LE_gz, str1)
close(UTF32LE_gz)

str1b = readall(`gunzip -c $unicode_gz_file`);
str1c = gzopen(readall, unicode_gz_file);
@assert str1 == str1b
@assert str1 == str1c

UTF8_gz = gzopen(unicode_gz_file, "w");
write(UTF8_gz, str2)
close(UTF8_gz)

str2b = readall(`gunzip -c $unicode_gz_file`);
str2c = gzopen(readall, unicode_gz_file);
@assert str2 == str2b
@assert str2 == str2c


run(`rm -Rf $tmp`)
