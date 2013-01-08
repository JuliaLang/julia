## file formats ##

const invalid_dlm = char(0xfffffffe)

function dlm_readrow(io::IO, dlm, eol::Char)
    row_string = readuntil(io, eol)
    while length(row_string)==1 && row_string[1] == eol
        row_string = readuntil(io, eol)
    end
    if dlm == invalid_dlm
        row = split(row_string)
    else
        row = split(row_string, dlm, true)
    end
    if ends_with(row[end], eol)
        row[end] = chop(row[end])
    end
    row
end

# all strings
function dlmread(a, io, dlm, nr, nc, row, eol)
    for i=1:nr
        a[i,:] = row
        if i < nr
            row = dlm_readrow(io, dlm, eol)
        end
    end
    a
end

# all numeric, with NaN for invalid data
function dlmread{T<:Number}(a::Array{T}, io, dlm, nr, nc, row, eol)
    tmp = Array(Float64,1)
    for i=1:nr
        for j=1:nc
            if float64_isvalid(row[j], tmp)
                a[i,j] = tmp[1]
            else
                a[i,j] = NaN
            end
        end
        if i < nr
            row = dlm_readrow(io, dlm, eol)
        end
    end
end

# float64 or string
dlmread(a::Array{Any}, io, dlm, nr, nc, row, eol) =
    dlmread(a, io, dlm, nr, nc, row, eol, 1, 1)
function dlmread(a::Array{Any}, io, dlm, nr, nc, row, eol, i0, j0)
    tmp = Array(Float64,1)
    j = j0
    for i=i0:nr
        while j <= nc
            el = row[j]
            if float64_isvalid(el, tmp)
                a[i,j] = tmp[1]
            else
                a[i,j] = el
            end
            j += 1
        end
        j = 1
        if i < nr
            row = dlm_readrow(io, dlm, eol)
        end
    end
    a
end

# float64 or cell depending on data
function dlmread_auto(a, io, dlm, nr, nc, row, eol)
    tmp = Array(Float64, 1)
    for i=1:nr
        for j=1:nc
            el = row[j]
            if !float64_isvalid(el, tmp)
                a = convert(Array{Any,2}, a)
                dlmread(a, io, dlm, nr, nc, row, eol, i, j)
                return a
            else
                a[i,j] = tmp[1]
            end
        end
        if i < nr
            row = dlm_readrow(io, dlm, eol)
        end
    end
    a
end

countlines(io) = countlines(io, '\n')
function countlines(filename::String, eol::Char)
    open(filename) do io
        countlines(io, eol)
    end
end
function countlines(io::IOStream, eol::Char)
    if !iswascii(eol)
        error("countlines: only ASCII line terminators supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    preceded_by_eol = true
    while !eof(io)
        fill!(a, uint8(eol))
        try
            read(io, a)
        end
        for i=1:length(a)
            if a[i] == eol
                preceded_by_eol = true
            elseif preceded_by_eol
                preceded_by_eol = false
                nl+=1
            end
        end
    end
    nl
end

function dlmread_setup(fname::String, dlm, eol)
    if length(dlm) == 0
        error("dlmread: no separator characters specified")
    end
    nr = countlines(fname,eol)
    io = open(fname)
    row = dlm_readrow(io, dlm, eol)
    nc = length(row)
    return (io, nr, nc, row)
end

dlmread(fname::String, T::Type) = dlmread(fname, invalid_dlm, T, '\n')

dlmread(fname::String, dlm, T::Type) = dlmread(fname, dlm, T, '\n')

function dlmread(fname::String, dlm, T::Type, eol::Char)
    (io, nr, nc, row) = dlmread_setup(fname, dlm, eol)
    a = Array(T, nr, nc)
    dlmread(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

dlmread(fname::String) = dlmread(fname, invalid_dlm, '\n')
dlmread(fname::String, dlm) = dlmread(fname, dlm, '\n')

function dlmread(fname::String, dlm, eol::Char)
    (io, nr, nc, row) = dlmread_setup(fname, dlm, eol)
    a = Array(Float64, nr, nc)
    a = dlmread_auto(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

# todo: keyword argument for # of digits to print
function dlmwrite(io, a::Matrix, dlm::Char)
    nr, nc = size(a)
    for i = 1:nr
        for j = 1:nc
            elt = a[i,j]
            if isa(elt,FloatingPoint)
                print_shortest(io, elt)
            else
                print(io, elt)
            end
            if j < nc
                write(io, dlm)
            end
        end
        write(io, '\n')
    end
    nothing
end

dlmwrite(io, a::Vector, dlm::Char) = dlmwrite(io, reshape(a,length(a),1), dlm)

function dlmwrite(fname::String, a::Matrix, dlm::Char)
    open(fname, "w") do io
        dlmwrite(io, a, dlm)
    end
end

dlmwrite(io, a) = dlmwrite(io, a, '\t')

# CSV input

# String buffering
const NULLSET = Set()
let extract_cache = IOString()
    global extract_string
    function extract_string(this, left::Int, right::Int, omitlist::Set)
        seek(extract_cache, 0)
        if length(this) >= 1
            while isvalid(this, right) && right > left && this[right] == ' '
                right -= 1
            end
            i = left
            while i <= right
                lasti = i
                ch, i = next(this, i)
                if !has(omitlist, lasti)
                    print(extract_cache, ch)
                end
            end
            return takebuf_string(extract_cache)
        else
            return ""
        end
    end
end
extract_string(this, left::Int, right::Int) = extract_string(this, left, right, NULLSET)

# State machine for reading a single line of CSV
const STATE_EXPECTING_VALUE = 0
const STATE_IN_BARE = 1
const STATE_IN_QUOTED = 2
const STATE_POSSIBLE_EOQUOTED = 3
const STATE_EXPECTING_SEP = 4

# Read one line of delimited text
# This is complex because delimited text can contain EOL inside quoted fields
function read_separated_line(io,
                             separator::Char,
                             quotation_character::Char)
    # Indexes into the current line for the current item
    left = 0
    right = 0

    # Was using RopeString for efficient appends, but rare case and makes
    # UTF-8 processing harder
    this = Base.chomp!(readline(io))

    # Short-circuit on the empty line
    if this == ""
      return Array(UTF8String, 0)
    end

    # 5-state machine. See list of possible states above
    state = STATE_EXPECTING_VALUE

    # Index of characters to remove
    omitlist = Set()

    # Where are we
    i = start(this)
    eol = false

    # Will eventually return a Vector of strings
    num_elems = 0
    ret = Array(ByteString, 0)

    # off we go! use manual loops because this can grow
    while true
        eol = done(this, i)
        if !eol
            this_i = i
            this_char, i = next(this, i)
        end
        if state == STATE_EXPECTING_VALUE
            if eol
                num_elems += 1
                push(ret, "")
                break
            elseif this_char == ' '
                continue
            elseif this_char == separator
                num_elems += 1
                push(ret, "")
            elseif this_char == quotation_character
                left = this_i + 1
                state = STATE_IN_QUOTED
            else
                left = this_i
                state = STATE_IN_BARE
            end
        elseif state == STATE_IN_BARE
            if eol
                right = this_i
                num_elems += 1
                push(ret, extract_string(this, left, right))
                break
            elseif this_char == separator
                right = this_i - 1
                num_elems += 1
                push(ret, extract_string(this, left, right))
                state = STATE_EXPECTING_VALUE
            else
                continue
            end
        elseif state == STATE_IN_QUOTED
            if eol
                this = strcat(this, "\n", Base.chomp!(readline(io)))
            elseif this_char == quotation_character
                state = STATE_POSSIBLE_EOQUOTED
            else
                continue
            end
        elseif state == STATE_POSSIBLE_EOQUOTED
            if eol
                right = this_i - 1
                num_elems += 1
                push(ret, extract_string(this, left, right, omitlist))
                break
            elseif this_char == quotation_character
                add(omitlist, this_i)
                state = STATE_IN_QUOTED
            elseif this_char == separator
                right = this_i - 2
                num_elems += 1
                push(ret, extract_string(this, left, right, omitlist))
                del_all(omitlist)
                state = STATE_EXPECTING_VALUE
            elseif this_char == ' '
                right = this_i - 2
                num_elems += 1
                push(ret, extract_string(this, left, right, omitlist))
                del_all(omitlist)
                state = STATE_EXPECTING_SEP
            else
                error("unexpected character after a quote")
            end
        elseif state == STATE_EXPECTING_SEP
            if eol
                break
            elseif this_char == ' '
                continue
            elseif this_char == separator
                state = STATE_EXPECTING_VALUE
            else
                error("expecting a separator but got something else")
            end
        end
    end
    ret
end

# Read CSV data line-by-line
function read_separated_text(io::IOStream,
                             nrows::Int,
                             separator::Char,
                             quotation_character::Char)
    # Read one line to determine the number of columns
    i = 1
    sp = read_separated_line(io, separator, quotation_character)
    ncols = length(sp)

    # If the line is blank, return a 0x0 array to signify this
    if ncols == 0
        return Array(UTF8String, 0, 0)
    end

    # Otherwise, allocate an array to store all of the text we'll read
    text_data = Array(UTF8String, nrows, ncols)
    text_data[i, :] = sp

    # Loop until we've read nrows of text or run out of text
    while i < nrows
        sp = read_separated_line(io, separator, quotation_character)
        if length(sp) == ncols
            i += 1
            text_data[i, :] = sp
        else
            break
        end
    end

    # Return as much text as we read
    return text_data[1:i, :]
end

function csvread(filename::String, separator::Char, quotation_character::Char)
    nrows = countlines(filename)
    io = open(filename, "r")
    res = read_separated_text(io, nrows, separator, quotation_character)
    close(io)
    return res
end
csvread(filename::String) = csvread(filename, ',', '"')
const readcsv = csvread

# CSV output

# Quotation rules
# * Quote all string fields
# * Don't quote real-valued fields
# * Quote non-string, non-real-valued fields
function in_quotes(val::String, quotation_character::Char)
  strcat(quotation_character, val, quotation_character)
end
function in_quotes(val::Real, quotation_character::Char)
  string(val)
end
function in_quotes(val::Any, quotation_character::Char)
  strcat(quotation_character, string(val), quotation_character)
end

# csvwrite()
function csvwrite(io::IOStream, a::Matrix,
                  separator::Char, quotation_character::Char)
  n, p = size(a)
  for i in 1:n
    for j in 1:p
      if j < p
        print(io, in_quotes(a[i, j], quotation_character))
        print(io, separator)
      else
        println(io, in_quotes(a[i, j], quotation_character))
      end
    end
  end
end
function csvwrite(filename::String, a::Matrix,
                  separator::Char, quotation_character::Char)
    io = open(filename, "w")
    csvwrite(io, a, separator, quotation_character)
    close(io)
end
csvwrite(io::IOStream, a::Matrix) = csvwrite(io, a, ',', '"')
csvwrite(filename::String, a::Matrix) = csvwrite(filename, a, ',', '"')
const writecsv = csvwrite
