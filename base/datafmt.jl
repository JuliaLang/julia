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
function readdlm(a, io, dlm, nr, nc, row, eol)
    for i=1:nr
        a[i,:] = row
        if i < nr
            row = dlm_readrow(io, dlm, eol)
        end
    end
    a
end

# all numeric, with NaN for invalid data
function readdlm{T<:Number}(a::Array{T}, io, dlm, nr, nc, row, eol)
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
readdlm(a::Array{Any}, io, dlm, nr, nc, row, eol) =
    readdlm(a, io, dlm, nr, nc, row, eol, 1, 1)
function readdlm(a::Array{Any}, io, dlm, nr, nc, row, eol, i0, j0)
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
function readdlm_auto(a, io, dlm, nr, nc, row, eol)
    tmp = Array(Float64, 1)
    for i=1:nr
        for j=1:nc
            el = row[j]
            if !float64_isvalid(el, tmp)
                a = convert(Array{Any,2}, a)
                readdlm(a, io, dlm, nr, nc, row, eol, i, j)
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

countlines(nameorfile) = countlines(nameorfile, '\n')
function countlines(filename::String, eol::Char)
    open(filename) do io
        countlines(io, eol)
    end
end
function countlines(io::IO, eol::Char)
    if !isascii(eol)
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

function readdlm_setup(fname::String, dlm, eol)
    if length(dlm) == 0
        error("readdlm: no separator characters specified")
    end
    nr = countlines(fname,eol)
    io = open(fname)
    row = dlm_readrow(io, dlm, eol)
    nc = length(row)
    return (io, nr, nc, row)
end

readdlm(fname::String, T::Type) = readdlm(fname, invalid_dlm, T, '\n')

readdlm(fname::String, dlm, T::Type) = readdlm(fname, dlm, T, '\n')

function readdlm(fname::String, dlm, T::Type, eol::Char)
    (io, nr, nc, row) = readdlm_setup(fname, dlm, eol)
    a = Array(T, nr, nc)
    readdlm(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

readdlm(fname::String) = readdlm(fname, invalid_dlm, '\n')
readdlm(fname::String, dlm) = readdlm(fname, dlm, '\n')

function readdlm(fname::String, dlm, eol::Char)
    (io, nr, nc, row) = readdlm_setup(fname, dlm, eol)
    a = Array(Float64, nr, nc)
    a = readdlm_auto(a, io, dlm, nr, nc, row, eol)
    close(io)
    return a
end

readcsv(io)          = readdlm(io, ',')
readcsv(io, T::Type) = readdlm(io, ',', T)

# todo: keyword argument for # of digits to print
function writedlm(io::IO, a::Matrix, dlm::Char)
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

writedlm(io::IO, a::Vector, dlm::Char) = writedlm(io, reshape(a,length(a),1), dlm)

function writedlm(fname::String, a::Matrix, dlm::Char)
    open(fname, "w") do io
        writedlm(io, a, dlm)
    end
end

writedlm(io, a) = writedlm(io, a, '\t')
writecsv(io, a) = writedlm(io, a, ',')
