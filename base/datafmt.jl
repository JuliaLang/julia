## file formats ##

const invalid_dlm = char(0xfffffffe)

countlines(nameorfile) = countlines(nameorfile, '\n')
function countlines(filename::String, eol::Char)
    open(filename) do io
        countlines(io, eol)
    end
end
function countlines(io::IO, eol::Char)
    if !isascii(eol)
        error("only ASCII line terminators are supported")
    end
    a = Array(Uint8, 8192)
    nl = 0
    preceded_by_eol = true
    while !eof(io)
        nb = readbytes!(io, a)
        for i=1:nb
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

readdlm(input, T::Type; opts...) = readdlm(input, invalid_dlm, T, '\n'; opts...)
readdlm(input, dlm::Char, T::Type; opts...) = readdlm(input, dlm, T, '\n'; opts...)

readdlm(input; opts...) = readdlm(input, invalid_dlm, '\n'; opts...)
readdlm(input, dlm::Char; opts...) = readdlm(input, dlm, '\n'; opts...)

readdlm(input, dlm::Char, eol::Char; opts...) = readdlm_auto(input, dlm, Float64, eol, true; opts...)
readdlm(input, dlm::Char, T::Type, eol::Char; opts...) = readdlm_auto(input, dlm, T, eol, false; opts...)

function readdlm_auto(input, dlm::Char, T::Type, eol::Char, auto::Bool; opts...)
    optsd = val_opts(opts)
    isa(input, String) && (fsz = filesize(input); input = get(optsd, :use_mmap, true) && fsz < typemax(Int) ? as_mmap(input,fsz) : readall(input))
    sinp = isa(input, Vector{Uint8}) ? ccall(:jl_array_to_string, ByteString, (Array{Uint8,1},), input) :
           isa(input, IO) ? readall(input) :
           input
    readdlm_string(sinp, dlm, T, eol, auto, optsd)
end

function as_mmap(fname::String, fsz::Int64)
    open(fname) do io
        mmap_array(Uint8, (int(fsz),), io)
    end
end

function ascii_if_possible(sbuff::String)
    isa(sbuff, ASCIIString) && return sbuff

    asci = true
    d = sbuff.data
    for idx in 1:length(d)
        (d[idx] < 0x80) ? continue : (asci = false; break)
    end
    asci ? ASCIIString(sbuff.data) : sbuff
end

function readdlm_string(sbuff::String, dlm::Char, T::Type, eol::Char, auto::Bool, optsd::Dict)
    ign_empty = (dlm == invalid_dlm)

    nrows,ncols = try
            dlm_dims(sbuff, eol, dlm, ign_empty)
        catch ex
            !get(optsd, :ignore_invalid_chars, false) && throw(ex)
            sbuff = ascii_if_possible(convert(typeof(sbuff), sbuff.data, ""))
            dlm_dims(sbuff, eol, dlm, ign_empty)
        end
    begin_offsets = zeros(Int, nrows, ncols)
    end_offsets = zeros(Int, nrows, ncols)
    has_header = get(optsd, :has_header, false)
    cells = Array(T, has_header ? nrows-1 : nrows, ncols)
    dlm_offsets(sbuff, dlm, eol, begin_offsets, end_offsets, ign_empty)
    has_header ? (dlm_fill(cells, begin_offsets, end_offsets, sbuff, auto, 1, dlm, eol, ign_empty), dlm_fill(Array(String, 1, ncols), begin_offsets, end_offsets, sbuff, auto, 0, dlm, eol, ign_empty)) : dlm_fill(cells, begin_offsets, end_offsets, sbuff, auto, 0, dlm, eol, ign_empty)
end

const valid_opts = [:has_header, :ignore_invalid_chars, :use_mmap]
function val_opts(opts)
    d = Dict{Symbol,Bool}()
    for opt in opts
        !in(opt[1], valid_opts) && error("unknown option $(opt[1])")
        !isa(opt[2], Bool) && error("$(opt[1]) can only be boolean")
        d[opt[1]] = opt[2]
    end
    d
end

function dlm_fill{T}(cells::Array{T,2}, begin_offsets::Array{Int,2}, end_offsets::Array{Int,2}, sbuff::String, auto::Bool, row_offset::Int, dlm::Char, eol::Char, ign_adj_dlm::Bool)
    maxrow,maxcol = size(cells)
    tmp64 = Array(Float64,1)

    for row in (1+row_offset):(maxrow+row_offset)
        cell_row = row-row_offset
        for col in 1:maxcol
            start_pos = begin_offsets[row,col] 
            end_pos = end_offsets[row,col]

            if start_pos > 0 && end_pos > 0
                end_idx = prevind(sbuff, nextind(sbuff,end_pos))
                (end_idx > 0) && ('\n' == eol) && ('\r' == sbuff[end_idx]) && (end_idx = prevind(sbuff, end_idx))
                if ign_adj_dlm
                    is_default_dlm = (dlm == invalid_dlm)
                    while start_pos <= end_idx
                        val = sbuff[start_pos] 
                        (is_default_dlm ? !in(val, _default_delims) : (val != dlm)) && break
                        start_pos = nextind(sbuff, start_pos)
                    end
                end
                sval = SubString(sbuff, start_pos, end_idx)
            else
                sval = SubString(sbuff, 1, 0)
            end
            
            if T <: Char
                (length(sval) != 1) && error("file entry \"$(sval)\" is not a Char")
                cells[cell_row,col] = next(sval,1)[1]
            elseif T <: Number
                if float64_isvalid(sval, tmp64)
                    cells[cell_row,col] = tmp64[1]
                elseif auto
                    return dlm_fill(Array(Any,maxrow,maxcol), begin_offsets, end_offsets, sbuff, false, row_offset, dlm, eol, ign_adj_dlm)
                else
                    cells[cell_row,col] = NaN
                end
            elseif T <: String
                cells[cell_row,col] = sval
            elseif T == Any
                cells[cell_row,col] = float64_isvalid(sval, tmp64) ? tmp64[1] : sval
            else
                error("file entry \"$(sval)\" cannot be converted to $T")
            end
        end
    end
    cells
end


function dlm_offsets(sbuff::UTF8String, dlm, eol, begin_offsets::Array{Int,2}, end_offsets::Array{Int,2}, ign_adj_dlm::Bool)
    isascii(dlm) && isascii(eol) && (return dlm_offsets(sbuff.data, uint8(dlm), uint8(eol), begin_offsets, end_offsets, ign_adj_dlm))

    col = 0
    row = 1
    maxrow,maxcol = size(begin_offsets)
    idx = 1
    is_default_dlm = (dlm == invalid_dlm)
    got_data = false
    last_offset = 0
    slen = length(sbuff.data)
    while idx <= slen
        val,idx = next(sbuff, idx)
        (val != eol) && (is_default_dlm ? !in(val, _default_delims) : (val != dlm)) && (got_data = true) && continue
        if got_data || !ign_adj_dlm
            col += 1
            end_offsets[row,col] = idx-2
            begin_offsets[row,col] = last_offset+1
        end
        last_offset = idx
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
        got_data = false
    end
    if (last_offset < slen) && (col < maxcol)
        col += 1
        begin_offsets[row,col] = last_offset+1
        end_offsets[row,col] = slen
    end
end

dlm_offsets(sbuff::ASCIIString, dlmc, eolc, begin_offsets::Array{Int,2}, end_offsets::Array{Int,2}, ign_adj_dlm::Bool) = dlm_offsets(sbuff.data, uint8(dlmc), uint8(eolc), begin_offsets, end_offsets, ign_adj_dlm)
function dlm_offsets(dbuff::Vector{Uint8}, dlm::Uint8, eol::Uint8, begin_offsets::Array{Int,2}, end_offsets::Array{Int,2}, ign_adj_dlm::Bool)
    col = 0
    row = 1
    is_default_dlm = (dlm == uint8(invalid_dlm))
    maxrow,maxcol = size(begin_offsets)
    got_data = false
    last_offset = 0
    slen = length(dbuff)
    for idx in 1:slen
        val = dbuff[idx]
        (val != eol) && (is_default_dlm ? !in(val, _default_delims) : (val != dlm)) && (got_data = true) && continue
        if got_data || !ign_adj_dlm
            col += 1
            end_offsets[row,col] = idx-1
            begin_offsets[row,col] = last_offset+1
        end
        last_offset = idx
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
        got_data = false
    end
    if (last_offset < slen) && (col < maxcol)
        col += 1
        begin_offsets[row,col] = last_offset+1
        end_offsets[row,col] = slen
    end
end

dlm_dims(s::ASCIIString, eol::Char, dlm::Char, ign_adj_dlm::Bool) = dlm_dims(s.data, uint8(eol), uint8(dlm), ign_adj_dlm)
function dlm_dims{T,D}(dbuff::T, eol::D, dlm::D, ign_adj_dlm::Bool)
    isa(dbuff, UTF8String) && isascii(eol) && isascii(dlm) && (return dlm_dims(dbuff.data, uint8(eol), uint8(dlm), ign_adj_dlm))
    ncols = nrows = col = 0
    is_default_dlm = (dlm == convert(D, invalid_dlm))
    try
        got_data = false
        for val in dbuff
            (val != eol) && (is_default_dlm ? !in(val, _default_delims) : (val != dlm)) && (got_data = true) && continue
            (got_data || !ign_adj_dlm) && (col += 1)
            (val == eol) && (nrows += 1; ncols = max(ncols, col); col = 0)
            got_data = false
        end
    catch ex
        error("at row $nrows, column $col : $ex)")
    end
    if col > 0 
        nrows += 1
        ncols = max(ncols, col+1)
    end
    ncols = max(ncols, col, 1)
    nrows = max(nrows, 1)
    return (nrows, ncols)
end

readcsv(io; opts...)          = readdlm(io, ','; opts...)
readcsv(io, T::Type; opts...) = readdlm(io, ',', T; opts...)

# todo: keyword argument for # of digits to print
writedlm_cell(io::IO, elt::FloatingPoint) = print_shortest(io, elt)
writedlm_cell(io::IO, elt) = print(io, elt)
function writedlm(io::IO, a::AbstractVecOrMat, dlm)
    pb = PipeBuffer()
    nr = size(a,1)
    nc = size(a,2)
    for i = 1:nr
        for j = 1:nc
            writedlm_cell(pb, a[i,j])
            j == nc ? write(pb,'\n') : print(pb,dlm)
        end
        (nb_available(pb) > (16*1024)) && write(io, takebuf_array(pb))
    end
    write(io, takebuf_array(pb))
    nothing
end

writedlm{T}(io::IO, a::AbstractArray{T,0}, dlm) = writedlm(io, reshape(a,1), dlm)

function writedlm(io::IO, a::AbstractArray, dlm)
    tail = size(a)[3:end]
    function print_slice(idxs...)
        writedlm(io, sub(a, 1:size(a,1), 1:size(a,2), idxs...), dlm)
        if idxs != tail
            print("\n")
        end
    end
    cartesianmap(print_slice, tail)
end

function writedlm(io::IO, itr, dlm)
    pb = PipeBuffer()
    for row in itr
        state = start(row)
        while !done(row, state)
            (x, state) = next(row, state)
            writedlm_cell(pb, x)
            done(row, state) ? write(pb,'\n') : print(pb,dlm)
        end
        (nb_available(pb) > (16*1024)) && write(io, takebuf_array(pb))
    end
    write(io, takebuf_array(pb))
    nothing
end

function writedlm(fname::String, a, dlm)
    open(fname, "w") do io
        writedlm(io, a, dlm)
    end
end

writedlm(io, a) = writedlm(io, a, '\t')
writecsv(io, a) = writedlm(io, a, ',')

writemime(io::IO, ::MIME"text/csv", a::AbstractVecOrMat) = writedlm(io, a, ',')
writemime(io::IO, ::MIME"text/tab-separated-values", a::AbstractVecOrMat) = writedlm(io, a, '\t')
