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
    nrows,ncols = try
            dlm_dims(sbuff, eol, dlm)
        catch ex
            !get(optsd, :ignore_invalid_chars, false) && throw(ex)
            sbuff = ascii_if_possible(convert(typeof(sbuff), sbuff.data, ""))
            dlm_dims(sbuff, eol, dlm)
        end
    offsets = zeros(Int, nrows, ncols)
    has_header = get(optsd, :has_header, false)
    cells = Array(T, has_header ? nrows-1 : nrows, ncols)
    dlm_offsets(sbuff, dlm, eol, offsets)
    has_header ? (dlm_fill(cells, offsets, sbuff, auto, 1, eol), dlm_fill(Array(String, 1, ncols), offsets, sbuff, auto, 0, eol)) : dlm_fill(cells, offsets, sbuff, auto, 0, eol)
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

function dlm_col_begin(ncols::Int, offsets::Array{Int,2}, row::Int, col::Int)
    (row == 1) && (col == 1) && return 1
    pp_row = (1 == col) ? (row-1) : row
    pp_col = (1 == col) ? ncols : (col-1)

    ret = offsets[pp_row, pp_col]
    (ret == 0) ? dlm_col_begin(ncols, offsets, pp_row, pp_col) : (ret+2)
end

function dlm_fill{T}(cells::Array{T,2}, offsets::Array{Int,2}, sbuff::String, auto::Bool, row_offset::Int, eol::Char)
    maxrow,maxcol = size(cells)
    tmp64 = Array(Float64,1)

    for row in (1+row_offset):(maxrow+row_offset)
        cell_row = row-row_offset
        for col in 1:maxcol
            start_pos = dlm_col_begin(maxcol, offsets, row, col)
            end_pos = offsets[row,col]
            
            end_idx = prevind(sbuff, nextind(sbuff,end_pos))
            (col == maxcol) && ('\n' == eol) && ('\r' == sbuff[end_idx]) && (end_idx = prevind(sbuff, end_idx))
            sval = SubString(sbuff, start_pos, end_idx)
            
            if T <: Char
                (length(sval) != 1) && error("file entry \"$(sval)\" is not a Char")
                cells[cell_row,col] = next(sval,1)[1]
            elseif T <: Number
                if float64_isvalid(sval, tmp64)
                    cells[cell_row,col] = tmp64[1]
                elseif auto
                    return dlm_fill(Array(Any,maxrow,maxcol), offsets, sbuff, false, row_offset, eol)
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


function dlm_offsets(sbuff::UTF8String, dlm, eol, offsets::Array{Int,2})
    isascii(dlm) && isascii(eol) && (return dlm_offsets(sbuff.data, uint8(dlm), uint8(eol), offsets))

    col = 0
    row = 1
    maxrow,maxcol = size(offsets)
    offsets[maxrow,maxcol] = length(sbuff.data)
    idx = 1
    while(idx <= length(sbuff.data))
        val,idx = next(sbuff, idx)
        (val != eol) && ((dlm == invalid_dlm) ? !in(val, _default_delims) : (val != dlm)) && continue
        col += 1
        offsets[row,col] = idx-2
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
    end
end

dlm_offsets(sbuff::ASCIIString, dlmc, eolc, offsets::Array{Int,2}) = dlm_offsets(sbuff.data, uint8(dlmc), uint8(eolc), offsets)
function dlm_offsets(dbuff::Vector{Uint8}, dlm::Uint8, eol::Uint8, offsets::Array{Int,2})
    col = 0
    row = 1
    maxrow,maxcol = size(offsets)
    offsets[maxrow,maxcol] = length(dbuff)
    for idx in 1:length(dbuff)
        val = dbuff[idx]
        (val != eol) && ((dlm == invalid_dlm) ? !in(val, _default_delims) : (val != dlm)) && continue
        col += 1
        offsets[row,col] = idx-1
        (row >= maxrow) && (col == maxcol) && break
        (val == eol) && (row += 1; col = 0)
    end
end

dlm_dims(s::ASCIIString, eol::Char, dlm::Char) = dlm_dims(s.data, uint8(eol), uint8(dlm))
function dlm_dims{T,D}(dbuff::T, eol::D, dlm::D)
    isa(dbuff, UTF8String) && isascii(eol) && isascii(dlm) && (return dlm_dims(dbuff.data, uint8(eol), uint8(dlm)))
    ncols = nrows = col = 0
    try
        for val in dbuff
            (val != eol) && ((dlm == invalid_dlm) ? !in(val, _default_delims) : (val != dlm)) && continue
            col += 1
            (val == eol) && (nrows += 1; ncols = max(ncols, col); col = 0)
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
function writedlm(io::IO, a::Union(AbstractMatrix,AbstractVector), dlm::Char)
    pb = PipeBuffer()
    nr = size(a,1)
    nc = size(a,2)
    for i = 1:nr
        for j = 1:nc
            writedlm_cell(pb, a[i,j])
            write(pb, (j == nc) ? '\n' : dlm)
        end
        (nb_available(pb) > (16*1024)) && write(io, takebuf_array(pb))
    end
    write(io, takebuf_array(pb))
    nothing
end

writedlm{T}(io::IO, a::AbstractArray{T,0}, dlm::Char) = writedlm(io, reshape(a,1), dlm)

function writedlm(io::IO, a::AbstractArray, dlm::Char)
    tail = size(a)[3:end]
    function print_slice(idxs...)
        writedlm(io, sub(a, 1:size(a,1), 1:size(a,2), idxs...), dlm)
        if idxs != tail
            print("\n")
        end
    end
    cartesianmap(print_slice, tail)
end

function writedlm(fname::String, a, dlm::Char)
    open(fname, "w") do io
        writedlm(io, a, dlm)
    end
end

writedlm(io, a) = writedlm(io, a, '\t')
writecsv(io, a) = writedlm(io, a, ',')

writemime(io::IO, ::MIME"text/csv", a::Union(AbstractVector,AbstractMatrix)) = writedlm(io, a, ',')
writemime(io::IO, ::MIME"text/tab-separated-values", a::Union(AbstractVector,AbstractMatrix)) = writedlm(io, a, '\t')
