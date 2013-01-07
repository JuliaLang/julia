module SProfile

# Initialize the profile data structures
# Have the timer fire every 1ms = 10^6ns
# Use a max size of 1M profile samples
ccall(:profile_init, Void, (Uint64, Uint), 1_000_000, 1_000_000)

sprofile_start_timer() = ccall(:profile_start_timer, Void, ())

sprofile_stop_timer() = ccall(:profile_stop_timer, Void, ())

sprofile_get() = pointer_to_array(
    convert(Ptr{Uint}, ccall(:profile_get_data, Ptr{Uint8}, ())), (convert(Int, ccall(:profile_len_data, Int32, ())),))

sprofile_clear() = ccall(:clear_profile_data, Void, ())

# A simple linecount parser
function sprofile_flat()
    data = sprofile_get()
    linecount = (Uint=>Int)[]
    for i = 1:length(data)
        if data[i] == 0
            continue
        end
        linecount[data[i]] = get(linecount, data[i], 0)+1
    end
    buf = Array(Uint, 0)
    n = Array(Int, 0)
    for (k,v) in linecount
        push(buf, k)
        push(n, v)
    end
    bt = Array(Any, length(buf))
    for i = 1:length(buf)
        bt[i] = ccall(:jl_parse_backtrace, Array{Any, 1}, (Uint, Ptr{Uint}), 1, buf[i:i])
    end
    # Keep only the ones that point to Julia functions
    keep = !Bool[isempty(x) for x in bt]
    n = n[keep]
    bt = bt[keep]
    # Sort
    comb = Array(ASCIIString, length(n))
    for i = 1:length(n)
        comb[i] = @sprintf("%s:%s:%06d", bt[i][2], bt[i][1], bt[i][3])
    end
    scomb, p = sortperm(comb)
    n = n[p]
    bt = bt[p]
    @printf("%6s %20s %30s %6s\n", "Count", "File", "Function", "Line")
    for i = 1:length(n)
        @printf("%6d %20s %30s %6d\n", n[i], truncto(string(bt[i][2]), 20), bt[i][1], bt[i][3])
    end
end

macro sprofile(ex)
    quote
        try
            sprofile_start_timer()
            $(esc(ex))
        finally
            sprofile_stop_timer()
        end
    end
end

function truncto(str::ASCIIString, w::Int)
    ret = str;
    if strlen(str) > w
        ret = strcat("...", str[end-16:end])
    end
    ret
end

export @sprofile, sprofile_flat, sprofile_clear

end # module