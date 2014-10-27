export rawcontent, sloc, text, isbinary, lookup_blob

#TODO: move blob related methods from repository

Base.sizeof(b::GitBlob) = begin
    @assert b.ptr != C_NULL
    return api.git_blob_rawsize(b.ptr)::Int64
end

#TODO: it would be better to implement julia's file api's

function rawcontent(b::GitBlob, max_bytes=-1)
    @assert b.ptr != C_NULL
    data_ptr = api.git_blob_rawcontent(b.ptr)
    data_size = api.git_blob_rawsize(b.ptr)
    if data_ptr == C_NULL || max_bytes == 0
        return Array(Uint8, 0)
    end
    if max_bytes < 0 || max_bytes > data_size
        data_copy = Array(Uint8, data_size)
        unsafe_copy!(pointer(data_copy), data_ptr, data_size)
        return data_copy
    end
    if max_bytes > 0 && max_bytes < data_size
        data_copy = Array(Uint8, max_bytes)
        unsafe_copy!(pointer(data_copy), data_ptr, max_bytes)
        return data_copy
    end
end

Base.bytestring(b::GitBlob) = bytestring(rawcontent(b))

function sloc(b::GitBlob)
    @assert b.ptr != nothing
    data_ptr = api.git_blob_rawcontent(b.ptr)
    data_end = data_ptr + api.git_blob_rawsize(b.ptr)
    if data_ptr == data_end
        return 0
    end
    loc = 0
    while uint(data_ptr) < uint(data_end)
        val = unsafe_load(data_ptr)
        data_ptr += 1
        if val == uint8(10) #"\n"
            while uint(data_ptr) < uint(data_end) && isspace(char(unsafe_load(data_ptr)))
                data_ptr += 1
            end
            loc += 1
        end
    end
    if unsafe_load(data_ptr-1) != uint8(10) #"\n"
        loc += 1
    end
    return loc
end

function text(b::GitBlob, max_lines=-1)
    @assert b.ptr != C_NULL
    data_ptr = api.git_blob_rawcontent(b.ptr)
    if data_ptr == C_NULL || max_lines == 0
        return bytestring("")
    elseif max_lines < 0
        return bytestring(data_ptr)
    end
    lines, i = 0, 1
    data_size = api.git_blob_rawsize(b.ptr)
    while i <= data_size && lines < max_lines
        if unsafe_load(data_ptr, i) == uint8(10) # "\n"
            lines += 1
        end
        i += 1
    end
    data_size = i - 1
    data_copy = Array(Uint8, data_size)
    unsafe_copy!(pointer(data_copy), data_ptr, data_size)
    return UTF8String(data_copy) 
end

function isbinary(b::GitBlob)
    @assert b.ptr != C_NULL
    res = api.git_blob_is_binary(b.ptr)
    return bool(res)
end
