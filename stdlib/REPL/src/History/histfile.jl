# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    REPL_DATE_FORMAT

The `DateFormat` used to parse and format timestamps in the REPL history file.
"""
const REPL_DATE_FORMAT = dateformat"yyyy-mm-dd HH:MM:SS"

const HIST_OPEN_FLAGS =
    Base.Filesystem.JL_O_APPEND |
    Base.Filesystem.JL_O_RDWR |
    Base.Filesystem.JL_O_CREAT |
    Base.Filesystem.JL_O_CLOEXEC

struct HistEntry
    mode::Symbol
    date::DateTime
    # cwd::String
    content::String
    # resulttype::String
    # session::UInt64
    index::UInt32
    # sindex::UInt16
    # error::Bool
end

"""
    HistoryFile(path::String) -> HistoryFile

Create a handle to the history file at `path`, and store the `HistEntry` records.

See also: `update!(::HistoryFile)`.
"""
struct HistoryFile <: AbstractVector{HistEntry}
    path::String
    file::Base.Filesystem.File
    lock::ReentrantLock
    records::Vector{HistEntry}
end

HistoryFile(path::String) = HistoryFile(
    path, Base.Filesystem.open(path, HIST_OPEN_FLAGS, 0o640), ReentrantLock(), [])

function HistoryFile()
    nofile = Base.Filesystem.File(Base.Filesystem.INVALID_OS_HANDLE)
    nofile.open = false
    HistoryFile("", nofile, ReentrantLock(), [])
end

Base.lock(hist::HistoryFile) = lock(hist.lock)
Base.trylock(hist::HistoryFile) = trylock(hist.lock)
Base.unlock(hist::HistoryFile) = unlock(hist.lock)

Base.size(hist::HistoryFile) = @lock hist (length(hist.records),)
Base.getindex(hist::HistoryFile, i::Int) = hist.records[i]

function ensureopen(hist::HistoryFile)
    isopen(hist.file) && return true
    isempty(hist.path) && return false
    try
        lock(hist)
        newfile = Base.Filesystem.open(hist.path, HIST_OPEN_FLAGS, 0o640)
        newfile.open || return false
        hist.file.handle = newfile.handle
        hist.file.open = true
    finally
        unlock(hist)
    end
end

Base.close(hist::HistoryFile) = close(hist.file)

"""
    update!(hist::HistoryFile) -> HistoryFile

Read any new entries from the history file and record them as `HistEntry`s.

Malformed entries are skipped, and if the last entry is incomplete the IO
position will be reset to the start of the entry.
"""
function update!(hist::HistoryFile)
    (; file, records) = hist
    # If the file has grown since the last read,
    # we need to trigger a synchronisation of the
    # stream state. This can be done with `fseek`,
    # but that can't easily be called from Julia.
    # Instead, we can use `filesize` to detect when
    # we need to do this, and then use `peek` to
    # trigger the synchronisation. This relies on
    # undocumented implementation details, but
    # there's not much to be done about that.
    ensureopen(hist) || return hist
    offset = position(file)
    offset == filesize(file) && return hist
    try
        lock(hist)
        bytes = if iszero(offset)
            @static if Sys.iswindows()
                mmap(open(hist.path, "r"))
            else
                mmap(file)
            end
        else
            read(file)
        end
        function findnext(data::Vector{UInt8}, index::Int, byte::UInt8, limit::Int = length(data))
            for i in index:limit
                data[i] == byte && return i
            end
            limit
        end
        function isstrmatch(data::Vector{UInt8}, at::Int, str::String)
            at + ncodeunits(str) <= length(data) || return false
            for (i, byte) in enumerate(codeunits(str))
                data[at + i - 1] == byte || return false
            end
            true
        end
        histindex = if isempty(hist.records)
            0
        else
            hist.records[end].index
        end
        pos = firstindex(bytes)
        while true
            pos >= length(bytes) && break
            entrystart = pos
            if bytes[pos] != UInt8('#')
                pos = findnext(bytes, pos, UInt8('\n')) + 1
                continue
            end
            time, mode = zero(DateTime), :julia
            while pos < length(bytes) && bytes[pos] == UInt8('#')
                pos += 1
                while pos < length(bytes) && bytes[pos] == UInt8(' ')
                    pos += 1
                end
                metastart = pos
                metaend = findnext(bytes, pos, UInt8(':'))
                pos = metaend + 1
                while pos < length(bytes) && bytes[pos] == UInt8(' ')
                    pos += 1
                end
                valstart = pos
                valend = findnext(bytes, pos, UInt8('\n'))
                pos = valend + 1
                if isstrmatch(bytes, metastart, "mode:")
                    mode = if isstrmatch(bytes, valstart, "julia") && bytes[valstart + ncodeunits("julia")] ∈ (UInt8('\n'), UInt8('\r'))
                        :julia
                    elseif isstrmatch(bytes, valstart, "help") && bytes[valstart + ncodeunits("help")] ∈ (UInt8('\n'), UInt8('\r'))
                        :help
                    else
                        Symbol(bytes[valstart:valend-1])
                    end
                elseif isstrmatch(bytes, metastart, "time:")
                    valend = min(valend, valstart + ncodeunits("0000-00-00 00:00:00"))
                    timestr = String(bytes[valstart:valend-1]) # It would be nice to avoid the string, but oh well
                    timeval = tryparse(DateTime, timestr, REPL_DATE_FORMAT)
                    if !isnothing(timeval)
                        time = timeval
                    end
                end
            end
            if bytes[pos] != UInt8('\t')
                if pos == length(bytes)
                    seek(file, offset + entrystart - 1)
                    break
                else
                    continue
                end
            end
            contentstart = pos
            nlines = 0
            while true
                pos = findnext(bytes, pos, UInt8('\n'))
                nlines += 1
                if pos < length(bytes) && bytes[pos+1] == UInt8('\t')
                    pos += 1
                else
                    break
                end
            end
            contentend, pos = pos, contentstart
            content = Vector{UInt8}(undef, contentend - contentstart - nlines)
            bytescopied = 0
            while pos < contentend
                lineend = findnext(bytes, pos, UInt8('\n'))
                nbytes = lineend - pos - (lineend == contentend)
                copyto!(content, bytescopied + 1, bytes, pos + 1, nbytes)
                bytescopied += nbytes
                pos = lineend + 1
            end
            entry = HistEntry(mode, time, String(content), histindex += 1)
            push!(records, entry)
        end
        seek(file, offset + pos - 1)
    finally
        unlock(hist)
    end
    hist
end

function Base.push!(hist::HistoryFile, entry::HistEntry)
    try
        lock(hist)
        update!(hist)
        if iszero(entry.index)
            entry = HistEntry(
                entry.mode, entry.date,
                entry.content,
                length(hist.records) + 1)
        end
        push!(hist.records, entry)
        isopen(hist.file) || return hist
        str = """
        # time: $(Dates.format(entry.date, REPL_DATE_FORMAT))Z
        # mode: $(entry.mode)
        $(replace(entry.content, r"^"ms => "\t"))
        """
        write(hist.file, str)
    finally
        unlock(hist)
    end
    hist
end
