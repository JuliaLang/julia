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
    # Original fields
    mode::Symbol
    date::DateTime
    content::String
    # Extended fields (ordered for better packing)
    session::UInt64
    cwd::String
    result::String
    elapsed::Float32
    index::UInt32 # computed
    sid::UInt32
    errored::Bool
end

HistEntry(session::UInt64, sid::Integer, cwd::String, mode::Symbol, date::DateTime, content::String) =
    HistEntry(mode, date, content, session, cwd, "", 0.0, 0, UInt32(sid), false)

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

Base.isopen(hist::HistoryFile) = isopen(hist.file)
Base.close(hist::HistoryFile) = close(hist.file)

# Reading entries

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
        bytes = read(file)
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
                @warn S"Malformed history entry: expected meta-line starting with {success:'#'} at byte {emphasis:$(offset + pos - 1)} in \
                       {(underline=grey),link=$(Base.Filesystem.uripath(hist.path)):$(contractuser(hist.path))}, but found \
                       {error:$(sprint(show, Char(bytes[pos])))} instead" _id=:invalid_history_entry maxlog=3 _file=nothing _line=nothing
                pos = findbyte(bytes, pos, UInt8('\n')) + 1
                continue
            end
            time, mode = zero(DateTime), :julia # Original attributes
            session, sid, cwd = zero(UInt64), zero(UInt16), "" # Extended attributes
            while pos < length(bytes) && bytes[pos] == UInt8('#')
                pos = skipspaces(bytes, pos + 1)
                metastart = pos
                metaend = findbyte(bytes, pos, UInt8(':'))
                pos = skipspaces(bytes, metaend + 1)
                valstart = pos
                valend = findbyte(bytes, pos, UInt8('\n'))
                while valend > valstart && bytes[valend - 1] == UInt8(' ')
                    valend -= 1
                end
                pos = valend + 1
                if isstrmatch(bytes, metastart, "id:")
                    slashpos = findbyte(bytes, valstart, UInt8('/'))
                    if slashpos < valend
                        sessionval = tryparse(UInt64, String(bytes[valstart:slashpos-1]), base=62)
                        sidval = tryparse(UInt32, String(bytes[slashpos+1:valend-1]))
                        if !isnothing(sessionval) && !isnothing(sidval)
                            session, sid = sessionval, sidval
                        end
                    end
                elseif isstrmatch(bytes, metastart, "mode:")
                    mode = if isstrmatch(bytes, valstart, "julia") && bytes[valstart + ncodeunits("julia")] ∈ (UInt8('\n'), UInt8('\r'))
                        :julia
                    elseif isstrmatch(bytes, valstart, "help") && bytes[valstart + ncodeunits("help")] ∈ (UInt8('\n'), UInt8('\r'))
                        :help
                    elseif all(>(0x5a), view(bytes, valstart:valend-1))
                        Symbol(bytes[valstart:valend-1])
                    else
                        Symbol(lowercase(String(bytes[valstart:valend-1])))
                    end
                elseif isstrmatch(bytes, metastart, "time:")
                    valend = min(valend, valstart + ncodeunits("0000-00-00 00:00:00"))
                    timestr = String(bytes[valstart:valend-1]) # It would be nice to avoid the string, but oh well
                    timeval = tryparse(DateTime, timestr, REPL_DATE_FORMAT)
                    if !isnothing(timeval)
                        time = timeval
                    end
                elseif isstrmatch(bytes, metastart, "cwd:")
                    cwd = String(bytes[valstart:valend-1])
                elseif isstrmatch(bytes, metastart, "result:")
                    updateresult!(records, bytes, valstart, valend - 1)
                end
            end
            if pos >= length(bytes)
                # Potentially incomplete entry; roll back to start
                seek(file, offset + entrystart - 1)
                break
            elseif bytes[pos] == UInt8(' ')
                @warn S"Malformed history content: expected line to start with {success:'\\t'} at byte {emphasis:$(offset + pos - 1)} in \
                        {(underline=grey),link=$(Base.Filesystem.uripath(hist.path)):$(contractuser(hist.path))}, but found \
                        space ({error:' '}) instead. A text editor may have converted tabs to spaces in the \
                        history file." _id=:invalid_history_content_spc maxlog=1 _file=nothing _line=nothing
                continue
            elseif bytes[pos] != UInt8('\t')
                @warn S"Malformed history content: expected line to start with {success:'\\t'} at byte {emphasis:$(offset + pos - 1)} in \
                        {(underline=grey),link=$(Base.Filesystem.uripath(hist.path)):$(contractuser(hist.path))}, but found \
                        {error:$(sprint(show, Char(bytes[pos])))} instead" _id=:invalid_history_content maxlog=3 _file=nothing _line=nothing
                continue
            end
            contentstart = pos
            nlines = 0
            while true
                pos = findbyte(bytes, pos, UInt8('\n'))
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
                lineend = findbyte(bytes, pos, UInt8('\n'))
                nbytes = lineend - pos - (lineend == contentend)
                copyto!(content, bytescopied + 1, bytes, pos + 1, nbytes)
                bytescopied += nbytes
                pos = lineend + 1
            end
            if isempty(cwd)
                cwd = getrecent(records, session, :cwd, "")
            end
            entry = HistEntry(mode, time, String(content), session, cwd, "", zero(Float32), histindex += 1, sid, false)
            push!(records, entry)
        end
        seek(file, offset + pos - 1)
    finally
        unlock(hist)
    end
    hist
end

function findbyte(data::Vector{UInt8}, index::Int, byte::UInt8, limit::Int = length(data))
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

function skipspaces(data::AbstractVector{UInt8}, at::Int)
    while at < length(data) && data[at] == UInt8(' ')
        at += 1
    end
    at
end

function updateresult!(recs::Vector{HistEntry}, resbytes::AbstractVector{UInt8}, pos::Int, resend::Int)
    # Cautiously parse the result value:
    # [error][type] in [elapsed]s ([session]/[sid])
    errored = false
    if resbytes[pos] == UInt8('!')
        errored = true
        pos += 1
    end
    pnext = findbyte(resbytes, pos, UInt8(' '))
    pnext < resend || return
    restype = String(resbytes[pos:pnext-1])
    pos = skipspaces(resbytes, pnext)
    isstrmatch(resbytes, pos, "in") || return
    pos = skipspaces(resbytes, pos + ncodeunits("in"))
    pnext = findbyte(resbytes, pos, UInt8('s'))
    pnext < resend || return
    elapsedval = tryparse(Float32, String(resbytes[pos:pnext-1]))
    isnothing(elapsedval) && return
    elapsed = elapsedval
    pos = skipspaces(resbytes, pnext + 1) + 1
    pos < resend && resbytes[pos - 1] == UInt8('(') || return
    pnext = findbyte(resbytes, pos, UInt8('/'))
    pnext < resend || return
    sessionval = tryparse(UInt64, String(resbytes[pos:pnext-1]), base=62)
    pos = findbyte(resbytes, pnext + 1, UInt8(')'))
    pnext < resend || return
    sidval = tryparse(UInt32, String(resbytes[pnext+1:pos-1]))
    !isnothing(sessionval) && !isnothing(sidval) || return
    session, sid = sessionval, sidval
    # Now find and update the matching record
    for i in Iterators.reverse(eachindex(recs))
        rec = recs[i]
        rec.session == session && rec.sid == sid || continue
        recs[i] = HistEntry(
            rec.mode,
            rec.date,
            rec.content,
            rec.session,
            rec.cwd,
            restype,
            elapsed,
            rec.index,
            rec.sid,
            errored)
        return
    end
end

function getrecent(records::Vector{HistEntry}, session::UInt64, attr::Symbol, default = nothing)
    for rec in Iterators.reverse(records)
        rec.session == session || continue
        val = getfield(rec, attr)
        return val
    end
    default
end

# Adding and updating entries

function Base.push!(hist::HistoryFile, entry::HistEntry)
    prevwd = getrecent(hist.records, entry.session, :cwd, "")
    try
        lock(hist)
        update!(hist)
        entry = HistEntry(
            if all(islowercase, String(entry.mode))
                entry.mode
            else
                Symbol(lowercase(String(entry.mode)))
            end,
            round(entry.date, Dates.Second),
            entry.content,
            entry.session,
            ifelse(prevwd == entry.cwd, prevwd, entry.cwd),
            "",
            zero(Float32),
            length(hist.records) + 1,
            entry.sid,
            false)
        push!(hist.records, entry)
        isopen(hist.file) || return hist
        content = IOBuffer()
        write(content,
              "# id: ", string(entry.session, base=62), '/', string(entry.sid), '\n',
              "# time: ", Dates.format(entry.date, REPL_DATE_FORMAT), "Z\n",
              "# mode: ", String(entry.mode), '\n')
        if prevwd != entry.cwd
            write(content, "# cwd: ", entry.cwd, '\n')
        end
        replace(content, entry.content, r"^"ms => "\t")
        write(content, '\n')
        # Short version:
        #
        # Libuv supports opening files with an atomic append flag,
        # and so if we pass the entire new entry to `uv_fs_write`
        # with an offset of `-1`, the OS will ensure that the write
        # is atomic. There are some caveats around this, but there's
        # no silver bullet.
        #
        # Long version:
        #
        # Normally, we would need to make sure we've got unique access to the file,
        # however because we opened it with `O_APPEND` the OS (as of POSIX.1-2017, and on:
        # Linux/FreeBSD/Darwin/Windows) guarantees that concurrent writes will not tear.
        #
        # This requires that a single `write` call be used to write the entire new entry.
        # This is not obvious, but if you look at `base/filesystem.jl` we can see that
        # the `unsafe_write` call below is turned into a `uv_fs_write` call.
        # Following this to `src/jl_uv.c` we can see this quickly turns into a `uv_fs_write`
        # call, which will produce a `uv__fs_write_all` call, and then calls `uv__fs_write`
        # in a loop until everything is written.
        #
        # This loop seems like it might allow writes to be interleaved, but since
        # we know that `nbufs = 1` and `off = -1` (from the parameters set in `unsafe_write`
        # and `jl_uv_write`), we can see that `uv__fs_write` will call the `write`
        # syscall directly, and so we get the `O_APPEND` semantics guaranteed by the OS.
        #
        # POSIX does mention that `write` may write less bytes than it is asked to,
        # but only when either:
        # 1. There is insufficient space on the device, or
        # 2. The size of the write exceeds `RLIMIT_FSIZE`, or
        # 3. The call is interrupted by a signal handler.
        #
        # Any of these would cause issues regardless.
        #
        # Over in Windows-land, `FILE_APPEND_DATA` has been around for a while (and is used
        # by libuv), and from reading `win/fs.c` we can see that a similar approach is taken
        # using `WriteFile` calls. Before Windows 10 (on NTFS), v10.0.14393 update atomicity
        # could be as small as 1 byte, but after that testing indicates that writes through
        # to 1MB are written in a single operation. Given that this is not an upper limit,
        # and it would be quite an extraordinary REPL entry, this seem safe enough.
        #
        # While in theory a split write may occur, in practice this seems exceptionally rare
        # (near non-existent), and the previous pidfile locking approach is no silver bullet
        # either, with its own set of "reasonable assumptions" like:
        # 1. PIDs not being rapidly recycled
        # 2. No process being able to delete and write a file faster than another
        #    process can do the same
        # 3. The PID number itself being written in one shot (see the above lack of
        #    formal guarantees around `write`, which also applies here)
        #
        # All in all, relying on kernel inode locking with `O_APPEND` and whole writes
        # seems like the sanest approach overall. Mutual exclusion isn't the priority
        # here, safe appending is.
        unsafe_write(hist.file, pointer(content.data), position(content) % UInt, Int64(-1))
    finally
        unlock(hist)
    end
    hist
end

struct HistUpdate
    session::UInt64
    result::Symbol
    iserror::Bool
    elapsed::Float32
end

function Base.push!(hist::HistoryFile, update::HistUpdate)
    sid = zero(UInt16)
    for i in Iterators.reverse(eachindex(hist.records))
        rec = hist.records[i]
        rec.session == update.session || continue
        # This must correspond to the most recent entry from this session
        hist.records[i] = HistEntry(
            rec.mode,
            rec.date,
            rec.content,
            rec.session,
            rec.cwd,
            String(update.result),
            update.elapsed,
            rec.index,
            rec.sid,
            update.iserror)
        sid = rec.sid
        break
    end
    isopen(hist) && !iszero(sid) || return hist
    try
        lock(hist)
        content = IOBuffer()
        write(content, "# result: ",
              ifelse(update.iserror, "!", ""),
              update.result, " in ",
              string(round(update.elapsed, sigdigits=4)),
              "s (", string(update.session, base=62),
              '/', string(sid), ")\n")
        unsafe_write(hist.file, pointer(content.data), position(content) % UInt, Int64(-1))
    finally
        unlock(hist)
    end
    hist
end
