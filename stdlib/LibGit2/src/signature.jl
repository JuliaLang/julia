# This file is a part of Julia. License is MIT: https://julialang.org/license

function Signature(ptr::Ptr{SignatureStruct})
    sig   = unsafe_load(ptr)::SignatureStruct
    name  = unsafe_string(sig.name)
    email = unsafe_string(sig.email)
    time   = sig.when.time
    offset = sig.when.offset
    return Signature(name, email, time, offset)
end
Signature(sig::GitSignature) = Signature(sig.ptr)

function Signature(name::AbstractString, email::AbstractString)
    ensure_initialized()
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_now, libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Cstring, Cstring), sig_ptr_ptr, name, email)
    sig = GitSignature(sig_ptr_ptr[])
    s = Signature(sig.ptr)
    close(sig)
    return s
end

function Signature(repo::GitRepo)
    sig = default_signature(repo)
    s = Signature(sig.ptr)
    close(sig)
    return s
end

function Base.convert(::Type{GitSignature}, sig::Signature)
    ensure_initialized()
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_new, libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Cstring, Cstring, Int64, Cint),
                 sig_ptr_ptr, sig.name, sig.email, sig.time, sig.time_offset)
    return GitSignature(sig_ptr_ptr[])
end

function yearmonthday(days)
    z = days + 306; h = 100z - 25; a = fld(h, 3652425); b = a - fld(a, 4)
    y = fld(100b + h, 36525); c = b + z - 365y - fld(y, 4); m = div(5c + 456, 153)
    d = c - div(153m - 457, 5); return m > 12 ? (y + 1, m - 12, d) : (y, m, d)
end
lpad0(x) = lpad(x, 2, '0')

function unix2date(t)
   UNIXEPOCH = 62135683200000
   rata = UNIXEPOCH + round(Int64, Int64(1000) * t)
   year, month, day = yearmonthday(fld(rata, 86400000))
   secs = t % (24 * 60 * 60)
   mins = div(secs, 60)
   m, d = lpad0(month), lpad0(day)
   h, mi, s = lpad0.(round.(Int, (div(mins, 60), mins % 60, secs % 60)))
   return "$year-$m-$d $h:$mi:$s"
end

function Base.show(io::IO, sig::Signature)
    print(io, "Name: ", sig.name, ", ")
    print(io, "Email: ", sig.email, ", ")
    print(io, "Time: ", unix2date(sig.time + 60*sig.time_offset))
    @printf(io, "%+03i:%02i", divrem(sig.time_offset, 60)...)
end

"""Return signature object. Free it after use."""
function default_signature(repo::GitRepo)
    ensure_initialized()
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_default, libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Ptr{Cvoid}), sig_ptr_ptr, repo.ptr)
    return GitSignature(sig_ptr_ptr[])
end
