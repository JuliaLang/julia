export Signature, name, email, time, time_offset

type Signature
    name::String
    email::String
    time::Int64
    time_offset::Cint
end

Base.show(io::IO, s::Signature) = begin
    fmt = "%Y-%m-%d %H:%M:%S %Z"
    time_str = strftime(fmt, s.time)
    print(io, "Signature(\"$(name(s))\",\"$(email(s))\",\"$time_str\")")
end

# Get signature at time...
function Signature(name::String, email::String, time::Int64, offset::Int)
    bname  = bytestring(name)
    bemail = bytestring(email)
    sig_ptr = Array(Ptr{api.GitSignature}, 1)
    @check api.git_signature_new(sig_ptr, bname, bemail, time, offset)
    gsig = unsafe_load(sig_ptr[1])
    sig = Signature(bytestring(gsig.name),
                    bytestring(gsig.email),
                    gsig.time,
                    gsig.time_offset)
    api.free!(gsig)
    return sig
end

# Get signature now...
function Signature(name::String, email::String)
    bname = bytestring(name)
    bemail = bytestring(email)
    sig_ptr = Array(Ptr{api.GitSignature}, 1)
    @check api.git_signature_now(sig_ptr, bname, bemail)
    gsig = unsafe_load(sig_ptr[1])
    sig = Signature(bytestring(gsig.name),
                    bytestring(gsig.email),
                    gsig.time,
                    gsig.time_offset)
    api.free!(gsig)
    return sig
end

function Signature(gsig::api.GitSignature)
    return Signature(bytestring(gsig.name),
                     bytestring(gsig.email),
                     gsig.time,
                     gsig.time_offset)
end

git_signature(sig::Signature) = begin
    #name_copy  = copy(sig.name)
    #email_copy = copy(sig.email) 
    return api.GitSignature(convert(Ptr{Cchar}, pointer(sig.name)),
                            convert(Ptr{Cchar}, pointer(sig.email)),
                            sig.time, sig.time_offset)
end

git_signature_ptr(sig::Signature) = begin
    sig_ptr = Array(Ptr{api.GitSignature}, 1)
    @check api.git_signature_new(sig_ptr, sig.name, sig.email, 
                                 sig.time, sig.time_offset)
    return sig_ptr[1]
end

function name(s::Signature)
    return s.name
end

function email(s::Signature)
    return s.email
end

function Base.time(s::Signature)
    return s.time
end

function time_offset(s::Signature)
    return s.time_offset
end
