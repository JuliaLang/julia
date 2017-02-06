# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractMsg

let REF_ID::Int = 1
    global next_ref_id
    next_ref_id() = (id = REF_ID; REF_ID += 1; id)
end

immutable RRID
    whence::Int
    id::Int

    RRID() = RRID(myid(),next_ref_id())
    RRID(whence, id) = new(whence,id)
end
hash(r::RRID, h::UInt) = hash(r.whence, hash(r.id, h))
==(r::RRID, s::RRID) = (r.whence==s.whence && r.id==s.id)

## Wire format description
#
# Each message has three parts, which are written in order to the worker's stream.
#  1) A header of type MsgHeader is serialized to the stream (via `serialize`).
#  2) A message of type AbstractMsg is then serialized.
#  3) Finally, a fixed bounday of 10 bytes is written.

# Message header stored separately from body to be able to send back errors if
# a deserialization error occurs when reading the message body.
immutable MsgHeader
    response_oid::RRID
    notify_oid::RRID
    MsgHeader(respond_oid=RRID(0,0), notify_oid=RRID(0,0)) =
        new(respond_oid, notify_oid)
end

# Special oid (0,0) uses to indicate a null ID.
# Used instead of Nullable to decrease wire size of header.
null_id(id) =  id == RRID(0, 0)

immutable CallMsg{Mode} <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable CallWaitMsg <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable RemoteDoMsg <: AbstractMsg
    f::Function
    args::Tuple
    kwargs::Array
end
immutable ResultMsg <: AbstractMsg
    value::Any
end


# Worker initialization messages
immutable IdentifySocketMsg <: AbstractMsg
    from_pid::Int
end

immutable IdentifySocketAckMsg <: AbstractMsg
end

immutable JoinPGRPMsg <: AbstractMsg
    self_pid::Int
    other_workers::Array
    topology::Symbol
    enable_threaded_blas::Bool
end
immutable JoinCompleteMsg <: AbstractMsg
    cpu_cores::Int
    ospid::Int
end

# Avoiding serializing AbstractMsg containers results in a speedup
# of approximately 10%. Can be removed once module Serializer
# has been suitably improved.

# replace  CallMsg{Mode} with specific invocations
const msgtypes = filter!(x->x!=CallMsg, subtypes(AbstractMsg))
push!(msgtypes, CallMsg{:call}, CallMsg{:call_fetch})

for (idx, tname) in enumerate(msgtypes)
    nflds = length(fieldnames(tname))
    @eval begin
        function serialize(s::AbstractSerializer, o::$tname)
            write(s.io, UInt8($idx))
            for fld in fieldnames($tname)
                serialize(s, getfield(o, fld))
            end
        end

        function deserialize_msg(s::AbstractSerializer, ::Type{$tname})
            data=Array{Any,1}($nflds)
            for i in 1:$nflds
                data[i] = deserialize(s)
            end
            return $tname(data...)
        end
    end
end

function deserialize_msg(s::AbstractSerializer)
    idx = read(s.io, UInt8)
    t = msgtypes[idx]
    return eval(current_module(), Expr(:body, Expr(:return, Expr(:call, deserialize_msg, QuoteNode(s), QuoteNode(t)))))
end

function send_msg_unknown(s::IO, header, msg)
    error("attempt to send to unknown socket")
end

function send_msg(s::IO, header, msg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg(worker_from_id(id), header, msg)
    end
    send_msg_unknown(s, header, msg)
end

function send_msg_now(s::IO, header, msg::AbstractMsg)
    id = worker_id_from_socket(s)
    if id > -1
        return send_msg_now(worker_from_id(id), header, msg)
    end
    send_msg_unknown(s, header, msg)
end
function send_msg_now(w::Worker, header, msg)
    send_msg_(w, header, msg, true)
end

function send_msg(w::Worker, header, msg)
    send_msg_(w, header, msg, false)
end

function flush_gc_msgs(w::Worker)
    if !isdefined(w, :w_stream)
        return
    end
    w.gcflag = false
    new_array = Any[]
    msgs = w.add_msgs
    w.add_msgs = new_array
    if !isempty(msgs)
        remote_do(add_clients, w, msgs)
    end

    # del_msgs gets populated by finalizers, so be very careful here about ordering of allocations
    new_array = Any[]
    msgs = w.del_msgs
    w.del_msgs = new_array
    if !isempty(msgs)
        #print("sending delete of $msgs\n")
        remote_do(del_clients, w, msgs)
    end
end

# Boundary inserted between messages on the wire, used for recovering
# from deserialization errors. Picked arbitrarily.
# A size of 10 bytes indicates ~ ~1e24 possible boundaries, so chance of collision
# with message contents is negligible.
const MSG_BOUNDARY = UInt8[0x79, 0x8e, 0x8e, 0xf5, 0x6e, 0x9b, 0x2e, 0x97, 0xd5, 0x7d]

# Faster serialization/deserialization of MsgHeader and RRID
function serialize_hdr_raw(io, hdr)
    write(io, hdr.response_oid.whence, hdr.response_oid.id, hdr.notify_oid.whence, hdr.notify_oid.id)
end

function deserialize_hdr_raw(io)
    data = Array{Int,1}(4)
    read!(io, data)
    return MsgHeader(RRID(data[1], data[2]), RRID(data[3], data[4]))
end

function send_msg_(w::Worker, header, msg, now::Bool)
    check_worker_state(w)
    io = w.w_stream
    lock(io.lock)
    try
        reset_state(w.w_serializer)
        serialize_hdr_raw(io, header)
        eval(current_module(), Expr(:body, Expr(:return, Expr(:call, serialize, QuoteNode(w.w_serializer), QuoteNode(msg)))))  # io is wrapped in w_serializer
        write(io, MSG_BOUNDARY)

        if !now && w.gcflag
            flush_gc_msgs(w)
        else
            flush(io)
        end
    finally
        unlock(io.lock)
    end
end

function flush_gc_msgs()
    try
        for w in (PGRP::ProcessGroup).workers
            if isa(w,Worker) && w.gcflag && (w.state == W_CONNECTED)
                flush_gc_msgs(w)
            end
        end
    catch e
        bt = catch_backtrace()
        @schedule showerror(STDERR, e, bt)
    end
end

function send_connection_hdr(w::Worker, cookie=true)
    # For a connection initiated from the remote side to us, we only send the version,
    # else when we initiate a connection we first send the cookie followed by our version.
    # The remote side validates the cookie.
    if cookie
        write(w.w_stream, LPROC.cookie)
    end
    write(w.w_stream, rpad(VERSION_STRING, HDR_VERSION_LEN)[1:HDR_VERSION_LEN])
end
