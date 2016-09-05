# This file is a part of Julia. License is MIT: http://julialang.org/license

##########################################
# Timer2
##########################################
#
# This is a backported replacement for the new Timer code introduced in v0.4
#

type Timer2
    handle::Ptr{Void}
    cond::Condition
    isopen::Bool

    function Timer2(timeout::Real, repeat::Real=0.0)
        timeout ≥ 0 || throw(ArgumentError("timer cannot have negative timeout of $timeout seconds"))
        repeat ≥ 0 || throw(ArgumentError("timer cannot repeat $repeat times"))

        this = new(Libc.malloc(Base._sizeof_uv_timer), Condition(), true)
        err = ccall(:uv_timer_init,Cint,(Ptr{Void},Ptr{Void}),Base.eventloop(),this.handle)
        if err != 0
            #TODO: this codepath is currently not tested
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(UVError("uv_make_timer",err))
        end

        Base.associate_julia_struct(this.handle, this)
        Base.preserve_handle(this)

        ccall(:uv_update_time, Void, (Ptr{Void},), Base.eventloop())
        @compat ccall(:uv_timer_start, Cint, (Ptr{Void},Ptr{Void},UInt64,UInt64),
              this.handle, Base.uv_jl_asynccb::Ptr{Void},
              UInt64(round(timeout*1000))+1, UInt64(round(repeat*1000)))
        this
    end
end

Base.wait(t::Timer2) = wait(t.cond)

Base.isopen(t::Timer2) = t.isopen

function Base.close(t::Timer2)
    if t.handle != C_NULL
        t.isopen = false
        ccall(:uv_timer_stop, Cint, (Ptr{Void},), t.handle)
        ccall(:jl_close_uv, Void, (Ptr{Void},), t.handle)
    end
end

function Base._uv_hook_close(t::Timer2)
    Base.unpreserve_handle(t)
    Base.disassociate_julia_struct(t)
    t.handle = C_NULL
    nothing
end

function Base._uv_hook_asynccb(t::Timer2)
    @compat if ccall(:uv_timer_get_repeat, UInt64, (Ptr{Void},), t.handle) == 0
        # timer is stopped now
        close(t)
    end
    notify(t.cond)
    nothing
end

# timer with repeated callback
function Base.Timer(cb::Function, timeout::Real, repeat::Real=0.0)
    t = Timer2(timeout, repeat)
    @schedule begin
        while isopen(t)
            wait(t)
            cb(t)
        end
    end
    t
end

Base.Timer(timeout::Real, repeat::Real=0.0) = Timer2(timeout, repeat)
