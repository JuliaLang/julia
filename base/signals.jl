module Signals

export signal_abbrev, signal_name

"""
    signal_abbrev(signum::Integer) -> Union{String,Nothing}

Returns the signal abbreviation (e.g. "TERM") associated with the signal number for standard
signals on this architecture. If the signal number is invalid `nothing` will be returned
instead.
"""
function signal_abbrev(signum::Integer)
    abbrev = ccall(:jl_sigabbrev, Cstring, (Cint,), signum)
    abbrev != C_NULL || return nothing
    return @static Sys.isbsd() ? uppercase(unsafe_string(abbrev)) : unsafe_string(abbrev)
end

"""
    signal_name(signum::Integer) -> Union{String,Nothing}

Returns the signal name (e.g. "SIGTERM") associated with the signal number for standard
signals on this architecture. If the signal number is invalid `nothing` will be returned
instead.
"""
function signal_name(signum::Integer)
    abbrev = signal_abbrev(signum)
    !isnothing(abbrev) || return nothing
    return string("SIG", abbrev)
end

end
