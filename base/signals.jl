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

# Generate the `SIG*` constants for standard POSIX signals. We need to generate these
# constants as the associated signal numbers are architecture specific.
for signum in 1:31
    signame = signal_name(signum)

    if !isnothing(signame)
        sigsym = Symbol(signame)
        @eval begin
            const $sigsym = $signum
            export $sigsym
        end
    end
end

Base.kill(pid::Integer, signum::Integer) = ccall(:kill, Cvoid, (Cint, Cint), pid, signum)
Base.kill(signum::Integer) = kill(getpid(), signum)

end
