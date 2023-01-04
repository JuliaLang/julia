import Logging

struct RemoteLogger <: Logging.AbstractLogger
    pid::Int
    min_level::Logging.LogLevel
end
function RemoteLogger(pid=1)
    RemoteLogger(pid, Logging.Info)
end

Logging.min_enabled_level(logger::RemoteLogger) = logger.min_level
Logging.shouldlog(logger::RemoteLogger, level, _module, group, id) = true

# TODO: probably should live in base/logging.jl?
function logmsg(level::Logging.LogLevel, message, _module, _group, _id, _file, _line; kwargs...)
    Logging.@logmsg level message _module=_module _group=_group _id=_id _file=_file _line=_line kwargs...
end

function Logging.handle_message(logger::RemoteLogger, level::Logging.LogLevel, message, _module, _group, _id,
    _file, _line; kwargs...)
    @nospecialize
    remote_do(logmsg, logger.pid, level, message, _module, _group, _id, _file, _line; pid=myid(), kwargs...)
end
