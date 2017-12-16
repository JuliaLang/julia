
"""
    parse_level(level)

Parse a Symbol into one of the standard `LogLevel`s
"""
parse_level(level::LogLevel) = level
function parse_level(level::Symbol)
    if      level == :belowminlevel  return  BelowMinLevel
    elseif  level == :debug          return  Debug
    elseif  level == :info           return  Info
    elseif  level == :warn           return  Warn
    elseif  level == :error          return  Error
    elseif  level == :abovemaxlevel  return  AboveMaxLevel
    else
        throw(ArgumentError("Unknown log level $level"))
    end
end

