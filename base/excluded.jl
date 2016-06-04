# This file is a part of Julia. License is MIT: http://julialang.org/license

module Excluded

macro exclude(typexpr)
    f = typexpr.args[2]
    if typexpr.head == :type
        return quote
            export $f
            type $f
                $(f)() = throw(ArgumentError("`$($f)` has been excluded in this build"))
            end
        end
    end
end

export Dates
module Dates end
@exclude type Date end
@exclude type DateTime end

end # module
