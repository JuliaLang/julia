# This file is a part of Julia. License is MIT: http://julialang.org/license

module I18n

export locale

LOCALE = nothing
CALLBACKS = Function[]

function locale()
    if LOCALE === nothing
        # XXX:TBD return default locale
        return ""
    end
    LOCALE
end

function locale(s::String)
    global LOCALE = s
    # XXX:TBD call setlocale
    for cb in CALLBACKS
        cb()
    end
end

end # module
