module I18n

export locale

CALLBACKS = Function[]

function locale()
    get(ENV, "JULIA_LOCALE", "")
end

function locale(s::ByteString)
    ENV["JULIA_LOCALE"] = s
    for cb in CALLBACKS
        cb()
    end
end

end # module
