using Base.BinaryPlatforms
function hook(p::Platform)
    p["foo"] = get(ENV, "PLATAUG_VAL", "bar")
    return p
end