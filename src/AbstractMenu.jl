abstract AbstractMenu

function request(msg::AbstractString, m::AbstractMenu)
    println(msg)
    request(m)
end
