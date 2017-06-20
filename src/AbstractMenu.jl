abstract type AbstractMenu end

function request(msg::AbstractString, m::AbstractMenu)
    println(msg)
    request(m)
end
