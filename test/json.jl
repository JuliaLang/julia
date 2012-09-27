load("json.jl")
import Json.*;

load("test/json_samples.jl")


@assert parse_json(a) != nothing
@assert parse_json(b) != nothing
@assert parse_json(c) != nothing
@assert parse_json(d) != nothing

j=parse_json(e) 
@assert  j!= nothing
typeof(j) == Dict{String, Any}
@assert length(j) == 1
typeof(j["menu"]) == Dict{String, Any}
@assert length(j["menu"]) == 2
@assert j["menu"]["header"] == "SVG\tViewerα"
@assert typeof(j["menu"]["items"]) == Array{Any,1}
@assert length(j["menu"]["items"]) == 22
@assert j["menu"]["items"][3] == nothing
@assert j["menu"]["items"][2]["id"] == "OpenNew"
@assert j["menu"]["items"][2]["label"] == "Open New"



@assert parse_json(gmaps) != nothing
@assert parse_json(colors1) != nothing
@assert parse_json(colors2) != nothing
@assert parse_json(colors3) != nothing
@assert parse_json(twitter) != nothing
@assert parse_json(facebook) != nothing

k=parse_json(flickr)
@assert k!= nothing
@assert k["totalItems"] == 222
@assert k["items"][1]["description"][12] == '\"'
@assert parse_json(youtube) != nothing
@assert parse_json(iphone) != nothing
@assert parse_json(customer) != nothing
@assert parse_json(product) != nothing
@assert parse_json(interop) != nothing

u=parse_json(unicode) 
@assert u!=nothing
@assert u["অলিম্পিকস"]["রেকর্ড"][2]["Marathon"] == "জনি হেইস"

#Uncomment while doing timing tests
@time for i=1:100 ; parse_json(d) ; end

