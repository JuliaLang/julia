load("json")

load("test/json_samples")


@assert JSON.parse(a) != nothing
@assert JSON.parse(b) != nothing
@assert JSON.parse(c) != nothing
@assert JSON.parse(d) != nothing

j=JSON.parse(e) 
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



@assert JSON.parse(gmaps) != nothing
@assert JSON.parse(colors1) != nothing
@assert JSON.parse(colors2) != nothing
@assert JSON.parse(colors3) != nothing
@assert JSON.parse(twitter) != nothing
@assert JSON.parse(facebook) != nothing

k=JSON.parse(flickr)
@assert k!= nothing
@assert k["totalItems"] == 222
@assert k["items"][1]["description"][12] == '\"'
@assert JSON.parse(youtube) != nothing
@assert JSON.parse(iphone) != nothing
@assert JSON.parse(customer) != nothing
@assert JSON.parse(product) != nothing
@assert JSON.parse(interop) != nothing

u=JSON.parse(unicode) 
@assert u!=nothing
@assert u["অলিম্পিকস"]["রেকর্ড"][2]["Marathon"] == "জনি হেইস"

#Uncomment while doing timing tests
#@time for i=1:100 ; JSON.parse(d) ; end

