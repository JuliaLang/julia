type UTF32String
    data::Array{Int32,1}
end

length(s::UTF32String) = length(s.data)
ref(s::UTF32String, i::Index) = s.data[i]
assign(s::UTF32String, c::Int32, i::Index) = (s.data[i] = c)
