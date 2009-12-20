type String < Any
    str
end

string(s) = new(String, s)
str(s::String) = s.str

function print(s::String)
    print(str(s))
end
