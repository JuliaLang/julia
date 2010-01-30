struct String <: Any
    str
end

string(s) = String.new(s)
str(s::String) = s.str

print(s::String) = print(str(s))

