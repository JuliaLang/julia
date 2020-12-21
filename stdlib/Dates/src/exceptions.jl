struct UndefDateFormatCode <: Exception
    letter::Char
end

function Base.showerror(io::IO, e::UndefDateFormatCode)
    print(io, "UndefDateFormatCode: Reserved date format code '$(e.letter)' is currently " *
              "undefined. Either escape the character (using backslash) or load the " *
              "package that defines it")
end
