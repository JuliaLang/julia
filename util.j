function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end

ls() = system("ls")

show(file::String) = system(strcat("cat ", file))
