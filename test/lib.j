function runtests(name)
    println("     \033[1m*\033[0m \033[31m$(name)\033[0m")
    load("$name.j")
end
