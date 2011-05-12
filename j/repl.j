function print_banner()

    julia_banner_plain = 
   "
                   _       
       _       _ _(_)_     |
      (_)     | (_) (_)    |  A fresh approach to technical computing
       _ _   _| |_  __ _   |  pre-release version
      | | | | | | |/ _` |  |
      | | |_| | | | (_| |  |
     _/ |\\__'_|_|_|\\__'_|  |
    |__/                   |\n"

    julia_banner_color = 
   "    \033[1m               \033[32m_\033[37m
       \033[34m_\033[37m       _ \033[31m_\033[32m(_)\033[35m_\033[37m     |
      \033[34m(_)\033[37m     | \033[31m(_) \033[35m(_)\033[37m    |  A fresh approach to technical computing
       _ _   _| |_  __ _   |  pre-release version
      | | | | | | |/ _` |  |
      | | |_| | | | (_| |  |
     _/ |\\__'_|_|_|\\__'_|  |
    |__/                   |\033[0m\n";

    term = getenv("TERM")
    if term == "xterm" || term == "xterm-color"
        print(julia_banner_color)
    else
        print(julia_banner_plain)
    end

end

