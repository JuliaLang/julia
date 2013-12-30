let user_data_dir
    ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]*
        ";C:\\Program Files\\Git\\bin;C:\\Program Files (x86)\\Git\\bin"*
        ";C:\\MinGW\\bin;C:\\MinGW\\msys\\1.0\\bin"*
        ";C:\\Python27;C:\\Python26;C:\\Python25"
    haskey(ENV,"JULIA_EDITOR") || (ENV["JULIA_EDITOR"] = "start")
    user_data_dir = abspath(ENV["AppData"],"julia")
    isdir(user_data_dir) || mkdir(user_data_dir)
end
