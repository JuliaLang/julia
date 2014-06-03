let user_data_dir
    ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]
    haskey(ENV,"JULIA_EDITOR") || (ENV["JULIA_EDITOR"] = "start")
    user_data_dir = abspath(ENV["AppData"],"julia")
    isdir(user_data_dir) || mkdir(user_data_dir)
end
