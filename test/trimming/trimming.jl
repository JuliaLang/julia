using Test

exe_path = ARGS[1]

@test readchomp(`$exe_path`) == "Hello, world!"

@test filesize(exe_path) < filesize(unsafe_string(Base.JLOptions().image_file))/10
