using Test

exe_path = joinpath(@__DIR__, "hello"*splitext(Base.julia_exename())[2])

@test readchomp(`$exe_path`) == "Hello, world!"

@test filesize(exe_path) < filesize(unsafe_string(Base.JLOptions().image_file))/10
