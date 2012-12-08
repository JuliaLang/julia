let io = IOString()
@assert eof(io)
@assert try read(io,Uint8); false; catch e; isa(e,EOFError); end
@assert write(io,"abc") == 3
@assert length(io) == 3
@assert position(io) == 3
@assert eof(io)
@assert seek(io, 0)
@assert read(io, Uint8) == 'a'
a = Array(Uint8, 2)
@assert read(io, a) == a
@assert a == ['b','c']
@assert bytestring(io) == "abc"
@assert seek(io, 1)
@assert truncate(io, 2)
@assert position(io) == 1
@assert !eof(io)
@assert seek_end(io)
@assert position(io) == 2
@assert truncate(io, 0)
@assert position(io) == 0
@assert truncate(io, 10)
@assert position(io) == 0
@assert all(io.data .== 0)
@assert write(io,Int16[1,2,3,4,5,6]) == 12
@assert seek(io,2)
@assert truncate(io, 10)
@assert length(io) == 10
io.readable = false
@assert try read(io,Uint8[0]); false; catch e; true; end
@assert truncate(io, 0)
@assert write(io,"boston\ncambridge\n") > 0
@assert takebuf_string(io) == "boston\ncambridge\n"
@assert takebuf_string(io) == ""
close(io)
@assert try write(io,Uint8[0]); false; catch e; true; end
@assert try seek(io,0); false; catch e; true; end
@assert eof(io)
end

let io = IOString("hamster\nguinea pig\nturtle")
@assert position(io) == 0
@assert readline(io) == "hamster\n"
@assert readall(io) == "guinea pig\nturtle"
@assert try read(io,Uint8); false; catch e; isa(e,EOFError); end
@assert seek(io,0)
@assert read(io,Uint8) == 'h'
@assert try truncate(io,0); false; catch e; true; end
@assert try grow(io,0); false; catch e; true; end
@assert try write(io,uint8(0)); false; catch e; true; end
@assert try write(io,Uint8[0]); false; catch e; true; end
@assert takebuf_string(io) == "hamster\nguinea pig\nturtle"
@assert takebuf_string(io) == "hamster\nguinea pig\nturtle" #should be unchanged
close(io)
end

let io = PipeString()
@assert try read(io,Uint8); false; catch e; isa(e,EOFError); end
@assert write(io,"pancakes\nwaffles\nblueberries\n") > 0
@assert position(io) == 0
@assert readline(io) == "pancakes\n"
Base.compact(io)
@assert readline(io) == "waffles\n"
@assert write(io,"whipped cream\n") > 0
@assert readline(io) == "blueberries\n"
@assert try seek(io,0); false; catch e; true; end
@assert try truncate(io,0); false; catch e; true; end
@assert readline(io) == "whipped cream\n"
Base.compact(io)
@assert position(io) == 0
@assert length(io) == 0
Base.ensureroom(io,50)
@assert position(io) == 0
@assert length(io) == 0
@assert length(io.data) == 50
Base.ensureroom(io,10)
@assert length(io) == 0
@assert length(io.data) == 50
io.maxsize = 75
Base.ensureroom(io,100)
@assert length(io) == 0
@assert length(io.data) == 75
@assert seek_end(io)
@assert length(io) == 0
@assert position(io) == 0
write(io,zeros(Uint8,200))
@assert length(io) == 75
@assert length(io.data) == 75
write(io,1)
@assert length(io) == 75
@assert length(io.data) == 75
write(io,[1,2,3])
@assert length(io) == 75
@assert length(io.data) == 75
skip(io,1)
@assert write(io,uint8(104)) == 1
skip(io,3)
@assert write(io,"apples".data) == 3
skip(io,71)
@assert write(io,'y') == 1
@assert readall(io) == "happy"
@assert eof(io)
write(io,zeros(Uint8,73))
write(io,'a')
write(io,'b')
write(io,'c')
write(io,'d')
write(io,'e')
@assert length(io) == 75
@assert length(io.data) == 75
@assert position(io) == 0
skip(io,72)
@assert takebuf_string(io) == "\0ab"
@assert takebuf_string(io) == ""
close(io)
end
