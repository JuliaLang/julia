# This test builds a full system image, so it can take a little while.
# We make it a separate test target here, so that it can run in parallel
# with the rest of the tests.



if !Sys.iswindows() && !(Sys.WORD_SIZE == 32) #Windows doesn't support large images
    mktempdir() do dir
        @test success(pipeline(`$(Base.julia_cmd()) --compile=all --strip-ir --output-o $(dir)/sys.o.a -e 'const ballast = Memory{UInt8}(undef, 1 << 31); exit()'`, stderr=stderr))
        if isfile(joinpath(dir, "sys.o.a"))
            Base.Linking.link_image(joinpath(dir, "sys.o.a"), joinpath(dir, "sys.so"))
            @test success(`$(Base.julia_cmd()) -J $(dir)/sys.so -e 'Base.scrub_repl_backtrace(nothing); exit()'`)
        end
    end
else
    mktempdir() do dir
        @test success(pipeline(`$(Base.julia_cmd()) --compile=all --strip-ir --output-o $(dir)/sys.o.a -e 'exit()'`, stderr=stderr)) skip=(Sys.WORD_SIZE == 32)
        if isfile(joinpath(dir, "sys.o.a"))
            Base.Linking.link_image(joinpath(dir, "sys.o.a"), joinpath(dir, "sys.so"))
            @test success(`$(Base.julia_cmd()) -J $(dir)/sys.so -e 'Base.scrub_repl_backtrace(nothing); exit()'`)
        end
    end
end
