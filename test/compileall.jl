# This test builds a full system image, so it can take a little while.
# We make it a separate test target here, so that it can run in parallel
# with the rest of the tests.

mktempdir() do dir
    run(`$(Base.julia_cmd()) --compile=all --strip-ir --output-o $(dir)/sys.o.a -e 'exit()'`)
    Base.Linking.link_image(joinpath(dir, "sys.o.a"), joinpath(dir, "sys.so"))
    @test success(`$(Base.julia_cmd()) -J $(dir)/sys.so -e 'Base.scrub_repl_backtrace(nothing); exit()'`)
end
