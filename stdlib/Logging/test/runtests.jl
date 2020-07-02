# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Logging

import Logging: min_enabled_level, shouldlog, handle_message

@noinline func1() = backtrace()

@testset "Logging" begin

@testset "Core" begin
    # Symbols imported from CoreLogging should appear in tab completions
    @test :AbstractLogger in names(Logging, all=true)  # exported public type
    @test :Info in names(Logging, all=true)            # non-exported public constant
    @test :handle_message in names(Logging, all=true)  # non-exported public function
end

@testset "ConsoleLogger" begin
    # First pass log limiting
    @test min_enabled_level(ConsoleLogger(devnull, Logging.Debug)) == Logging.Debug
    @test min_enabled_level(ConsoleLogger(devnull, Logging.Error)) == Logging.Error

    # Second pass log limiting
    logger = ConsoleLogger(devnull)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === true
    handle_message(logger, Logging.Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === true
    handle_message(logger, Logging.Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === false

    # Check that maxlog works without an explicit ID (#28786)
    buf = IOBuffer()
    io = IOContext(buf, :displaysize=>(30,80), :color=>false)
    logger = ConsoleLogger(io)
    with_logger(logger) do
        for i in 1:2
            @info "test" maxlog=1
        end
    end
    @test String(take!(buf)) ==
    """
    [ Info: test
    """
    with_logger(logger) do
        for i in 1:2
            @info "test" maxlog=0
        end
    end
    @test String(take!(buf)) == ""

    @testset "Default metadata formatting" begin
        @test Logging.default_metafmt(Logging.Debug, Base, :g, :i, expanduser("~/somefile.jl"), 42) ==
            (:blue,      "Debug:",   "@ Base ~/somefile.jl:42")
        @test Logging.default_metafmt(Logging.Info,  Main, :g, :i, "a.jl", 1) ==
            (:cyan,      "Info:",    "")
        @test Logging.default_metafmt(Logging.Warn,  Main, :g, :i, "b.jl", 2) ==
            (:yellow,    "Warning:", "@ Main b.jl:2")
        @test Logging.default_metafmt(Logging.Error, Main, :g, :i, "", 0) ==
            (:light_red, "Error:",   "@ Main :0")
        # formatting of nothing
        @test Logging.default_metafmt(Logging.Warn,  nothing, :g, :i, "b.jl", 2) ==
            (:yellow,    "Warning:", "@ b.jl:2")
        @test Logging.default_metafmt(Logging.Warn,  Main, :g, :i, nothing, 2) ==
            (:yellow,    "Warning:", "@ Main")
        @test Logging.default_metafmt(Logging.Warn,  Main, :g, :i, "b.jl", nothing) ==
            (:yellow,    "Warning:", "@ Main b.jl")
        @test Logging.default_metafmt(Logging.Warn,  nothing, :g, :i, nothing, 2) ==
            (:yellow,    "Warning:", "")
        @test Logging.default_metafmt(Logging.Warn,  Main, :g, :i, "b.jl", 2:5) ==
            (:yellow,    "Warning:", "@ Main b.jl:2-5")
    end

    function dummy_metafmt(level, _module, group, id, file, line)
        :cyan,"PREFIX","SUFFIX"
    end

    # Log formatting
    function genmsg(message; level=Logging.Info, _module=Main,
                    file="some/path.jl", line=101, color=false, width=75,
                    meta_formatter=dummy_metafmt, show_limited=true,
                    right_justify=0, kws...)
        buf = IOBuffer()
        io = IOContext(buf, :displaysize=>(30,width), :color=>color)
        logger = ConsoleLogger(io, Logging.Debug,
                               meta_formatter=meta_formatter,
                               show_limited=show_limited,
                               right_justify=right_justify)
        Logging.handle_message(logger, level, message, _module, :a_group, :an_id,
                               file, line; kws...)
        String(take!(buf))
    end

    # Basic tests for the default setup
    @test genmsg("msg", level=Logging.Info, meta_formatter=Logging.default_metafmt) ==
    """
    [ Info: msg
    """
    @test genmsg("line1\nline2", level=Logging.Warn, _module=Base,
                 file="other.jl", line=42, meta_formatter=Logging.default_metafmt) ==
    """
    ┌ Warning: line1
    │ line2
    └ @ Base other.jl:42
    """
    # Full metadata formatting
    @test genmsg("msg", level=Logging.Debug,
                 meta_formatter=(level, _module, group, id, file, line)->
                                (:white,"Foo!", "$level $_module $group $id $file $line")) ==
    """
    ┌ Foo! msg
    └ Debug Main a_group an_id some/path.jl 101
    """

    @testset "Prefix and suffix layout" begin
        @test genmsg("") ==
        replace("""
        ┌ PREFIX EOL
        └ SUFFIX
        """, "EOL"=>"")
        @test genmsg("msg") ==
        """
        ┌ PREFIX msg
        └ SUFFIX
        """
        # Behavior with empty prefix / suffix
        @test genmsg("msg", meta_formatter=(args...)->(:white, "PREFIX", "")) ==
        """
        [ PREFIX msg
        """
        @test genmsg("msg", meta_formatter=(args...)->(:white, "", "SUFFIX")) ==
        """
        ┌ msg
        └ SUFFIX
        """
    end

    @testset "Metadata suffix, right justification" begin
        @test genmsg("xxx", width=20, right_justify=200) ==
        """
        [ PREFIX xxx  SUFFIX
        """
        @test genmsg("xxx\nxxx", width=20, right_justify=200) ==
        """
        ┌ PREFIX xxx
        └ xxx         SUFFIX
        """
        # When adding the suffix would overflow the display width, add it on
        # the next line:
        @test genmsg("xxxx", width=20, right_justify=200) ==
        """
        ┌ PREFIX xxxx
        └             SUFFIX
        """
        # Same for multiline messages
        @test genmsg("""xxx
                        xxxxxxxxxx""", width=20, right_justify=200) ==
        """
        ┌ PREFIX xxx
        └ xxxxxxxxxx  SUFFIX
        """
        @test genmsg("""xxx
                        xxxxxxxxxxx""", width=20, right_justify=200) ==
        """
        ┌ PREFIX xxx
        │ xxxxxxxxxxx
        └             SUFFIX
        """
        # min(right_justify,width) is used
        @test genmsg("xxx", width=200, right_justify=20) ==
        """
        [ PREFIX xxx  SUFFIX
        """
        @test genmsg("xxxx", width=200, right_justify=20) ==
        """
        ┌ PREFIX xxxx
        └             SUFFIX
        """
    end

    # Keywords
    @test genmsg("msg", a=1, b="asdf") ==
    """
    ┌ PREFIX msg
    │   a = 1
    │   b = "asdf"
    └ SUFFIX
    """
    # Exceptions shown with showerror
    @test genmsg("msg", exception=DivideError()) ==
    """
    ┌ PREFIX msg
    │   exception = DivideError: integer division error
    └ SUFFIX
    """

    # Attaching backtraces
    bt = func1()
    @test startswith(genmsg("msg", exception=(DivideError(),bt)),
    """
    ┌ PREFIX msg
    │   exception =
    │    DivideError: integer division error
    │    Stacktrace:
    │      [1] func1()""")


    @testset "Limiting large data structures" begin
        @test genmsg("msg", a=fill(1.00001, 100,100), b=fill(2.00002, 10,10)) ==
        replace("""
        ┌ PREFIX msg
        │   a =
        │    100×100 Matrix{Float64}:
        │     1.00001  1.00001  1.00001  1.00001  …  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
        │     ⋮                                   ⋱                    EOL
        │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
        │   b =
        │    10×10 Matrix{Float64}:
        │     2.00002  2.00002  2.00002  2.00002  …  2.00002  2.00002  2.00002
        │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
        │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
        │     ⋮                                   ⋱                    EOL
        │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
        │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
        └ SUFFIX
        """, "EOL"=>"") # EOL hack to work around git whitespace errors
        # Limiting the amount which is printed
        @test genmsg("msg", a=fill(1.00001, 10,10), show_limited=false) ==
        """
        ┌ PREFIX msg
        │   a =
        │    10×10 Matrix{Float64}:
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        │     1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001  1.00001
        └ SUFFIX
        """
    end

    # Basic colorization test
    @test genmsg("line1\nline2", color=true) ==
    """
    \e[36m\e[1m┌ \e[22m\e[39m\e[36m\e[1mPREFIX \e[22m\e[39mline1
    \e[36m\e[1m│ \e[22m\e[39mline2
    \e[36m\e[1m└ \e[22m\e[39m\e[90mSUFFIX\e[39m
    """

end

end
