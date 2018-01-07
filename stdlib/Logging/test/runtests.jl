using Logging

import Logging: min_enabled_level, shouldlog, handle_message

@noinline func1() = backtrace()

@testset "Logging" begin

@testset "ConsoleLogger" begin
    # First pass log limiting
    @test min_enabled_level(ConsoleLogger(DevNull, Logging.Debug)) == Logging.Debug
    @test min_enabled_level(ConsoleLogger(DevNull, Logging.Error)) == Logging.Error

    # Second pass log limiting
    logger = ConsoleLogger(DevNull)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === true
    handle_message(logger, Logging.Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === true
    handle_message(logger, Logging.Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Logging.Info, Base, :group, :asdf) === false

    # Log formatting
    function genmsg(level, message, _module, filepath, line; color=false,
                    meta_formatter=Logging.default_metafmt, show_limited=true, kws...)
        buf = IOBuffer()
        io = IOContext(buf, :displaysize=>(30,75), :color=>color)
        logger = ConsoleLogger(io, Logging.Debug,
                               meta_formatter=meta_formatter,
                               show_limited=show_limited)
        Logging.handle_message(logger, level, message, _module, :a_group, :an_id,
                               filepath, line; kws...)
        String(take!(buf))
    end

    # Single line formatting, default metadata
    @test genmsg(Logging.Debug, "msg", Main, "some/path.jl", 101) ==
    """
    [ Debug: msg                                             @ Main path.jl:101
    """
    @test genmsg(Logging.Info, "msg", Main, "some/path.jl", 101) ==
    """
    [ Info: msg                                              @ Main path.jl:101
    """
    @test genmsg(Logging.Warn, "msg", Main, "some/path.jl", 101) ==
    """
    [ Warning: msg                                           @ Main path.jl:101
    """
    @test genmsg(Logging.Error, "msg", Main, "some/path.jl", 101) ==
    """
    [ Error: msg                                             @ Main path.jl:101
    """

    # Configurable metadata formatting
    @test genmsg(Logging.Debug, "msg", Main, "some/path.jl", 101,
                 meta_formatter=(level, _module, group, id, file, line)->
                                ("Foo!", "$level $_module $group $id $file $line")) ==
    """
    [ Foo! msg                        Debug Main a_group an_id some/path.jl 101
    """

    # Empty message string
    @test genmsg(Logging.Error, "", Main, "some/path.jl", 101) ==
    """
    [ Error:                                                 @ Main path.jl:101
    """
    @test genmsg(Logging.Error, "", Main, "some/path.jl", 101, a=1) ==
    replace("""
    ┌ Error: EOL
    │   a = 1
    └                                                        @ Main path.jl:101
    """, "EOL"=>"")

    # Long message line
    @test genmsg(Logging.Error, "msg msg msg msg msg msg msg msg msg msg msg msg msg", Main, "some/path.jl", 101) ==
    """
    ┌ Error: msg msg msg msg msg msg msg msg msg msg msg msg msg
    └                                                        @ Main path.jl:101
    """

    # Multiline messages
    @test genmsg(Logging.Warn, "line1\nline2", Main, "some/path.jl", 101) ==
    """
    ┌ Warning: line1
    │ line2
    └                                                        @ Main path.jl:101
    """

    # Keywords
    @test genmsg(Logging.Error, "msg", Base, "other.jl", 101, a=1, b="asdf") ==
    """
    ┌ Error: msg
    │   a = 1
    │   b = "asdf"
    └                                                       @ Base other.jl:101
    """
    # Exceptions use showerror
    @test genmsg(Logging.Info, "msg", Base, "other.jl", 101, exception=DivideError()) ==
    """
    ┌ Info: msg
    │   exception = DivideError: integer division error
    └                                                       @ Base other.jl:101
    """
    # Attaching backtraces
    bt = func1()
    @test startswith(genmsg(Logging.Info, "msg", Base, "other.jl", 101,
                            exception=(DivideError(),bt)),
    """
    ┌ Info: msg
    │   exception =
    │    DivideError: integer division error
    │    Stacktrace:
    │     [1] func1() at""")


    # Keywords - limiting the amount which is printed
    @test genmsg(Logging.Info, "msg", Main, "some/path.jl", 101,
                 a=fill(1.00001, 100,100), b=fill(2.00002, 10,10)) ==
    replace("""
    ┌ Info: msg
    │   a =
    │    100×100 Array{Float64,2}:
    │     1.00001  1.00001  1.00001  1.00001  …  1.00001  1.00001  1.00001
    │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
    │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
    │     ⋮                                   ⋱                           EOL
    │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
    │     1.00001  1.00001  1.00001  1.00001     1.00001  1.00001  1.00001
    │   b =
    │    10×10 Array{Float64,2}:
    │     2.00002  2.00002  2.00002  2.00002  …  2.00002  2.00002  2.00002
    │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
    │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
    │     ⋮                                   ⋱                           EOL
    │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
    │     2.00002  2.00002  2.00002  2.00002     2.00002  2.00002  2.00002
    └                                                        @ Main path.jl:101
    """, "EOL"=>"") # EOL hack to work around git whitespace errors
    # Limiting the amount which is printed
    @test genmsg(Logging.Info, "msg", Main, "some/path.jl", 101,
                 a=fill(1.00001, 10,10), show_limited=false) ==
    """
    ┌ Info: msg
    │   a =
    │    10×10 Array{Float64,2}:
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
    └                                                        @ Main path.jl:101
    """

    # Basic color test
    @test genmsg(Logging.Info, "line1\nline2", Main, "some/path.jl", 101, color=true) ==
    """
    \e[1m\e[36m┌ \e[39m\e[22m\e[1m\e[36mInfo: \e[39m\e[22mline1
    \e[1m\e[36m│ \e[39m\e[22mline2
    \e[1m\e[36m└ \e[39m\e[22m                                                      \e[90m @ Main path.jl:101\e[39m
    """

end

end
