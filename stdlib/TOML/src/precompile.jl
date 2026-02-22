if Base.generating_output()
let
    # Test TOML content
    test_toml = """
    title = "Example"
    with_quotes = \"quoted\"
    number = 42
    float = 3.14
    boolean = true
    date = 2023-01-01
    datetime = 2023-01-01T12:00:00Z
    time = 12:00:00
    array = [1, 2, 3]
    inline = {a=1, b=1.0, c=[1,2,3], d="foo"}

    [nested]
    key = "value"
    """

    test_dict = TOML.parse(test_toml)

    TOML.parse(test_toml)

    io_stream = IOBuffer(test_toml)
    TOML.parse(io_stream)

    mktemp() do path, io
        write(io, test_toml)
        close(io)
        TOML.parsefile(path)
    end

    mktemp() do path, io
        TOML.print(io, test_dict)
        close(io)
    end
end
end
