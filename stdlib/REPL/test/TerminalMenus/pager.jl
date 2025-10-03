# This file is a part of Julia. License is MIT: https://julialang.org/license

content =
    """
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
    incididunt ut labore et dolore magna aliqua. Arcu non sodales neque sodales.
    Placerat orci nulla pellentesque dignissim enim sit amet venenatis. Mauris
    augue neque gravida in fermentum et sollicitudin. Amet venenatis urna cursus
    eget. Enim praesent elementum facilisis leo vel fringilla est. Vitae sapien
    pellentesque habitant morbi tristique. Ornare lectus sit amet est placerat in.
    Leo urna molestie at elementum eu facilisis. Aliquam vestibulum morbi blandit
    cursus risus at ultrices. Id aliquet lectus proin nibh. Facilisi etiam
    dignissim diam quis enim lobortis scelerisque fermentum. Pretium lectus quam id
    leo in vitae turpis massa sed. Elementum facilisis leo vel fringilla est.
    Vulputate ut pharetra sit amet aliquam. Quis enim lobortis scelerisque
    fermentum dui faucibus in ornare. Cursus turpis massa tincidunt dui ut.

    A arcu cursus vitae congue mauris rhoncus. Tellus rutrum tellus pellentesque
    eu. Fringilla phasellus faucibus scelerisque eleifend donec pretium. Aliquam
    etiam erat velit scelerisque. Volutpat lacus laoreet non curabitur gravida.
    Felis imperdiet proin fermentum leo vel orci. Viverra tellus in hac habitasse
    platea dictumst vestibulum rhoncus est. Ullamcorper dignissim cras tincidunt
    lobortis feugiat vivamus. Sit amet luctus venenatis lectus. Odio facilisis
    mauris sit amet massa vitae tortor condimentum. Purus sit amet volutpat
    consequat mauris nunc congue. Enim nunc faucibus a pellentesque sit amet. Purus
    non enim praesent elementum facilisis leo vel fringilla est.
    """ |> strip

let p = Pager(content)
    @test p.pagesize == 10
    @test length(p.lines) == 22
    @test startswith(content, p.lines[1])
    @test endswith(content, p.lines[end])
    buffer = IOBuffer()
    TerminalMenus.printmenu(buffer, p, 1)
    str = String(take!(buffer))
    @test contains(str, "(10 / 22)  45%")
    @test endswith(str, "leo in vitae turpis massa sed. Elementum facilisis leo vel fringilla est.")
end
