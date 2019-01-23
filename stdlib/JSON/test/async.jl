finished_async_tests = RemoteChannel()

using Sockets

@async begin
    s = listen(7777)
    s = accept(s)

    Base.start_reading(s)

    @test JSON.parse(s) != nothing  # a
    @test JSON.parse(s) != nothing  # b
    validate_c(s)                   # c
    @test JSON.parse(s) != nothing  # d
    validate_svg_tviewer_menu(s)    # svg_tviewer_menu
    @test JSON.parse(s) != nothing  # gmaps
    @test JSON.parse(s) != nothing  # colors1
    @test JSON.parse(s) != nothing  # colors2
    @test JSON.parse(s) != nothing  # colors3
    @test JSON.parse(s) != nothing  # twitter
    @test JSON.parse(s) != nothing  # facebook
    validate_flickr(s)              # flickr
    @test JSON.parse(s) != nothing  # youtube
    @test JSON.parse(s) != nothing  # iphone
    @test JSON.parse(s) != nothing  # customer
    @test JSON.parse(s) != nothing  # product
    @test JSON.parse(s) != nothing  # interop
    validate_unicode(s)             # unicode
    @test JSON.parse(s) != nothing  # issue5
    @test JSON.parse(s) != nothing  # dollars
    @test JSON.parse(s) != nothing  # brackets

    put!(finished_async_tests, nothing)
end

w = connect("localhost", 7777)

@test JSON.parse(a) != nothing
write(w, a)

@test JSON.parse(b) != nothing
write(w, b)

validate_c(c)
write(w, c)

@test JSON.parse(d) != nothing
write(w, d)

validate_svg_tviewer_menu(svg_tviewer_menu)
write(w, svg_tviewer_menu)

@test JSON.parse(gmaps) != nothing
write(w, gmaps)

@test JSON.parse(colors1) != nothing
write(w, colors1)

@test JSON.parse(colors2) != nothing
write(w, colors2)

@test JSON.parse(colors3) != nothing
write(w, colors3)

@test JSON.parse(twitter) != nothing
write(w, twitter)

@test JSON.parse(facebook) != nothing
write(w, facebook)

validate_flickr(flickr)
write(w, flickr)

@test JSON.parse(youtube) != nothing
write(w, youtube)

@test JSON.parse(iphone) != nothing
write(w, iphone)

@test JSON.parse(customer) != nothing
write(w, customer)

@test JSON.parse(product) != nothing
write(w, product)

@test JSON.parse(interop) != nothing
write(w, interop)

validate_unicode(unicode)
write(w, unicode)

# issue #5
issue5 = "[\"A\",\"B\",\"C\\n\"]"
JSON.parse(issue5)
write(w, issue5)

# $ escaping issue
dollars = ["all of the \$s", "µniçø∂\$"]
json_dollars = json(dollars)
@test JSON.parse(json_dollars) != nothing
write(w, json_dollars)

# unmatched brackets
brackets = Dict("foo"=>"ba}r", "be}e]p"=>"boo{p")
json_brackets = json(brackets)
@test JSON.parse(json_brackets) != nothing
write(w, json_dollars)

fetch(finished_async_tests)
