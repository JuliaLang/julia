# check indented json has same final value as non indented
fb = JSON.parse(facebook)
fbjson1 = json(fb, 2)
fbjson2 = json(fb)
@test JSON.parse(fbjson1) == JSON.parse(fbjson2)

ev = JSON.parse(svg_tviewer_menu)
ejson1 = json(ev, 2)
ejson2 = json(ev)
@test JSON.parse(ejson1) == JSON.parse(ejson2)
