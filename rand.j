# http://en.wikipedia.org/wiki/Linear_congruential_generator
function make_rand(seed)
    function rand()
        seed = (22695477*seed + 1) % 4294967296
        interval_01 = seed / 4294967296
        # This is so badly broken, I have no idea
        # At least make it look random for now
        if interval_01 < 0.0
            interval_01 = -interval_01
        end
        interval_01
    end
    return rand
end

rand = make_rand(12345)
