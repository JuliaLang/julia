# http://en.wikipedia.org/wiki/Linear_congruential_generator
function make_rand(seed)
    function rand()
        seed = (22695477*seed + 1) % 4294967296
        #interval_01 = seed / 4294967296.0
    end
    return rand
end

rand = make_rand(12345)
