#inclde("extras/tk.jl")
#import Tk.*
#import Cairo.*

function life_rule(new, old)
    for j = 2:size(old,2)-1
        for i = 2:size(old,1)-1
            nc = +(old[i-1,j-1], old[i-1,j], old[i-1,j+1],
                   old[i  ,j-1],             old[i  ,j+1],
                   old[i+1,j-1], old[i+1,j], old[i+1,j+1])

            new[i-1,j-1] = (nc == 3 ? 1 :
                            nc == 2 ? old[i,j] :
                            0)
        end
    end
    new
end

function life_step(d)
    DArray(size(d),[2:nprocs()]) do I
        m, n = length(I[1]), length(I[2])
        # fetch neighborhood - toroidal boundaries
        old = Array(Bool, m+2, n+2)
        top   = mod(first(I[1])-2,size(d,1))+1
        bot   = mod( last(I[1])  ,size(d,1))+1
        left  = mod(first(I[2])-2,size(d,2))+1
        right = mod( last(I[2])  ,size(d,2))+1

        if top < bot && left < right
            old[:, :] = d[top:bot, left:right]
        else
            old[2:end-1, 2:end-1] = d[I...]

            # top
            old[1  , 2:end-1] = d[top, I[2]]
            # bottom
            old[end, 2:end-1] = d[bot, I[2]]

            # sides
            if top < bot
                old[:, 1  ] = d[top:bot, left]
                old[:, end] = d[top:bot, right]
            else
                old[1      , 1  ] = d[top , left]
                old[2:end-1, 1  ] = d[I[1], left]
                old[end    , 1  ] = d[bot , left]
                old[1      , end] = d[top , right]
                old[2:end-1, end] = d[I[1], right]
                old[end    , end] = d[bot , right]
            end
        end

        life_rule(Array(Bool, m, n), old)
    end
end

function plife(m, n)
    w = Window("parallel life", n, m)
    c = Canvas(w)
    pack(c)
    done = false
    c.mouse.button1press = (c,x,y)->(done=true)
    cr = cairo_context(c)

    grid = DArray(I->randbool(map(length,I)), (m, n), [2:nprocs()])
    f = 1
    t0 = last = time()
    while !done
        @async begin
            # this went from 5.5 to 8 FPS just by adding @async
            # (at 500x500 with 9 remote cpus)
            img = convert(Array,grid) .* 0x00ffffff
            set_source_surface(cr, CairoRGBSurface(img), 0, 0)
            paint(cr)
            reveal(c)
        end
        t = time()
        if t-last > 2
            println("$(f/(t-t0)) FPS")
            last = t
        end
        f+=1
        grid = life_step(grid)
        #sleep(0.03)
    end
end
