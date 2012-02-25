function lut(pal::Vector, a)
    out = similar(a, eltype(pal))
    n = numel(pal)
    for i=1:numel(a)
        out[i] = pal[clamp(a[i], 1, n)]
    end
    out
end

function indexedcolor(data, pal)
    mn = min(data); mx = max(data)
    indexedcolor(data, pal, mx-mn, (mx+mn)/2)
end

function indexedcolor(data, pal, w, l)
    n = numel(pal)-1
    if n == 0
        return fill(pal[1], size(data))
    end
    w_min = l - w/2
    scale = w==0 ? 1 : w/n
    lut(pal, iround((data - w_min)./scale) + 1)
end

const palette_gray32 = uint32([4278190080, 4278716424, 4279242768, 4279769112, 4280295456, 4280887593, 4281413937, 4281940281, 4282466625, 4283058762, 4283585106, 4284111450, 4284637794, 4285164138, 4285756275, 4286282619, 4286808963, 4287335307, 4287927444, 4288453788, 4288980132, 4289506476, 4290032820, 4290624957, 4291151301, 4291677645, 4292203989, 4292796126, 4293322470, 4293848814, 4294375158, 4294967295])

const palette_fire = uint32([4284111450, 4284702808, 4285294423, 4285886038, 4286477397, 4287069012, 4287660627, 4288251985, 4288843600, 4289435215, 4290026574, 4290618189, 4291209804, 4291801162, 4292392777, 4292984392, 4293575751, 4294167366, 4294824517, 4294757442, 4294755904, 4294754365, 4294687291, 4294685752, 4294684214, 4294617139, 4294615601, 4294614062, 4294612524, 4294545449, 4294543911, 4294542372, 4294475298, 4294473759, 4294472221, 4294405146, 4294403608, 4294402069, 4294400531, 4294333456, 4294331918, 4294330379, 4294263305, 4294261766, 4294260228, 4294258946, 4293340673, 4292422401, 4291569665, 4290651649, 4289733377, 4288880641, 4287962369, 4287109889, 4286191617, 4285273344, 4284420864, 4283502592, 4282649856, 4281731584, 4280813568, 4279960832, 4279042560, 4278190080])

const palette_rainbow = uint32([4279125737, 4279064810, 4279004140, 4279009006, 4278948336, 4278953201, 4278892531, 4278897397, 4278836727, 4278841593, 4278644669, 4278513537, 4278382149, 4278251018, 4280086024, 4281921031, 4283756038, 4285591045, 4287491588, 4289326595, 4291161602, 4292996609, 4294897152, 4294759425, 4294687234, 4294615300, 4294543109, 4294471175, 4294398984, 4294327050, 4294254859, 4294182925])

redval(p)   = (p>>>16)&0xff
greenval(p) = (p>>>8)&0xff
blueval(p)  = p&0xff

function ppmwrite(img, file::String)
    s = open(file, "w")

    write(s, "P6\n")
    write(s, "# ppm file written by julia\n")
    n, m = size(img)
    write(s, "$m $n 255\n")
    if ndims(img)==3 && size(img,3)==3
        for i=1:n, j=1:m
            write(s, uint8(img[i,j,1]))
            write(s, uint8(img[i,j,2]))
            write(s, uint8(img[i,j,3]))
        end
    elseif ndims(img)==2 && (is(eltype(img),Int8) || is(eltype(img), Uint8))
        for i=1:n, j=1:m, k = 1:3
            write(s, uint8(img[i,j]))
        end
    elseif is(eltype(img),Int32) || is(eltype(img),Uint32)
        for i=1:n, j=1:m
            p = img[i,j]
            write(s, uint8(redval(p)))
            write(s, uint8(greenval(p)))
            write(s, uint8(blueval(p)))
        end
    else
        error("unsupported array type")
    end

    close(s)
end

# demo:
# m = [ mandel(complex(r,i)) | i=-1:.01:1, r=-2:.01:0.5 ];
# ppmwrite(indexedcolor(m, palette_fire), "mandel.ppm")

function imread(file::String)
    cmd = `convert -format "%w %h" -identify $file rgb:-`
    stream = fdio(read_from(cmd).fd, true)
    spawn(cmd)
    szline = readline(stream)
    spc = strchr(szline, ' ')
    w = parse_int(szline[1:spc-1])
    h = parse_int(szline[spc+1:end-1])
    img = Array(Uint8, h, w, 3)
    for i=1:h, j=1:w
        img[i,j,1] = read(stream, Uint8)
        img[i,j,2] = read(stream, Uint8)
        img[i,j,3] = read(stream, Uint8)
    end
    img
end

function imwrite(I, file::String)
    h, w = size(I)
    cmd = `convert -size $(w)x$(h) -depth 8 rgb:- $file`
    stream = fdio(write_to(cmd).fd, true)
    spawn(cmd)
    if ndims(I)==3 && size(I,3)==3
        for i=1:h, j=1:w
            write(stream, uint8(I[i,j,1]))
            write(stream, uint8(I[i,j,2]))
            write(stream, uint8(I[i,j,3]))
        end
    elseif is(eltype(I),Int32) || is(eltype(I),Uint32)
        for i=1:h, j=1:w
            p = I[i,j]
            write(stream, uint8(redval(p)))
            write(stream, uint8(greenval(p)))
            write(stream, uint8(blueval(p)))
        end
    else
        error("unsupported image data format")
    end
    close(stream)
    wait(cmd)
end

function imshow(img)
    tmp::String = "./tmp.ppm"
    ppmwrite(img, tmp)
    cmd = `feh $tmp`
    spawn(cmd)
end

function rgb2gray(img)
    n, m = size(img)
    red_weight = 0.30
    green_weight = 0.59
    blue_weight = 0.11
    out = Array(Uint8, n, m)
    if ndims(img)==3 && size(img,3)==3
        for i=1:n, j=1:m
            out[i,j] = red_weight*img[i,j,1]+green_weight*img[i,j,2]+blue_weight*img[i,j,3];
        end
    elseif is(eltype(img),Int32) || is(eltype(img),Uint32)
        for i=1:n, j=1:m
            p = img[i,j]
            out[i,j] = red_weight*redval(p)+green_weight*greenval(p)+blue_weight*blueval(p);
        end
    else
        error("unsupported array type")
    end
    out
end

function fspecial(filter::String, filter_size, sigma)
    for i = filter_size
        if mod(i, 2) != 1
            error("filter size must be odd")
        end
    end

    if length(filter_size) == 2
        m = filter_size[1]
        n = filter_size[2]
    elseif length(filter_size) == 1
        m = filter_size
        n = m
    elseif length(filter_size) == 0
        m = 3
        n = 3
    else
        error("filter size must consist of one or two values")
    end

    if filter == "average"
        out = ones(Float, m, n)/(m*n)
    elseif filter == "sobel"
        out = [1.0 2.0 1.0; 0 0 0; -1.0 -2.0 -1.0]
    elseif filter == "prewitt"
        out = [1.0 1.0 1.0; 0 0 0; -1.0 -1.0 -1.0]
    elseif filter == "gaussian"
        out = gaussian2d(sigma, [m n])
    else
        error("unkown filter type")
    end
end

function gaussian2d(sigma, filter_size)
    for i = filter_size
        if mod(i, 2) != 1
            error("filter size must be odd")
        end
    end

    if length(sigma) == 0
        sigma = 0.5
    end

    if length(filter_size) == 2
        m = filter_size[1]
        n = filter_size[2]
    elseif length(filter_size) == 1
        m = filter_size
        n = m
    elseif length(filter_size) == 0
        # choose 'good' size 
        m = 4*ceil(sigma)+1
        n = m
    else
        error("filter size must consist of one or two values")
    end

    X, Y = meshgrid(-floor(n/2):floor(n/2), -floor(m/2):floor(m/2))
    g = exp(-(X.^2+Y.^2)/(2*sigma^2))
    gaussian = g/sum(g)
end
