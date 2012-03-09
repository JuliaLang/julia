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
    if eltype(img) <: Integer
        if ndims(img) == 3 && size(img,3) == 3
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(img[i,j,k]))
            end
        elseif ndims(img) == 2
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(img[i,j]))
            end
        else
            error("unsupported array dimensions")
        end
    elseif eltype(img) <: Float
        if ndims(img) == 3 && size(img,3) == 3
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(255*img[i,j,k]))
            end
        elseif ndims(img) == 2
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(255*img[i,j]))
            end
        else
            error("unsupported array dimensions")
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
    img = Array(Float64, h, w, 3)
    for i=1:h, j=1:w, k = 1:3
        img[i,j,k] = float64(read(stream, Uint8))/255.0
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

function imshow(img, range)
    if ndims(img) == 2 
        # only makes sense for gray scale images
        img = imadjustintensity(img, range)
    end
    tmp::String = "./tmp.ppm"
    ppmwrite(img, tmp)
    cmd = `feh $tmp`
    spawn(cmd)
end

imshow(img) = imshow(img, [])

function imadjustintensity(img, range)
    if length(range) == 0
        range = [min(img) max(img)]
    elseif length(range) == 1
        error("incorrect range")
    end
    tmp = (img - range[1])/(range[2] - range[1])
    tmp[tmp > 1] = 1
    tmp[tmp < 0] = 0
    out = tmp
end

function rgb2gray{T}(img::Array{T,3})
    n, m = size(img)
    weights = [0.30 0.59 0.11] # RGB
    out = Array(T, n, m)
    if ndims(img)==3 && size(img,3)==3
        for i=1:n, j=1:m
            out[i,j] = sum(weights.*squeeze(img[i,j,:])')
        end
    elseif is(eltype(img),Int32) || is(eltype(img),Uint32)
        for i=1:n, j=1:m
            p = img[i,j]
            out[i,j] = sum(weights.*[redval(p) greenval(p) blueval(p)])
        end
    else
        error("unsupported array type")
    end
    out
end

rgb2gray{T}(img::Array{T,2}) = img

function sobel()
    f = [1.0 2.0 1.0; 0.0 0.0 0.0; -1.0 -2.0 -1.0]
    return f, f'
end

function prewitt()
    f = [1.0 1.0 1.0; 0.0 0.0 0.0; -1.0 -1.0 -1.0]
    return f, f'
end

# average filter
function imaverage(filter_size)
    if length(filter_size) != 2
        error("wrong filter size")
    end
    m, n = filter_size[1], filter_size[2]
    if mod(m, 2) != 1 || mod(n, 2) != 1
        error("filter dimensions must be odd")
    end
    f = ones(Float64, m, n)/(m*n)
end

imaverage() = imaverage([3 3])

# laplacian filter kernel
function imlaplacian(diagonals::String)
    if diagonals == "diagonals"
        return [1.0 1.0 1.0; 1.0 -8.0 1.0; 1.0 1.0 1.0]
    elseif diagonals == "nodiagonals"
        return [0.0 1.0 0.0; 1.0 -4.0 1.0; 0.0 1.0 0.0]
    end
end

imlaplacian() = imlaplacian("nodiagonals")

# 2D gaussian filter kernel
function gaussian2d(sigma, filter_size)
    if length(filter_size) == 0
        # choose 'good' size 
        m = 4*ceil(sigma)+1
        n = m
    elseif length(filter_size) != 2
        error("wrong filter size")
    else
        m, n = filter_size[1], filter_size[2]
    end
    if mod(m, 2) != 1 || mod(n, 2) != 1
        error("filter dimensions must be odd")
    end
    g = [exp(-(X.^2+Y.^2)/(2*sigma.^2)) | X=-floor(m/2):floor(m/2), Y=-floor(n/2):floor(n/2)]
    return g/sum(g)
end

gaussian2d(sigma) = gaussian2d(sigma, [])
gaussian2d() = gaussian2d(0.5, [])

# difference of gaussian
function imdog(sigma)
    m = 4*ceil(sqrt(2)*sigma)+1
    return gaussian2d(sqrt(2)*sigma, [m m]) - gaussian2d(sigma, [m m])
end

imdog() = imdog(0.5)

# Sum of squared differences
function ssd{T}(A::Array{T}, B::Array{T})
    return sum((A-B).^2)
end

# normalized by Array size
ssdn{T}(A::Array{T}, B::Array{T}) = ssd(A, B)/numel(A)

# sum of absolute differences
function sad{T}(A::Array{T}, B::Array{T})
    return sum(abs(A-B))
end

# normalized by Array size
sadn{T}(A::Array{T}, B::Array{T}) = sad(A, B)/numel(A)

# normalized cross correlation
function ncc{T}(A::Array{T}, B::Array{T})
    Am = A[:]-mean(A[:])
    Bm = B[:]-mean(B[:])
    res = ((Am/norm(Am))'*(Bm/norm(Bm)))
    return res
end

function imfilter{T}(img::Matrix{T}, filter::Matrix{T}, border::String, value)
    si, sf = size(img), size(filter)
    A = zeros(T, si[1]+sf[1]-1, si[2]+sf[2]-1)
    s1, s2 = int((sf[1]-1)/2), int((sf[2]-1)/2)
    if border == "replicate"
        A[s1+1:end-s1, s2+1:end-s2] = img
        A[s1+1:end-s1, 1:s2] = repmat(img[:,1], 1, s2)
        A[s1+1:end-s1, end-s2+1:end] = repmat(img[:,end], 1, s2)
        A[1:s1, s2+1:end-s2] = repmat(img[1,:], s1, 1)
        A[end-s1+1:end, s2+1:end-s2] = repmat(img[end,:], s1, 1)
        A[1:s1, 1:s2] = fliplr(fliplr(img[1:s1, 1:s2])')
        A[end-s1+1:end, 1:s2] = img[end-s1+1:end, 1:s2]'
        A[1:s1, end-s2+1:end] = img[1:s1, end-s2+1:end]'
        A[end-s1+1:end, end-s2+1:end] = flipud(fliplr(img[end-s1+1:end, end-s2+1:end]))'
    elseif border == "circular"
        A[s1+1:end-s1, s2+1:end-s2] = img
        A[s1+1:end-s1, 1:s2] = img[:, end-s2:end]
        A[s1+1:end-s1, end-s2+1:end] = img[:, 1:s2]
        A[1:s1, s2+1:end-s2] = img[end-s1+1:end, :]
        A[end-s1+1:end, s2+1:end-s2] = img[1:s1, :]
        A[1:s1, 1:s2] = img[end-s1+1:end, end-s2+1:end]
        A[end-s1+1:end, 1:s2] = img[1:s1, end-s2+1:end]
        A[1:s1, end-s2+1:end] = img[end-s1+1:end, 1:s2]
        A[end-s1+1:end, end-s2+1:end] = img[1:s1, 1:s2]
    elseif border == "mirror"
        A[s1+1:end-s1, s2+1:end-s2] = img
        A[s1+1:end-s1, 1:s2] = fliplr(img[:, 1:s2])
        A[s1+1:end-s1, end-s2+1:end] = fliplr(img[:, end-s2:end])
        A[1:s1, s2+1:end-s2] = flipud(img[1:s1, :])
        A[end-s1+1:end, s2+1:end-s2] = flipud(img[end-s1+1:end, :])
        A[1:s1, 1:s2] = fliplr(fliplr(img[1:s1, 1:s2])')
        A[end-s1+1:end, 1:s2] = img[end-s1+1:end, 1:s2]'
        A[1:s1, end-s2+1:end] = img[1:s1, end-s2+1:end]'
        A[end-s1+1:end, end-s2+1:end] = flipud(fliplr(img[end-s1+1:end, end-s2+1:end]))'
    elseif border == "value"
        A += value
        A[s1+1:end-s1, s2+1:end-s2] = img
    else
        error("wrong border treatment")
    end
    # check if separable
    U, S, V = svd(filter)
    separable = true;
    for i = 2:length(S)
        # assumption that <10^-7 \approx 0
        separable = separable && (abs(S[i]) < 10^-7)
    end
    if separable
        C = conv2(squeeze(U[:,1]*sqrt(S[1])), squeeze(V[1,:]*sqrt(S[1])), A)
    else
        C = conv2(A, filter)
    end
    sc = size(C)
    out = C[int(sc[1]/2-si[1]/2):int(sc[1]/2+si[1]/2)-1, int(sc[2]/2-si[2]/2):int(sc[2]/2+si[2]/2)-1]
end

# imfilter for multi channel images
function imfilter{T}(img::Array{T,3}, filter::Matrix{T}, border::String, value)
    x, y, c = size(img)
    out = zeros(T, x, y, c)
    for i = 1:c
        out[:,:,i] = imfilter(squeeze(img[:,:,i]), filter, border, value)
    end
    out
end

imfilter(img, filter) = imfilter(img, filter, "replicate", 0)
imfilter(img, filter, border) = imfilter(img, filter, border, 0)

function imlineardiffusion{T}(img::Array{T,2}, dt::Float, iterations::Integer)
    u = img
    f = imlaplacian()
    for i = dt:dt:dt*iterations
        u = u + dt*imfilter(u, f, "replicate")
    end
    u
end
