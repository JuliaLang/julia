abstract ImageCoordinate
abstract Space <: ImageCoordinate
abstract Time <: ImageCoordinate
abstract Channel <: ImageCoordinate

abstract Image
# Defines an image type with all data held in memory
type ImageArray{T<:Number} <: Image
    data::Array{T}
    coordinate_types::Vector{ImageCoordinate}
    coordinate_units::Vector{Any}  # vector of strings, "microns" or I"\mu m"
    coordinate_names::Vector{Any}  # vector of strings, "X" or "Y"
    coordinate_ranges::Vector{Range1}
    space_directions::Matrix{Float64}
    valid::Array{Any,1}     # can be used to store bad frame/pixel data
    metadata::CompositeKind  # arbitrary metadata, like acquisition time, etc.

    ImageArray{T}() = new()
end
function ImageArray{T<:Number}(dat::Array{T})
    ret = ImageArray{T}()
    ret.data = dat
    n_dims = ndims(dat)
    ret.coordinate_types = cell(n_dims)
    ret.coordinate_types[1:n_dims] = Space
    ret.coordinate_units = cell(n_dims)
    ret.coordinate_units[1:n_dims] = ""
    ret.coordinate_names = cell(n_dims)
    ret.coordinate_names[1:n_dims] = ""
    ret.space_directions = eye(n_dims)
    ret.valid = cell(n_dims)
    onec = cell(n_dims)
    onec[1:n_dims] = 1
    for idim = 1:n_dims
        sz = copy(onec)
        sz[idim] = size(dat,idim)
        ret.valid[idim] = trues(tuple(sz...))
    end
    return ret
end
function ndims(img::ImageArray)
    return length(img.coordinate_types)
end
function set_orthogonal_spacing(img::Image,s::Vector)
    n_dims = ndims(img)
    if n_dims != length(s)
        error("Dimensions do not match")
    end
    for idim = 1:n_dims
        img.space_directions[idim,idim] = s[idim]
    end
    return img
end


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
        # prevent overflow
        a = copy(img)
        a[img > 1] = 1
        a[img < 0] = 0
        if ndims(a) == 3 && size(a,3) == 3
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(255*a[i,j,k]))
            end
        elseif ndims(a) == 2
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(255*a[i,j]))
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

function imadjustintensity{T}(img::Array{T,2}, range)
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
    wr, wg, wb = 0.30, 0.59, 0.11
    out = Array(T, n, m)
    if ndims(img)==3 && size(img,3)==3
        for i=1:n, j=1:m
            out[i,j] = wr*img[i,j,1] + wg*img[i,j,2] + wb*img[i,j,3]
        end
    elseif is(eltype(img),Int32) || is(eltype(img),Uint32)
        for i=1:n, j=1:m
            p = img[i,j]
            out[i,j] = wr*redval(p) + wg*greenval(p) + wb*blueval(p)
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

# more general version
function imlaplacian(alpha::Number)
    lc = alpha/(1 + alpha)
    lb = (1 - alpha)/(1 + alpha)
    lm = -4/(1 + alpha)
    return [lc lb lc; lb lm lb; lc lb lc]
end

# 2D gaussian filter kernel
function gaussian2d(sigma::Number, filter_size)
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

gaussian2d(sigma::Number) = gaussian2d(sigma, [])
gaussian2d() = gaussian2d(0.5, [])

# difference of gaussian
function imdog(sigma::Number)
    m = 4*ceil(sqrt(2)*sigma)+1
    return gaussian2d(sqrt(2)*sigma, [m m]) - gaussian2d(sigma, [m m])
end

imdog() = imdog(0.5)

# laplacian of gaussian
function imlog(sigma::Number)
    m = 4*ceil(sigma)+1
    return [((x^2+y^2-sigma^2)/sigma^4)*exp(-(x^2+y^2)/(2*sigma^2)) | x=-floor(m/2):floor(m/2), y=-floor(m/2):floor(m/2)]
end

imlog() = imlog(0.5)

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
    Am = (A-mean(A))[:]
    Bm = (B-mean(B))[:]
    return dot(Am,Bm)/(norm(Am)*norm(Bm))
end

function imfilter{T}(img::Matrix{T}, filter::Matrix{T}, border::String, value)
    si, sf = size(img), size(filter)
    A = zeros(T, si[1]+sf[1]-1, si[2]+sf[2]-1)
    s1, s2 = int((sf[1]-1)/2), int((sf[2]-1)/2)
    # correlation instead of convolution
    filter = fliplr(fliplr(filter).')
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
        # conv2 isn't suitable for this (kernel center should be the actual center of the kernel)
        #C = conv2(squeeze(U[:,1]*sqrt(S[1])), squeeze(V[1,:]*sqrt(S[1])), A)
        x = squeeze(U[:,1]*sqrt(S[1]))
        y = squeeze(V[1,:]*sqrt(S[1]))
        sa = size(A)
        m = length(y)+sa[1]
        n = length(x)+sa[2]
        B = zeros(T, m, n)
        B[int((length(x))/2)+1:sa[1]+int((length(x))/2),int((length(y))/2)+1:sa[2]+int((length(y))/2)] = A
        y = fft([zeros(T,int((m-length(y)-1)/2)); y; zeros(T,int((m-length(y)-1)/2))])./m
        x = fft([zeros(T,int((m-length(x)-1)/2)); x; zeros(T,int((n-length(x)-1)/2))])./n
        C = fftshift(ifft2(fft2(B) .* (y * x.')))
        if T <: Real
            C = real(C)
        end
    else
        #C = conv2(A, filter)
        sa, sb = size(A), size(filter)
        At = zeros(T, sa[1]+sb[1], sa[2]+sb[2])
        Bt = zeros(T, sa[1]+sb[1], sa[2]+sb[2])
        At[int(end/2-sa[1]/2)+1:int(end/2+sa[1]/2), int(end/2-sa[2]/2)+1:int(end/2+sa[2]/2)] = A
        Bt[int(end/2-sb[1]/2)+1:int(end/2+sb[1]/2), int(end/2-sb[2]/2)+1:int(end/2+sb[2]/2)] = filter
        C = fftshift(ifft2(fft2(At).*fft2(Bt))./((sa[1]+sb[1]-1)*(sa[2]+sb[2]-1)))
        if T <: Real
            C = real(C)
        end
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

function imthresh{T}(img::Array{T,2}, threshold::Float)
    if !(0.0 <= threshold <= 1.0)
        error("threshold must be between 0 and 1")
    end
    img_max, img_min = max(img), min(img)
    tmp = zeros(T, size(img))
    # matter of taste?
    #tmp[img >= threshold*(img_max-img_min)+img_min] = 1
    tmp[img >= threshold] = 1
    return tmp
end

function imgaussiannoise{T}(img::Array{T}, variance::Number, mean::Number)
    return img + sqrt(variance)*randn(size(img)) + mean
end

imgaussiannoise{T}(img::Array{T}, variance::Number) = imgaussiannoise(img, variance, 0)
imgaussiannoise{T}(img::Array{T}) = imgaussiannoise(img, 0.01, 0)

# 'illustrates' fourier transform
ftshow{T}(A::Array{T,2}) = imshow(log(1+abs(fftshift(A))),[])

function rgb2ntsc{T}(img::Array{T})
    trans = [0.299 0.587 0.114; 0.596 -0.274 -0.322; 0.211 -0.523 0.312]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = trans * squeeze(img[i,j,:])
    end
    return out
end

function ntsc2rgb{T}(img::Array{T})
    trans = [1 0.956 0.621; 1 -0.272 -0.647; 1 -1.106 1.703]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = trans * squeeze(img[i,j,:])
    end
    return out
end

function rgb2ycbcr{T}(img::Array{T})
    trans = [65.481 128.533 24.966; -37.797 -74.203 112; 112 -93.786 -18.214]
    offset = [16.0; 128.0; 128.0]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = offset + trans * squeeze(img[i,j,:])
    end
    return out
end

function ycbcr2rgb{T}(img::Array{T})
    trans = inv([65.481 128.533 24.966; -37.797 -74.203 112; 112 -93.786 -18.214])
    offset = [16.0; 128.0; 128.0]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = trans * (squeeze(img[i,j,:]) - offset)
    end
    return out
end

function imcomplement{T}(img::Array{T})
    return 1 - img
end

function rgb2hsi{T}(img::Array{T})
    R = img[:,:,1]
    G = img[:,:,2]
    B = img[:,:,3]
    H = acos((1/2*(2*R - G - B)) ./ (((R - G).^2 + (R - B).*(G - B)).^(1/2)+eps(T))) 
    H[B > G] = 2*pi - H[B > G]
    H /= 2*pi
    rgb_sum = R + G + B
    rgb_sum[rgb_sum == 0] = eps(T)
    S = 1 - 3./(rgb_sum).*min(R, G, B)
    H[S == 0] = 0
    I = 1/3*(R + G + B)
    return cat(3, H, S, I)
end

function hsi2rgb{T}(img::Array{T})
    H = img[:,:,1]*(2pi)
    S = img[:,:,2]
    I = img[:,:,3]
    R = zeros(T, size(img,1), size(img,2))
    G = zeros(T, size(img,1), size(img,2))
    B = zeros(T, size(img,1), size(img,2))
    RG = 0 <= H < 2*pi/3
    GB = 2*pi/3 <= H < 4*pi/3
    BR = 4*pi/3 <= H < 2*pi
    # RG sector
    B[RG] = I[RG].*(1 - S[RG])
    R[RG] = I[RG].*(1 + (S[RG].*cos(H[RG]))./cos(pi/3 - H[RG]))
    G[RG] = 3*I[RG] - R[RG] - B[RG]
    # GB sector
    R[GB] = I[GB].*(1 - S[GB])
    G[GB] = I[GB].*(1 + (S[GB].*cos(H[GB] - pi/3))./cos(H[GB]))
    B[GB] = 3*I[GB] - R[GB] - G[GB]
    # BR sector
    G[BR] = I[BR].*(1 - S[BR])
    B[BR] = I[BR].*(1 + (S[BR].*cos(H[BR] - 2*pi/3))./cos(-pi/3 - H[BR]))
    R[BR] = 3*I[BR] - G[BR] - B[BR]
    return cat(3, R, G, B)
end

function imstretch{T}(img::Array{T,2}, m::Number, slope::Number)
    return 1./(1 + (m./(img + eps(T))).^slope)
end

function imedge{T}(img::Array{T}, method::String, border::String)
    # needs more methods
    if method == "sobel"
        s1, s2 = sobel()
        img1 = imfilter(img, s1, border)
        img2 = imfilter(img, s2, border)
        return img1, img2, sqrt(img1.^2 + img2.^2), atan2(img2, img1)
    elseif method == "prewitt"
        s1, s2 = prewitt()
        img1 = imfilter(img, s1, border)
        img2 = imfilter(img, s2, border)
        return img1, img2, sqrt(img1.^2 + img2.^2), atan2(img2, img1)
    end
end

imedge{T}(img::Array{T}, method::String) = imedge(img, method, "replicate")
imedge{T}(img::Array{T}) = imedge(img, "sobel", "replicate")

# forward and backward differences 
# can be very helpful for discretized continuous models 
forwarddiffy{T}(u::Array{T,2}) = [u[2:end,:]; u[end,:]] - u
forwarddiffx{T}(u::Array{T,2}) = [u[:,2:end] u[:,end]] - u
backdiffy{T}(u::Array{T,2}) = u - [u[1,:]; u[1:end-1,:]]
backdiffx{T}(u::Array{T,2}) = u - [u[:,1] u[:,1:end-1]]

function imROF{T}(img::Array{T,2}, lambda::Number, iterations::Integer)
    # Total Variation regularized image denoising using the primal dual algorithm
    # Also called Rudin Osher Fatemi (ROF) model
    # lambda: regularization parameter
    s1, s2 = size(img)
    p = zeros(T, s1, s2, 2)
    u = zeros(T, s1, s2)
    grad_u = zeros(T, s1, s2, 2)
    div_p = zeros(T, s1, s2)
    dt = lambda/4
    for i = 1:iterations
        div_p = backdiffx(squeeze(p[:,:,1])) + backdiffy(squeeze(p[:,:,2]))
        u = img + div_p/lambda
        grad_u = cat(3, forwarddiffx(u), forwarddiffy(u))
        grad_u_mag = sqrt(grad_u[:,:,1].^2 + grad_u[:,:,2].^2)
        tmp = 1 + grad_u_mag*dt
        p = (dt*grad_u + p)./cat(3, tmp, tmp)
    end
    return u
end

# ROF Model for color images
function imROF{T}(img::Array{T,3}, lambda::Number, iterations::Integer)
    out = zeros(T, size(img))
    for i = 1:size(img, 3)
        out[:,:,i] = imROF(squeeze(img[:,:,i]), lambda, iterations)
    end
    return out
end
