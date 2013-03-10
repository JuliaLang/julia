import Base.getindex, Base.setindex!, Base.sub, Base.size, Base.copy

## Color spaces
abstract ColorSpace
type CSnil <: ColorSpace
end
type CSgray <: ColorSpace
end
type CSsRGB <: ColorSpace
end
type CSCMYK <: ColorSpace
end
# Color space where each channel is given a name, e.g.,
#   cs = CSNamed("GFP","tdTomato")
# or
#   cs = CSNamed(["GFP","tdTomato"])
type CSNamed <: ColorSpace
    str::Vector{ASCIIString}
end
CSNamed(t...) = CSNamed([t...])  # allow tuple
function getindex(n::CSNamed,ind::Int)
    return n.str[ind]
end
function setindex!(n::CSNamed,value,key)
    n.str[key] = value
end

# The super-type of all images
abstract Image

# General principles:

# Image types implement fields needed to specify an image data array
#   A, and all fields critical to the interpretation of this array. In
#   addition, a "metadata" field is present so that users can store
#   other information as desired.

# The dimensions of the data array fall into 3 general categories:
#   space dimensions (e.g., x and y), time (if the data array
#   represents a sequence of images over time), and channel dimension
#   (e.g., color channel index). It's important to know the storage order of
#   the data array. This is signaled by a storage order string, which
#   introduces a one-character name for each array dimension.  For
#   example, one could create an image out of a data array A[y,x,c] as
#      img = ImageArray(A,"yxc")
#   Images can then be manipulated in "ordinary" ways,
#      imgsnip = img[50:200,100:175,1:3]
#   but also in terms of these coordinate names, e.g.,
#      imsnip = img['x',100:175,'y',50:200]
#   which would yield the same result.
#
#   There are two reserved choices for the array dimension names: 't'
#   (for time) and 'c' (for channel). All other choices are assumed to
#   be spatial. The comparison is case-sensitive, so 'T' and 'C' can
#   be used for spatial axes. Thus, "yxc" might be used for an RGB
#   image with color data in the third array dimension, and "xyzt" for
#   3d grayscale over time.
#
#   Other than the usage of 't' and 'c', the choices of dimension
#   names is arbitrary.  One suggestion is to choose a name that
#   indicates the direction of increasing array index. For example,
#   choosing "br" (for "bottom-right") instead of "yx" might more
#   clearly signal that the upper-left corner (as displayed on the
#   screen) should be A[1,1], and the bottom-right corner is
#   A[sz1,sz2]. In MRI, "RAS" might indicate a standard right-handed
#   coordinate system ('R' = rightward-increasing, 'A' =
#   anterior-increasing, 'S' = superior-increasing). Note that "ASR"
#   would imply the same coordinate system but that the data are
#   stored in [anterior/posterior, superior/inferior, right/left]
#   order.
#
# The remaining "principal" fields are those that are minimally needed
#   to interpret the data array:
#     The spatial geometry is specified by fields that document how
#       indices for the array's spatial dimensions correspond to
#       physical space (any linear relationship is supported, via a
#       transform matrix);
#     The timing information is supplied by a lookup vector specifying
#       the time of each temporal slice;
#     The channel data is specified in terms of a colorspace string
#       ("sRGB"), or a list of strings specifying the meaning of each
#       channel index (e.g., ["GFP","tdTomato"] for a 2-color
#       fluorescence image)

# More detail on spatial specifications:
# arrayi stands for "array index", i.e., the (integer) indices into
#   the data array
# physc means "physical coordinate", i.e., units in actual space (e.g.,
#   position in millimeters)
# arrayi2physc is a n-by-(n+1) transform matrix T; p = T*[a,1] would
#   take a column vector a containing the index coordinates of a
#   single element and convert it into a column vector of physical
#   coordinates. In other words, the separation between adjacent
#   "pixels" along the jth array dimension corresponds to a
#   physical-space displacement of T*[ej,0], where ej is the unit
#   vector along the jth dimension. The right-hand column of T can be
#   used to encode translations, so that the (ficticious) array item
#   A[0,...,0] corresponds to a physical-space origin of coordinates
#   at position T[:,end]. There are utility functions that make
#   it easy to specify T in common cases, e.g.,
#     set_pixel_spacing(im,[0.15 0.15 3])
#   would create a transform matrix for a confocal microscopy stack
#   with 0.15 microns between pixels in the x- and y- dimensions, and
#   3 microns between slices along the z-dimension, with the result
#     T = [0.15 0    0  0;
#          0    0.15 0  0;
#          0    0    3  0]
#       

# A final important task is representing valid pixels, since
#   real-world imaging devices/situations can result in a subset of
#   the data being untrustworthy. In cases where the underlying data
#   type has NaN, you can easily mark the invalid pixels in the data
#   array itself. However, when the data type does not have NaN, the
#   valid field is necessary. Note that when both are possible, the
#   validity of a pixel should be evaluated as !isnan(data) & valid,
#   meaning that marking a pixel as invalid by either NaN or setting
#   valid false suffices.
# The valid field can be set to true (all pixels are trustworthy), a
#   boolean matrix of the size of the data array (marking trustworthy
#   pixels individually), or as a container of arrays whose product
#   would be equal to the full-size valid pixels array. For example,
#      valid = [good_pixels_per_frame,good_frames]
#   where good_pixels_per_frame is a boolean of size [sizex sizey] and
#   good_frames is a boolean of size [1 1 n_frames].


# Utility functions:
# Make sure the storage order string doesn't duplicate any names
function assert_chars_unique(s::ASCIIString)
    ss = sort(b"$s")
    for i = 2:length(ss)
        if ss[i] == ss[i-1]
            error("Array dimension names cannot repeat")
        end
    end
end

function isspatial(s::ASCIIString)
# this returns a vector of bools, is it OK to have the name start
# with "is"?
    indx = trues(length(s))
    matcht = match(r"t",s)
    if !is(matcht,nothing)
        indx[matcht.offset] = false;
    end
    matchc = match(r"c",s)
    if !is(matchc,nothing)
        indx[matchc.offset] = false;
    end
    return indx
end

getminmax(::Type{Uint8}) = [typemin(Uint8),typemax(Uint8)]
getminmax(::Type{Uint16}) = [typemin(Uint16),typemax(Uint16)]
getminmax(::Type{Uint32}) = [typemin(Uint32),typemax(Uint32)]
getminmax(::Type{Int8}) = [typemin(Int8),typemax(Int8)]
getminmax(::Type{Int16}) = [typemin(Int16),typemax(Int16)]
getminmax(::Type{Int32}) = [typemin(Int32),typemax(Int32)]
getminmax(::Type{Float32}) = [typemin(Float32),typemax(Float32)]
getminmax(::Type{Float64}) = [typemin(Float64),typemax(Float64)]

# An image type with all data held in memory
type ImageArray{DType<:Number} <: Image
    data::Array{DType}         # the raw data
    arrayi_order::ASCIIString     # storage order of data array, e.g. "yxc"
    minmax::Vector{DType}      # min and max possible values
    size_ancestor::Vector{Int}    # size of the _original_ array (pre-snip)
    arrayi_range::Vector{Range1{Int}} # vector of ranges (snipping out blocks)
    arrayi2physc::Matrix{Float64} # transform matrix
    physc_unit::Vector            # vector of strings, e.g., "microns"
    physc_name::Vector            # vector of strings, like "X" or "horizontal"
    arrayti2physt::Vector{Float64}# time coordinate lookup table 
    t_unit::String                # time coordinate unit string
    color_space                   # object of type ColorSpace
    valid                         # which pixels can be trusted?
    metadata       # arbitrary metadata, like acquisition date&time, etc.
end
# Empty constructor (doesn't seem to work now, for unknown reason)
ImageArray{DType<:Number}() =
    ImageArray{DType}(Array(DType,0),
                         "",
                         getminmax(DType),
                         Array(Int,0),
                         Array(Range1,0),
                         zeros(0,0),
                         Array(ASCIIString,0),
                         Array(ASCIIString,0),
                         zeros(0),
                         "",
                         "",
                         false,
                         "")
# Construct from a data array, providing defaults for everything
# except the storage order
function ImageArray{DType<:Number}(data::Array{DType},arrayi_order::ASCIIString)
    sz = size(data)
    szv = vcat(sz...)
    n_dims = length(sz)
    if length(arrayi_order) != n_dims
        error("storage order string must have a length equal to the number of dimensions in the array")
    end
    # Enforce uniqueness of each array coordinate name
    assert_chars_unique(arrayi_order)
    # Count # of spatial dimensions
    matcht = match(r"t",arrayi_order)
    matchc = match(r"c",arrayi_order)
    n_spatial_dims = n_dims - !is(matcht,nothing) - !is(matchc,nothing)
    # Set up defaults for other fields
    arrayi_range = map(x->1:x,szv)
    physc_unit = Array(ASCIIString,n_spatial_dims)
    physc_name = Array(ASCIIString,n_spatial_dims)
    physc_unit[1:n_spatial_dims] = ""
    physc_name[1:n_spatial_dims] = ""
    T = [eye(n_spatial_dims) zeros(n_spatial_dims)]
    if !is(matcht,nothing)
        tindex = matcht.offset
        arrayti2physt = linspace(1.0,sz[tindex],sz[tindex])
        t_unit = ""
    else
        arrayti2physt = zeros(0)
        t_unit = ""
    end
    color_space = CSnil
    if !is(matchc,nothing)
        if size(data,matchc.offset) == 1
            color_space = CSgray
        elseif size(data,matchc.offset) == 3
            color_space = CSsRGB
        elseif szv[matchc.offset] == 4
            color_space = CSCMYK
        end
    end
    ImageArray{DType}(data,arrayi_order,getminmax(DType),szv,arrayi_range,T,physc_unit,physc_name,arrayti2physt,t_unit,color_space,true,"")
end

### Copy and getindex functions ###
# Deep copy---copies everything that is immutable
function copy(img::ImageArray)
    ImageArray(copy(img.data),
               copy(img.arrayi_order),
               copy(img.minmax),
               copy(img.size_ancestor),
               img.arrayi_range, # ranges are immutable, or will be
               copy(img.arrayi2physc),
               copy(img.physc_unit),
               copy(img.physc_name),
               copy(img.arrayti2physt),
               copy(img.t_unit),
               copy(img.color_space),
               copy(img.valid),
               copy(img.metadata))
end

# Copy everything but the data and the metadata
function copy_pfields(img::ImageArray)
    ImageArray(img.data,
               copy(img.arrayi_order),
               copy(img.minmax),
               copy(img.size_ancestor),
               img.arrayi_range,
               copy(img.arrayi2physc),
               copy(img.physc_unit),
               copy(img.physc_name),
               copy(img.arrayti2physt),
               copy(img.t_unit),
               copy(img.color_space),
               copy(img.valid),
               img.metadata)
end

# Copy just the data
function copy_data{DType}(image_out::ImageArray{DType},image_in::ImageArray{DType})
    image_out.data = image_in.data[image_out.arrayi_range...]
end

# Copy just the metadata
function copy_metadata{DType}(image_out::ImageArray{DType},image_in::ImageArray{DType})
    image_out.metadata = copy(image_in.metadata)
end

# Private function for converting name/value lists into indices
function _image_named_coords_sub(img::Image,ind...)
    if length(ind) % 2 != 0
        println(ind...)
        error("Coordinate/value must come in pairs")
    end
    # Prepare the coordinates
    sniprange = map(x->1:x,vcat(size(img.data)...))
    for iarg = 1:2:length(ind)
        idim = strchr(img.arrayi_order,ind[iarg])
        if idim == 0
            error(string("Array index name '",ind[iarg],"' does not match any of the names in \"",img.arrayi_order,"\""))
        end
        sniprange[idim] = ind[iarg+1]
    end
    return sniprange
end

# This supports two getindex syntaxes
function getindex(img::ImageArray,ind...)
    if isa(ind[1],Char)
        ## Named getindex syntax: getindex(img,'a',20:50,'b',40:200,...)
        imgret = copy_pfields(img)
        sniprange = _image_named_coords_sub(img,ind...)
        # Do the snip
        imgret.data = img.data[sniprange...]
        imgret.arrayi_range = sniprange
        return imgret
    else
        # Normal getindex syntax: img[20:50,40:200,...]
        imgret = copy_pfields(img)
        imgret.data = getindex(img.data,ind...)
        for i = 1:length(ind)
            imgret.arrayi_range[i] = ind[i]
        end
        return imgret
    end
end
function sub(img::ImageArray,ind...)
    if isa(ind[1],Char)
        ## Named sub syntax: sub(img,'a',20:50,'b',40:200,...)
        imgret = copy_pfields(img)
        sniprange = _image_named_coords_sub(img,ind)
        # Do the snip
        imgret.data = sub(img.data,sniprange...)
        imgret.arrayi_range = sniprange
        return imgret
    else
        # Normal sub syntax: img[20:50,40:200,...]
        imgret = copy_pfields(img)
        imgret.data = sub(img.data,ind...)
        for i = 1:length(ind)
            imgret.arrayi_range[i] = ind[i]
        end
        return imgret
    end
end
function setindex!(img::ImageArray,val,ind...)
    if isa(ind[1],Char)
        ## Named setindex! syntax: setindex!(img,'a',20:50,'b',40:200,...)
        sniprange = _image_named_coords_sub(img,ind)
        # Do the snip
        img.data[sniprange...] = val
    else
        # Normal setindex! syntax: img[20:50,40:200,...]
        imgret = copy_pfields(img)
        img.data[ind...] = val
    end
end


### Utility functions ###
function size(img::ImageArray)
    return size(img.data)
end

function set_pixel_spacing(img::Image,dx::Vector)
    n_spatial_dims = size(img.arrayi2physc,1)
    if n_spatial_dims != length(dx)
        error("Dimensions do not match")
    end
    for idim = 1:n_spatial_dims
        img.arrayi2physc[idim,idim] = dx[idim]
    end
end

function get_pixel_spacing(img::Image)
    n_spatial_dims = size(img.arrayi2physc,1)
    dx = zeros(n_spatial_dims)
    for idim = 1:n_spatial_dims
        dx[idim] = img.arrayi2physc[idim,idim]
    end
    return dx
end


### Manipulations
function permutedims!{DType}(img::ImageArray{DType},perm)
    img.data = permutedims(img.data,perm)
    img.size_ancestor = img.size_ancestor[perm]
    img.arrayi_range = img.arrayi_range[perm]
    # Permute arrayi2physc: first compute the spatial permutation
    flag = isspatial(img.arrayi_order)
    cflag = cumsum(int(flag))
    perm_spatial = cflag[perm[flag[perm]]]
    img.arrayi2physc = [img.arrayi2physc[:,perm_spatial] img.arrayi2physc[:,end]]
    # Finally, permute the storage order string
    img.arrayi_order = img.arrayi_order[perm]
end

#############################################################

function lut(pal::Vector, a)
    out = similar(a, eltype(pal))
    n = length(pal)
    for i=1:length(a)
        out[i] = pal[clamp(a[i], 1, n)]
    end
    out
end

function indexedcolor(data, pal)
    mn = min(data); mx = max(data)
    indexedcolor(data, pal, mx-mn, (mx+mn)/2)
end

function indexedcolor(data, pal, w, l)
    n = length(pal)-1
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

function write_bitmap_data(s, img)
    n, m = size(img)
    if eltype(img) <: Integer
        if ndims(img) == 3 && size(img,3) == 3
            for i=1:n, j=1:m, k=1:3
                write(s, uint8(img[i,j,k]))
            end
        elseif ndims(img) == 2
            if is(eltype(img),Int32) || is(eltype(img),Uint32)
                for i=1:n, j=1:m
                    p = img[i,j]
                    write(s, uint8(redval(p)))
                    write(s, uint8(greenval(p)))
                    write(s, uint8(blueval(p)))
                end
            else
                for i=1:n, j=1:m, k=1:3
                    write(s, uint8(img[i,j]))
                end
            end
        else
            error("unsupported array dimensions")
        end
    elseif eltype(img) <: FloatingPoint
        # prevent overflow
        a = copy(img)
        a[img .> 1] = 1
        a[img .< 0] = 0
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
end

function ppmwrite(img, file::String)
    s = open(file, "w")
    write(s, "P6\n")
    write(s, "# ppm file written by julia\n")
    n, m = size(img)
    write(s, "$m $n 255\n")
    write_bitmap_data(s, img)
    close(s)
end

# demo:
# m = [ mandel(complex(r,i)) for i=-1:.01:1, r=-2:.01:0.5 ];
# ppmwrite(indexedcolor(m, palette_fire), "mandel.ppm")

function imread(file::String)
    cmd = `convert -format "%w %h" -identify $file rgb:-`
    stream, _ = readsfrom(cmd)
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
    if length(file) > 3 && file[end-3:end]==".ppm"
        # fall back to built-in ppmwrite in case convert not available
        return ppmwrite(I, file)
    end
    h, w = size(I)
    cmd = `convert -size $(w)x$(h) -depth 8 rgb: $file`
    stream, _ = writesto(cmd)
    spawn(cmd)
    write_bitmap_data(stream, I)
    close(stream)
    #wait(cmd) # currently missing?
end

function imshow(img, range)
    if ndims(img) == 2 
        # only makes sense for gray scale images
        img = imadjustintensity(img, range)
    end
    imgout_path, imgout = mktemp()
    close(imgout) # just want the path
    imgout_path = imgout_path * ".tiff"
    imwrite(img, imgout_path)
    @osx_only spawn(`open $imgout_path`)
    @linux_only begin
        for checkcmd in (:feh, :gwenview)
            if success(`which $checkcmd` > SpawnNullStream())
                spawn(`$checkcmd $imgout_path`)
                break
            end
        end
    end
end

imshow(img) = imshow(img, [])

function imadjustintensity{T}(img::Array{T,2}, range)
    if length(range) == 0
        range = [min(img) max(img)]
    elseif length(range) == 1
        error("incorrect range")
    end
    tmp = (img - range[1])/(range[2] - range[1])
    tmp[tmp .> 1] = 1
    tmp[tmp .< 0] = 0
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
    g = [exp(-(X.^2+Y.^2)/(2*sigma.^2)) for X=-floor(m/2):floor(m/2), Y=-floor(n/2):floor(n/2)]
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
    return [((x^2+y^2-sigma^2)/sigma^4)*exp(-(x^2+y^2)/(2*sigma^2)) for x=-floor(m/2):floor(m/2), y=-floor(m/2):floor(m/2)]
end

imlog() = imlog(0.5)

# Sum of squared differences
function ssd{T}(A::Array{T}, B::Array{T})
    return sum((A-B).^2)
end

# normalized by Array size
ssdn{T}(A::Array{T}, B::Array{T}) = ssd(A, B)/length(A)

# sum of absolute differences
function sad{T}(A::Array{T}, B::Array{T})
    return sum(abs(A-B))
end

# normalized by Array size
sadn{T}(A::Array{T}, B::Array{T}) = sad(A, B)/length(A)

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
    mid1 = s1+1:s1+si[1]
    mid2 = s2+1:s2+si[2]
    left = 1:s2
    right = size(A,2)-s2+1:size(A,2)
    top = 1:s1
    bot = size(A,1)-s1+1:size(A,1)
    if border == "replicate"
        A[mid1, mid2] = img
        A[mid1, left] = repmat(img[:,1], 1, s2)
        A[mid1, right] = repmat(img[:,end], 1, s2)
        A[top, mid2] = repmat(img[1,:], s1, 1)
        A[bot, mid2] = repmat(img[end,:], s1, 1)
        A[top, left] = fliplr(fliplr(img[top, left])')
        A[bot, left] = img[end-s1+1:end, left]'
        A[top, right] = img[top, end-s2+1:end]'
        A[bot, right] = flipud(fliplr(img[end-s1+1:end, end-s2+1:end]))'
    elseif border == "circular"
        A[mid1, mid2] = img
        A[mid1, left] = img[:, end-s2+1:end]
        A[mid1, right] = img[:, left]
        A[top, mid2] = img[end-s1+1:end, :]
        A[bot, mid2] = img[top, :]
        A[top, left] = img[end-s1+1:end, end-s2+1:end]
        A[bot, left] = img[top, end-s2+1:end]
        A[top, right] = img[end-s1+1:end, left]
        A[bot, right] = img[top, left]
    elseif border == "mirror"
        A[mid1, mid2] = img
        A[mid1, left] = fliplr(img[:, left])
        A[mid1, right] = fliplr(img[:, end-s2+1:end])
        A[top, mid2] = flipud(img[top, :])
        A[bot, mid2] = flipud(img[end-s1+1:end, :])
        A[top, left] = fliplr(fliplr(img[top, left])')
        A[bot, left] = img[end-s1+1:end, left]'
        A[top, right] = img[top, end-s2+1:end]'
        A[bot, right] = flipud(fliplr(img[end-s1+1:end, end-s2+1:end]))'
    elseif border == "value"
        A += value
        A[mid1, mid2] = img
    else
        error("wrong border treatment")
    end
    # check if separable
    U, S, V = svdt(filter)
    separable = true;
    for i = 2:length(S)
        # assumption that <10^-7 \approx 0
        separable = separable && (abs(S[i]) < 1e-7)
    end
    if separable
        # conv2 isn't suitable for this (kernel center should be the actual center of the kernel)
        #C = conv2(U[:,1]*sqrt(S[1]), vec(V[1,:])*sqrt(S[1]), A)
        x = U[:,1]*sqrt(S[1])
        y = vec(V[1,:])*sqrt(S[1])
        sa = size(A)
        m = length(y)+sa[1]
        n = length(x)+sa[2]
        B = zeros(T, m, n)
        B[int(length(x)/2)+1:sa[1]+int(length(x)/2),int(length(y)/2)+1:sa[2]+int(length(y)/2)] = A
        yp = zeros(T, m)
        halfy = int((m-length(y)-1)/2)
        yp[halfy+1:halfy+length(y)] = y
        y = fft(yp)
        xp = zeros(T, n)
        halfx = int((n-length(x)-1)/2)
        xp[halfx+1:halfx+length(x)] = x
        x = fft(xp)
        C = fftshift(ifft(fft(B) .* (y * x.')))
        if T <: Real
            C = real(C)
        end
    else
        #C = conv2(A, filter)
        sa, sb = size(A), size(filter)
        At = zeros(T, sa[1]+sb[1]-1, sa[2]+sb[2]-1)
        Bt = zeros(T, sa[1]+sb[1]-1, sa[2]+sb[2]-1)
        halfa1 = ifloor((size(At,1)-sa[1])/2)
        halfa2 = ifloor((size(At,2)-sa[2])/2)
        halfb1 = ifloor((size(Bt,1)-sb[1])/2)
        halfb2 = ifloor((size(Bt,2)-sb[2])/2)
        At[halfa1+1:halfa1+sa[1], halfa2+1:halfa2+sa[2]] = A
        Bt[halfb1+1:halfb1+sb[1], halfb2+1:halfb2+sb[2]] = filter
        C = fftshift(ifft(fft(At).*fft(Bt)))
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
        out[:,:,i] = imfilter(img[:,:,i], filter, border, value)
    end
    out
end

imfilter(img, filter) = imfilter(img, filter, "replicate", 0)
imfilter(img, filter, border) = imfilter(img, filter, border, 0)

function imlineardiffusion{T}(img::Array{T,2}, dt::FloatingPoint, iterations::Integer)
    u = img
    f = imlaplacian()
    for i = dt:dt:dt*iterations
        u = u + dt*imfilter(u, f, "replicate")
    end
    u
end

function imthresh{T}(img::Array{T,2}, threshold::FloatingPoint)
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
        out[i,j,:] = trans * vec(img[i,j,:])
    end
    return out
end

function ntsc2rgb{T}(img::Array{T})
    trans = [1 0.956 0.621; 1 -0.272 -0.647; 1 -1.106 1.703]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = trans * vec(img[i,j,:])
    end
    return out
end

function rgb2ycbcr{T}(img::Array{T})
    trans = [65.481 128.533 24.966; -37.797 -74.203 112; 112 -93.786 -18.214]
    offset = [16.0; 128.0; 128.0]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = offset + trans * vec(img[i,j,:])
    end
    return out
end

function ycbcr2rgb{T}(img::Array{T})
    trans = inv([65.481 128.533 24.966; -37.797 -74.203 112; 112 -93.786 -18.214])
    offset = [16.0; 128.0; 128.0]
    out = zeros(T, size(img))
    for i = 1:size(img,1), j = 1:size(img,2)
        out[i,j,:] = trans * (vec(img[i,j,:]) - offset)
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
    H[B .> G] = 2*pi - H[B .> G]
    H /= 2*pi
    rgb_sum = R + G + B
    rgb_sum[rgb_sum .== 0] = eps(T)
    S = 1 - 3./(rgb_sum).*min(R, G, B)
    H[S .== 0] = 0
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
    RG = 0 .<= H .< 2*pi/3
    GB = 2*pi/3 .<= H .< 4*pi/3
    BR = 4*pi/3 .<= H .< 2*pi
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
        div_p = backdiffx(p[:,:,1]) + backdiffy(p[:,:,2])
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
        out[:,:,i] = imROF(img[:,:,i], lambda, iterations)
    end
    return out
end
