import Base.Test.@test, Base.parapply

using Images 

function imfilter{T}(img::Matrix{T}, filter::Matrix{T}, border::String, value::T; numthreads=0)
    sf = size(filter)
    fw = int(([sf...] .- 1) / 2)
    A = padarray(img, fw, fw, border, value)

    m, n = sf
    offsets = int(broadcast(+, [0:m-1] .- floor((m - 1) / 2), ([0:n-1]' .- floor((n - 1) / 2)) * size(A, 1)))''
    I = filter .!= 0
    kernel_values = filter[I]
    kernel_offsets = offsets[I]
    start_index = int(floor(m / 2) + floor(n / 2) * size(A, 1))
    if(numthreads > 0)
        return _pimfilter(A[:], kernel_values, kernel_offsets, size(A, 1), size(A, 2), size(img, 1), size(img, 2), start_index, numthreads=numthreads)
    else
        return _imfilter(A[:], kernel_values, kernel_offsets, size(A, 1), size(A, 2), size(img, 1), size(img, 2), start_index)
    end
end


function _imfilter{T}(A::Vector{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, in_width::Int, out_height::Int, out_width::Int, start_index::Int)
    B = zeros(T, out_height, out_width)
    _pimfilter_core(1:out_width,A,B,coefficients,offsets,in_height,out_height,start_index) 
    return B    
end

function _pimfilter{T}(A::Vector{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, in_width::Int, out_height::Int, out_width::Int, start_index::Int; numthreads=2)
    B = zeros(T, out_height, out_width)
    num_coefficients = length(coefficients)
    parapply(_pimfilter_core, 1:out_width, A,B,coefficients,offsets,in_height,out_height,start_index, numthreads=numthreads)
    return B
end

function _pimfilter_core{T}(r, A::Vector{T}, B::Matrix{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, out_height::Int, start_index::Int)
    num_coefficients = length(coefficients)
    for n in r
        for m = 1:out_height
            index = m + (n - 1) * in_height + start_index;
            sum = zero(T)
            for k = 1:num_coefficients
                @inbounds sum += A[index + offsets[k]] * coefficients[k]
            end
            @inbounds B[m, n] = sum;
        end
    end
end



let N = 1024
    A = rand(N,N)
    kern = ones(19,19)

    B = imfilter(A,kern,"replicate",0.0)
    B = imfilter(A,kern,"replicate",0.0,numthreads=1)
    println("imfilter - serial")
    @time B = imfilter(A,kern,"replicate",0.0)
    println("imfilter - 1 thread")
    @time D = imfilter(A,kern,"replicate",0.0,numthreads=1)
    println("imfilter - 2 threads")
    @time C = imfilter(A,kern,"replicate",0.0,numthreads=2)

    @test B == C
end

