import Base.Test.@test

using Images 

parapply = Base.parapply_jl

function imfilter{T}(img::Matrix{T}, filter::Matrix{T}, border::String, value::T; num_threads=0)
    sf = size(filter)
    fw = int(([sf...] .- 1) / 2)
    A = padarray(img, fw, fw, border, value)

    m, n = sf
    offsets = int(broadcast(+, [0:m-1] .- floor((m - 1) / 2), ([0:n-1]' .- floor((n - 1) / 2)) * size(A, 1)))''
    I = filter .!= 0
    kernel_values = filter[I]
    kernel_offsets = offsets[I]
    start_index = int(floor(m / 2) + floor(n / 2) * size(A, 1))
    if(num_threads > 0)
        return _pimfilter(A[:], kernel_values, kernel_offsets, size(A, 1), size(A, 2), size(img, 1), size(img, 2), start_index, num_threads=num_threads)
    else
        return _imfilter(A[:], kernel_values, kernel_offsets, size(A, 1), size(A, 2), size(img, 1), size(img, 2), start_index)
    end
end


function _imfilter{T}(A::Vector{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, in_width::Int, out_height::Int, out_width::Int, start_index::Int)
    B = zeros(T, out_height, out_width)
    num_coefficients = length(coefficients)
    for n = 1:out_width
        _pimfilter_core(A,B,coefficients,offsets,in_height,out_height,start_index,n)    
    end
    return B    
end

function _pimfilter{T}(A::Vector{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, in_width::Int, out_height::Int, out_width::Int, start_index::Int; num_threads=2)
    B = zeros(T, out_height, out_width)
    num_coefficients = length(coefficients)
    parapply(_pimfilter_core, (A,B,coefficients,offsets,in_height,out_height,start_index), num_threads, 1:out_width, preapply=false)
    return B
end

function _pimfilter_core{T}(A::Vector{T}, B::Matrix{T}, coefficients::Vector{T}, offsets::Vector{Int}, in_height::Int, out_height::Int, start_index::Int, n::Int)
    num_coefficients = length(coefficients)
        for m = 1:out_height
            index = m + (n - 1) * in_height + start_index;
            sum = zero(T)
            for k = 1:num_coefficients
                @inbounds sum += A[index + offsets[k]] * coefficients[k]
            end
            @inbounds B[m, n] = sum;
        end
    nothing
end



let N = 1024
  A=rand(N,N)
  kern = ones(19,19)

  B = imfilter(A,kern,"replicate",0.0)
  B = imfilter(A,kern,"replicate",0.0,num_threads=1)
  println("imfilter - serial")
  @time B = imfilter(A,kern,"replicate",0.0)
  println("imfilter - 1 thread")
  @time D = imfilter(A,kern,"replicate",0.0,num_threads=1)
  println("imfilter - 2 threads")
  @time C = imfilter(A,kern,"replicate",0.0,num_threads=2)

  @test B == C

end

