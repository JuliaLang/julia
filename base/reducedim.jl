
# for reductions that expand 0 dims to 1
reduced_dims(A, region) = ntuple(ndims(A), i->(in(i, region) ? 1 :
                                               size(A,i)))

# keep 0 dims in place
reduced_dims0(A, region) = ntuple(ndims(A), i->(size(A,i)==0 ? 0 :
                                                in(i, region) ? 1 :
                                                size(A,i)))

reducedim(f::Function, A, region, v0) =
    reducedim(f, A, region, v0, similar(A, reduced_dims(A, region)))

maximum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : reducedim(scalarmax,A,region,typemin(T))
minimum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : reducedim(scalarmin,A,region,typemax(T))
sum{T}(A::AbstractArray{T}, region)  = reducedim(+,A,region,zero(T))
prod{T}(A::AbstractArray{T}, region) = reducedim(*,A,region,one(T))

all(A::AbstractArray{Bool}, region) = reducedim(&,A,region,true)
any(A::AbstractArray{Bool}, region) = reducedim(|,A,region,false)
sum(A::AbstractArray{Bool}, region) = reducedim(+,A,region,0,similar(A,Int,reduced_dims(A,region)))

prod(A::AbstractArray{Bool}, region) =
    error("use all() instead of prod() for boolean arrays")

    