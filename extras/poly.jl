
type Polynomial{T<:Number}
    a::Vector{T}
end

# allowing Int based polynomial gives all sorts of bad results below,
# so following the spirit of / of Integers, we promote to float64 automatically
Polynomial{T<:Int}(a::Vector{T}) = Polynomial(float64(a)) 

length(p::Polynomial) = length(p.a)

function zero{T}(p::Polynomial{T})
    Polynomial([zero(T)])
end

function one{T}(p::Polynomial{T})
    Polynomial([one(T)])
end

function show(p::Polynomial)
    n = length(p)
    print("Polynomial(")
    if n > 0
        for i = 1:n-1
            if p.a[i] != 0
                print(p.a[i])
                print("x^")
                print(n-i)
                print(" + ")
            end
        end
        print(p.a[n])
    else
        print("0")
    end
    print(")")
end

ref(p::Polynomial, i) = p.a[i]
assign(p::Polynomial, v, i) = (p.a[i] = v)

function copy(p::Polynomial)
    Polynomial(copy(p.a))
end

*(c::Number, p::Polynomial) = Polynomial(c * p.a)
*(p::Polynomial, c::Number) = Polynomial(c * p.a)
/(p::Polynomial, c::Number) = Polynomial(p.a / c)

function +{T,S}(p1::Polynomial{T}, p2::Polynomial{S})
    R = promote_type(T,S)
    n = length(p1)
    m = length(p2)
    if n > m
        a = Array(R, n)
        for i = 1:m
            a[n-m+i] = p1.a[n-m+i] + p2.a[i]
        end
        for i = 1:n-m
            a[i] = p1.a[i]
        end
    else
        a = Array(R, m)
        for i = 1:n
            a[m-n+i] = p1.a[i] + p2.a[m-n+i]
        end
        for i = 1:m-n
            a[i] = p2.a[i]
        end
    end
    Polynomial(a)
end

function -{T,S}(p1::Polynomial{T}, p2::Polynomial{S})
    R = promote_type(T,S)
    n = length(p1)
    m = length(p2)
    if n > m
        a = Array(R, n)
        for i = 1:m
            a[n-m+i] = p1.a[n-m+i] - p2.a[i]
        end
        for i = 1:n-m
            a[i] = p1.a[i]
        end
    else
        a = Array(R, m)
        for i = 1:n
            a[m-n+i] = p1.a[i] - p2.a[m-n+i]
        end
        for i = 1:m-n
            a[i] = -p2.a[i]
        end
    end
    Polynomial(a)
end

function *{T,S}(p1::Polynomial{T}, p2::Polynomial{S})
    R = promote_type(T,S)
    n = length(p1)
    m = length(p2)
    a = zeros(R, n+m-1)
    for i = 1:length(p1)
        for j = 1:length(p2)
            a[i+j-1] += p1.a[i] * p2.a[j]
        end
    end
    Polynomial(a)
end

polyval(p::Polynomial, x::Number) = polyval(p.a, x)

function polyval(a::AbstractVector, x::Number)
    y = a[1]
    for i = 2:length(a)
        y = a[i] + x.*y
    end
    return y
end

function polyval(a::AbstractVector, x::AbstractVector)
    y = zeros(size(x))
    for i = 1:length(x)
        y[i] = polyval(a, x[i])
    end
    return y
end

polyint(p::Polynomial, k::Number) = polyint(p.a, k)
polyint(p::Polynomial) = polyint(p.a, 0)
function polyint{T}(a::AbstractVector{T}, k::Number)
    n = length(a)
    a2 = Array(T, n+1)
    for i = 1:n
        a2[i] = a[i] / (n-i+1)
    end
    a2[end] = k
    Polynomial(a2)
end
polyint(a::AbstractVector) = polyint(a, 0)

polydir(p::Polynomial) = polydir(p.a)
function polydir{T}(a::AbstractVector{T})
    n = length(a)
    if n > 0
        a2 = Array(T, n-1)
        for i = 1:n-1
            a2[i] = a[i] * (n-i)
        end
    else
        a2 = Array(T, 0)
    end
    Polynomial(a2)
end

function poly{T}(r::AbstractVector{T})
    n = length(r)
    c = zeros(T, n+1)
    c[1] = 1
    for j = 1:n
        c[2:j+1] = c[2:j+1]-r[j]*c[1:j]
    end
    return Polynomial(c)
end
poly(A::Matrix) = poly(eig(A)[1])

function roots{T}(p::Polynomial{T})
    num_zeros = 0
    if length(p) == 0 return Array(T,0) end
    while p[end-num_zeros] == 0
        if num_zeros == length(p)-1
            return Array(T, 0)
        end
        num_zeros += 1
    end
    n = length(p)-num_zeros-1
    if n < 1
        return zeros(T, length(p)-1)
    end
    companion = zeros(T, n, n)
    a0 = p[end-num_zeros]
    for i = 1:n-1
        companion[1,i] = -p.a[end-num_zeros-i] / a0
        companion[i+1,i] = 1;
    end
    companion[1,end] = -p.a[1] / a0
    D,V = eig(companion)
    T_r = typeof(real(D[1]))
    T_i = typeof(imag(D[1]))
    if all(imag(D) < 2*eps(T_i))
        r = zeros(T_r, length(p)-1)
        r[1:n] = 1./real(D)
        return r
    else
        r = zeros(typeof(D[1]),length(p)-1)
        r[1:n] = 1./D
        return r
    end
end


