abstract Distribution{T}
abstract DiscreteDistribution{T<:Integer} <: Distribution{T}
abstract ContinuousDistribution{T<:Float} <: Distribution{T}

type Normal{T} <: ContinuousDistribution{T}
    mean::T
    stddev::T
end
const Gaussian = Normal

type Alpha{T} <: ContinuousDistribution{T}
    location::T
    scale::T
end
const Levy = Alpha

type Arcsine{T} <: ContinuousDistribution{T}
end

type Beta{T} <: ContinuousDistribution{T}
    alpha::T
    beta::T
end

type BetaPrime{T} <: ContinuousDistribution{T}
    alpha::T
    beta::T
end

type Cauchy{T} <: ContinuousDistribution{T}
    location::T
    scale::T
end

type Chi{T} <: ContinuousDistribution{T}
    degrees::Int
end

type Chi2{T} <: ContinuousDistribution{T}
    degrees::Int
end

type Erlang{T} <: ContinuousDistribution{T}
    shape::Int
    rate::T
end

type Exponential{T} <: ContinuousDistribution{T}
    rate::T
end

type FDist{T} <: ContinuousDistribution{T}
    num_degrees::Int
    den_degrees::Int
end

type Gamma{T} <: ContinuousDistribution{T}
    alpha::T
    beta::T
end

type Laplace{T} <: ContinuousDistribution{T}
    mean::T
    scale::T
end

type Logistic{T} <: ContinuousDistribution{T}
    mean::T
    scale::T
end

