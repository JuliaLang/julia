# Statistics

```@meta
DocTestSetup = :(using Statistics)
```

The Statistics module contains basic statistics functionality: mean, median, quantiles,
standard deviation, variance, skewness, kurtosis, correlation and covariance.
Statistics can be weighted, and several weights types are distinguished to apply appropriate
corrections where necessary.

## Mean, median and quantiles

```@docs
Statistics.mean
Statistics.mean!
Statistics.median
Statistics.median!
Statistics.middle
Statistics.quantile
Statistics.quantile!
```

## Moments

```@docs
Statistics.std
Statistics.stdm
Statistics.var
Statistics.varm
Statistics.skewness
Statistics.kurtosis
```

## Correlation and covariance

```@docs
Statistics.cor
Statistics.cov
```

## Weights types

Four statistical weights types are provided which inherit from the `AbstractWeights` type:

- `Weights` is a generic type for arbitary weights. Using this type will trigger an error
  with functions which rely on assumptions about a particular definition of weights.
- `AnalyticWeights` describe the relative importance for each observation.
  These weights may also be referred to as reliability weights, precision weights
  or inverse variance weights. These are typically used when the observations
  are aggregate values (e.g. averages) with differing variances.
- `FrequencyWeights` describe the number of times (or frequency) each observation
  was observed. These weights may also be referred to as case weights or repeat weights.
- `ProbabilityWeights` represent the inverse of the sampling probability
  for each observation, providing a correction mechanism for under- or over-sampling
  certain population groups. These weights may also be referred to as sampling weights.

The choice of weights impacts how bias is corrected in several methods.
See the [`var`](@ref), [`std`](@ref), [`cov`](@ref) and [`quantile`](@ref)
docstrings for more details.

Short-hand constructors `weights`, `aweights`, `fweights` and `pweights`
are provided for convenience.

!!! note
    - The weight vector is a light-weight wrapper of the input vector.
      The input vector is NOT copied during construction.
    - The weight vector maintains the sum of weights, which is computed upon construction.
      If the value of the sum is pre-computed, one can supply it as the second argument
      to the constructor and save the time of computing the sum again.

```@docs
Statistics.AbstractWeights
Statistics.Weights
Statistics.AnalyticWeights
Statistics.FrequencyWeights
Statistics.ProbabilityWeights
Statistics.weights
Statistics.aweights
Statistics.fweights
Statistics.pweights
```

```@meta
DocTestSetup = nothing
```
