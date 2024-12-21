const features = Set{String}([
    "features",
])

"""
    has_feature(feature::String)

Checks if this version of Julia has the given feature.
"""
@assume_effects :foldable has_feature(feature::String) = feature in features
