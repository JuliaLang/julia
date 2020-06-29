struct MethodMatchInfo
    applicable::Any
    ambig::Bool
end

struct UnionSplitInfo
    # TODO: In principle we shouldn't have to store this, but could just
    # recompute it using `switchtuple` union. However, it is not the case
    # that if T == S, then switchtupleunion(T) == switchtupleunion(S), e.g. for
    # T = Tuple{Tuple{Union{Float64, Int64},String}}
    # S = Tuple{Union{Tuple{Float64, String}, Tuple{Int64, String}}}
    sigs::Vector{Any}
    matches::Vector{MethodMatchInfo}
end

struct CallMeta
    rt::Any
    info::Any
end
