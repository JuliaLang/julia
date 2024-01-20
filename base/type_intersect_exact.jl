# This file is a part of Julia. License is MIT: https://julialang.org/license

module TypeIntersectExact

struct OK end

"""
When `OK <: ok`, `R` is the exact type intersection. When `ok <: Union{}`, the
exact type intersection wasn't found and `R` has no significance.
"""
struct Result{R, ok<:OK} end

result(::Type{T}) where {T} = Result{T,       OK     }()
failure()                   = Result{nothing, Union{}}()

"""
    get_result(::Type, ::Result)::Type

Returns the exact type intersection if it was found, otherwise returns the
fallback.
"""
function get_result end

get_result(::Type{Fallback}, ::Result{<:Any, Union{}}) where {Fallback}    = Fallback
get_result(::Type{Fallback}, ::Result{R,     OK     }) where {Fallback, R} = R::Type{R}

"""
    type_intersect_exact(types...)::Result

Finds an exact type intersection or reports failure.
"""
function type_intersect_exact end

type_intersect_exact() = result(Any)::Result

function type_intersect_exact(@nospecialize A::Type)
    Core.@_foldable_meta
    result(A)::Result
end

function type_intersect_exact((@nospecialize A::Type), (@nospecialize B::Type))
    Core.@_foldable_meta
    if A <: B
        result(A)
    elseif B <: A
        result(B)
    else
        let AB = typeintersect(A, B), BA = typeintersect(B, A)
            if (AB <: A) && (AB <: B)
                result(AB)
            elseif (BA <: A) && (BA <: B)
                result(BA)
            else
                failure()
            end
        end
    end::Result
end

function type_intersect_exact(
    (@nospecialize A::Type), (@nospecialize B::Type), (@nospecialize C::Type)
)
    Core.@_foldable_meta  # the loop below doesn't infer as terminating
    candidates = let
        AB = typeintersect(A, B)
        BA = typeintersect(B, A)
        AC = typeintersect(A, C)
        CA = typeintersect(C, A)
        BC = typeintersect(B, C)
        CB = typeintersect(C, B)

        AB_C = typeintersect(AB, C)
        C_AB = typeintersect(C, AB)
        BA_C = typeintersect(BA, C)
        C_BA = typeintersect(C, BA)
        AC_B = typeintersect(AC, B)
        B_AC = typeintersect(B, AC)
        CA_B = typeintersect(CA, B)
        B_CA = typeintersect(B, CA)
        BC_A = typeintersect(BC, A)
        A_BC = typeintersect(A, BC)
        CB_A = typeintersect(CB, A)
        A_CB = typeintersect(A, CB)

        (
            A, B, C,
            AB, BA, AC, CA, BC, CB,
            AB_C, C_AB, BA_C, C_BA, AC_B, B_AC, CA_B, B_CA, BC_A, A_BC, CB_A, A_CB
        )
    end
    for T ∈ candidates
        is_exact = (T <: A) && (T <: B) && (T <: C)
        is_exact && (return result(T)::Result)
    end
    failure()::Result
end

end
