using Base.Bottom
using Base.Test

macro UnionAll(var, expr)
    lb = :Bottom
    ub = :Any
    if isa(var,Expr) && var.head === :comparison
        if length(var.args) == 3
            v = var.args[1]
            if var.args[2] == :(<:)
                ub = esc(var.args[3])
            elseif var.args[2] == :(>:)
                lb = esc(var.args[3])
            else
                error("invalid bounds in UnionAll")
            end
        elseif length(var.args) == 5
            v = var.args[3]
            if var.args[2] == var.args[4] == :(<:)
                lb = esc(var.args[1])
                ub = esc(var.args[5])
            else
                error("invalid bounds in UnionAll")
            end
        else
            error("invalid bounds in UnionAll")
        end
    elseif isa(var,Expr) && var.head === :(<:)
        v = var.args[1]
        ub = esc(var.args[2])
    elseif isa(var,Expr) && var.head === :(>:)
        v = var.args[1]
        lb = esc(var.args[2])
    elseif !isa(var,Symbol)
        error("invalid variable in UnionAll")
    else
        v = var
    end
    quote
        let $(esc(v)) = TypeVar($(Expr(:quote,v)), $lb, $ub)
            UnionAll($(esc(v)), $(esc(expr)))
        end
    end
end

const issub = issubtype
issub_strict(x::ANY,y::ANY) = issub(x,y) && !issub(y,x)
isequal_type(x::ANY,y::ANY) = issub(x,y) && issub(y,x)

# level 1: no varags, union, UnionAll
function test_1()
    @test issub_strict(Int, Integer)
    @test issub_strict(Array{Int,1}, AbstractArray{Int,1})

    @test isequal_type(Int, Int)
    @test isequal_type(Integer, Integer)
    @test isequal_type(Array{Int,1}, Array{Int,1})
    @test isequal_type(AbstractArray{Int,1}, AbstractArray{Int,1})

    @test issub_strict(Tuple{Int,Int}, Tuple{Integer,Integer})
    @test issub_strict(Tuple{Array{Int,1}}, Tuple{AbstractArray{Int,1}})

    @test isequal_type(Tuple{Integer,Integer}, Tuple{Integer,Integer})

    @test !issub(Tuple{Int,Int}, Tuple{Int})
    @test !issub(Tuple{Int}, Tuple{Integer,Integer})

    @test !issub(Array{Int,1}, Array{Integer,1})
end

# level 2: varargs
function test_2()
    @test issub_strict(Tuple{Int,Int}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Vararg{Int}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Vararg{Integer}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Int,Vararg{Integer}})
    @test issub_strict(Tuple{Int,Vararg{Int}}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int,Int}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int,Int}, Tuple{Integer,Vararg{Int}})
    @test issub_strict(Tuple{Int}, Tuple{Any})
    @test issub_strict(Tuple{}, Tuple{Vararg{Any}})

    @test isequal_type(Tuple{Int}, Tuple{Int})
    @test isequal_type(Tuple{Vararg{Integer}}, Tuple{Vararg{Integer}})

    @test !issub(Tuple{}, Tuple{Int, Vararg{Int}})
    @test !issub(Tuple{Int}, Tuple{Int, Int, Vararg{Int}})

    @test !issub(Tuple{Int, Tuple{Real, Integer}}, Tuple{Vararg{Int}})

    @test isequal_type(Tuple{Int,Int}, Tuple{Vararg{Int,2}})
end

function test_diagonal()
    @test !issub(Tuple{Integer,Integer}, @UnionAll T Tuple{T,T})
    @test issub(Tuple{Integer,Int}, (@UnionAll T @UnionAll S<:T Tuple{T,S}))
    @test issub(Tuple{Integer,Int}, (@UnionAll T @UnionAll T<:S<:T Tuple{T,S}))
    @test !issub(Tuple{Integer,Int,Int}, (@UnionAll T @UnionAll T<:S<:T Tuple{T,S,S}))

    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S<:T Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll T<:S<:T Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S>:T Tuple{T,S}))

    @test !issub(Tuple{Real,Real}, @UnionAll T<:Real Tuple{T,T})

    @test issub((@UnionAll S<:Int (@UnionAll R<:AbstractString Tuple{S,R,Vector{Any}})),
                (@UnionAll T Tuple{T, T, Array{T,1}}))

    @test issub_strict(Tuple{String, Real, Ref{Number}},
                       (@UnionAll T Tuple{Union{T,String}, T, Ref{T}}))

    @test issub_strict(Tuple{String, Real},
                       (@UnionAll T Tuple{Union{T,String}, T}))

    @test !issub(      Tuple{Real, Real},
                       (@UnionAll T Tuple{Union{T,String}, T}))

    @test issub_strict(Tuple{Int, Int},
                       (@UnionAll T Tuple{Union{T,String}, T}))
end

# level 3: UnionAll
function test_3()
    @test issub_strict(Array{Int,1}, @UnionAll T Vector{T})
    @test issub_strict((@UnionAll T Pair{T,T}), Pair)
    @test issub(Pair{Int,Int8}, Pair)
    @test issub(Pair{Int,Int8}, (@UnionAll S Pair{Int,S}))

    @test !issub((@UnionAll T<:Real T), (@UnionAll T<:Integer T))

    @test isequal_type((@UnionAll T Tuple{T,T}), (@UnionAll R Tuple{R,R}))

    @test !issub((@UnionAll T<:Integer @UnionAll S<:Number Tuple{T,S}),
                 (@UnionAll T<:Integer @UnionAll S<:Number Tuple{S,T}))

    @test issub_strict((@UnionAll T Tuple{Array{T},Array{T}}),
                       Tuple{Array, Array})

    AUA = Array{(@UnionAll T Array{T,1}), 1}
    UAA = (@UnionAll T Array{Array{T,1}, 1})

    @test !issub(AUA, UAA)
    @test !issub(UAA, AUA)
    @test !isequal_type(AUA, UAA)

    @test issub_strict((@UnionAll T Int), (@UnionAll T<:Integer Integer))

    @test isequal_type((@UnionAll T @UnionAll S Tuple{T, Tuple{S}}),
                       (@UnionAll T Tuple{T, @UnionAll S Tuple{S}}))

    @test !issub((@UnionAll T Pair{T,T}), Pair{Int,Int8})
    @test !issub((@UnionAll T Pair{T,T}), Pair{Int,Int})

    @test isequal_type((@UnionAll T Tuple{T}), Tuple{Any})
    @test isequal_type((@UnionAll T<:Real Tuple{T}), Tuple{Real})

    @test  issub(Tuple{Array{Integer,1}, Int},
                 @UnionAll T<:Integer @UnionAll S<:T Tuple{Array{T,1},S})

    @test !issub(Tuple{Array{Integer,1}, Real},
                 @UnionAll T<:Integer Tuple{Array{T,1},T})

    @test !issub(Tuple{Int,String,Vector{Integer}},
                 @UnionAll T Tuple{T, T, Array{T,1}})
    @test !issub(Tuple{String,Int,Vector{Integer}},
                 @UnionAll T Tuple{T, T, Array{T,1}})
    @test !issub(Tuple{Int,String,Vector{Tuple{Integer}}},
                 @UnionAll T Tuple{T,T,Array{Tuple{T},1}})

    @test issub(Tuple{Int,String,Vector{Any}},
                @UnionAll T Tuple{T, T, Array{T,1}})

    @test isequal_type(Array{Int,1}, Array{(@UnionAll T<:Int T), 1})
    @test isequal_type(Array{Tuple{Any},1}, Array{(@UnionAll T Tuple{T}), 1})

    @test isequal_type(Array{Tuple{Int,Int},1},
                       Array{(@UnionAll T<:Int Tuple{T,T}), 1})
    @test !issub(Array{Tuple{Int,Integer},1},
                 Array{(@UnionAll T<:Integer Tuple{T,T}), 1})

    @test !issub(Pair{Int,Int8}, (@UnionAll T Pair{T,T}))

    @test !issub(Tuple{Array{Int,1}, Integer},
                 @UnionAll T<:Integer Tuple{Array{T,1},T})

    @test !issub(Tuple{Integer, Array{Int,1}},
                 @UnionAll T<:Integer Tuple{T, Array{T,1}})

    @test !issub(Pair{Array{Int,1},Integer}, @UnionAll T Pair{Array{T,1},T})
    @test  issub(Pair{Array{Int,1},Int}, @UnionAll T Pair{Array{T,1},T})

    @test  issub(Tuple{Integer,Int}, @UnionAll T<:Integer @UnionAll S<:T Tuple{T,S})
    @test !issub(Tuple{Integer,Int}, @UnionAll T<:Int     @UnionAll S<:T Tuple{T,S})
    @test !issub(Tuple{Integer,Int}, @UnionAll T<:String  @UnionAll S<:T Tuple{T,S})

    @test issub(Tuple{Float32,Array{Float32,1}},
                @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test !issub(Tuple{Float32,Array{Float64,1}},
                 @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test issub(Tuple{Float32,Array{Real,1}},
                @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test !issub(Tuple{Number,Array{Real,1}},
                 @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test issub((@UnionAll Int<:T<:Integer T), @UnionAll T<:Real T)
    @test issub((@UnionAll Int<:T<:Integer Array{T,1}),
                (@UnionAll T<:Real Array{T,1}))

    @test  issub((@UnionAll Int<:T<:Integer T), (@UnionAll Integer<:T<:Real T))
    @test !issub((@UnionAll Int<:T<:Integer Array{T,1}), (@UnionAll Integer<:T<:Real Array{T,1}))

    X = (@UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})
    Y = (@UnionAll A<:Real @UnionAll B<:AbstractArray{A,1} Tuple{A,B})
    @test isequal_type(X,Y)
    Z = (@UnionAll A<:Real @UnionAll B<:AbstractArray{A,1} Tuple{Real,B})
    @test issub_strict(X,Z)

    @test issub_strict((@UnionAll T @UnionAll S<:T Pair{T,S}),
                       (@UnionAll T @UnionAll S    Pair{T,S}))
    @test issub_strict((@UnionAll T @UnionAll S>:T Pair{T,S}),
                       (@UnionAll T @UnionAll S    Pair{T,S}))

    # these would be correct if the diagonal rule applied to type vars occurring
    # only once in covariant position.
    #@test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
    #                   (@UnionAll T @UnionAll S<:T Tuple{Ref{T},S}))
    #@test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
    #                   (@UnionAll T @UnionAll S<:T @UnionAll R<:S Tuple{Ref{T},R}))

    @test isequal_type((@UnionAll T Tuple{Ref{T}, T}),
                       (@UnionAll T @UnionAll T<:S<:T Tuple{Ref{T},S}))
    @test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{T}, S}))
end

# level 4: Union
function test_4()
    @test isequal_type(Union{Bottom,Bottom}, Bottom)

    @test issub_strict(Int, Union{Int,String})
    @test issub_strict(Union{Int,Int8}, Integer)

    @test isequal_type(Union{Int,Int8}, Union{Int,Int8})

    @test isequal_type(Union{Int,Integer}, Integer)

    @test isequal_type(Tuple{Union{Int,Int8},Int16}, Union{Tuple{Int,Int16},Tuple{Int8,Int16}})

    @test issub_strict(Tuple{Int,Int8,Int}, Tuple{Vararg{Union{Int,Int8}}})
    @test issub_strict(Tuple{Int,Int8,Int}, Tuple{Vararg{Union{Int,Int8,Int16}}})

    # nested unions
    @test !issub(Union{Int,Ref{Union{Int,Int8}}}, Union{Int,Ref{Union{Int8,Int16}}})

    A = Int;   B = Int8
    C = Int16; D = Int32
    @test  issub(Union{Union{A,Union{A,Union{B,C}}}, Union{D,Bottom}},
                 Union{Union{A,B},Union{C,Union{B,D}}})
    @test !issub(Union{Union{A,Union{A,Union{B,C}}}, Union{D,Bottom}},
                 Union{Union{A,B},Union{C,Union{B,A}}})

    @test isequal_type(Union{Union{A,B,C}, Union{D}}, Union{A,B,C,D})
    @test isequal_type(Union{Union{A,B,C}, Union{D}}, Union{A,Union{B,C},D})
    @test isequal_type(Union{Union{Union{Union{A}},B,C}, Union{D}},
                       Union{A,Union{B,C},D})

    @test issub_strict(Union{Union{A,C}, Union{D}}, Union{A,B,C,D})

    @test !issub(Union{Union{A,B,C}, Union{D}}, Union{A,C,D})

    # obviously these unions can be simplified, but when they aren't there's trouble
    X = Union{Union{A,B,C},Union{A,B,C},Union{A,B,C},Union{A,B,C},
              Union{A,B,C},Union{A,B,C},Union{A,B,C},Union{A,B,C}}
    Y = Union{Union{D,B,C},Union{D,B,C},Union{D,B,C},Union{D,B,C},
              Union{D,B,C},Union{D,B,C},Union{D,B,C},Union{A,B,C}}
    @test issub_strict(X,Y)
end

# level 5: union and UnionAll
function test_5()
    u = Union{Int8,Int}

    @test issub(Tuple{String,Array{Int,1}},
                (@UnionAll T Union{Tuple{T,Array{T,1}}, Tuple{T,Array{Int,1}}}))

    @test issub(Tuple{Union{Vector{Int},Vector{Int8}}},
                @UnionAll T Tuple{Array{T,1}})

    @test !issub(Tuple{Union{Vector{Int},Vector{Int8}},Vector{Int}},
                 @UnionAll T Tuple{Array{T,1}, Array{T,1}})

    @test !issub(Tuple{Union{Vector{Int},Vector{Int8}},Vector{Int8}},
                 @UnionAll T Tuple{Array{T,1}, Array{T,1}})

    @test !issub(Vector{Int}, @UnionAll T>:u Array{T,1})
    @test  issub(Vector{Integer}, @UnionAll T>:u Array{T,1})
    @test  issub(Vector{Union{Int,Int8}}, @UnionAll T>:u Array{T,1})

    @test issub((@UnionAll Int<:T<:u Array{T,1}), (@UnionAll Int<:T<:u Array{T,1}))

    # with varargs
    @test !issub(Array{Tuple{Array{Int},Array{Vector{Int16}},Array{Vector{Int}},Array{Int}}},
                 @UnionAll T<:(@UnionAll S Tuple{Vararg{Union{Array{S}, Array{Array{S,1}}}}}) Array{T})

    @test  issub(Array{Tuple{Array{Int},Array{Vector{Int  }},Array{Vector{Int}},Array{Int}}},
                 @UnionAll T<:(@UnionAll S Tuple{Vararg{Union{Array{S}, Array{Array{S,1}}}}}) Array{T})

    @test !issub(Tuple{Array{Int},Array{Vector{Int16}},Array{Vector{Int}},Array{Int}},
                 @UnionAll S Tuple{Vararg{Union{Array{S},Array{Array{S,1}}}}})

    @test  issub(Tuple{Array{Int},Array{Vector{Int  }},Array{Vector{Int}},Array{Int}},
                 @UnionAll S Tuple{Vararg{Union{Array{S},Array{Array{S,1}}}}})

    B = @UnionAll S<:u Tuple{S, Tuple{Any,Any,Any}, Ref{S}}
    # these tests require renaming in issub_unionall
    @test  issub((@UnionAll T<:B Tuple{Int8, T, Ref{Int8}}), B)
    @test !issub((@UnionAll T<:B Tuple{Int8, T, Ref{T}}),    B)

    # the `convert(Type{T},T)` pattern, where T is a Union
    # required changing priority of unions and vars
    @test issub(Tuple{Array{u,1},Int}, @UnionAll T Tuple{Array{T,1}, T})
    @test issub(Tuple{Array{u,1},Int}, @UnionAll T @UnionAll S<:T Tuple{Array{T,1}, S})
end

# tricky type variable lower bounds
function test_6()
    @test  issub((@UnionAll S<:Int (@UnionAll R<:String Tuple{S,R,Vector{Any}})),
                 (@UnionAll T Tuple{T, T, Array{T,1}}))

    @test !issub((@UnionAll S<:Int (@UnionAll R<:String Tuple{S,R,Vector{Integer}})),
                 (@UnionAll T Tuple{T, T, Array{T,1}}))

    t = @UnionAll T Tuple{T,T,Ref{T}}
    @test isequal_type(t, @UnionAll S Tuple{S,S,Ref{S}})

    @test !issub((@UnionAll T Tuple{T,String,Ref{T}}), (@UnionAll T Tuple{T,T,Ref{T}}))

    @test !issub((@UnionAll T Tuple{T,Ref{T},String}), (@UnionAll T Tuple{T,Ref{T},T}))

    i = Int; ai = Integer
    @test isequal_type((@UnionAll i<:T<:i   Ref{T}), Ref{i})
    @test isequal_type((@UnionAll ai<:T<:ai Ref{T}), Ref{ai})

    # Pair{T,S} <: Pair{T,T} can be true with certain bounds
    @test issub_strict((@UnionAll i<:T<:i @UnionAll i<:S<:i Pair{T,S}),
                       @UnionAll T Pair{T,T})

    @test issub_strict(Tuple{i, Ref{i}},
                       (@UnionAll T @UnionAll S<:T Tuple{S,Ref{T}}))

    @test !issub(Tuple{Real, Ref{i}},
                 (@UnionAll T @UnionAll S<:T Tuple{S,Ref{T}}))

    # S >: T
    @test issub_strict(Tuple{Real, Ref{i}},
                       (@UnionAll T @UnionAll S>:T Tuple{S,Ref{T}}))

    @test !issub(Tuple{Ref{i}, Ref{ai}},
                 (@UnionAll T @UnionAll S>:T Tuple{Ref{S},Ref{T}}))

    @test issub_strict(Tuple{Ref{Real}, Ref{ai}},
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{S},Ref{T}}))

    @test issub_strict(Tuple{Real, Ref{Tuple{i}}},
                       (@UnionAll T @UnionAll S>:T Tuple{S,Ref{Tuple{T}}}))

    @test !issub(Tuple{Ref{Tuple{i}}, Ref{Tuple{ai}}},
                 (@UnionAll T @UnionAll S>:T Tuple{Ref{Tuple{S}},Ref{Tuple{T}}}))

    @test issub_strict(Tuple{Ref{Tuple{Real}}, Ref{Tuple{ai}}},
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{Tuple{S}},Ref{Tuple{T}}}))

    # (@UnionAll x<:T<:x Q{T}) == Q{x}
    @test isequal_type(Ref{Ref{i}}, Ref{@UnionAll i<:T<:i Ref{T}})
    @test isequal_type((@UnionAll i<:T<:i Ref{Ref{T}}), Ref{@UnionAll i<:T<:i Ref{T}})
    @test !issub((@UnionAll i<:T<:i Ref{Ref{T}}), Ref{@UnionAll T<:i Ref{T}})

    u = Union{Int8,Int64}
    A = Ref{Bottom}
    B = @UnionAll S<:u Ref{S}
    @test issub(Ref{B}, @UnionAll A<:T<:B Ref{T})

    C = @UnionAll S<:u S
    @test issub(Ref{C}, @UnionAll u<:T<:u Ref{T})

    BB = @UnionAll S<:Bottom S
    @test issub(Ref{B}, @UnionAll BB<:U<:B Ref{U})
end

const menagerie =
    Any[Bottom, Any, Int, Int8, Integer, Real,
        Array{Int,1}, AbstractArray{Int,1},
        Tuple{Int,Vararg{Integer}}, Tuple{Integer,Vararg{Int}}, Tuple{},
        Union{Int,Int8},
        (@UnionAll T Array{T,1}),
        (@UnionAll T Pair{T,T}),
        (@UnionAll T @UnionAll S Pair{T,S}),
        Pair{Int,Int8},
        (@UnionAll S Pair{Int,S}),
        (@UnionAll T Tuple{T,T}),
        (@UnionAll T<:Integer Tuple{T,T}),
        (@UnionAll T @UnionAll S Tuple{T,S}),
        (@UnionAll T<:Integer @UnionAll S<:Number Tuple{T,S}),
        (@UnionAll T<:Integer @UnionAll S<:Number Tuple{S,T}),
        Array{(@UnionAll T Array{T,1}),1},
        (@UnionAll T Array{Array{T,1},1}),
        Array{(@UnionAll T<:Int T), 1},
        (@UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S}),
        Union{Int,Ref{Union{Int,Int8}}},
        (@UnionAll T Union{Tuple{T,Array{T,1}}, Tuple{T,Array{Int,1}}}),
        ]

let new = Any[]
    # add variants of each type
    for T in menagerie
        push!(new, Ref{T})
        push!(new, Tuple{T})
        push!(new, Tuple{T,T})
        push!(new, Tuple{Vararg{T}})
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T Ref{S})
    end
    append!(menagerie, new)
end

function test_properties()
    x→y = !x || y
    ¬T = @UnionAll X>:T Ref{X}

    for T in menagerie
        # top and bottom identities
        @test issub(Bottom, T)
        @test issub(T, Any)
        @test issub(T, Bottom) → isequal_type(T, Bottom)
        @test issub(Any, T) → isequal_type(T, Any)

        # unionall identity
        @test isequal_type(T, @UnionAll S<:T S)
        @test isequal_type(Ref{T}, @UnionAll T<:U<:T Ref{U})

        # equality under renaming
        if isa(T, UnionAll)
            lb, ub = T.var.lb, T.var.ub
            @test isequal_type(T, (@UnionAll lb<:Y<:ub T{Y}))
        end

        # inequality under wrapping
        @test !isequal_type(T, Ref{T})

        for S in menagerie
            issubTS = issub(T, S)
            # transitivity
            if issubTS
                for R in menagerie
                    if issub(S, R)
                        @test issub(T, R)  # issub(T,S) ∧ issub(S,R) → issub(T,R)
                        @test issub(Ref{S}, @UnionAll T<:U<:R Ref{U})
                    end
                end
            end

            # union subsumption
            @test isequal_type(T, Union{T,S}) → issub(S, T)

            # invariance
            @test isequal_type(T, S) == isequal_type(Ref{T}, Ref{S})

            # covariance
            @test issubTS == issub(Tuple{T}, Tuple{S})
            @test issubTS == issub(Tuple{Vararg{T}}, Tuple{Vararg{S}})
            @test issubTS == issub(Tuple{T}, Tuple{Vararg{S}})

            # pseudo-contravariance
            @test issubTS == issub(¬S, ¬T)
        end
    end
end

test_1()
test_2()
test_diagonal()
test_3()
test_4()
test_5()
test_6()
test_properties()
