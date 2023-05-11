import Base.Unicode

const EOF_CHAR = typemax(Char)

function is_identifier_char(c::Char)
    c == EOF_CHAR && return false
    Base.isvalid(c) || return false
    return Base.is_id_char(c)
end

function is_identifier_start_char(c::Char)
    c == EOF_CHAR && return false
    Base.isvalid(c) || return false
    return Base.is_id_start_char(c)
end

# Chars that we will never allow to be part of a valid non-operator identifier
function is_never_id_char(ch::Char)
    Base.isvalid(ch) || return true
    cat = Unicode.category_code(ch)
    c = UInt32(ch)
    return (
        # spaces and control characters:
        (cat >= Unicode.UTF8PROC_CATEGORY_ZS && cat <= Unicode.UTF8PROC_CATEGORY_CS) ||

        # ASCII and Latin1 non-connector punctuation
        (c < 0xff &&
         cat >= Unicode.UTF8PROC_CATEGORY_PD && cat <= Unicode.UTF8PROC_CATEGORY_PO) ||

        c == UInt32('`') ||

        # mathematical brackets
        (c >= 0x27e6 && c <= 0x27ef) ||
        # angle, corner, and lenticular brackets
        (c >= 0x3008 && c <= 0x3011) ||
        # tortoise shell, square, and more lenticular brackets
        (c >= 0x3014 && c <= 0x301b) ||
        # fullwidth parens
        (c == 0xff08 || c == 0xff09) ||
        # fullwidth square brackets
        (c == 0xff3b || c == 0xff3d)
    )
end

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)

# Some unicode operators are normalized by the tokenizer into their equivalent
# kinds. See also normalize_identifier()
const _ops_with_unicode_aliases = [
    # \minus '−' is normalized into K"-",
    '−' => K"-"
    # Lookalikes which are normalized into K"⋅",
    # https://github.com/JuliaLang/julia/pull/25157,
    '\u00b7' => K"⋅" # '·' Middle Dot,,
    '\u0387' => K"⋅" # '·' Greek Ano Teleia,,
]

function _nondot_symbolic_operator_kinds()
    op_range = reinterpret(UInt16, K"BEGIN_OPS"):reinterpret(UInt16, K"END_OPS")
    setdiff(reinterpret.(Kind, op_range), [
        K"ErrorInvalidOperator"
        K"Error**"
        K"..."
        K"."
        K"where"
        K"isa"
        K"in"
        K".'"
    ])
end

function _char_in_set_expr(varname, firstchars)
    codes = sort!(UInt32.(unique(firstchars)))
    terms = []
    i = 1
    while i <= length(codes)
        j = i
        while j < length(codes) && codes[j+1] == codes[j]+1
            j += 1
        end
        if i == j
            push!(terms, :($varname == $(codes[i])))
        else
            push!(terms, :($(codes[i]) <= $varname <= $(codes[j])))
        end
        i = j+1
    end
    foldr((t1,t2)->:($t1 || $t2), terms)
end

@eval function is_operator_start_char(c)
   if c == EOF_CHAR || !Base.isvalid(c)
       return false
   end
   u = UInt32(c)
   return $(_char_in_set_expr(:u,
       append!(first.(string.(_nondot_symbolic_operator_kinds())),
               first.(_ops_with_unicode_aliases))))
end

# Checks whether a Char is an operator which can be prefixed with a dot `.`
function is_dottable_operator_start_char(c)
    return c != '?' && c != '$' && c != ':' && c != '\'' && is_operator_start_char(c)
end

@eval function isopsuffix(c::Char)
    c == EOF_CHAR && return false
    Base.isvalid(c) || return false
    u = UInt32(c)
    if (u < 0xa1 || u > 0x10ffff)
        return false
    end
    cat = Base.Unicode.category_code(u)
    if (cat == Base.Unicode.UTF8PROC_CATEGORY_MN ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_MC ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_ME)
        return true
    end
    # Additional allowed cases
    return $(_char_in_set_expr(:u,
        collect("²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ′″‴‵‶‷⁗⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽꜛꜜꜝ")))
end

function optakessuffix(k)
    (K"BEGIN_OPS" <= k <= K"END_OPS") &&
    !(
        k == K"..." ||
        K"BEGIN_ASSIGNMENTS" <= k <= K"END_ASSIGNMENTS" ||
        k == K"?"   ||
        k == K"<:"  ||
        k == K">:"  ||
        k == K"&&"  ||
        k == K"||"  ||
        k == K"in"  ||
        k == K"isa" ||
        k == K"≔"   ||
        k == K"⩴"   ||
        k == K":"   ||
        k == K".."  ||
        k == K"$"   ||
        k == K"::"  ||
        k == K"where" ||
        k == K"."   ||
        k == K"!"   ||
        k == K".'"  ||
        k == K"->"  ||
        K"¬" <= k <= K"∜"
    )
end

const _unicode_ops = let
    ks = _nondot_symbolic_operator_kinds()
    ss = string.(ks)

    ops = Dict{Char, Kind}([first(s)=>k for (k,s) in zip(ks,ss)
                            if length(s) == 1 && !isascii(s[1])])
    for ck in _ops_with_unicode_aliases
        push!(ops, ck)
    end
    ops
end
