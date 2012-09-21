###
### GLPK API Wrapper
###

require("sparse.jl")

## Shared library interface setup
#{{{
load("glpk_h.jl")

_jl_libglpk = dlopen("libglpk")
_jl_libglpk_wrapper = dlopen("libglpk_wrapper")

macro glpk_ccall(func, args...)
    f = "glp_$(func)"
    quote
        ccall(dlsym(_jl_libglpk, $f), $(args...))
    end
end

macro glpkw_ccall(func, args...)
    f = "_jl_glpkw__$(func)"
    quote
        ccall(dlsym(_jl_libglpk_wrapper, $f), $(args...))
    end
end

# We need to define glp_version as first thing
# in order to perform a sanity check
# (since we import structs from the header,
# we must ensure that the binary is the correct
# one)
function glp_version()
    csp = @glpk_ccall version Ptr{Uint8} ()
    str = Array(Uint8, 100)
    strp = pointer(str)
    k = 0
    for i = 1 : 100
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Int), strp, csp, sizeof(Uint8))
        if str[i] == '\0'
            k = i
            break
        end
        strp += sizeof(Uint8)
        csp += sizeof(Uint8)
    end
    if k == 0
        throw(GLPError("error reading version"))
    end
    vstr = ASCIIString(str[1:k - 1])
    return tuple(map(x->int32(parse_int(x)), split(vstr, '.'))...)
end

if glp_version() != (GLP_MAJOR_VERSION, GLP_MINOR_VERSION)
    bv = glp_version()
    hv = (GLP_MAJOR_VERSION, GLP_MINOR_VERSION)
    error("GLPK error: mismatched versions: header=$(hv[1]).$(hv[2]) binary=$(bv[1]).$(bv[2])")
end
#}}}


## Preliminary definitions
#{{{

# General structure for the parameters types

abstract GLPParam

pointer(param::GLPParam) = param.struct

typealias GLPParamFieldDescriptor (ASCIIString, BitsKind)

type GLPParamDescriptor
    struct_name::String
    field_names::Vector{ASCIIString}
    field_types::Vector{BitsKind}
    function GLPParamDescriptor(cstr::String, struct_desc)
        struct_name = cstr
        c_struct_desc = convert(Vector{GLPParamFieldDescriptor}, struct_desc)
        field_names = [ x[1]::ASCIIString for x = c_struct_desc ]
        field_types = [ x[2]::BitsKind for x = c_struct_desc ]
        new(struct_name, field_names, field_types)
    end
end

function assign{T}(param::GLPParam, val::T, field_name::String)
    if pointer(param) == C_NULL
        error("param is not allocated")
    end
    for i = 1 : length(param.desc.field_names)
        if field_name == param.desc.field_names[i]
            if pointer(param) == C_NULL
                throw(GLPError("invalid struct"))
            end
            t = param.desc.field_types[i]
            csf = strcat("_jl_glpkw__", param.desc.struct_name, "_set_", field_name)
            ccs = :(ccall(dlsym(_jl_libglpk_wrapper, $csf), Void, (Ptr{Void}, $t), pointer($param), $val))
            eval(ccs)
            return
        end
    end
    error("field '$field_name' not found in struct '$(param.desc.struct_name)'")
end

function ref(param::GLPParam, field_name::String)
    if pointer(param) == C_NULL
        error("param is not allocated")
    end
    for i = 1 : length(param.desc.field_names)
        if field_name == param.desc.field_names[i]
            if pointer(param) == C_NULL
                throw(GLPError("invalid struct"))
            end
            t = param.desc.field_types[i]
            cgf = strcat("_jl_glpkw__", param.desc.struct_name, "_get_", field_name)
            ccg = :(ccall(dlsym(_jl_libglpk_wrapper, $cgf), $t, (Ptr{Void},), pointer($param)))
            return eval(ccg)
        end
    end
    error("field '$field_name' not found in struct '$(param.desc.struct_name)'")
end




# We define some types which allow to pass optional agruments
# to the function.
# In this framework, optional arguments can be passed either
# as an empty vector [] or as the 'nothing' constant

typealias VecOrNothing Union(Vector, Nothing)
function _jl_glpk__convert_vecornothing{T}(::Type{T}, a::VecOrNothing)
    if isequal(a, nothing) || isa(a, Array{None})
        return T[]
    elseif T <: Integer
        if !(eltype(a) <: Integer)
            throw(GLPError("integer-valued array required, or [] or nothing"))
        end
    elseif T <: Real
        if !(eltype(a) <: Real)
            throw(GLPError("real-valued array required, or [] or nothing"))
        end
    end
    convert(Array{T}, a)
end
_jl_glpk__vecornothing_length(a::VecOrNothing) = is(a, nothing) ? 0 : length(a)


# General exception: all GLP functions
# throw this in case of errors
type GLPError <: Exception
    msg::String
end
#}}}


## Main types definitions
#{{{
# All structs in original glpk are wrapped up in
# composite types, which initialize and destroy themselves
# as needed, and expose pointers when asked to by
# ccall's.
#
# Therefore, the original C glp API
#
#  int glp_simplex(glp_prob * lp, glp_smpc * param)
#
# becomes
#
#  glp_simplex(lp::GLPProb, param::GLPSimplexParam)
#
#
# The map between names is as follows:
#
# +-------------+------------------------+
# |  C          |  Julia                 |
# +-------------+------------------------+
# |  glp_prob   |  GLPProb               |
# |  glp_smcp   |  GLPSimplexParam       |
# |  glp_iptcp  |  GLPInteriorParam      |
# |  glp_iocp   |  GLPIntoptParam        |
# |  glp_bfcp   |  GLPBasisFactParam     |
# |  glp_tran   |  GLPMathProgWorkspace  |
# |  glp_data   |  GLPData               |
# +-------------+------------------------+
#
# In order to get/set the value of a cstruct field, you can
# use vector-like referncing with the field name as an argument,
# e.g.:
#
#   lps_opts = GLPSimplexParam()
#   lps_opts["msg_lev"] = GLP_MSG_ERR
#   lps_opts["presolve"] = GLP_ON
#

type GLPProb
    p::Ptr{Void}
    function GLPProb()
        p = @glpk_ccall create_prob Ptr{Void} ()
        prob = new(p)
        finalizer(prob, glp_delete_prob)
        return prob
    end
end

function glp_delete_prob(glp_prob::GLPProb)
    if glp_prob.p == C_NULL
        return
    end
    @glpk_ccall delete_prob Void (Ptr{Void},) glp_prob.p
    glp_prob.p = C_NULL
    return
end


_jl_glpk__simplex_param_struct_desc = GLPParamDescriptor("smcp",
        [("msg_lev", Int32), ("meth", Int32), ("pricing", Int32),
         ("r_test", Int32), ("tol_bnd", Float64), ("tol_dj", Float64),
         ("tol_piv", Float64), ("obj_ll", Float64), ("obj_ul", Float64),
         ("it_lim", Int32), ("tm_lim", Int32), ("out_frq", Int32),
         ("out_dly", Int32), ("presolve", Int32)])

type GLPSimplexParam <: GLPParam
    struct::Ptr{Void}
    desc::GLPParamDescriptor
    function GLPSimplexParam()
        struct = @glpkw_ccall smcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__simplex_param_struct_desc)
        finalizer(param, _jl_glpkw__smcp_delete)
        return param
    end
end

function _jl_glpkw__smcp_delete(param::GLPSimplexParam)
    @glpkw_ccall smcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__interior_param_struct_desc = GLPParamDescriptor("iptcp",
        [("msg_lev", Int32), ("ord_alg", Int32)])

type GLPInteriorParam <: GLPParam
    struct::Ptr{Void}
    desc::GLPParamDescriptor
    function GLPInteriorParam()
        struct = @glpkw_ccall iptcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__interior_param_struct_desc)
        finalizer(param, _jl_glpkw__iptcp_delete)
        return param
    end
end

function _jl_glpkw__iptcp_delete(param::GLPInteriorParam)
    @glpkw_ccall iptcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__intopt_param_struct_desc = GLPParamDescriptor("iocp",
    [("msg_lev", Int32), ("br_tech", Int32), ("bt_tech", Int32),
     ("pp_tech", Int32), ("fp_heur", Int32), ("gmi_cuts", Int32),
     ("mir_cuts", Int32), ("cov_cuts", Int32), ("clq_cuts", Int32),
     ("tol_int", Float64), ("tol_obj", Float64), ("mip_gap", Float64),
     ("tm_lim", Int32), ("out_frq", Int32), ("out_dly", Int32),
     ("cb_func", Ptr{Void}), ("cb_info", Ptr{Void}), ("cb_size", Int32),
     ("presolve", Int32), ("binarize", Int32)])

type GLPIntoptParam <: GLPParam
    struct::Ptr{Void}
    desc::GLPParamDescriptor
    function GLPIntoptParam()
        struct = @glpkw_ccall iocp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__intopt_param_struct_desc)
        finalizer(param, _jl_glpkw__iocp_delete)
        return param
    end
end

function _jl_glpkw__iocp_delete(param::GLPIntoptParam)
    @glpkw_ccall iocp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__basisfact_param_struct_desc = GLPParamDescriptor("bfcp",
    [("type", Int32), ("lu_size", Int32), ("piv_tol", Float64),
     ("piv_lim", Int32), ("suhl", Int32), ("eps_tol", Float64),
     ("max_gro", Float64), ("nfs_max", Int32), ("upd_tol", Float64),
     ("nrs_max", Int32), ("rs_size", Int32)])

type GLPBasisFactParam <: GLPParam
    struct::Ptr{Void}
    desc::GLPParamDescriptor
    function GLPBasisFactParam()
        struct = @glpkw_ccall bfcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__basisfact_param_struct_desc)
        finalizer(param, _jl_glpkw__bfcp_delete)
        return param
    end
end

function _jl_glpkw__bfcp_delete(param::GLPBasisFactParam)
    @glpkw_ccall bfcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end

type GLPData
    p::Ptr{Void}
end

pointer(data::GLPData) = data.p

type GLPMathProgWorkspace
    p::Ptr{Void}
    function GLPMathProgWorkspace()
        glp_tran = @glpk_ccall mpl_alloc_wksp Ptr{Void} ()
        wksp = new(glp_tran)
        finalizer(wksp, glp_mpl_free_wksp)
        return wksp
    end
end

function glp_mpl_free_wksp(glp_tran::GLPMathProgWorkspace)
    if glp_tran.p == C_NULL
        return
    end
    @glpk_ccall mpl_free_wksp Void (Ptr{Void},) glp_tran.p
    glp_tran.p = C_NULL
    return
end

# old cstruct-based version
#{{{
#_jl_glpk__simplex_param_struct_desc = CStructDescriptor(["glpk.h"], "glp_smcp",
#        [("msg_lev", Int32), ("meth", Int32), ("pricing", Int32),
#         ("r_test", Int32), ("tol_bnd", Float64), ("tol_dj", Float64),
#         ("tol_piv", Float64), ("obj_ll", Float64), ("obj_ul", Float64),
#         ("it_lim", Int32), ("tm_lim", Int32), ("out_frq", Int32),
#         ("out_dly", Int32), ("presolve", Int32)])
#
#abstract GLPParam <: CStructWrapper
#
#type GLPSimplexParam <: GLPParam
#    struct::CStruct
#    function GLPSimplexParam()
#        struct = CStruct(_jl_glpk__simplex_param_struct_desc)
#        @glpk_ccall init_smcp Int32 (Ptr{Void},) pointer(struct)
#        new(struct)
#    end
#end

#_jl_glpk__interior_param_struct_desc = CStructDescriptor(["glpk.h"], "glp_iptcp",
#        [("msg_lev", Int32), ("ord_alg", Int32)])
#
#type GLPInteriorParam <: GLPParam
#    struct::CStruct
#    function GLPInteriorParam()
#        struct = CStruct(_jl_glpk__interior_param_struct_desc)
#        @glpk_ccall init_iptcp Int32 (Ptr{Void},) pointer(struct)
#        new(struct)
#    end
#end

#_jl_glpk__intopt_param_struct_desc = CStructDescriptor(["glpk.h"], "glp_iocp",
#    [("msg_lev", Int32), ("br_tech", Int32), ("bt_tech", Int32),
#     ("pp_tech", Int32), ("fp_heur", Int32), ("gmi_cuts", Int32),
#     ("mir_cuts", Int32), ("cov_cuts", Int32), ("clq_cuts", Int32),
#     ("tol_int", Float64), ("tol_obj", Float64), ("mip_gap", Float64),
#     ("tm_lim", Int32), ("out_frq", Int32), ("out_dly", Int32),
#     ("cb_func", Ptr{Void}), ("cb_info", Ptr{Void}), ("cb_size", Int32),
#     ("presolve", Int32), ("binarize", Int32)])
#
#type GLPIntoptParam <: GLPParam
#    struct::CStruct
#    function GLPIntoptParam()
#        struct = CStruct(_jl_glpk__intopt_param_struct_desc)
#        @glpk_ccall init_iocp Int32 (Ptr{Void},) pointer(struct)
#        new(struct)
#    end
#end

#_jl_glpk__basisfact_param_struct_desc = CStructDescriptor(["glpk.h"], "glp_bfcp",
#    [("type", Int32), ("lu_size", Int32), ("piv_tol", Float64),
#     ("piv_lim", Int32), ("suhl", Int32), ("eps_tol", Float64),
#     ("max_gro", Float64), ("nfs_max", Int32), ("upd_tol", Float64),
#     ("nrs_max", Int32), ("rs_size", Int32)])
#
#type GLPBasisFactParam <: GLPParam
#    struct::CStruct
#    function GLPBasisFactParam()
#        struct = CStruct(_jl_glpk__basisfact_param_struct_desc)
#        new(struct)
#    end
#end
#}}}

#}}}


## Check functions for internal use
#{{{
# Functions which perform all sorts of
# sanity checks on input parameters and
# throw exceptions in case of errors.
# Ideally, it should never be possible
# to pass an invalid parameter to the
# underlying glp API.

function _jl_glpk__check_glp_prob(glp_prob::GLPProb)
    if glp_prob.p == C_NULL
        throw(GLPError("Invalid GLPProb"))
    end
    return true
end

function _jl_glpk__check_string_length(s::String, minl::Integer, maxl::Integer)
    l = length(s)
    if !(minl <= l <= maxl)
        throw(GLPError("Invalid string length $l (must be $minl <= length <= $maxl)"))
    end
    return true
end

function _jl_glpk__check_row_is_valid(glp_prob::GLPProb, row::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    if (row < 1 || row > rows)
        throw(GLPError("Invalid row $row (must be 1 <= row <= $rows)"))
    end
    return true
end

function _jl_glpk__check_col_is_valid(glp_prob::GLPProb, col::Integer)
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
    if (col < 1 || col > cols)
        throw(GLPError("Invalid col $col (must be 1 <= col <= $cols)"))
    end
    return true
end

function _jl_glpk__check_col_is_valid_w0(glp_prob::GLPProb, col::Integer)
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
    if (col < 0 || col > cols)
        throw(GLPError("Invalid col $col (must be 0 <= col <= $cols)"))
    end
    return true
end

function _jl_glpk__check_obj_dir_is_valid(dir::Integer)
    if !(dir == GLP_MIN || dir == GLP_MAX)
        throw(GLPError("Invalid obj_dir $dir (use GLP_MIN or GLP_MAX)"))
    end
    return true
end

function _jl_glpk__check_bounds_type_is_valid(bounds_type::Integer)
    if !(bounds_type == GLP_FR ||
         bounds_type == GLP_LO ||
         bounds_type == GLP_UP ||
         bounds_type == GLP_DB ||
         bounds_type == GLP_FX)
        throw(GLPError("Invalid bounds_type $bounds_type (allowed values: GLP_FR, GLP_LO, GLP_UP, GLP_DB, GLP_FX)"))
    end
    return true
end

function _jl_glpk__check_bounds_are_valid(bounds_type::Integer, lb::Real, ub::Real)
    if bounds_type == GLP_DB && lb > ub
        throw(GLPError("Invalid bounds for double-bounded variable: $lb > $ub"))
    elseif bounds_type == GLP_FX && lb != ub
        throw(GLPError("Invalid bounds for fixed variable: $lb != $ub"))
    end
    return true
end

function _jl_glpk__check_vectors_size(numel::Integer, vecs...)
    if numel < 0
        throw(GLPError("Invalid numer of elements: $numel"))
    end
    if numel > 0
        for v = vecs
            if isempty(v)
                throw(GLPError("Number of elements is $numel but vector is empty or nothing"))
            elseif length(v) < numel
                throw(GLPError("Wrong vector size: $(length(v)) (numel declared as $numel)"))
            end
        end
    end
    return true
end

function _jl_glpk__check_vectors_all_same_size(vec0::VecOrNothing, vecs::VecOrNothing...)
    l0 = _jl_glpk__vecornothing_length(vec0)
    for v in vecs
        l = _jl_glpk__vecornothing_length(v)
        if l != l0
            throw(GLPError("incosistent vector lengths: $l0 and $l"))
        end
    end
    return true
end

function _jl_glpk__check_indices_vectors_dup(glp_prob::GLPProb, numel::Integer, ia::Vector{Int32}, ja::Vector{Int32})
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
    #numel = length(ia)

    off32 = sizeof(Int32)
    iap = pointer(ia) - off32
    jap = pointer(ja) - off32

    k = @glpk_ccall check_dup Int32 (Int32, Int32, Int32, Ptr{Int32}, Ptr{Int32}) rows cols numel iap jap
    if k < 0
        throw(GLPError("indices out of bounds: $(ia[-k]),$(ja[-k]) (bounds are (1,1) <= (ia,ja) <= ($rows,$cols))"))
    elseif k > 0
        throw(GLPError("duplicate index entry: $(ia[k]),$(ja[k])"))
    end
    return true
end

function _jl_glpk__check_rows_and_cols(rows::Integer, cols::Integer)
    if (rows < 0)
        throw(GLPError("rows < 0 : $rows"))
    end
    if (cols < 0)
        throw(GLPError("cols < 0 : $rows"))
    end
end

function _jl_glpk__check_rows_ids(glp_prob::GLPProb, min_size::Integer, num_rows::Integer, rows_ids::Vector{Int32})
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    if num_rows < min_size || num_rows > rows
        throw(GLPError("invalid vector size: $num_rows (min=$min_size max=$rows)"))
    end
    if num_rows == 0
        return true
    end
    if length(rows_ids) < num_rows
        throw(GLPError("invalid vector size: declared>=$num_rows actual=$(length(rows_ids))"))
    end
    ind_set = IntSet()
    add_each(ind_set, rows_ids[1 : num_rows])
    if min(ind_set) < 1 || max(ind_set) > rows
        throw(GLPError("index out of bounds (min=1 max=$rows)"))
    elseif length(ind_set) != length(rows_ids)
        throw(GLPError("one or more duplicate index(es) found"))
    end
    return true
end

function _jl_glpk__check_cols_ids(glp_prob::GLPProb, min_size::Integer, num_cols::Integer, cols_ids::Vector{Int32})
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
    if num_cols < min_size || num_cols > cols
        throw(GLPError("invalid vector size: $num_cols (min=$min_size max=$cols)"))
    end
    if num_cols == 0
        return 0
    end
    if length(cols_ids) < num_cols
        throw(GLPError("invalid vector size: declared>=$num_cols actual=$(length(cols_ids))"))
    end
    ind_set = IntSet()
    add_each(ind_set, cols_ids[1 : num_cols])
    if min(ind_set) < 1 || max(ind_set) > cols
        throw(GLPError("index out of bounds (min=1 max=$cols)"))
    elseif length(ind_set) != length(cols_ids)
        throw(GLPError("one or more duplicate index(es) found"))
    end
    return true
end

function _jl_glpk__check_list_ids(glp_prob::GLPProb, len::Integer, list_ids::Vector{Int32})
    if len == 0
        return true
    end
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
    # note1 the documentation does not mention forbidding duplicates in this case
    # note2 the size should already be checked as this function is only called
    #       by glp_print_ranges
    #if len < 0 #|| len > rows + cols
    ##throw(GLPError("invalid vector size: $len (min=0 max=$(rows + cols))"))
    #throw(GLPError("invalid vector size: $len < 0"))
    #end
    #if length(list_ids) < len
    #throw(GLPError("invalid vector size: declared>=$len actual=$(length(list_ids))"))
    #end
    if min(list_ids[1:len]) < 1 || max(list_ids[1:len]) > rows + cols
        throw(GLPError("index out of bounds (min=1 max=$(rows + cols))"))
    end
    return true
end

function _jl_glpk__check_status_is_optimal(glp_prob::GLPProb)
    ret = @glpk_ccall get_status Int32 (Ptr{Void},) glp_prob.p
    if ret == GLP_OPT
        throw(GLPError("current basic solution is not optimal"))
    end
    return true
end

function _jl_glpk__check_bf_exists(glp_prob::GLPProb)
    ret = @glpk_ccall bf_exists Int32 (Ptr{Void},) glp_prob.p
    if ret == 0
        throw(GLPError("no bf solution found (use glp_factorize)"))
    end
    return true
end

function _jl_glpk__check_var_is_basic(glp_prob::GLPProb, ind::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    if ind <= rows
        j = @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) glp_prob.p ind
        if j != GLP_BS
            throw(GLPError("variable $ind is non-basic"))
        end
    else
        j = @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) glp_prob.p ind-rows
        if j != GLP_BS
            throw(GLPError("variable $ind is non-basic"))
        end
    end
end

function _jl_glpk__check_var_is_non_basic(glp_prob::GLPProb, ind::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    if ind <= rows
        j = @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) glp_prob.p ind
        if j == GLP_BS
            throw(GLPError("variable $ind is basic"))
        end
    else
        j = @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) glp_prob.p ind-rows
        if j == GLP_BS
            throw(GLPError("variable $ind is basic"))
        end
    end
end

function _jl_glpk__check_is_prim_feasible(glp_prob::GLPProb)
    if GLP_FEAS != @glpk_ccall get_prim_stat Int32 (Ptr{Void},) glp_prob.p
        throw(GLPError("problem is not primal feasible"))
    end
    return true
end

function _jl_glpk__check_is_dual_feasible(glp_prob::GLPProb)
    if GLP_FEAS != @glpk_ccall get_dual_stat Int32 (Ptr{Void},) glp_prob.p
        throw(GLPError("problem is not dual feasible"))
    end
    return true
end

function _jl_glpk__check_copy_names_flag(names::Integer)
    if names != GLP_ON && names != GLP_OFF
        throw(GLPError("invalid copy_names flag $names (use GLP_ON or GLP_OFF)"))
    end
    return true
end

function _jl_glpk__check_scale_flags(flags::Integer)
    all = (GLP_SF_GM | GLP_SF_EQ | GLP_SF_2N | GLP_SF_SKIP)
    if (flags | all) != all && flags != GLP_SF_AUTO
        throw(GLPError("invalid scale flags $flags"))
    end
    return true
end

function _jl_glpk__check_stat_is_valid(stat::Integer)
    if (stat != GLP_BS &&
        stat != GLP_NL &&
        stat != GLP_NU &&
        stat != GLP_NF &&
        stat != GLP_NS)
        throw(GLPError("invalid status $stat (use GLP_BS or GLP_NL or GLP_NU or GLP_NF or GLP_NS)"))
    end
end

function _jl_glpk__check_adv_basis_flags(flags::Integer)
    if flags != 0
        throw(GLPError("adv_basis flags must be set to 0 (found $flags instead)"))
    end
    return true
end

function _jl_glpk__check_simplex_param(glp_param::GLPSimplexParam)
    if pointer(glp_param) == C_NULL
        throw(GLPError("glp_param = NULL"))
    end
    return true
end

function _jl_glpk__check_interior_param(glp_param::GLPInteriorParam)
    if pointer(glp_param) == C_NULL
        throw(GLPError("glp_param = NULL"))
    end
    return true
end

function _jl_glpk__check_kind_is_valid(kind::Integer)
    if (kind != GLP_CV &&
        kind != GLP_IV &&
        kind != GLP_BV)
        throw(GLPError("invalid kind $kind (use GLP_CV or GLP_IV or GLP_BV)"))
    end
    return true
end

function _jl_glpk__check_intopt_param(glp_param::GLPIntoptParam)
    if pointer(glp_param) == C_NULL
        throw(GLPError("glp_param = NULL"))
    end
    return true
end

function _jl_glpk__check_file_is_readable(filename::String)
    try
        f = open(filename, "r")
        close(f)
    catch err
        throw(GLPError("file $filename not readable"))
    end
    return true
end

function _jl_glpk__check_file_is_writable(filename::String)
    try
        f = open(filename, "w")
        close(f)
    catch err
        throw(GLPError("file $filename not writable"))
    end
    return true
end

function _jl_glpk__check_mps_format(format::Integer)
    if (format != GLP_MPS_DECK &&
        format != GLP_MPS_FILE)
        throw(GLPError("invalid MPS format $format (use GLP_MPS_DECK or GLP_MPS_FILE)"))
    end
    return true
end

function _jl_glpk__check_mps_param(param)
    if param != C_NULL
        throw(GLPError("MPS param must be C_NULL"))
    end
    return true
end

function _jl_glpk__check_lp_param(param)
    if param != C_NULL
        throw(GLPError("LP param must be C_NULL"))
    end
    return true
end

function _jl_glpk__check_read_prob_flags(flags::Integer)
    if flags != 0
        throw(GLPError("read_prob flags must be 0"))
    end
    return true
end

function _jl_glpk__check_write_prob_flags(flags::Integer)
    if flags != 0
        throw(GLPError("write_prob flags must be 0"))
    end
    return true
end

function _jl_glpk__check_print_ranges_flags(flags::Integer)
    if flags != 0
        throw(GLPError("print_ranges flags must be set to 0 (found $flags instead)"))
    end
    return true
end


function _jl_glpk__check_bfcp(glp_param::GLPBasisFactParam)
    if pointer(glp_param) == C_NULL
        throw(GLPError("Invalid GLPBasisFactParam"))
    end
    return true
end

function _jl_glpk__check_data(glp_data::GLPData)
    if pointer(glp_data) == C_NULL
        throw(GLPError("Invalid GLPData"))
    end
    return true
end

function _jl_glpk__check_mpl_workspace(glp_tran::GLPMathProgWorkspace)
    if glp_tran.p == C_NULL
        throw(GLPError("Invalid GLPMathProgWorkspace"))
    end
    return true
end

function _jl_glpk__check_rowcol_is_valid(glp_prob::GLPProb, k::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(GLPError("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end
    return true
end

function _jl_glpk__check_dir_is_valid(dir::Integer)
    if !(dir == 1 || dir == -1)
        throw(GLPError("invalid direction $dir (must be 1 or -1)"))
    end
    return true
end

function _jl_glpk__check_eps_is_valid(eps::Real)
    if (eps < 0)
        throw(GLPError("invalid eps $eps (must be >= 0)"))
    end
    return true
end

function _jl_glpk__check_init_env_succeeded(ret::Integer)
    if !(0 <= ret <= 1)
        throw(GLPError("initialization failed"))
    end
    return true
end

function _jl_glpk__check_term_out_flag(flag::Integer)
    if !(flag == GLP_ON || flag == GLP_OFF)
        throw(GLPError("invalid flag $flag (use GLP_ON or GLP_OFF)"))
    end
    return true
end

function _jl_glpk__check_open_tee_succeeded(ret::Integer)
    if !(0 <= ret <= 1)
        throw(GLPError("glp_open_tee failed"))
    end
    return true
end

function _jl_glpk__check_alloc_size(n::Integer)
    if n <= 0
        throw(GLPError("invalid alloc size $n"))
    end
    return true
end

function _jl_glpk__check_pointer_is_valid(ptr::Ptr)
    if ptr == C_NULL
        throw(GLPError("invalid pointer"))
    end
    return true
end

function _jl_glpk__check_sdf_file_opened(data_p::Ptr)
    if data_p == C_NULL
        throw(GLPError("glp_sdf_open_file failed"))
    end
    return true
end
#}}}


## GLP functions
#{{{
# The API interface is as close as possible to the original
# one.
# The general translation rules are:
#
#  * whenever the C library accepts NULL as argument,
#    the Julia one will accept the nothing constant.
#  * vectors do not need to have an extra element at the
#    beginning to accomodate to the 1-based GLP indexing.
#  * most functions will accept any kind of vectors as inputs,
#    provided they can be converted to be C-compatible
#    (i.e. to either Int32 (int) or Float64 (double) elements).
#  * the exceptions to the above are those functions which write
#    their output in a vector, in which case the vector type must
#    be strictly C-compatible.
#  * all char[] strings become Strings, both in inputs and in output.
#
#  A single exception to the strict compatibility is glp_version(),
#  which returns a tuple of integers in the form (major, minor)
#  rather than a string.

function glp_set_prob_name(glp_prob::GLPProb, name::Union(String,Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_prob_name Void (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(name)
end

function glp_set_obj_name(glp_prob::GLPProb, name::Union(String,Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_obj_name Void (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(name)
end

function glp_set_row_name(glp_prob::GLPProb, row::Integer, name::Union(String,Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_row_name Void (Ptr{Void}, Int32, Ptr{Uint8}) glp_prob.p row bytestring(name)
end

function glp_set_col_name(glp_prob::GLPProb, col::Integer, name::Union(String,Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_col_name Void (Ptr{Void}, Int32, Ptr{Uint8}) glp_prob.p col bytestring(name)
end

function glp_set_obj_dir(glp_prob::GLPProb, dir::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_obj_dir_is_valid(dir)
    @glpk_ccall set_obj_dir Void (Ptr{Void}, Int32) glp_prob.p dir
end

function glp_add_rows(glp_prob::GLPProb, rows::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall add_rows Int32 (Ptr{Void}, Int32) glp_prob.p rows
end

function glp_add_cols(glp_prob::GLPProb, cols::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall add_cols Int32 (Ptr{Void}, Int32) glp_prob.p cols
end

function glp_set_row_bnds(glp_prob::GLPProb, row::Integer, bounds_type::Integer, lb::Real, ub::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    _jl_glpk__check_bounds_type_is_valid(bounds_type)
    _jl_glpk__check_bounds_are_valid(bounds_type, lb, ub)
    @glpk_ccall set_row_bnds Void (Ptr{Void}, Int32, Int32, Float64, Float64) glp_prob.p row bounds_type lb ub
end

function glp_set_col_bnds(glp_prob::GLPProb, col::Integer, bounds_type::Integer, lb::Real, ub::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    _jl_glpk__check_bounds_type_is_valid(bounds_type)
    _jl_glpk__check_bounds_are_valid(bounds_type, lb, ub)
    @glpk_ccall set_col_bnds Void (Ptr{Void}, Int32, Int32, Float64, Float64) glp_prob.p col bounds_type lb ub
end

function glp_set_obj_coef(glp_prob::GLPProb, col::Integer, coef::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid_w0(glp_prob, col)
    @glpk_ccall set_obj_coef Void (Ptr{Void}, Int32, Float64) glp_prob.p col coef
end

function glp_set_mat_row{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, row::Integer, len::Integer, ind::Vector{Ti}, val::Vector{Tv})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    if len > 0
        ind32 = int32(ind)
        val64 = float64(val)
        off32 = sizeof(Int32)
        off64 = sizeof(Float64)
        ind32p = pointer(ind32) - off32
        val64p = pointer(val64) - off64
        _jl_glpk__check_cols_ids(glp_prob, 0, len, ind32)
    else
        ind32p = C_NULL
        val64p = C_NULL
    end

    @glpk_ccall set_mat_row Void (Ptr{Void}, Int32, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p row len ind32p val64p
end
function glp_set_mat_row(glp_prob::GLPProb, row::Integer, len::Integer, ind::VecOrNothing, val::VecOrNothing)
    ind = _jl_glpk__convert_vecornothing(Int32, ind)
    val = _jl_glpk__convert_vecornothing(Float64, ar)
    glp_set_mat_row(glp_prob, row, len, ind, val)
end
function glp_set_mat_row(glp_prob::GLPProb, row::Integer, ind::VecOrNothing, val::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    l = _jl_glpk__vecornothing_length(ind)
    glp_set_mat_row(glp_prob, row, l, ind, val)
end


function glp_set_mat_col{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, col::Integer, len::Integer, ind::Vector{Ti}, val::Vector{Tv})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    if len > 0
        ind32 = int32(ind)
        val64 = float64(val)
        off32 = sizeof(Int32)
        off64 = sizeof(Float64)
        ind32p = pointer(ind32) - off32
        val64p = pointer(val64) - off64
        _jl_glpk__check_rows_ids(glp_prob, 0, len, ind32)
    else
        ind32p = C_NULL
        val64p = C_NULL
    end

    @glpk_ccall set_mat_col Void (Ptr{Void}, Int32, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p col len ind32p val64p
end
function glp_set_mat_col(glp_prob::GLPProb, col::Integer, len::Integer, ind::VecOrNothing, val::VecOrNothing)
    ind = _jl_glpk__convert_vecornothing(Int32, ind)
    val = _jl_glpk__convert_vecornothing(Float64, ar)
    glp_set_mat_col(glp_prob, col, len, ind, val)
end
function glp_set_mat_col(glp_prob::GLPProb, col::Integer, ind::VecOrNothing, val::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    l = _jl_glpk__vecornothing_length(ind)
    glp_set_mat_col(glp_prob, col, l, ind, val)
end

function glp_load_matrix{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, numel::Integer, ia::Vector{Ti}, ja::Vector{Ti}, ar::Vector{Tv})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_vectors_size(numel, ia, ja, ar)
    if numel == 0
        return
    end
    ia32 = int32(ia)
    ja32 = int32(ja)
    ar64 = float64(ar)
    _jl_glpk__check_indices_vectors_dup(glp_prob, numel, ia32, ja32)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ia32p = pointer(ia32) - off32
    ja32p = pointer(ja32) - off32
    ar64p = pointer(ar64) - off64

    @glpk_ccall load_matrix Void (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Int32}, Ptr{Float64}) glp_prob.p numel ia32p ja32p ar64p
end

function glp_load_matrix(glp_prob::GLPProb, numel::Integer, ia::VecOrNothing, ja::VecOrNothing, ar::VecOrNothing)
    ia = _jl_glpk__convert_vecornothing(Int32, ia)
    ja = _jl_glpk__convert_vecornothing(Int32, ja)
    ar = _jl_glpk__convert_vecornothing(Float64, ar)
    glp_load_matrix(glp_prob, numel, ia, ja, ar)
end

function glp_load_matrix(glp_prob::GLPProb, ia::VecOrNothing, ja::VecOrNothing, ar::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ia, ja, ar)
    l = _jl_glpk__vecornothing_length(ar)
    glp_load_matrix(glp_prob, l, ia, ja, ar)
end

function glp_load_matrix{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, a::SparseMatrixCSC{Tv, Ti})
    (ia, ja, ar) = findn_nzs(a)
    glp_load_matrix(glp_prob, ia, ja, ar)
end

function glp_check_dup{Ti<:Integer}(rows::Integer, cols::Integer, numel::Integer, ia::Vector{Ti}, ja::Vector{Ti})
    _jl_glpk__check_rows_and_cols(rows, cols)
    _jl_glpk__check_vectors_size(numel, ia, ja)
    ia32 = int32(ia)
    ja32 = int32(ja)

    off32 = sizeof(Int32)
    ia32p = pointer(ia32) - off32
    ja32p = pointer(ja32) - off32

    @glpk_ccall check_dup Int32 (Int32, Int32, Int32, Ptr{Int32}, Ptr{Int32}) rows cols numel ia32p ja32p
end

function glp_check_dup(rows::Integer, cols::Integer, numel::Integer, ia::VecOrNothing, ja::VecOrNothing)
    ia = _jl_glpk__convert_vecornothing(Int32, ia)
    ja = _jl_glpk__convert_vecornothing(Int32, ja)
    glp_check_dup(rows, cols, numel, ia, ja)
end

function glp_check_dup(rows::Integer, cols::Integer, ia::VecOrNothing, ja::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ia, ja)
    l = _jl_glpk__vecornothing_length(ia)
    glp_check_dup(rows, cols, l, ia, ja)
end

function glp_sort_matrix(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall sort_matrix Void (Ptr{Void},) glp_prob.p
end

function glp_del_rows{Ti<:Integer}(glp_prob::GLPProb, num_rows::Integer, rows_ids::AbstractVector{Ti})
    _jl_glpk__check_glp_prob(glp_prob)
    rows_ids32 = int32(rows_ids)
    _jl_glpk__check_rows_ids(glp_prob, 1, num_rows, rows_ids32)

    off32 = sizeof(Int32)
    rows_ids32p = pointer(rows_ids32) - off32
    @glpk_ccall del_rows Void (Ptr{Void}, Int32, Ptr{Int32}) glp_prob.p num_rows rows_ids32p
end
glp_del_rows{Ti<:Integer}(glp_prob::GLPProb, rows_ids::AbstractVector{Ti}) =
    glp_del_rows(glp_prob, length(rows_ids), rows_ids)

function glp_del_cols{Ti<:Integer}(glp_prob::GLPProb, num_cols::Integer, cols_ids::AbstractVector{Ti})
    _jl_glpk__check_glp_prob(glp_prob)
    cols_ids32 = int32(cols_ids)
    _jl_glpk__check_cols_ids(glp_prob, 1, num_cols, cols_ids32)

    off32 = sizeof(Int32)
    cols_ids32p = pointer(cols_ids32) - off32
    @glpk_ccall del_cols Void (Ptr{Void}, Int32, Ptr{Int32}) glp_prob.p num_cols cols_ids32p
end
glp_del_cols{Ti<:Integer}(glp_prob::GLPProb, cols_ids::AbstractVector{Ti}) =
    glp_del_cols(glp_prob, length(cols_ids), cols_ids)

function glp_copy_prob(glp_prob_dest::GLPProb, glp_prob::GLPProb, copy_names::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_copy_names_flag(copy_names)
    @glpk_ccall copy_prob Void (Ptr{Void}, Ptr{Void}, Int32) glp_prob_dest.p glp_prob.p copy_names
end

function glp_erase_prob(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall erase_prob Void (Ptr{Void},) glp_prob.p
end

function glp_get_prob_name(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    name_cstr = @glpk_ccall get_prob_name Ptr{Uint8} (Ptr{Void},) glp_prob.p
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function glp_get_obj_name(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    name_cstr = @glpk_ccall get_obj_name Ptr{Uint8} (Ptr{Void},) glp_prob.p
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function glp_get_obj_dir(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_obj_dir Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_num_rows(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_num_cols(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_row_name(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    name_cstr = @glpk_ccall get_row_name Ptr{Uint8} (Ptr{Void}, Int32) glp_prob.p row
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function glp_get_col_name(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    name_cstr = @glpk_ccall get_col_name Ptr{Uint8} (Ptr{Void}, Int32) glp_prob.p col
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function glp_get_row_type(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_type Int32 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_row_lb(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_lb Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_row_ub(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_ub Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_col_type(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_type Int32 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_col_lb(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_lb Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_col_ub(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_ub Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_obj_coef(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid_w0(glp_prob, col)
    @glpk_ccall get_obj_coef Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_num_nz(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_num_nz Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_mat_row(glp_prob::GLPProb, row::Integer, ind::Union(Vector{Int32},Nothing), val::Union(Vector{Float64},Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    numel = @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p row C_NULL C_NULL
    if numel == 0
        return 0
    end
    if !isequal(ind, nothing)
        _jl_glpk__check_vectors_size(numel, ind)
        off32 = sizeof(Int32)
        ind32p = pointer(ind) - off32
    else
        ind32p = C_NULL
    end
    if !isequal(val, nothing)
        _jl_glpk__check_vectors_size(numel, val)
        off64 = sizeof(Float64)
        val64p = pointer(val) - off64
    else
        val64p = C_NULL
    end
    @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p row ind32p val64p
end

function glp_get_mat_row(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    numel = @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p row C_NULL C_NULL
    if numel == 0
        return (Int32[], Float64[])
    end
    ind = Array(Int32, numel)
    val = Array(Float64, numel)

    off32 = sizeof(Int32)
    ind32p = pointer(ind) - off32
    off64 = sizeof(Float64)
    val64p = pointer(val) - off64
    @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p row ind32p val64p
    return ind, val
end

function glp_get_mat_col(glp_prob::GLPProb, col::Integer, ind::Union(Vector{Int32},Nothing), val::Union(Vector{Float64},Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    numel = @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p col C_NULL C_NULL
    if numel == 0
        return 0
    end
    if !isequal(ind, nothing)
        _jl_glpk__check_vectors_size(numel, ind)
        off32 = sizeof(Int32)
        ind32p = pointer(ind) - off32
    else
        ind32p = C_NULL
    end
    if !isequal(val, nothing)
        _jl_glpk__check_vectors_size(numel, val)
        off64 = sizeof(Float64)
        val64p = pointer(val) - off64
    else
        val64p = C_NULL
    end
    @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p col ind32p val64p
end

function glp_get_mat_col(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    numel = @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p col C_NULL C_NULL
    if numel == 0
        return (Int32[], Float64[])
    end
    ind = Array(Int32, numel)
    val = Array(Float64, numel)

    off32 = sizeof(Int32)
    ind32p = pointer(ind) - off32
    off64 = sizeof(Float64)
    val64p = pointer(val) - off64
    @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p col ind32p val64p
    return ind, val
end

function glp_create_index(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall create_index Void (Ptr{Void},) glp_prob.p
end

function glp_find_row(glp_prob::GLPProb, name::String)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall find_row Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(name)
end

function glp_find_col(glp_prob::GLPProb, name::String)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall find_col Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(name)
end

function glp_delete_index(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall delete_index Void (Ptr{Void},) glp_prob.p
end

function glp_set_rii(glp_prob::GLPProb, row::Integer, rii::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall set_rii Void (Ptr{Void}, Int32, Float64) glp_prob.p row rii
end

function glp_set_sjj(glp_prob::GLPProb, col::Integer, sjj::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall set_sjj Void (Ptr{Void}, Int32, Float64) glp_prob.p col sjj
end

function glp_get_rii(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_rii Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_sjj(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_sjj Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_scale_prob(glp_prob::GLPProb, flags::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_scale_flags(flags)
    @glpk_ccall scale_prob Void (Ptr{Void}, Int32) glp_prob.p flags
end

function glp_unscale_prob(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall unscale_prob Void (Ptr{Void},) glp_prob.p
end

function glp_set_row_stat(glp_prob::GLPProb, row::Integer, stat::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    _jl_glpk__check_stat_is_valid(stat)
    @glpk_ccall set_row_stat Void (Ptr{Void}, Int32, Int32) glp_prob.p row stat
end

function glp_set_col_stat(glp_prob::GLPProb, col::Integer, stat::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    _jl_glpk__check_stat_is_valid(stat)
    @glpk_ccall set_col_stat Void (Ptr{Void}, Int32, Int32) glp_prob.p col stat
end

function glp_std_basis(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall std_basis Void (Ptr{Void},) glp_prob.p
end

function glp_adv_basis(glp_prob::GLPProb, flags::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_adv_basis_flags(flags)
    @glpk_ccall adv_basis Void (Ptr{Void}, Int32) glp_prob.p flags
end
glp_adv_basis(glp_prob::GLPProb) = glp_adv_basis(glp_prob, 0)

function glp_cpx_basis(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall cpx_basis Void (Ptr{Void},) glp_prob.p
end

function glp_simplex{Tp<:Union(GLPSimplexParam, Nothing)}(glp_prob::GLPProb, glp_param::Tp)
    _jl_glpk__check_glp_prob(glp_prob)
    if glp_param == nothing
        param_ptr = C_NULL
    else
        param_ptr = pointer(glp_param)
    end
    @glpk_ccall simplex Int32 (Ptr{Void}, Ptr{Void}) glp_prob.p param_ptr
end

glp_simplex(glp_prob::GLPProb) =
    glp_simplex(glp_prob, nothing)

function glp_exact{Tp<:Union(GLPSimplexParam, Nothing)}(glp_prob::GLPProb, glp_param::Tp)
    _jl_glpk__check_glp_prob(glp_prob)
    if glp_param == nothing
        param_ptr = C_NULL
    else
        param_ptr = pointer(glp_param)
    end
    @glpk_ccall exact Int32 (Ptr{Void}, Ptr{Void}) glp_prob.p param_ptr
end

glp_exact(glp_prob::GLPProb) =
    glp_exact(glp_prob, nothing)

function glp_init_smcp(glp_param::GLPSimplexParam)
    _jl_glpk__check_simplex_param(glp_param)
    @glpk_ccall init_smcp Int32 (Ptr{Void},) pointer(glp_param)
end

function glp_get_status(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_status Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_prim_stat(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_prim_stat Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_dual_stat(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_dual_stat Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_obj_val(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_obj_val Float64 (Ptr{Void},) glp_prob.p
end

function glp_get_row_stat(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_row_prim(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_prim Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_row_dual(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_dual Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_col_stat(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_col_prim(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_prim Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_col_dual(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_dual Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_unbnd_ray(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_unbnd_ray Int32 (Ptr{Void},) glp_prob.p
end

function glp_interior{Tp<:Union(GLPInteriorParam, Nothing)}(glp_prob::GLPProb, glp_param::Tp)
    _jl_glpk__check_glp_prob(glp_prob)
    if glp_param == nothing
        param_ptr::Ptr{Void} = C_NULL
    else
        param_ptr = pointer(glp_param)
    end
    @glpk_ccall interior Int32 (Ptr{Void}, Ptr{Void}) glp_prob.p param_ptr
end

glp_interior(glp_prob::GLPProb) = glp_interior(glp_prob, nothing)

function glp_init_iptcp(glp_param::GLPInteriorParam)
    _jl_glpk__check_interior_param(glp_param)
    @glpk_ccall init_iptcp Int32 (Ptr{Void},) pointer(glp_param)
end

function glp_ipt_status(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall ipt_status Int32 (Ptr{Void},) glp_prob.p
end

function glp_ipt_obj_val(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall ipt_obj_val Float64 (Ptr{Void},) glp_prob.p
end

function glp_ipt_row_prim(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall ipt_row_prim Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_ipt_row_dual(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall ipt_row_dual Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_ipt_col_prim(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall ipt_col_prim Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_ipt_col_dual(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall ipt_col_dual Float64 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_set_col_kind(glp_prob::GLPProb, col::Integer, kind::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    _jl_glpk__check_kind_is_valid(kind)
    @glpk_ccall set_col_kind Void (Ptr{Void}, Int32, Int32) glp_prob.p col kind
end

function glp_get_col_kind(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_kind Int32 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_get_num_int(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_num_int Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_num_bin(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall get_num_bin Int32 (Ptr{Void},) glp_prob.p
end

function glp_intopt{Tp<:Union(GLPIntoptParam, Nothing)}(glp_prob::GLPProb, glp_param::Tp)
    _jl_glpk__check_glp_prob(glp_prob)
    if glp_param == nothing
        param_ptr::Ptr{Void} = C_NULL
    else
        param_ptr = pointer(glp_param)
    end
    @glpk_ccall intopt Int32 (Ptr{Void}, Ptr{Void}) glp_prob.p param_ptr
end

glp_intopt(glp_prob::GLPProb) = glp_intopt(glp_prob, nothing)

function glp_init_iocp(glp_param::GLPIntoptParam)
    _jl_glpk__check_intopt_param(glp_param)
    @glpk_ccall init_iocp Int32 (Ptr{Void},) pointer(glp_param)
end

function glp_mip_status(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall mip_status Int32 (Ptr{Void},) glp_prob.p
end

function glp_mip_obj_val(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall mip_obj_val Float64 (Ptr{Void},) glp_prob.p
end

function glp_mip_row_val(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall mip_row_val Float64 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_mip_col_val(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall mip_col_val Float64 (Ptr{Void}, Int32) glp_prob.p col
end

#TODO
#function lpx_check_kkt(glp_prob::GLPProb, scaled::Integer, kkt)

function glp_read_mps(glp_prob::GLPProb, format::Integer, param, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_mps_format(format)
    if is(param, nothing)
        param = C_NULL
    else
        _jl_glpk__check_mps_param(param)
    end

    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_mps Int32 (Ptr{Void}, Int32, Ptr{Void}, Ptr{Uint8}) glp_prob.p format param bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading MPS file"))
    end
    return ret
end

glp_read_mps(glp_prob::GLPProb, format::Integer, filename::String) =
    glp_read_mps(glp_prob, format, C_NULL, filename)

function glp_write_mps(glp_prob::GLPProb, format::Integer, param, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_mps_format(format)
    if is(param, nothing)
        param = C_NULL
    else
        _jl_glpk__check_mps_param(param)
    end
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_mps Int32 (Ptr{Void}, Int32, Ptr{Void}, Ptr{Uint8}) glp_prob.p format param bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing MPS file"))
    end
    return ret
end

glp_write_mps(glp_prob::GLPProb, format::Integer, filename::String) =
    glp_write_mps(glp_prob, format, C_NULL, filename)

function glp_read_lp(glp_prob::GLPProb, param, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_lp_param(param)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_lp Int32 (Ptr{Void}, Ptr{Void}, Ptr{Uint8}) glp_prob.p param bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading LP file"))
    end
    return ret
end

glp_read_lp(glp_prob::GLPProb, filename::String) =
    glp_read_lp(glp_prob, C_NULL, filename)

function glp_write_lp(glp_prob::GLPProb, param, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_lp_param(param)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_lp Int32 (Ptr{Void}, Ptr{Void}, Ptr{Uint8}) glp_prob.p param bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing LP file"))
    end
    return ret
end

glp_write_lp(glp_prob::GLPProb, filename::String) =
    glp_write_lp(glp_prob, C_NULL, filename)

function glp_read_prob(glp_prob::GLPProb, flags::Integer, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_read_prob_flags(flags)
    _jl_glpk__check_file_is_readable(filename)
    @glpk_ccall read_prob Int32 (Ptr{Void}, Int32, Ptr{Uint8}) glp_prob.p flags bytestring(filename)
end

glp_read_prob(glp_prob::GLPProb, filename::String) =
    glp_read_prob(glp_prob, 0, filename)

function glp_write_prob(glp_prob::GLPProb, flags::Integer, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_write_prob_flags(flags)
    _jl_glpk__check_file_is_writable(filename)
    @glpk_ccall write_prob Int32 (Ptr{Void}, Int32, Ptr{Uint8}) glp_prob.p flags bytestring(filename)
end

glp_write_prob(glp_prob::GLPProb, filename::String) =
    glp_write_prob(glp_prob, 0, filename)

function glp_mpl_read_model(glp_tran::GLPMathProgWorkspace, filename::String, skip::Integer)
    _jl_glpk__check_mpl_workspace(glp_tran)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall mpl_read_model Int32 (Ptr{Void}, Ptr{Uint8}, Int32) glp_tran.p bytestring(filename) skip
    if ret != 0
        throw(GLPError("Error reading MathProg file"))
    end
    return ret
end

function glp_mpl_read_data(glp_tran::GLPMathProgWorkspace, filename::String)
    _jl_glpk__check_mpl_workspace(glp_tran)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall mpl_read_data Int32 (Ptr{Void}, Ptr{Uint8}) glp_tran.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading MathProg data file"))
    end
    return ret
end

function glp_mpl_generate(glp_tran::GLPMathProgWorkspace, filename::Union(String, Nothing))
    _jl_glpk__check_mpl_workspace(glp_tran)
    if is(filename, nothing)
        cfilename = C_NULL
    else
        _jl_glpk__check_file_is_writable(filename)
        cfilename = bytestring(filename)
    end
    ret = @glpk_ccall mpl_generate Int32 (Ptr{Void}, Ptr{Uint8}) glp_tran.p cfilename
    if ret != 0
        throw(GLPError("Error generating MathProg model"))
    end
    return ret

end
glp_mpl_generate(glp_tran::GLPMathProgWorkspace) = glp_mpl_generate(glp_tran, nothing)

function glp_mpl_build_prob(glp_tran::GLPMathProgWorkspace, glp_prob::GLPProb)
    _jl_glpk__check_mpl_workspace(glp_tran)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall mpl_build_prob Void (Ptr{Void}, Ptr{Void}) glp_tran.p glp_prob.p
end

function glp_mpl_postsolve(glp_tran::GLPMathProgWorkspace, glp_prob::GLPProb, sol::Integer)
    _jl_glpk__check_mpl_workspace(glp_tran)
    _jl_glpk__check_glp_prob(glp_prob)
    if !(sol == GLP_SOL || sol == GLP_IPT || sol == GLP_MIP)
        throw(GLPError("Invalid parameter sol $sol (use GLP_SOL, GLP_IPT or GLP_MIP)"))
    end
    ret = @glpk_ccall mpl_postsolve Int32 (Ptr{Void}, Ptr{Void}, Int32) glp_tran.p glp_prob.p sol
    if ret != 0
        throw(GLPError("Error postsolving MathProg model"))
    end
    return ret
end

function glp_print_sol(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_sol Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error printing solution"))
    end
    return ret
end

function glp_read_sol(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_sol Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading solution"))
    end
    return ret
end

function glp_write_sol(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_sol Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing solution"))
    end
    return ret
end

function glp_print_ipt(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_ipt Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error printing interior point solution"))
    end
    return ret
end

function glp_read_ipt(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_ipt Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading interior point solution"))
    end
    return ret
end

function glp_write_ipt(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_ipt Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing interior point solution"))
    end
    return ret
end

function glp_print_mip(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_mip Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error printing mixed integer programming solution"))
    end
    return ret
end

function glp_read_mip(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_mip Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading mixed integer programming solution"))
    end
    return ret
end

function glp_write_mip(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_mip Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing mixed integer programming solution"))
    end
    return ret
end

function glp_print_ranges{Ti<:Integer}(glp_prob::GLPProb, len::Integer, list::Vector{Ti}, flags::Integer, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_vectors_size(len, list)
    _jl_glpk__check_status_is_optimal(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_print_ranges_flags(flags)
    _jl_glpk__check_file_is_writable(filename)

    if len > 0
        list32 = int32(list)
        _jl_glpk__check_list_ids(glp_prob, len, list32)

        off32 = sizeof(Int32)
        list32p = pointer(list32) - off32
    else
        list32p = C_NULL
    end

    @glpk_ccall print_ranges Int32 (Ptr{Void}, Int32, Ptr{Int32}, Int32, Ptr{Uint8}) glp_prob.p len list32p flags bytestring(filename)
end

glp_print_ranges{Ti<:Integer}(glp_prob::GLPProb, list::Vector{Ti}, flags::Integer, filename::String) =
    glp_print_ranges(glp_prob, length(list), list, flags, filename)

glp_print_ranges{Ti<:Integer}(glp_prob::GLPProb, len::Integer, list::Vector{Ti}, filename::String) = 
    glp_print_ranges(glp_prob, len, list, 0, filename)

glp_print_ranges{Ti<:Integer}(glp_prob::GLPProb, list::Vector{Ti}, filename::String) =
    glp_print_ranges(glp_prob, length(list), list, 0, filename)

function glp_print_ranges(glp_prob::GLPProb, len::Integer, list::VecOrNothing, flags::Integer, filename::String)
    list = _jl_glpk__convert_vecornothing(Int32, list)
    glp_print_ranges(glp_prob, len, list, flags, filename)
end

glp_print_ranges(glp_prob::GLPProb, list::VecOrNothing, flags::Integer, filename::String) =
    glp_print_ranges(glp_prob, _jl_glpk__vecornothing_length(list), list, flags, filename)

glp_print_ranges(glp_prob::GLPProb, len::Integer, list::VecOrNothing, filename::String) =
    glp_print_ranges(glp_prob, len, list, 0, filename)

glp_print_ranges(glp_prob::GLPProb, list::VecOrNothing, filename::String) =
    glp_print_ranges(glp_prob, _jl_glpk__vecornothing_length(list), list, 0, filename)

glp_print_ranges(glp_prob::GLPProb, filename::String) =
    glp_print_ranges(glp_prob, 0, nothing, 0, filename)

function glp_bf_exists(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall bf_exists Int32 (Ptr{Void},) glp_prob.p
end

function glp_factorize(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall factorize Int32 (Ptr{Void},) glp_prob.p
end

function glp_bf_updated(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall bf_updated Int32 (Ptr{Void},) glp_prob.p
end

function glp_get_bfcp(glp_prob::GLPProb, glp_param::GLPBasisFactParam)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bfcp(glp_param)
    @glpk_ccall get_bfcp Void (Ptr{Void}, Ptr{Void}) glp_prob.p pointer(glp_param)
end

function glp_set_bfcp(glp_prob::GLPProb, glp_param::Union(GLPBasisFactParam,Nothing))
    _jl_glpk__check_glp_prob(glp_prob)
    if is(glp_param, nothing)
        glp_param_p = C_NULL
    else
        _jl_glpk__check_bfcp(glp_param)
        glp_param_p = pointer(glp_param)
    end
    @glpk_ccall set_bfcp Void (Ptr{Void}, Ptr{Void}) glp_prob.p glp_param_p
end
glp_set_bfcp(glp_prob::GLPProb) = glp_set_bfcp(glp_prob, nothing)

function glp_get_bhead(glp_prob::GLPProb, k::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, k)
    @glpk_ccall get_bhead Int32 (Ptr{Void}, Int32) glp_prob.p k
end

function glp_get_row_bind(glp_prob::GLPProb, row::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, row)
    @glpk_ccall get_row_bind Int32 (Ptr{Void}, Int32) glp_prob.p row
end

function glp_get_col_bind(glp_prob::GLPProb, col::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, col)
    @glpk_ccall get_col_bind Int32 (Ptr{Void}, Int32) glp_prob.p col
end

function glp_ftran(glp_prob::GLPProb, x::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    _jl_glpk__check_vectors_size(rows, x)
    off64 = sizeof(Float64)
    x64p = pointer(x) - off64
    @glpk_ccall ftran Void (Ptr{Void}, Ptr{Float64}) glp_prob.p x64p
end

function glp_btran(glp_prob::GLPProb, x::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    _jl_glpk__check_vectors_size(rows, x)
    off64 = sizeof(Float64)
    x64p = pointer(x) - off64
    @glpk_ccall btran Void (Ptr{Void}, Ptr{Float64}) glp_prob.p x64p
end

function glp_warm_up(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall warm_up Int32 (Ptr{Void},) glp_prob.p
end

function glp_eval_tab_row(glp_prob::GLPProb, k::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(GLPError("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_basic(glp_prob, k)

    grow(ind, k_max - length(ind))
    grow(val, k_max - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return len
end

function glp_eval_tab_row(glp_prob::GLPProb, k::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(GLPError("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_basic(glp_prob, k)

    ind = Array(Int32, k_max)
    val = Array(Float64, k_max)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return ind, val
end

function glp_eval_tab_col(glp_prob::GLPProb, k::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(GLPError("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_non_basic(glp_prob, k)

    grow(ind, k_max - length(ind))
    grow(val, k_max - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return len
end

function glp_eval_tab_col(glp_prob::GLPProb, k::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(GLPError("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_non_basic(glp_prob, k)

    ind = Array(Int32, k_max)
    val = Array(Float64, k_max)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return ind, val
end

function glp_transform_row(glp_prob::GLPProb, len::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)

    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) glp_prob.p

    grow(ind, cols - length(ind))
    grow(val, cols - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len1 = @glpk_ccall transform_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p len ind32p val64p

    del(ind, len1+1:length(ind))
    del(val, len1+1:length(val))

    return len1
end

function glp_transform_row(glp_prob::GLPProb, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_vectors_all_same_size(ind, val)
    glp_transform_row(glp_prob, length(ind), ind, val)
end

function glp_transform_col(glp_prob::GLPProb, len::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) glp_prob.p

    grow(ind, rows - length(ind))
    grow(val, rows - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len1 = @glpk_ccall transform_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) glp_prob.p len ind32p val64p

    del(ind, len1+1:length(ind))
    del(val, len1+1:length(val))

    return len1
end

function glp_transform_col(glp_prob::GLPProb, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_vectors_all_same_size(ind, val)
    glp_transform_col(glp_prob, length(ind), ind, val)
end

function glp_prim_rtest{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, len::Integer, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_is_prim_feasible(glp_prob)
    _jl_glpk__check_row_is_valid(glp_prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_dir_is_valid(dir)
    _jl_glpk__check_eps_is_valid(eps)

    for i = 1:len
        _jl_glpk__check_var_is_basic(glp_prob, ind[i])
    end

    ind32 = int32(ind)
    val64 = float64(val)
    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind32) - off32
    val64p = pointer(val64) - off64

    piv = @glpk_ccall prim_rtest Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}, Int32, Float64) glp_prob.p len ind32p val64p dir eps
    return piv
end

function glp_prim_rtest{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    glp_prim_rtest(glp_prob, length(ind), ind, val, dir, eps)
end

function glp_dual_rtest{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, len::Integer, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_is_dual_feasible(glp_prob)
    _jl_glpk__check_col_is_valid(glp_prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_dir_is_valid(dir)
    _jl_glpk__check_eps_is_valid(eps)

    for i = 1:len
        _jl_glpk__check_var_is_non_basic(glp_prob, ind[i])
    end

    ind32 = int32(ind)
    val64 = float64(val)
    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind32) - off32
    val64p = pointer(val64) - off64

    piv = @glpk_ccall dual_rtest Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}, Int32, Float64) glp_prob.p len ind32p val64p dir eps
    return piv
end

function glp_dual_rtest{Ti<:Integer, Tv<:Real}(glp_prob::GLPProb, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    glp_dual_rtest(glp_prob, length(ind), ind, val, dir, eps)
end

function glp_analyze_bound(glp_prob::GLPProb, k, limit1, var1, limit2, var2)
    error("Unsupported. Use glp_analyze_bound(glp_prob, k) instead.")
end

function glp_analyze_bound(glp_prob::GLPProb, k::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_rowcol_is_valid(glp_prob, k)
    _jl_glpk__check_var_is_non_basic(glp_prob, k)

    limit1 = Array(Float64, 1)
    var1 = Array(Int32, 1)
    limit2 = Array(Float64, 1)
    var2 = Array(Int32, 1)

    @glpk_ccall analyze_bound Void (Ptr{Void}, Int32, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}) glp_prob.p k pointer(limit1) pointer(var1) pointer(limit2) pointer(var2)

    return limit1[1], var1[1], limit2[1], var2[1]
end

function glp_analyze_coef(glp_prob::GLPProb, k, coef1, var1, value1, coef2, var2, value2)
    error("Unsupported. Use glp_analyze_coef(glp_prob, k) instead.")
end

function glp_analyze_coef(glp_prob::GLPProb, k::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_bf_exists(glp_prob)
    _jl_glpk__check_rowcol_is_valid(glp_prob, k)
    _jl_glpk__check_var_is_basic(glp_prob, k)

    coef1 = Array(Float64, 1)
    var1 = Array(Int32, 1)
    value1 = Array(Float64, 1)
    coef2 = Array(Float64, 1)
    var2 = Array(Int32, 1)
    value2 = Array(Float64, 1)

    @glpk_ccall analyze_coef Void (Ptr{Void}, Int32, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}) glp_prob.p k pointer(coef1) pointer(var1) pointer(value1) pointer(coef2) pointer(var2) pointer(value2)

    return coef1[1], var1[1], value1[1], coef2[1], var2[1], value2[1] 
end

function glp_init_env()
    ret = @glpk_ccall init_env Int32 ()
    _jl_glpk__check_init_env_succeeded(ret)
    return ret
end

function glp_free_env()
    ret = @glpk_ccall free_env Int32 ()
end

function glp_term_out(flag::Integer)
    _jl_glpk__check_term_out_flag(flag)
    @glpk_ccall term_out Int32 (Int32,) flag
end

function glp_open_tee(filename::String)
    ret = @glpk_ccall open_tee Int32 (Ptr{Uint8},) bytestring(filename)
    _jl_glpk__check_open_tee_succeeded(ret)
    return ret
end

function glp_close_tee()
    @glpk_ccall close_tee Int32 ()
end

function glp_malloc(size::Integer)
    _jl_glpk__check_alloc_size(size)
    @glpk_ccall malloc Ptr{Void} (Int32,) size
end

function glp_calloc(n::Integer, size::Integer)
    _jl_glpk__check_alloc_size(n)
    _jl_glpk__check_alloc_size(size)
    @glpk_ccall calloc Ptr{Void} (Int32, Int32) n size
end

function glp_free(ptr::Ptr)
    _jl_glpk__check_pointer_is_valid(ptr)
    @glpk_ccall free Void (Ptr{Void},) ptr
end

function glp_mem_usage(count, cpeak, total, tpeak)
    error("Unsupported. Use glp_mem_usage() instead.")
end

function glp_mem_usage()
    data32 = Array(Int32, 2)
    data32_p = pointer(data32)
    off32 = sizeof(Int32)
    count_p = data32_p
    cpeak_p = data32_p + off32

    data64 = Array(Int64, 2)
    data64_p = pointer(data64)
    off64 = sizeof(Int64)

    total_p = data64_p
    tpeak_p = data64_p + off64

    @glpk_ccall mem_usage Void (Ptr{Int32}, Ptr{Int32}, Ptr{Int64}, Ptr{Int64}) count_p cpeak_p total_p tpeak_p 

    count = data32[1]
    cpeak = data32[2]
    total = data64[1]
    tpeak = data64[2]

    return count, cpeak, total, tpeak
end

function glp_mem_limit(limit::Integer)
    @glpk_ccall mem_limit Void (Int32,) limit
end

function glp_time()
    @glpk_ccall time Int64 ()
end

function glp_difftime(t1::Integer, t0::Integer)
    @glpk_ccall difftime Float64 (Int64, Int64) t1 t0
end

function glp_sdf_open_file(filename::String)
    _jl_glpk__check_file_is_readable(filename)
    data_p = @glpk_ccall sdf_open_file Ptr{Void} (Ptr{Uint8},) bytestring(filename)
    _jl_glpk__check_sdf_file_opened(data_p)
    return GLPData(data_p)
end

function glp_sdf_read_int(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    @glpk_ccall sdf_read_int Int32 (Ptr{Void},) pointer(glp_data)
end

function glp_sdf_read_num(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    @glpk_ccall sdf_read_num Float64 (Ptr{Void},) pointer(glp_data)
end

function glp_sdf_read_item(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    item_cstr = @glpk_ccall sdf_read_item Ptr{Uint8} (Ptr{Void},) pointer(glp_data)
    return bytestring(item_cstr)
end

function glp_sdf_read_text(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    text_cstr = @glpk_ccall sdf_read_text Ptr{Uint8} (Ptr{Void},) pointer(glp_data)
    return bytestring(text_cstr)
end

function glp_sdf_line(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    @glpk_ccall sdf_line Int32 (Ptr{Void},) pointer(glp_data)
end

function glp_sdf_close_file(glp_data::GLPData)
    _jl_glpk__check_data(glp_data)
    @glpk_ccall sdf_close_file Void (Ptr{Void},) pointer(glp_data)
end

function glp_read_cnfsat(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_cnfsat Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error reading CNF file"))
    end
    return ret
end

function glp_check_cnfsat(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall check_cnfsat Int32 (Ptr{Void},) glp_prob.p
end

function glp_write_cnfsat(glp_prob::GLPProb, filename::String)
    _jl_glpk__check_glp_prob(glp_prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_cnfsat Int32 (Ptr{Void}, Ptr{Uint8}) glp_prob.p bytestring(filename)
    if ret != 0
        throw(GLPError("Error writing CNF file"))
    end
    return ret
end

function glp_minisat1(glp_prob::GLPProb)
    _jl_glpk__check_glp_prob(glp_prob)
    @glpk_ccall minisat1 Int32 (Ptr{Void},) glp_prob.p
end

function glp_intfeas1(glp_prob::GLPProb, use_bound::Integer, obj_bound::Integer)
    _jl_glpk__check_glp_prob(glp_prob)
    # TODO : more checks:
    #   1) columns must be GLP_BV od GLP_FX
    #   2) constraints and objj coeffs must be integer
    @glpk_ccall intfeas1 Int32 (Ptr{Void}, Int32, Int32) glp_prob.p use_bound obj_bound
end


# FUNCTIONS NOT WRAPPED:
#
# 1) All glp_ios_* [because they should be called
#    from within a callback]:
#
#    glp_ios_reason
#    glp_ios_get_prob
#    glp_ios_row_attr
#    glp_ios_mip_gap
#    glp_ios_node_data
#    glp_ios_select_node
#    glp_ios_heur_sol
#    glp_ios_can_branch
#    glp_ios_branch_upon
#    glp_ios_terminate
#    glp_ios_tree_size
#    glp_ios_curr_node
#    glp_ios_next_node
#    glp_ios_prev_node
#    glp_ios_up_node
#    glp_ios_node_level
#    glp_ios_node_bound
#    glp_ios_best_node
#    glp_ios_pool_size
#    glp_ios_add_row
#    glp_ios_del_row
#    glp_ios_clear_pool
#
# 2) Printout functions [because they use varargs/va_list,
#    or need callbacks, or are implemented as macros]:
#
#    glp_printf
#    glp_vprintf
#    glp_term_hook
#    glp_error
#    glp_assert
#    glp_error_hook
#
# 3) Plain data file reading functions [because they either
#    use longjmp or varargs]:
#
#    glp_sdf_set_jump
#    glp_sdf_error
#    glp_sdf_warning
#
# 4) Additional functions [because wat?]:
#
#    lpx_check_kkt

#}}}
