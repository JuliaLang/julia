###
### GLPK API Wrapper
###

module GLPK

export
    # Types
    Param,
    Error,
    Prob,
    SimplexParam,
    InteriorParam,
    IntoptParam,
    BasisFact,
    Data,
    MathProgWorkspace,

    # Methods
    version,
    set_prob_name,
    set_obj_name,
    set_row_name,
    set_col_name,
    set_obj_dir,
    add_rows,
    add_cols,
    set_row_bnds,
    set_col_bnds,
    set_obj_coef,
    set_mat_row,
    set_mat_col,
    load_matrix,
    check_dup,
    sort_matrix,
    del_rows,
    del_cols,
    copy_prob,
    erase_prob,
    get_prob_name,
    get_obj_name,
    get_obj_dir,
    get_num_rows,
    get_num_cols,
    get_row_name,
    get_col_name,
    get_row_type,
    get_row_lb,
    get_row_ub,
    get_col_type,
    get_col_lb,
    get_col_ub,
    get_obj_coef,
    get_num_nz,
    get_mat_row,
    get_mat_col,
    create_index,
    find_row,
    find_col,
    delete_index,
    set_rii,
    set_sjj,
    get_rii,
    get_sjj,
    scale_prob,
    unscale_prob,
    set_row_stat,
    set_col_stat,
    std_basis,
    adv_basis,
    cpx_basis,
    simplex,
    exact,
    init_smcp,
    get_status,
    get_prim_stat,
    get_dual_stat,
    get_obj_val,
    get_row_stat,
    get_row_prim,
    get_row_dual,
    get_col_stat,
    get_col_prim,
    get_col_dual,
    get_unbnd_ray,
    interior,
    init_iptcp,
    ipt_status,
    ipt_obj_val,
    ipt_row_prim,
    ipt_row_dual,
    ipt_col_prim,
    ipt_col_dual,
    set_col_kind,
    get_col_kind,
    get_num_int,
    get_num_bin,
    intopt,
    init_iocp,
    mip_status,
    mip_obj_val,
    mip_row_val,
    mip_col_val,
    read_mps,
    write_mps,
    read_lp,
    write_lp,
    read_prob,
    write_prob,
    mpl_read_model,
    mpl_read_data,
    mpl_generate,
    mpl_build_prob,
    mpl_postsolve,
    print_sol,
    read_sol,
    write_sol,
    print_ipt,
    read_ipt,
    write_ipt,
    print_mip,
    read_mip,
    write_mip,
    print_ranges,
    print_ranges,
    bf_exists,
    factorize,
    bf_updated,
    get_bfcp,
    set_bfcp,
    get_bhead,
    get_row_bind,
    get_col_bind,
    ftran,
    btran,
    warm_up,
    eval_tab_row,
    eval_tab_col,
    transform_row,
    transform_col,
    prim_rtest,
    dual_rtest,
    analyze_bound,
    analyze_coef,
    init_env,
    free_env,
    term_out,
    open_tee,
    close_tee,
    malloc,
    calloc,
    free,
    mem_usage,
    mem_limit,
    time,
    difftime,
    sdf_open_file,
    sdf_read_int,
    sdf_read_num,
    sdf_read_item,
    sdf_read_text,
    sdf_line,
    sdf_close_file,
    read_cnfsat,
    check_cnfsat,
    write_cnfsat,
    minisat1,
    intfeas1

import Base.pointer, Base.assign, Base.ref

## Shared library interface setup
#{{{
include("$JULIA_HOME/../share/julia/extras/glpk_h.jl")

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

# We need to define GLPK.version as first thing
# in order to perform a sanity check
# (since we import structs from the header,
# we must ensure that the binary is the correct
# one)
function version()
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

if version() != (MAJOR_VERSION, MINOR_VERSION)
    bv = version()
    hv = (MAJOR_VERSION, MINOR_VERSION)
    error("GLPK error: mismatched versions: header=$(hv[1]).$(hv[2]) binary=$(bv[1]).$(bv[2])")
end
#}}}

## Preliminary definitions
#{{{

# General structure for the parameters types

abstract Param

pointer(param::Param) = param.struct

typealias ParamFieldDescriptor (ASCIIString, BitsKind)

type ParamDescriptor
    struct_name::String
    field_names::Vector{ASCIIString}
    field_types::Vector{BitsKind}
    function ParamDescriptor(cstr::String, struct_desc)
        struct_name = cstr
        c_struct_desc = convert(Vector{ParamFieldDescriptor}, struct_desc)
        field_names = [ x[1]::ASCIIString for x = c_struct_desc ]
        field_types = [ x[2]::BitsKind for x = c_struct_desc ]
        new(struct_name, field_names, field_types)
    end
end

function assign{T}(param::Param, val::T, field_name::String)
    if pointer(param) == C_NULL
        error("param is not allocated")
    end
    for i = 1 : length(param.desc.field_names)
        if field_name == param.desc.field_names[i]
            if pointer(param) == C_NULL
                throw(Error("invalid struct"))
            end
            t = param.desc.field_types[i]
            csf = strcat("_jl_glpkw__", param.desc.struct_name, "_set_", field_name)
            ccs = :(ccall(dlsym(GLPK._jl_libglpk_wrapper, $csf), Void, (Ptr{Void}, $t), pointer($param), $val))
            eval(ccs)
            return
        end
    end
    error("field '$field_name' not found in struct '$(param.desc.struct_name)'")
end

function ref(param::Param, field_name::String)
    if pointer(param) == C_NULL
        error("param is not allocated")
    end
    for i = 1 : length(param.desc.field_names)
        if field_name == param.desc.field_names[i]
            if pointer(param) == C_NULL
                throw(Error("invalid struct"))
            end
            t = param.desc.field_types[i]
            cgf = strcat("_jl_glpkw__", param.desc.struct_name, "_get_", field_name)
            ccg = :(ccall(dlsym(GLPK._jl_libglpk_wrapper, $cgf), $t, (Ptr{Void},), pointer($param)))
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
            throw(Error("integer-valued array required, or [] or nothing"))
        end
    elseif T <: Real
        if !(eltype(a) <: Real)
            throw(Error("real-valued array required, or [] or nothing"))
        end
    end
    convert(Array{T}, a)
end
_jl_glpk__vecornothing_length(a::VecOrNothing) = is(a, nothing) ? 0 : length(a)


# General exception: all GLP functions
# throw this in case of errors
type Error <: Exception
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
#  int glp_simplex(prob * lp, glp_smpc * param)
#
# becomes
#
#  GLPK.simplex(lp::GLPK.Prob, param::GLPK.SimplexParam)
#
#
# The map between names is as follows:
#
# +-------------+--------------------------+
# |  C          |  Julia                   |
# +-------------+--------------------------+
# |  glp_prob   |  GLPK.Prob               |
# |  glp_smcp   |  GLPK.SimplexParam       |
# |  glp_iptcp  |  GLPK.InteriorParam      |
# |  glp_iocp   |  GLPK.IntoptParam        |
# |  glp_bfcp   |  GLPK.BasisFactParam     |
# |  glp_tran   |  GLPK.MathProgWorkspace  |
# |  glp_data   |  GLPK.Data               |
# +-------------+--------------------------+
#
# In order to get/set the value of a cstruct field, you can
# use vector-like referncing with the field name as an argument,
# e.g.:
#
#   lps_opts = GLPK.SimplexParam()
#   lps_opts["msg_lev"] = GLPK.MSG_ERR
#   lps_opts["presolve"] = GLPK.ON
#

type Prob
    p::Ptr{Void}
    function Prob()
        p = @glpk_ccall create_prob Ptr{Void} ()
        prob = new(p)
        finalizer(prob, delete_prob)
        return prob
    end
end

function delete_prob(prob::Prob)
    if prob.p == C_NULL
        return
    end
    @glpk_ccall delete_prob Void (Ptr{Void},) prob.p
    prob.p = C_NULL
    return
end


_jl_glpk__simplex_param_struct_desc = ParamDescriptor("smcp",
        [("msg_lev", Int32), ("meth", Int32), ("pricing", Int32),
         ("r_test", Int32), ("tol_bnd", Float64), ("tol_dj", Float64),
         ("tol_piv", Float64), ("obj_ll", Float64), ("obj_ul", Float64),
         ("it_lim", Int32), ("tm_lim", Int32), ("out_frq", Int32),
         ("out_dly", Int32), ("presolve", Int32)])

type SimplexParam <: Param
    struct::Ptr{Void}
    desc::ParamDescriptor
    function SimplexParam()
        struct = @glpkw_ccall smcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__simplex_param_struct_desc)
        finalizer(param, _jl_glpkw__smcp_delete)
        return param
    end
end

function _jl_glpkw__smcp_delete(param::SimplexParam)
    @glpkw_ccall smcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__interior_param_struct_desc = ParamDescriptor("iptcp",
        [("msg_lev", Int32), ("ord_alg", Int32)])

type InteriorParam <: Param
    struct::Ptr{Void}
    desc::ParamDescriptor
    function InteriorParam()
        struct = @glpkw_ccall iptcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__interior_param_struct_desc)
        finalizer(param, _jl_glpkw__iptcp_delete)
        return param
    end
end

function _jl_glpkw__iptcp_delete(param::InteriorParam)
    @glpkw_ccall iptcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__intopt_param_struct_desc = ParamDescriptor("iocp",
    [("msg_lev", Int32), ("br_tech", Int32), ("bt_tech", Int32),
     ("pp_tech", Int32), ("fp_heur", Int32), ("gmi_cuts", Int32),
     ("mir_cuts", Int32), ("cov_cuts", Int32), ("clq_cuts", Int32),
     ("tol_int", Float64), ("tol_obj", Float64), ("mip_gap", Float64),
     ("tm_lim", Int32), ("out_frq", Int32), ("out_dly", Int32),
     ("cb_func", Ptr{Void}), ("cb_info", Ptr{Void}), ("cb_size", Int32),
     ("presolve", Int32), ("binarize", Int32)])

type IntoptParam <: Param
    struct::Ptr{Void}
    desc::ParamDescriptor
    function IntoptParam()
        struct = @glpkw_ccall iocp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__intopt_param_struct_desc)
        finalizer(param, _jl_glpkw__iocp_delete)
        return param
    end
end

function _jl_glpkw__iocp_delete(param::IntoptParam)
    @glpkw_ccall iocp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end


_jl_glpk__basisfact_param_struct_desc = ParamDescriptor("bfcp",
    [("type", Int32), ("lu_size", Int32), ("piv_tol", Float64),
     ("piv_lim", Int32), ("suhl", Int32), ("eps_tol", Float64),
     ("max_gro", Float64), ("nfs_max", Int32), ("upd_tol", Float64),
     ("nrs_max", Int32), ("rs_size", Int32)])

type BasisFactParam <: Param
    struct::Ptr{Void}
    desc::ParamDescriptor
    function BasisFactParam()
        struct = @glpkw_ccall bfcp_init Ptr{Void} ()
        param = new(struct, _jl_glpk__basisfact_param_struct_desc)
        finalizer(param, _jl_glpkw__bfcp_delete)
        return param
    end
end

function _jl_glpkw__bfcp_delete(param::BasisFactParam)
    @glpkw_ccall bfcp_delete Void (Ptr{Void},) pointer(param)
    param.struct = C_NULL
end

type Data
    p::Ptr{Void}
end

pointer(data::Data) = data.p

type MathProgWorkspace
    p::Ptr{Void}
    function MathProgWorkspace()
        tran = @glpk_ccall mpl_alloc_wksp Ptr{Void} ()
        wksp = new(tran)
        finalizer(wksp, GLPK.mpl_free_wksp)
        return wksp
    end
end

function mpl_free_wksp(tran::MathProgWorkspace)
    if tran.p == C_NULL
        return
    end
    @glpk_ccall mpl_free_wksp Void (Ptr{Void},) tran.p
    tran.p = C_NULL
    return
end
#}}}


## Check functions for internal use
#{{{
# Functions which perform all sorts of
# sanity checks on input parameters and
# throw exceptions in case of errors.
# Ideally, it should never be possible
# to pass an invalid parameter to the
# underlying glp API.

function _jl_glpk__check_prob(prob::Prob)
    if prob.p == C_NULL
        throw(Error("Invalid GLPK.Prob"))
    end
    return true
end

function _jl_glpk__check_string_length(s::String, minl::Integer, maxl::Integer)
    l = length(s)
    if !(minl <= l <= maxl)
        throw(Error("Invalid string length $l (must be $minl <= length <= $maxl)"))
    end
    return true
end

function _jl_glpk__check_row_is_valid(prob::Prob, row::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    if (row < 1 || row > rows)
        throw(Error("Invalid row $row (must be 1 <= row <= $rows)"))
    end
    return true
end

function _jl_glpk__check_col_is_valid(prob::Prob, col::Integer)
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
    if (col < 1 || col > cols)
        throw(Error("Invalid col $col (must be 1 <= col <= $cols)"))
    end
    return true
end

function _jl_glpk__check_col_is_valid_w0(prob::Prob, col::Integer)
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
    if (col < 0 || col > cols)
        throw(Error("Invalid col $col (must be 0 <= col <= $cols)"))
    end
    return true
end

function _jl_glpk__check_obj_dir_is_valid(dir::Integer)
    if !(dir == MIN || dir == MAX)
        throw(Error("Invalid obj_dir $dir (use MIN or MAX)"))
    end
    return true
end

function _jl_glpk__check_bounds_type_is_valid(bounds_type::Integer)
    if !(bounds_type == FR ||
         bounds_type == LO ||
         bounds_type == UP ||
         bounds_type == DB ||
         bounds_type == FX)
        throw(Error("Invalid bounds_type $bounds_type (allowed values: GLPK.FR, GLPK.LO, GLPK.UP, GLPK.DB, GLPK.FX)"))
    end
    return true
end

function _jl_glpk__check_bounds_are_valid(bounds_type::Integer, lb::Real, ub::Real)
    if bounds_type == DB && lb > ub
        throw(Error("Invalid bounds for double-bounded variable: $lb > $ub"))
    elseif bounds_type == FX && lb != ub
        throw(Error("Invalid bounds for fixed variable: $lb != $ub"))
    end
    return true
end

function _jl_glpk__check_vectors_size(numel::Integer, vecs...)
    if numel < 0
        throw(Error("Invalid numer of elements: $numel"))
    end
    if numel > 0
        for v = vecs
            if isempty(v)
                throw(Error("Number of elements is $numel but vector is empty or nothing"))
            elseif length(v) < numel
                throw(Error("Wrong vector size: $(length(v)) (numel declared as $numel)"))
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
            throw(Error("incosistent vector lengths: $l0 and $l"))
        end
    end
    return true
end

function _jl_glpk__check_indices_vectors_dup(prob::Prob, numel::Integer, ia::Vector{Int32}, ja::Vector{Int32})
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
    #numel = length(ia)

    off32 = sizeof(Int32)
    iap = pointer(ia) - off32
    jap = pointer(ja) - off32

    k = @glpk_ccall check_dup Int32 (Int32, Int32, Int32, Ptr{Int32}, Ptr{Int32}) rows cols numel iap jap
    if k < 0
        throw(Error("indices out of bounds: $(ia[-k]),$(ja[-k]) (bounds are (1,1) <= (ia,ja) <= ($rows,$cols))"))
    elseif k > 0
        throw(Error("duplicate index entry: $(ia[k]),$(ja[k])"))
    end
    return true
end

function _jl_glpk__check_rows_and_cols(rows::Integer, cols::Integer)
    if (rows < 0)
        throw(Error("rows < 0 : $rows"))
    end
    if (cols < 0)
        throw(Error("cols < 0 : $rows"))
    end
end

function _jl_glpk__check_rows_ids(prob::Prob, min_size::Integer, num_rows::Integer, rows_ids::Vector{Int32})
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    if num_rows < min_size || num_rows > rows
        throw(Error("invalid vector size: $num_rows (min=$min_size max=$rows)"))
    end
    if num_rows == 0
        return true
    end
    if length(rows_ids) < num_rows
        throw(Error("invalid vector size: declared>=$num_rows actual=$(length(rows_ids))"))
    end
    ind_set = IntSet()
    add_each(ind_set, rows_ids[1 : num_rows])
    if min(ind_set) < 1 || max(ind_set) > rows
        throw(Error("index out of bounds (min=1 max=$rows)"))
    elseif length(ind_set) != length(rows_ids)
        throw(Error("one or more duplicate index(es) found"))
    end
    return true
end

function _jl_glpk__check_cols_ids(prob::Prob, min_size::Integer, num_cols::Integer, cols_ids::Vector{Int32})
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
    if num_cols < min_size || num_cols > cols
        throw(Error("invalid vector size: $num_cols (min=$min_size max=$cols)"))
    end
    if num_cols == 0
        return 0
    end
    if length(cols_ids) < num_cols
        throw(Error("invalid vector size: declared>=$num_cols actual=$(length(cols_ids))"))
    end
    ind_set = IntSet()
    add_each(ind_set, cols_ids[1 : num_cols])
    if min(ind_set) < 1 || max(ind_set) > cols
        throw(Error("index out of bounds (min=1 max=$cols)"))
    elseif length(ind_set) != length(cols_ids)
        throw(Error("one or more duplicate index(es) found"))
    end
    return true
end

function _jl_glpk__check_list_ids(prob::Prob, len::Integer, list_ids::Vector{Int32})
    if len == 0
        return true
    end
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
    # note1 the documentation does not mention forbidding duplicates in this case
    # note2 the size should already be checked as this function is only called
    #       by GLPK.print_ranges
    #if len < 0 #|| len > rows + cols
    ##throw(Error("invalid vector size: $len (min=0 max=$(rows + cols))"))
    #throw(Error("invalid vector size: $len < 0"))
    #end
    #if length(list_ids) < len
    #throw(Error("invalid vector size: declared>=$len actual=$(length(list_ids))"))
    #end
    if min(list_ids[1:len]) < 1 || max(list_ids[1:len]) > rows + cols
        throw(Error("index out of bounds (min=1 max=$(rows + cols))"))
    end
    return true
end

function _jl_glpk__check_status_is_optimal(prob::Prob)
    ret = @glpk_ccall get_status Int32 (Ptr{Void},) prob.p
    if ret == OPT
        throw(Error("current basic solution is not optimal"))
    end
    return true
end

function _jl_glpk__check_bf_exists(prob::Prob)
    ret = @glpk_ccall bf_exists Int32 (Ptr{Void},) prob.p
    if ret == 0
        throw(Error("no bf solution found (use GLPK.factorize)"))
    end
    return true
end

function _jl_glpk__check_var_is_basic(prob::Prob, ind::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    if ind <= rows
        j = @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) prob.p ind
        if j != BS
            throw(Error("variable $ind is non-basic"))
        end
    else
        j = @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) prob.p ind-rows
        if j != BS
            throw(Error("variable $ind is non-basic"))
        end
    end
end

function _jl_glpk__check_var_is_non_basic(prob::Prob, ind::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    if ind <= rows
        j = @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) prob.p ind
        if j == BS
            throw(Error("variable $ind is basic"))
        end
    else
        j = @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) prob.p ind-rows
        if j == BS
            throw(Error("variable $ind is basic"))
        end
    end
end

function _jl_glpk__check_is_prim_feasible(prob::Prob)
    if FEAS != @glpk_ccall get_prim_stat Int32 (Ptr{Void},) prob.p
        throw(Error("problem is not primal feasible"))
    end
    return true
end

function _jl_glpk__check_is_dual_feasible(prob::Prob)
    if FEAS != @glpk_ccall get_dual_stat Int32 (Ptr{Void},) prob.p
        throw(Error("problem is not dual feasible"))
    end
    return true
end

function _jl_glpk__check_copy_names_flag(names::Integer)
    if names != ON && names != OFF
        throw(Error("invalid copy_names flag $names (use GLPK.ON or GLPK.OFF)"))
    end
    return true
end

function _jl_glpk__check_scale_flags(flags::Integer)
    all = (SF_GM | SF_EQ | SF_2N | SF_SKIP)
    if (flags | all) != all && flags != SF_AUTO
        throw(Error("invalid scale flags $flags"))
    end
    return true
end

function _jl_glpk__check_stat_is_valid(stat::Integer)
    if (stat != BS &&
        stat != NL &&
        stat != NU &&
        stat != NF &&
        stat != NS)
        throw(Error("invalid status $stat (use GLPK.BS or GLPK.NL or GLPK.NU or GLPK.NF or GLPK.NS)"))
    end
end

function _jl_glpk__check_adv_basis_flags(flags::Integer)
    if flags != 0
        throw(Error("adv_basis flags must be set to 0 (found $flags instead)"))
    end
    return true
end

function _jl_glpk__check_simplex_param(param::SimplexParam)
    if pointer(param) == C_NULL
        throw(Error("param = NULL"))
    end
    return true
end

function _jl_glpk__check_interior_param(param::InteriorParam)
    if pointer(param) == C_NULL
        throw(Error("param = NULL"))
    end
    return true
end

function _jl_glpk__check_kind_is_valid(kind::Integer)
    if (kind != CV &&
        kind != IV &&
        kind != BV)
        throw(Error("invalid kind $kind (use GLPK.CV or GLPK.IV or GLPK.BV)"))
    end
    return true
end

function _jl_glpk__check_intopt_param(param::IntoptParam)
    if pointer(param) == C_NULL
        throw(Error("param = NULL"))
    end
    return true
end

function _jl_glpk__check_file_is_readable(filename::String)
    try
        f = open(filename, "r")
        close(f)
    catch err
        throw(Error("file $filename not readable"))
    end
    return true
end

function _jl_glpk__check_file_is_writable(filename::String)
    try
        f = open(filename, "w")
        close(f)
    catch err
        throw(Error("file $filename not writable"))
    end
    return true
end

function _jl_glpk__check_mps_format(format::Integer)
    if (format != MPS_DECK &&
        format != MPS_FILE)
        throw(Error("invalid MPS format $format (use GLPK.MPS_DECK or GLPK.MPS_FILE)"))
    end
    return true
end

function _jl_glpk__check_mps_param(param)
    if param != C_NULL
        throw(Error("MPS param must be C_NULL"))
    end
    return true
end

function _jl_glpk__check_lp_param(param)
    if param != C_NULL
        throw(Error("LP param must be C_NULL"))
    end
    return true
end

function _jl_glpk__check_read_prob_flags(flags::Integer)
    if flags != 0
        throw(Error("read_prob flags must be 0"))
    end
    return true
end

function _jl_glpk__check_write_prob_flags(flags::Integer)
    if flags != 0
        throw(Error("write_prob flags must be 0"))
    end
    return true
end

function _jl_glpk__check_print_ranges_flags(flags::Integer)
    if flags != 0
        throw(Error("print_ranges flags must be set to 0 (found $flags instead)"))
    end
    return true
end


function _jl_glpk__check_bfcp(param::BasisFactParam)
    if pointer(param) == C_NULL
        throw(Error("Invalid GLPK.BasisFactParam"))
    end
    return true
end

function _jl_glpk__check_data(data::Data)
    if pointer(data) == C_NULL
        throw(Error("Invalid GLPK.Data"))
    end
    return true
end

function _jl_glpk__check_mpl_workspace(tran::MathProgWorkspace)
    if tran.p == C_NULL
        throw(Error("Invalid GLPK.MathProgWorkspace"))
    end
    return true
end

function _jl_glpk__check_rowcol_is_valid(prob::Prob, k::Integer)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(Error("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end
    return true
end

function _jl_glpk__check_dir_is_valid(dir::Integer)
    if !(dir == 1 || dir == -1)
        throw(Error("invalid direction $dir (must be 1 or -1)"))
    end
    return true
end

function _jl_glpk__check_eps_is_valid(eps::Real)
    if (eps < 0)
        throw(Error("invalid eps $eps (must be >= 0)"))
    end
    return true
end

function _jl_glpk__check_init_env_succeeded(ret::Integer)
    if !(0 <= ret <= 1)
        throw(Error("initialization failed"))
    end
    return true
end

function _jl_glpk__check_term_out_flag(flag::Integer)
    if !(flag == ON || flag == OFF)
        throw(Error("invalid flag $flag (use GLPK.ON or GLPK.OFF)"))
    end
    return true
end

function _jl_glpk__check_open_tee_succeeded(ret::Integer)
    if !(0 <= ret <= 1)
        throw(Error("GLPK.open_tee failed"))
    end
    return true
end

function _jl_glpk__check_alloc_size(n::Integer)
    if n <= 0
        throw(Error("invalid alloc size $n"))
    end
    return true
end

function _jl_glpk__check_pointer_is_valid(ptr::Ptr)
    if ptr == C_NULL
        throw(Error("invalid pointer"))
    end
    return true
end

function _jl_glpk__check_sdf_file_opened(data_p::Ptr)
    if data_p == C_NULL
        throw(Error("GLPK.sdf_open_file failed"))
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
#  * function names tranlsate like this: glp_func -> GLPK.func
#  * constant names tranlsate like this: GLPK_CONST -> GLPK.CONST
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
#  A single exception to the strict compatibility is GLPK.version(),
#  which returns a tuple of integers in the form (major, minor)
#  rather than a string.

function set_prob_name(prob::Prob, name::Union(String,Nothing))
    _jl_glpk__check_prob(prob)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_prob_name Void (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(name)
end

function set_obj_name(prob::Prob, name::Union(String,Nothing))
    _jl_glpk__check_prob(prob)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_obj_name Void (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(name)
end

function set_row_name(prob::Prob, row::Integer, name::Union(String,Nothing))
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_row_name Void (Ptr{Void}, Int32, Ptr{Uint8}) prob.p row bytestring(name)
end

function set_col_name(prob::Prob, col::Integer, name::Union(String,Nothing))
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    if is(name, nothing)
        name = ""
    end
    _jl_glpk__check_string_length(name, 0, 255)
    @glpk_ccall set_col_name Void (Ptr{Void}, Int32, Ptr{Uint8}) prob.p col bytestring(name)
end

function set_obj_dir(prob::Prob, dir::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_obj_dir_is_valid(dir)
    @glpk_ccall set_obj_dir Void (Ptr{Void}, Int32) prob.p dir
end

function add_rows(prob::Prob, rows::Integer)
    _jl_glpk__check_prob(prob)
    @glpk_ccall add_rows Int32 (Ptr{Void}, Int32) prob.p rows
end

function add_cols(prob::Prob, cols::Integer)
    _jl_glpk__check_prob(prob)
    @glpk_ccall add_cols Int32 (Ptr{Void}, Int32) prob.p cols
end

function set_row_bnds(prob::Prob, row::Integer, bounds_type::Integer, lb::Real, ub::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    _jl_glpk__check_bounds_type_is_valid(bounds_type)
    _jl_glpk__check_bounds_are_valid(bounds_type, lb, ub)
    @glpk_ccall set_row_bnds Void (Ptr{Void}, Int32, Int32, Float64, Float64) prob.p row bounds_type lb ub
end

function set_col_bnds(prob::Prob, col::Integer, bounds_type::Integer, lb::Real, ub::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    _jl_glpk__check_bounds_type_is_valid(bounds_type)
    _jl_glpk__check_bounds_are_valid(bounds_type, lb, ub)
    @glpk_ccall set_col_bnds Void (Ptr{Void}, Int32, Int32, Float64, Float64) prob.p col bounds_type lb ub
end

function set_obj_coef(prob::Prob, col::Integer, coef::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid_w0(prob, col)
    @glpk_ccall set_obj_coef Void (Ptr{Void}, Int32, Float64) prob.p col coef
end

function set_mat_row{Ti<:Integer, Tv<:Real}(prob::Prob, row::Integer, len::Integer, ind::Vector{Ti}, val::Vector{Tv})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_row_is_valid(prob, row)
    if len > 0
        ind32 = int32(ind)
        val64 = float64(val)
        off32 = sizeof(Int32)
        off64 = sizeof(Float64)
        ind32p = pointer(ind32) - off32
        val64p = pointer(val64) - off64
        _jl_glpk__check_cols_ids(prob, 0, len, ind32)
    else
        ind32p = C_NULL
        val64p = C_NULL
    end

    @glpk_ccall set_mat_row Void (Ptr{Void}, Int32, Int32, Ptr{Int32}, Ptr{Float64}) prob.p row len ind32p val64p
end
function set_mat_row(prob::Prob, row::Integer, len::Integer, ind::VecOrNothing, val::VecOrNothing)
    ind = _jl_glpk__convert_vecornothing(Int32, ind)
    val = _jl_glpk__convert_vecornothing(Float64, ar)
    set_mat_row(prob, row, len, ind, val)
end
function set_mat_row(prob::Prob, row::Integer, ind::VecOrNothing, val::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    l = _jl_glpk__vecornothing_length(ind)
    set_mat_row(prob, row, l, ind, val)
end


function set_mat_col{Ti<:Integer, Tv<:Real}(prob::Prob, col::Integer, len::Integer, ind::Vector{Ti}, val::Vector{Tv})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_col_is_valid(prob, col)
    if len > 0
        ind32 = int32(ind)
        val64 = float64(val)
        off32 = sizeof(Int32)
        off64 = sizeof(Float64)
        ind32p = pointer(ind32) - off32
        val64p = pointer(val64) - off64
        _jl_glpk__check_rows_ids(prob, 0, len, ind32)
    else
        ind32p = C_NULL
        val64p = C_NULL
    end

    @glpk_ccall set_mat_col Void (Ptr{Void}, Int32, Int32, Ptr{Int32}, Ptr{Float64}) prob.p col len ind32p val64p
end
function set_mat_col(prob::Prob, col::Integer, len::Integer, ind::VecOrNothing, val::VecOrNothing)
    ind = _jl_glpk__convert_vecornothing(Int32, ind)
    val = _jl_glpk__convert_vecornothing(Float64, ar)
    set_mat_col(prob, col, len, ind, val)
end
function set_mat_col(prob::Prob, col::Integer, ind::VecOrNothing, val::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    l = _jl_glpk__vecornothing_length(ind)
    set_mat_col(prob, col, l, ind, val)
end

function load_matrix{Ti<:Integer, Tv<:Real}(prob::Prob, numel::Integer, ia::Vector{Ti}, ja::Vector{Ti}, ar::Vector{Tv})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_vectors_size(numel, ia, ja, ar)
    if numel == 0
        return
    end
    ia32 = int32(ia)
    ja32 = int32(ja)
    ar64 = float64(ar)
    _jl_glpk__check_indices_vectors_dup(prob, numel, ia32, ja32)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ia32p = pointer(ia32) - off32
    ja32p = pointer(ja32) - off32
    ar64p = pointer(ar64) - off64

    @glpk_ccall load_matrix Void (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Int32}, Ptr{Float64}) prob.p numel ia32p ja32p ar64p
end

function load_matrix(prob::Prob, numel::Integer, ia::VecOrNothing, ja::VecOrNothing, ar::VecOrNothing)
    ia = _jl_glpk__convert_vecornothing(Int32, ia)
    ja = _jl_glpk__convert_vecornothing(Int32, ja)
    ar = _jl_glpk__convert_vecornothing(Float64, ar)
    load_matrix(prob, numel, ia, ja, ar)
end

function load_matrix(prob::Prob, ia::VecOrNothing, ja::VecOrNothing, ar::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ia, ja, ar)
    l = _jl_glpk__vecornothing_length(ar)
    load_matrix(prob, l, ia, ja, ar)
end

function load_matrix{Ti<:Integer, Tv<:Real}(prob::Prob, a::SparseMatrixCSC{Tv, Ti})
    (ia, ja, ar) = findn_nzs(a)
    load_matrix(prob, ia, ja, ar)
end

function check_dup{Ti<:Integer}(rows::Integer, cols::Integer, numel::Integer, ia::Vector{Ti}, ja::Vector{Ti})
    _jl_glpk__check_rows_and_cols(rows, cols)
    _jl_glpk__check_vectors_size(numel, ia, ja)
    ia32 = int32(ia)
    ja32 = int32(ja)

    off32 = sizeof(Int32)
    ia32p = pointer(ia32) - off32
    ja32p = pointer(ja32) - off32

    @glpk_ccall check_dup Int32 (Int32, Int32, Int32, Ptr{Int32}, Ptr{Int32}) rows cols numel ia32p ja32p
end

function check_dup(rows::Integer, cols::Integer, numel::Integer, ia::VecOrNothing, ja::VecOrNothing)
    ia = _jl_glpk__convert_vecornothing(Int32, ia)
    ja = _jl_glpk__convert_vecornothing(Int32, ja)
    check_dup(rows, cols, numel, ia, ja)
end

function check_dup(rows::Integer, cols::Integer, ia::VecOrNothing, ja::VecOrNothing)
    _jl_glpk__check_vectors_all_same_size(ia, ja)
    l = _jl_glpk__vecornothing_length(ia)
    check_dup(rows, cols, l, ia, ja)
end

function sort_matrix(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall sort_matrix Void (Ptr{Void},) prob.p
end

function del_rows{Ti<:Integer}(prob::Prob, num_rows::Integer, rows_ids::AbstractVector{Ti})
    _jl_glpk__check_prob(prob)
    rows_ids32 = int32(rows_ids)
    _jl_glpk__check_rows_ids(prob, 1, num_rows, rows_ids32)

    off32 = sizeof(Int32)
    rows_ids32p = pointer(rows_ids32) - off32
    @glpk_ccall del_rows Void (Ptr{Void}, Int32, Ptr{Int32}) prob.p num_rows rows_ids32p
end
del_rows{Ti<:Integer}(prob::Prob, rows_ids::AbstractVector{Ti}) =
    del_rows(prob, length(rows_ids), rows_ids)

function del_cols{Ti<:Integer}(prob::Prob, num_cols::Integer, cols_ids::AbstractVector{Ti})
    _jl_glpk__check_prob(prob)
    cols_ids32 = int32(cols_ids)
    _jl_glpk__check_cols_ids(prob, 1, num_cols, cols_ids32)

    off32 = sizeof(Int32)
    cols_ids32p = pointer(cols_ids32) - off32
    @glpk_ccall del_cols Void (Ptr{Void}, Int32, Ptr{Int32}) prob.p num_cols cols_ids32p
end
del_cols{Ti<:Integer}(prob::Prob, cols_ids::AbstractVector{Ti}) =
    del_cols(prob, length(cols_ids), cols_ids)

function copy_prob(prob_dest::Prob, prob::Prob, copy_names::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_copy_names_flag(copy_names)
    @glpk_ccall copy_prob Void (Ptr{Void}, Ptr{Void}, Int32) prob_dest.p prob.p copy_names
end

function erase_prob(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall erase_prob Void (Ptr{Void},) prob.p
end

function get_prob_name(prob::Prob)
    _jl_glpk__check_prob(prob)
    name_cstr = @glpk_ccall get_prob_name Ptr{Uint8} (Ptr{Void},) prob.p
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function get_obj_name(prob::Prob)
    _jl_glpk__check_prob(prob)
    name_cstr = @glpk_ccall get_obj_name Ptr{Uint8} (Ptr{Void},) prob.p
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function get_obj_dir(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_obj_dir Int32 (Ptr{Void},) prob.p
end

function get_num_rows(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
end

function get_num_cols(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p
end

function get_row_name(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    name_cstr = @glpk_ccall get_row_name Ptr{Uint8} (Ptr{Void}, Int32) prob.p row
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function get_col_name(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    name_cstr = @glpk_ccall get_col_name Ptr{Uint8} (Ptr{Void}, Int32) prob.p col
    if name_cstr == C_NULL
        return ""
    else
        return bytestring(name_cstr)
    end
end

function get_row_type(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_type Int32 (Ptr{Void}, Int32) prob.p row
end

function get_row_lb(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_lb Float64 (Ptr{Void}, Int32) prob.p row
end

function get_row_ub(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_ub Float64 (Ptr{Void}, Int32) prob.p row
end

function get_col_type(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_type Int32 (Ptr{Void}, Int32) prob.p col
end

function get_col_lb(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_lb Float64 (Ptr{Void}, Int32) prob.p col
end

function get_col_ub(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_ub Float64 (Ptr{Void}, Int32) prob.p col
end

function get_obj_coef(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid_w0(prob, col)
    @glpk_ccall get_obj_coef Float64 (Ptr{Void}, Int32) prob.p col
end

function get_num_nz(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_num_nz Int32 (Ptr{Void},) prob.p
end

function get_mat_row(prob::Prob, row::Integer, ind::Union(Vector{Int32},Nothing), val::Union(Vector{Float64},Nothing))
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    numel = @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p row C_NULL C_NULL
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
    @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p row ind32p val64p
end

function get_mat_row(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    numel = @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p row C_NULL C_NULL
    if numel == 0
        return (Int32[], Float64[])
    end
    ind = Array(Int32, numel)
    val = Array(Float64, numel)

    off32 = sizeof(Int32)
    ind32p = pointer(ind) - off32
    off64 = sizeof(Float64)
    val64p = pointer(val) - off64
    @glpk_ccall get_mat_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p row ind32p val64p
    return ind, val
end

function get_mat_col(prob::Prob, col::Integer, ind::Union(Vector{Int32},Nothing), val::Union(Vector{Float64},Nothing))
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    numel = @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p col C_NULL C_NULL
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
    @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p col ind32p val64p
end

function get_mat_col(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    numel = @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p col C_NULL C_NULL
    if numel == 0
        return (Int32[], Float64[])
    end
    ind = Array(Int32, numel)
    val = Array(Float64, numel)

    off32 = sizeof(Int32)
    ind32p = pointer(ind) - off32
    off64 = sizeof(Float64)
    val64p = pointer(val) - off64
    @glpk_ccall get_mat_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p col ind32p val64p
    return ind, val
end

function create_index(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall create_index Void (Ptr{Void},) prob.p
end

function find_row(prob::Prob, name::String)
    _jl_glpk__check_prob(prob)
    @glpk_ccall find_row Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(name)
end

function find_col(prob::Prob, name::String)
    _jl_glpk__check_prob(prob)
    @glpk_ccall find_col Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(name)
end

function delete_index(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall delete_index Void (Ptr{Void},) prob.p
end

function set_rii(prob::Prob, row::Integer, rii::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall set_rii Void (Ptr{Void}, Int32, Float64) prob.p row rii
end

function set_sjj(prob::Prob, col::Integer, sjj::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall set_sjj Void (Ptr{Void}, Int32, Float64) prob.p col sjj
end

function get_rii(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_rii Float64 (Ptr{Void}, Int32) prob.p row
end

function get_sjj(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_sjj Float64 (Ptr{Void}, Int32) prob.p col
end

function scale_prob(prob::Prob, flags::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_scale_flags(flags)
    @glpk_ccall scale_prob Void (Ptr{Void}, Int32) prob.p flags
end

function unscale_prob(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall unscale_prob Void (Ptr{Void},) prob.p
end

function set_row_stat(prob::Prob, row::Integer, stat::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    _jl_glpk__check_stat_is_valid(stat)
    @glpk_ccall set_row_stat Void (Ptr{Void}, Int32, Int32) prob.p row stat
end

function set_col_stat(prob::Prob, col::Integer, stat::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    _jl_glpk__check_stat_is_valid(stat)
    @glpk_ccall set_col_stat Void (Ptr{Void}, Int32, Int32) prob.p col stat
end

function std_basis(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall std_basis Void (Ptr{Void},) prob.p
end

function adv_basis(prob::Prob, flags::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_adv_basis_flags(flags)
    @glpk_ccall adv_basis Void (Ptr{Void}, Int32) prob.p flags
end
adv_basis(prob::Prob) = adv_basis(prob, 0)

function cpx_basis(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall cpx_basis Void (Ptr{Void},) prob.p
end

function simplex{Tp<:Union(SimplexParam, Nothing)}(prob::Prob, param::Tp)
    _jl_glpk__check_prob(prob)
    if param == nothing
        param_ptr = C_NULL
    else
        param_ptr = pointer(param)
    end
    @glpk_ccall simplex Int32 (Ptr{Void}, Ptr{Void}) prob.p param_ptr
end

simplex(prob::Prob) =
    simplex(prob, nothing)

function exact{Tp<:Union(SimplexParam, Nothing)}(prob::Prob, param::Tp)
    _jl_glpk__check_prob(prob)
    if param == nothing
        param_ptr = C_NULL
    else
        param_ptr = pointer(param)
    end
    @glpk_ccall exact Int32 (Ptr{Void}, Ptr{Void}) prob.p param_ptr
end

exact(prob::Prob) =
    exact(prob, nothing)

function init_smcp(param::SimplexParam)
    _jl_glpk__check_simplex_param(param)
    @glpk_ccall init_smcp Int32 (Ptr{Void},) pointer(param)
end

function get_status(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_status Int32 (Ptr{Void},) prob.p
end

function get_prim_stat(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_prim_stat Int32 (Ptr{Void},) prob.p
end

function get_dual_stat(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_dual_stat Int32 (Ptr{Void},) prob.p
end

function get_obj_val(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_obj_val Float64 (Ptr{Void},) prob.p
end

function get_row_stat(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_stat Int32 (Ptr{Void}, Int32) prob.p row
end

function get_row_prim(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_prim Float64 (Ptr{Void}, Int32) prob.p row
end

function get_row_dual(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_dual Float64 (Ptr{Void}, Int32) prob.p row
end

function get_col_stat(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_stat Int32 (Ptr{Void}, Int32) prob.p col
end

function get_col_prim(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_prim Float64 (Ptr{Void}, Int32) prob.p col
end

function get_col_dual(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_dual Float64 (Ptr{Void}, Int32) prob.p col
end

function get_unbnd_ray(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_unbnd_ray Int32 (Ptr{Void},) prob.p
end

function interior{Tp<:Union(InteriorParam, Nothing)}(prob::Prob, param::Tp)
    _jl_glpk__check_prob(prob)
    if param == nothing
        param_ptr::Ptr{Void} = C_NULL
    else
        param_ptr = pointer(param)
    end
    @glpk_ccall interior Int32 (Ptr{Void}, Ptr{Void}) prob.p param_ptr
end

interior(prob::Prob) = interior(prob, nothing)

function init_iptcp(param::InteriorParam)
    _jl_glpk__check_interior_param(param)
    @glpk_ccall init_iptcp Int32 (Ptr{Void},) pointer(param)
end

function ipt_status(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall ipt_status Int32 (Ptr{Void},) prob.p
end

function ipt_obj_val(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall ipt_obj_val Float64 (Ptr{Void},) prob.p
end

function ipt_row_prim(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall ipt_row_prim Float64 (Ptr{Void}, Int32) prob.p row
end

function ipt_row_dual(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall ipt_row_dual Float64 (Ptr{Void}, Int32) prob.p row
end

function ipt_col_prim(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall ipt_col_prim Float64 (Ptr{Void}, Int32) prob.p col
end

function ipt_col_dual(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall ipt_col_dual Float64 (Ptr{Void}, Int32) prob.p col
end

function set_col_kind(prob::Prob, col::Integer, kind::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    _jl_glpk__check_kind_is_valid(kind)
    @glpk_ccall set_col_kind Void (Ptr{Void}, Int32, Int32) prob.p col kind
end

function get_col_kind(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_kind Int32 (Ptr{Void}, Int32) prob.p col
end

function get_num_int(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_num_int Int32 (Ptr{Void},) prob.p
end

function get_num_bin(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall get_num_bin Int32 (Ptr{Void},) prob.p
end

function intopt{Tp<:Union(IntoptParam, Nothing)}(prob::Prob, param::Tp)
    _jl_glpk__check_prob(prob)
    if param == nothing
        param_ptr::Ptr{Void} = C_NULL
    else
        param_ptr = pointer(param)
    end
    @glpk_ccall intopt Int32 (Ptr{Void}, Ptr{Void}) prob.p param_ptr
end

intopt(prob::Prob) = intopt(prob, nothing)

function init_iocp(param::IntoptParam)
    _jl_glpk__check_intopt_param(param)
    @glpk_ccall init_iocp Int32 (Ptr{Void},) pointer(param)
end

function mip_status(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall mip_status Int32 (Ptr{Void},) prob.p
end

function mip_obj_val(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall mip_obj_val Float64 (Ptr{Void},) prob.p
end

function mip_row_val(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall mip_row_val Float64 (Ptr{Void}, Int32) prob.p row
end

function mip_col_val(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall mip_col_val Float64 (Ptr{Void}, Int32) prob.p col
end

#TODO
#function lpx_check_kkt(prob::Prob, scaled::Integer, kkt)

function read_mps(prob::Prob, format::Integer, param, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_mps_format(format)
    if is(param, nothing)
        param = C_NULL
    else
        _jl_glpk__check_mps_param(param)
    end

    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_mps Int32 (Ptr{Void}, Int32, Ptr{Void}, Ptr{Uint8}) prob.p format param bytestring(filename)
    if ret != 0
        throw(Error("Error reading MPS file"))
    end
    return ret
end

read_mps(prob::Prob, format::Integer, filename::String) =
    read_mps(prob, format, C_NULL, filename)

function write_mps(prob::Prob, format::Integer, param, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_mps_format(format)
    if is(param, nothing)
        param = C_NULL
    else
        _jl_glpk__check_mps_param(param)
    end
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_mps Int32 (Ptr{Void}, Int32, Ptr{Void}, Ptr{Uint8}) prob.p format param bytestring(filename)
    if ret != 0
        throw(Error("Error writing MPS file"))
    end
    return ret
end

write_mps(prob::Prob, format::Integer, filename::String) =
    write_mps(prob, format, C_NULL, filename)

function read_lp(prob::Prob, param, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_lp_param(param)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_lp Int32 (Ptr{Void}, Ptr{Void}, Ptr{Uint8}) prob.p param bytestring(filename)
    if ret != 0
        throw(Error("Error reading LP file"))
    end
    return ret
end

read_lp(prob::Prob, filename::String) =
    read_lp(prob, C_NULL, filename)

function write_lp(prob::Prob, param, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_lp_param(param)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_lp Int32 (Ptr{Void}, Ptr{Void}, Ptr{Uint8}) prob.p param bytestring(filename)
    if ret != 0
        throw(Error("Error writing LP file"))
    end
    return ret
end

write_lp(prob::Prob, filename::String) =
    write_lp(prob, C_NULL, filename)

function read_prob(prob::Prob, flags::Integer, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_read_prob_flags(flags)
    _jl_glpk__check_file_is_readable(filename)
    @glpk_ccall read_prob Int32 (Ptr{Void}, Int32, Ptr{Uint8}) prob.p flags bytestring(filename)
end

read_prob(prob::Prob, filename::String) =
    read_prob(prob, 0, filename)

function write_prob(prob::Prob, flags::Integer, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_write_prob_flags(flags)
    _jl_glpk__check_file_is_writable(filename)
    @glpk_ccall write_prob Int32 (Ptr{Void}, Int32, Ptr{Uint8}) prob.p flags bytestring(filename)
end

write_prob(prob::Prob, filename::String) =
    write_prob(prob, 0, filename)

function mpl_read_model(tran::MathProgWorkspace, filename::String, skip::Integer)
    _jl_glpk__check_mpl_workspace(tran)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall mpl_read_model Int32 (Ptr{Void}, Ptr{Uint8}, Int32) tran.p bytestring(filename) skip
    if ret != 0
        throw(Error("Error reading MathProg file"))
    end
    return ret
end

function mpl_read_data(tran::MathProgWorkspace, filename::String)
    _jl_glpk__check_mpl_workspace(tran)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall mpl_read_data Int32 (Ptr{Void}, Ptr{Uint8}) tran.p bytestring(filename)
    if ret != 0
        throw(Error("Error reading MathProg data file"))
    end
    return ret
end

function mpl_generate(tran::MathProgWorkspace, filename::Union(String, Nothing))
    _jl_glpk__check_mpl_workspace(tran)
    if is(filename, nothing)
        cfilename = C_NULL
    else
        _jl_glpk__check_file_is_writable(filename)
        cfilename = bytestring(filename)
    end
    ret = @glpk_ccall mpl_generate Int32 (Ptr{Void}, Ptr{Uint8}) tran.p cfilename
    if ret != 0
        throw(Error("Error generating MathProg model"))
    end
    return ret

end
mpl_generate(tran::MathProgWorkspace) = mpl_generate(tran, nothing)

function mpl_build_prob(tran::MathProgWorkspace, prob::Prob)
    _jl_glpk__check_mpl_workspace(tran)
    _jl_glpk__check_prob(prob)
    @glpk_ccall mpl_build_prob Void (Ptr{Void}, Ptr{Void}) tran.p prob.p
end

function mpl_postsolve(tran::MathProgWorkspace, prob::Prob, sol::Integer)
    _jl_glpk__check_mpl_workspace(tran)
    _jl_glpk__check_prob(prob)
    if !(sol == SOL || sol == IPT || sol == MIP)
        throw(Error("Invalid parameter sol $sol (use GLPK.SOL, GLPK.IPT or GLPK.MIP)"))
    end
    ret = @glpk_ccall mpl_postsolve Int32 (Ptr{Void}, Ptr{Void}, Int32) tran.p prob.p sol
    if ret != 0
        throw(Error("Error postsolving MathProg model"))
    end
    return ret
end

function print_sol(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_sol Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error printing solution"))
    end
    return ret
end

function read_sol(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_sol Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error reading solution"))
    end
    return ret
end

function write_sol(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_sol Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error writing solution"))
    end
    return ret
end

function print_ipt(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_ipt Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error printing interior point solution"))
    end
    return ret
end

function read_ipt(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_ipt Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error reading interior point solution"))
    end
    return ret
end

function write_ipt(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_ipt Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error writing interior point solution"))
    end
    return ret
end

function print_mip(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall print_mip Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error printing mixed integer programming solution"))
    end
    return ret
end

function read_mip(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_mip Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error reading mixed integer programming solution"))
    end
    return ret
end

function write_mip(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_mip Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error writing mixed integer programming solution"))
    end
    return ret
end

function print_ranges{Ti<:Integer}(prob::Prob, len::Integer, list::Vector{Ti}, flags::Integer, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_vectors_size(len, list)
    _jl_glpk__check_status_is_optimal(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_print_ranges_flags(flags)
    _jl_glpk__check_file_is_writable(filename)

    if len > 0
        list32 = int32(list)
        _jl_glpk__check_list_ids(prob, len, list32)

        off32 = sizeof(Int32)
        list32p = pointer(list32) - off32
    else
        list32p = C_NULL
    end

    @glpk_ccall print_ranges Int32 (Ptr{Void}, Int32, Ptr{Int32}, Int32, Ptr{Uint8}) prob.p len list32p flags bytestring(filename)
end

print_ranges{Ti<:Integer}(prob::Prob, list::Vector{Ti}, flags::Integer, filename::String) =
    print_ranges(prob, length(list), list, flags, filename)

print_ranges{Ti<:Integer}(prob::Prob, len::Integer, list::Vector{Ti}, filename::String) = 
    print_ranges(prob, len, list, 0, filename)

print_ranges{Ti<:Integer}(prob::Prob, list::Vector{Ti}, filename::String) =
    print_ranges(prob, length(list), list, 0, filename)

function print_ranges(prob::Prob, len::Integer, list::VecOrNothing, flags::Integer, filename::String)
    list = _jl_glpk__convert_vecornothing(Int32, list)
    print_ranges(prob, len, list, flags, filename)
end

print_ranges(prob::Prob, list::VecOrNothing, flags::Integer, filename::String) =
    print_ranges(prob, _jl_glpk__vecornothing_length(list), list, flags, filename)

print_ranges(prob::Prob, len::Integer, list::VecOrNothing, filename::String) =
    print_ranges(prob, len, list, 0, filename)

print_ranges(prob::Prob, list::VecOrNothing, filename::String) =
    print_ranges(prob, _jl_glpk__vecornothing_length(list), list, 0, filename)

print_ranges(prob::Prob, filename::String) =
    print_ranges(prob, 0, nothing, 0, filename)

function bf_exists(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall bf_exists Int32 (Ptr{Void},) prob.p
end

function factorize(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall factorize Int32 (Ptr{Void},) prob.p
end

function bf_updated(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall bf_updated Int32 (Ptr{Void},) prob.p
end

function get_bfcp(prob::Prob, param::BasisFactParam)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bfcp(param)
    @glpk_ccall get_bfcp Void (Ptr{Void}, Ptr{Void}) prob.p pointer(param)
end

function set_bfcp(prob::Prob, param::Union(BasisFactParam,Nothing))
    _jl_glpk__check_prob(prob)
    if is(param, nothing)
        param_p = C_NULL
    else
        _jl_glpk__check_bfcp(param)
        param_p = pointer(param)
    end
    @glpk_ccall set_bfcp Void (Ptr{Void}, Ptr{Void}) prob.p param_p
end
set_bfcp(prob::Prob) = set_bfcp(prob, nothing)

function get_bhead(prob::Prob, k::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_row_is_valid(prob, k)
    @glpk_ccall get_bhead Int32 (Ptr{Void}, Int32) prob.p k
end

function get_row_bind(prob::Prob, row::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_row_is_valid(prob, row)
    @glpk_ccall get_row_bind Int32 (Ptr{Void}, Int32) prob.p row
end

function get_col_bind(prob::Prob, col::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_col_is_valid(prob, col)
    @glpk_ccall get_col_bind Int32 (Ptr{Void}, Int32) prob.p col
end

function ftran(prob::Prob, x::Vector{Float64})
    _jl_glpk__check_prob(prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    _jl_glpk__check_vectors_size(rows, x)
    off64 = sizeof(Float64)
    x64p = pointer(x) - off64
    @glpk_ccall ftran Void (Ptr{Void}, Ptr{Float64}) prob.p x64p
end

function btran(prob::Prob, x::Vector{Float64})
    _jl_glpk__check_prob(prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    _jl_glpk__check_vectors_size(rows, x)
    off64 = sizeof(Float64)
    x64p = pointer(x) - off64
    @glpk_ccall btran Void (Ptr{Void}, Ptr{Float64}) prob.p x64p
end

function warm_up(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall warm_up Int32 (Ptr{Void},) prob.p
end

function eval_tab_row(prob::Prob, k::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(Error("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_basic(prob, k)

    grow(ind, k_max - length(ind))
    grow(val, k_max - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return len
end

function eval_tab_row(prob::Prob, k::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(Error("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_basic(prob, k)

    ind = Array(Int32, k_max)
    val = Array(Float64, k_max)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return ind, val
end

function eval_tab_col(prob::Prob, k::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(Error("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_non_basic(prob, k)

    grow(ind, k_max - length(ind))
    grow(val, k_max - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return len
end

function eval_tab_col(prob::Prob, k::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p
    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    k_max = rows + cols

    if !(1 <= k <= k_max)
        throw(Error("index out of bounds: $k (bounds are 1 <= k <= $k_max"))
    end

    _jl_glpk__check_var_is_non_basic(prob, k)

    ind = Array(Int32, k_max)
    val = Array(Float64, k_max)

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len = @glpk_ccall eval_tab_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p k ind32p val64p

    del(ind, len+1:length(ind))
    del(val, len+1:length(val))

    return ind, val
end

function transform_row(prob::Prob, len::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_col_is_valid(prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)

    cols = @glpk_ccall get_num_cols Int32 (Ptr{Void},) prob.p

    grow(ind, cols - length(ind))
    grow(val, cols - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len1 = @glpk_ccall transform_row Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p len ind32p val64p

    del(ind, len1+1:length(ind))
    del(val, len1+1:length(val))

    return len1
end

function transform_row(prob::Prob, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_vectors_all_same_size(ind, val)
    transform_row(prob, length(ind), ind, val)
end

function transform_col(prob::Prob, len::Integer, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_row_is_valid(prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)

    rows = @glpk_ccall get_num_rows Int32 (Ptr{Void},) prob.p

    grow(ind, rows - length(ind))
    grow(val, rows - length(val))

    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind) - off32
    val64p = pointer(val) - off64

    len1 = @glpk_ccall transform_col Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}) prob.p len ind32p val64p

    del(ind, len1+1:length(ind))
    del(val, len1+1:length(val))

    return len1
end

function transform_col(prob::Prob, ind::Vector{Int32}, val::Vector{Float64})
    _jl_glpk__check_vectors_all_same_size(ind, val)
    transform_col(prob, length(ind), ind, val)
end

function prim_rtest{Ti<:Integer, Tv<:Real}(prob::Prob, len::Integer, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_is_prim_feasible(prob)
    _jl_glpk__check_row_is_valid(prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_dir_is_valid(dir)
    _jl_glpk__check_eps_is_valid(eps)

    for i = 1:len
        _jl_glpk__check_var_is_basic(prob, ind[i])
    end

    ind32 = int32(ind)
    val64 = float64(val)
    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind32) - off32
    val64p = pointer(val64) - off64

    piv = @glpk_ccall prim_rtest Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}, Int32, Float64) prob.p len ind32p val64p dir eps
    return piv
end

function prim_rtest{Ti<:Integer, Tv<:Real}(prob::Prob, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    prim_rtest(prob, length(ind), ind, val, dir, eps)
end

function dual_rtest{Ti<:Integer, Tv<:Real}(prob::Prob, len::Integer, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_is_dual_feasible(prob)
    _jl_glpk__check_col_is_valid(prob, len)
    _jl_glpk__check_vectors_size(len, ind, val)
    _jl_glpk__check_dir_is_valid(dir)
    _jl_glpk__check_eps_is_valid(eps)

    for i = 1:len
        _jl_glpk__check_var_is_non_basic(prob, ind[i])
    end

    ind32 = int32(ind)
    val64 = float64(val)
    off32 = sizeof(Int32)
    off64 = sizeof(Float64)
    ind32p = pointer(ind32) - off32
    val64p = pointer(val64) - off64

    piv = @glpk_ccall dual_rtest Int32 (Ptr{Void}, Int32, Ptr{Int32}, Ptr{Float64}, Int32, Float64) prob.p len ind32p val64p dir eps
    return piv
end

function dual_rtest{Ti<:Integer, Tv<:Real}(prob::Prob, ind::Vector{Ti}, val::Vector{Tv}, dir::Integer, eps::Real)
    _jl_glpk__check_vectors_all_same_size(ind, val)
    dual_rtest(prob, length(ind), ind, val, dir, eps)
end

function analyze_bound(prob::Prob, k, limit1, var1, limit2, var2)
    error("Unsupported. Use GLPK.analyze_bound(prob, k) instead.")
end

function analyze_bound(prob::Prob, k::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_rowcol_is_valid(prob, k)
    _jl_glpk__check_var_is_non_basic(prob, k)

    limit1 = Array(Float64, 1)
    var1 = Array(Int32, 1)
    limit2 = Array(Float64, 1)
    var2 = Array(Int32, 1)

    @glpk_ccall analyze_bound Void (Ptr{Void}, Int32, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}) prob.p k pointer(limit1) pointer(var1) pointer(limit2) pointer(var2)

    return limit1[1], var1[1], limit2[1], var2[1]
end

function analyze_coef(prob::Prob, k, coef1, var1, value1, coef2, var2, value2)
    error("Unsupported. Use GLPK.analyze_coef(prob, k) instead.")
end

function analyze_coef(prob::Prob, k::Integer)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_bf_exists(prob)
    _jl_glpk__check_rowcol_is_valid(prob, k)
    _jl_glpk__check_var_is_basic(prob, k)

    coef1 = Array(Float64, 1)
    var1 = Array(Int32, 1)
    value1 = Array(Float64, 1)
    coef2 = Array(Float64, 1)
    var2 = Array(Int32, 1)
    value2 = Array(Float64, 1)

    @glpk_ccall analyze_coef Void (Ptr{Void}, Int32, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}) prob.p k pointer(coef1) pointer(var1) pointer(value1) pointer(coef2) pointer(var2) pointer(value2)

    return coef1[1], var1[1], value1[1], coef2[1], var2[1], value2[1] 
end

function init_env()
    ret = @glpk_ccall init_env Int32 ()
    _jl_glpk__check_init_env_succeeded(ret)
    return ret
end

function free_env()
    ret = @glpk_ccall free_env Int32 ()
end

function term_out(flag::Integer)
    _jl_glpk__check_term_out_flag(flag)
    @glpk_ccall term_out Int32 (Int32,) flag
end

function open_tee(filename::String)
    ret = @glpk_ccall open_tee Int32 (Ptr{Uint8},) bytestring(filename)
    _jl_glpk__check_open_tee_succeeded(ret)
    return ret
end

function close_tee()
    @glpk_ccall close_tee Int32 ()
end

function malloc(size::Integer)
    _jl_glpk__check_alloc_size(size)
    @glpk_ccall malloc Ptr{Void} (Int32,) size
end

function calloc(n::Integer, size::Integer)
    _jl_glpk__check_alloc_size(n)
    _jl_glpk__check_alloc_size(size)
    @glpk_ccall calloc Ptr{Void} (Int32, Int32) n size
end

function free(ptr::Ptr)
    _jl_glpk__check_pointer_is_valid(ptr)
    @glpk_ccall free Void (Ptr{Void},) ptr
end

function mem_usage(count, cpeak, total, tpeak)
    error("Unsupported. Use GLPK.mem_usage() instead.")
end

function mem_usage()
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

function mem_limit(limit::Integer)
    @glpk_ccall mem_limit Void (Int32,) limit
end

function time()
    @glpk_ccall time Int64 ()
end

function difftime(t1::Integer, t0::Integer)
    @glpk_ccall difftime Float64 (Int64, Int64) t1 t0
end

function sdf_open_file(filename::String)
    _jl_glpk__check_file_is_readable(filename)
    data_p = @glpk_ccall sdf_open_file Ptr{Void} (Ptr{Uint8},) bytestring(filename)
    _jl_glpk__check_sdf_file_opened(data_p)
    return Data(data_p)
end

function sdf_read_int(data::Data)
    _jl_glpk__check_data(data)
    @glpk_ccall sdf_read_int Int32 (Ptr{Void},) pointer(data)
end

function sdf_read_num(data::Data)
    _jl_glpk__check_data(data)
    @glpk_ccall sdf_read_num Float64 (Ptr{Void},) pointer(data)
end

function sdf_read_item(data::Data)
    _jl_glpk__check_data(data)
    item_cstr = @glpk_ccall sdf_read_item Ptr{Uint8} (Ptr{Void},) pointer(data)
    return bytestring(item_cstr)
end

function sdf_read_text(data::Data)
    _jl_glpk__check_data(data)
    text_cstr = @glpk_ccall sdf_read_text Ptr{Uint8} (Ptr{Void},) pointer(data)
    return bytestring(text_cstr)
end

function sdf_line(data::Data)
    _jl_glpk__check_data(data)
    @glpk_ccall sdf_line Int32 (Ptr{Void},) pointer(data)
end

function sdf_close_file(data::Data)
    _jl_glpk__check_data(data)
    @glpk_ccall sdf_close_file Void (Ptr{Void},) pointer(data)
end

function read_cnfsat(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_readable(filename)
    ret = @glpk_ccall read_cnfsat Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error reading CNF file"))
    end
    return ret
end

function check_cnfsat(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall check_cnfsat Int32 (Ptr{Void},) prob.p
end

function write_cnfsat(prob::Prob, filename::String)
    _jl_glpk__check_prob(prob)
    _jl_glpk__check_file_is_writable(filename)
    ret = @glpk_ccall write_cnfsat Int32 (Ptr{Void}, Ptr{Uint8}) prob.p bytestring(filename)
    if ret != 0
        throw(Error("Error writing CNF file"))
    end
    return ret
end

function minisat1(prob::Prob)
    _jl_glpk__check_prob(prob)
    @glpk_ccall minisat1 Int32 (Ptr{Void},) prob.p
end

function intfeas1(prob::Prob, use_bound::Integer, obj_bound::Integer)
    _jl_glpk__check_prob(prob)
    # TODO : more checks:
    #   1) columns must be GLPK.BV od GLPK.FX
    #   2) constraints and objj coeffs must be integer
    @glpk_ccall intfeas1 Int32 (Ptr{Void}, Int32, Int32) prob.p use_bound obj_bound
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

end # module
