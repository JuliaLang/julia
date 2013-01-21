### Linear programming

cd("../extras") # we can't use do-notation because of "using"
require("linprog")

using LinProgGLPK

## Simplex method

# Set options (disable all output
# excpept for errors, turn on presolver)
lps_opts = GLPK.SimplexParam()
lps_opts["msg_lev"] = GLPK.MSG_ERR
lps_opts["presolve"] = GLPK.ON
lps_opts["it_lim"] = 1000

# A small dense optimization problem

f = [ 3.; 2. ];
A = [ 2. 1. ;
      1. 1. ];
b = [ 100.; 80 ];
lb = [ 0.; 0.];

(z, x, flag) = linprog_simplex(-f, A, b, [], [], lb, [], lps_opts);

@test flag == 0
@test z == -180.0
@test isequal(x, [20.; 60.])


# A constraint satisfaction (matching) problem
# passing a sparse representation
# to linprog_simplex

f = [ 3. 2. 2. ;
      1. 0. 1. ;
      3. 3. 5. ];
f = reshape(f, (9,));
Aeq = [ 1. 1. 1. 0. 0. 0. 0. 0. 0. ;
        0. 0. 0. 1. 1. 1. 0. 0. 0. ;
        0. 0. 0. 0. 0. 0. 1. 1. 1. ;
        1. 0. 0. 1. 0. 0. 1. 0. 0. ;
        0. 1. 0. 0. 1. 0. 0. 1. 0. ;
        0. 0. 1. 0. 0. 1. 0. 0. 1. ];
Aeq = sparse(Aeq);
beq = ones(Float64, 6);
lb = zeros(Float64, 9);
ub = ones(Float64, 9);

(z, x, flag) = linprog_simplex(f, [], [], Aeq, beq, lb, ub, lps_opts);

@test flag == 0
@test z == 5.
@test isequal(x, [ 0.; 0.; 1. ;
                     0.; 1.; 0. ;
                     1.; 0.; 0. ])


## Interior point method

# Same problem and options as above

lpi_opts = GLPK.InteriorParam()
lpi_opts["msg_lev"] = GLPK.MSG_ERR

(z, x, ret) = linprog(f, [], [], Aeq, beq, lb, ub, lpi_opts);

tol = 1e-4

@test flag == 0
@test abs(z - 5.) < tol
@test max(abs(x - [ 0.; 0.; 1. ;
                      0.; 1.; 0. ;
                      1.; 0.; 0. ])) < tol


### Mixed interger progamming

# Same problem and options as above

mip_opts = GLPK.IntoptParam()
mip_opts["msg_lev"] = GLPK.MSG_ERR
mip_opts["presolve"] = GLPK.ON

# Use binary variables
colkind = int32([ GLPK.BV for i = 1 : 9 ])

(z, x, ret, ret_ps) = mixintprog(f, [], [], Aeq, beq, [], [], colkind, mip_opts);

@test flag == 0
@test z == 5.
@test isequal(x, [ 0.; 0.; 1. ;
                     0.; 1.; 0. ;
                     1.; 0.; 0. ])

cd("../test")
