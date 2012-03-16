# Disable output except on error

lps_opts = GLPSimplexParam()
lps_opts["msg_lev"] = GLP_MSG_ERR
lps_opts["presolve"] = GLP_ON
lps_opts["it_lim"] = 1000


#   f = [ 10.; 6.; 4 ];
#   A = [ 1.  1. 1. ;
#         10. 4. 5. ;
#         2.  2. 6  ];
#   b = [ 100.; 600.; 300. ];
#   lb = [ 0.; 0.; 0. ];
#
#   (z, x, ret) = linprog_simplex(-f, A, b, [], [], lb, [], lps_opts);
#   println("z=$z")
#   println("x=$x")

# A small dense constraint satisfaction problem

f = [ 3.; 2. ];
A = [ 2. 1. ;
      1. 1. ]; 
b = [ 100.; 80 ];
lb = [ 0.; 0;];

(z, x, ret) = linprog_simplex(-f, A, b, [], [], lb, [], lps_opts);
#println("z=$z")
#println("x=$x")
@assert z == -180.0
@assert x == [20.; 60.]



# Test a matching problem
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
(z, x, ret) = linprog_simplex(f, [], [], Aeq, beq, lb, ub, lps_opts);
#println("z=$z");
#println("x=$x");
@assert z == 5.
@assert x == [ 0.; 0.; 1. ;
               0.; 1.; 0. ;
               1.; 0.; 0. ]


lpi_opts = GLPInteriorParam()
lpi_opts["msg_lev"] = GLP_MSG_ALL
lpi_opts["ord_alg"] = GLP_ORD_QMD

(z, x, ret) = linprog(f, [], [], Aeq, beq, lb, ub, lpi_opts);
println("z=$z");
println("x=$x");

mip_opts = GLPIntoptParam()
mip_opts["msg_lev"] = GLP_MSG_ERR
mip_opts["presolve"] = GLP_ON
colkind = int32([ GLP_BV | i = 1 : 9 ])
#(z, x, ret, ret_ps) = mixintprog(f, [], [], Aeq, beq, lb, ub)
(z, x, ret, ret_ps) = mixintprog(f, [], [], Aeq, beq, [], [], colkind, mip_opts);
