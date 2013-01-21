cd("../extras") do
require("glpk")

# Same example as in the GLPK manual

lp = GLPK.Prob()
GLPK.set_prob_name(lp, "sample")
GLPK.set_obj_dir(lp, GLPK.MAX)
GLPK.add_rows(lp, 3)
GLPK.set_row_name(lp, 1, "p")
GLPK.set_row_bnds(lp, 1, GLPK.UP, 0.0, 100.0)
GLPK.set_row_name(lp, 2, "q")
GLPK.set_row_bnds(lp, 2, GLPK.UP, 0.0, 600.0)
GLPK.set_row_name(lp, 3, "r")
GLPK.set_row_bnds(lp, 3, GLPK.UP, 0.0, 300.0)
GLPK.add_cols(lp, 3)
GLPK.set_col_name(lp, 1, "x1")
GLPK.set_col_bnds(lp, 1, GLPK.LO, 0.0, 0.0)
GLPK.set_obj_coef(lp, 1, 10.0)
GLPK.set_col_name(lp, 2, "x2")
GLPK.set_col_bnds(lp, 2, GLPK.LO, 0.0, 0.0)
GLPK.set_obj_coef(lp, 2, 6.0)
GLPK.set_col_name(lp, 3, "x3")
GLPK.set_col_bnds(lp, 3, GLPK.LO, 0.0, 0.0)
GLPK.set_obj_coef(lp, 3, 4.0)
ia = zeros(Int, 9)
ja = zeros(Int, 9)
ar = zeros(FloatingPoint, 9)
ia[1] = 1; ja[1] = 1; ar[1] = 1.0
ia[2] = 1; ja[2] = 2; ar[2] = 1.0
ia[3] = 1; ja[3] = 3; ar[3] = 1.0
ia[4] = 2; ja[4] = 1; ar[4] = 10.0
ia[5] = 3; ja[5] = 1; ar[5] = 2.0
ia[6] = 2; ja[6] = 2; ar[6] = 4.0
ia[7] = 3; ja[7] = 2; ar[7] = 2.0
ia[8] = 2; ja[8] = 3; ar[8] = 5.0
ia[9] = 3; ja[9] = 3; ar[9] = 6.0
GLPK.load_matrix(lp, 9, ia, ja, ar)

glp_param = GLPK.SimplexParam()
glp_param["msg_lev"] = GLPK.MSG_ERR

flag = GLPK.simplex(lp, glp_param)
z = GLPK.get_obj_val(lp)
x1 = GLPK.get_col_prim(lp, 1)
x2 = GLPK.get_col_prim(lp, 2)
x3 = GLPK.get_col_prim(lp, 3)

tol = 1e-10
@test abs(z - 2200. / 3) < tol
@test abs(x1 - 100. / 3) < tol
@test abs(x2 - 200. / 3) < tol
@test abs(x3) < tol

end # cd
