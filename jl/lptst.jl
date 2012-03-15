load("linprog_glpk.jl");

f = [ 10.; 6.; 4 ];
A = [ 1.  1. 1. ;
      10. 4. 5. ;
      2.  2. 6  ];
b = [ 100.; 600.; 300. ];
lb = [ 0.; 0.; 0. ];
(z, x) = linprog(-f, A, b, [], [], lb, []);
println("z=$z")
println("x=$x")

f = [ 3.; 2. ];
A = [ 2. 1. ;
      1. 1. ]; 
b = [ 100.; 80 ];
lb = [ 0.; 0;];
ub = [ 40.; 1e15];

(z, x) = linprog(-f, A, b, [], [], lb, []);
println("z=$z")
println("x=$x")


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
println(Aeq);
Aeq = sparse(Aeq);
println(Aeq)
#Aeq = sparse(Aeq);
#println(Aeq);
#(I, J, V) = find(Aeq);
#println("I=$I");
#println("J=$J");
#println("V=$V");

beq = [ 1.; 1.; 1.; 1.; 1.; 1. ];
lb = zeros(Float64, 9);
ub = ones(Float64, 9);
#(z, x) = linprog(f, [], [], Aeq, beq, lb, ub);

lpoptions = GLPSimplexParam()
lpoptions["msg_lev"] = GLP_MSG_ERR
lpoptions["presolve"] = GLP_ON
#lpoptions["it_lim"] = 2

(z, x) = linprog(f, [], [], Aeq, beq, lb, ub, lpoptions);

#cstruct_delete(lpoptions)

#(z, x) = mixintprog_bin(f, [], [], Aeq, beq);

println("z=$z");
println("x=$x");
