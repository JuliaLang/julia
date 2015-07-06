function fem()

  % run tests once to compile
  run_fem(10);

  % runs the tests
  NN = 2.^[3:8];
  TT = zeros(3, length(NN));
  fprintf(1, '(All times are seconds)\n')
  fprintf(1, '     N   |   assembly |   slice   |   lufact  | slice / N^4 \n')
  fprintf(1, '---------|------------|-----------|-----------|-------------\n')
  for n = 1:length(NN)
    [t1, t2, t3] = run_fem(NN(n));
    fprintf(1, ' %4.1e |  %4.2e  | %4.2e  |  %4.2e |   %4.2e \n', ...
           NN(n), t1, t2, t3, t2 / NN(n)^4);
  end

end

% assemble the finite-difference laplacian
function F = fdlaplacian(N)
    % create a 1D laplacian and a sparse identity
    fdl1 = spdiags([ones(N,1) -2*ones(N,1) ones(N,1)], [-1,0,1], N, N);
    % laplace operator on the full grid
    F = kron(speye(N), fdl1) + kron(fdl1, speye(N));
end

% get the list of boundary dof-indices
function f = get_free(N)
  L = zeros(N, N);
  L(2:N-1, 2:N-1) = 1;
  f = find(L);
end

% timing of assembly, slice and solve
function [t1,t2,t3] = run_fem(N)
    Ifree = get_free(N);
    % timing for assembly
    tic; A = fdlaplacian(N); t1 = toc;
    % timing for the boundary condition
    tic; B = A(Ifree, Ifree); t2 = toc;
    % timing for the solver
    tic; [l,u,p,q,r] = lu(B); t3 = toc;
end
