function V = bench_eu(numPaths)
%Simple European
steps = 250;
r = (0.05);
sigma = (0.4);
T = (1);
dt = T/(steps);
K = (100);

S = 100 * ones(numPaths,1);

for i=1:steps
 rnd = randn(numPaths,1);
 S = S .* exp((r-0.5*sigma.^2)*dt + sigma*sqrt(dt)*rnd);
end
V = mean( exp(-r*T)*max(K-S,0) )
