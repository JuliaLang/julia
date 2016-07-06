%% Simulate two correlated assets
%% Original code from Mike Croucher (http://www.walkingrandomly.com/?p=3604)

%% Correlated asset information
CurrentPrice = [78 102];       %Initial Prices of the two stocks
Corr = [1 0.4; 0.4 1];         %Correlation Matrix
T = 500;                       %Number of days to simulate = 2years = 500days 
Div=[0.01 0.01];               %Dividend
Vol=[0.2 0.3];                 %Volatility

%% Market Information
r = 0.03;                      %Risk-free rate

%% Simulation parameters 
n=1000000;                      %Number of simulation
dt=1/250;                       %Time step (1year = 250days)

%% Define storages
SimulPrices=repmat(CurrentPrice,n,1);
CorrWiener = zeros(T-1,2,n);

%% Generating the paths of stock prices by Geometric Brownian Motion
UpperTriangle=chol(Corr);    %UpperTriangle Matrix by Cholesky decomposition

tic;
for i=1:n
     CorrWiener(:,:,i)=randn(T-1,2)*UpperTriangle;
end
Volr = repmat(Vol,[T-1,1,n]);
Divr = repmat(Div,[T-1,1,n]);

%% do simulation
sim = cumprod(exp((r-Divr-Volr.^2./2).*dt+Volr.*sqrt(dt).*CorrWiener));
%get just the final prices
SimulPrices = SimulPrices.*reshape(sim(end,:,:),2,n)';
toc;

%% Plot the distribution of final prices
% Comment this section out if doing timings
%subplot(1,2,1);hist(SimulPrices(:,1),100);
%subplot(1,2,2);hist(SimulPrices(:,2),100);

