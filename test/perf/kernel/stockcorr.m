%ORIGINAL_CORR - The original, unoptimised code that simulates two correlated assets

%% Correlated asset information
CurrentPrice = [78 102];       %Initial Prices of the two stocks
Corr = [1 0.4; 0.4 1];         %Correlation Matrix
T = 500;                       %Number of days to simulate = 2years = 500days
n = 10000;                     %Number of simulations
dt = 1/250;                    %Time step (1year = 250days)
Div=[0.01 0.01];               %Dividend
Vol=[0.2 0.3];                 %Volatility

%%Market Information
r = 0.03;                      %Risk-free rate

%% Define storages
SimulPriceA=zeros(T,n);    %Simulated Price of Asset A
SimulPriceA(1,:)=CurrentPrice(1);
SimulPriceB=zeros(T,n);    %Simulated Price of Asset B
SimulPriceB(1,:)=CurrentPrice(2);

%% Generating the paths of stock prices by Geometric Brownian Motion
UpperTriangle=chol(Corr);    %UpperTriangle Matrix by Cholesky decomposition

for i=1:n
   Wiener=randn(T-1,2);
   CorrWiener=Wiener*UpperTriangle;
   for j=2:T
      SimulPriceA(j,i)=SimulPriceA(j-1,i)*exp((r-Div(1)-Vol(1)^2/2)*dt+Vol(1)*sqrt(dt)*CorrWiener(j-1,1));
      SimulPriceB(j,i)=SimulPriceB(j-1,i)*exp((r-Div(2)-Vol(2)^2/2)*dt+Vol(2)*sqrt(dt)*CorrWiener(j-1,2));
   end
end

%% Plot the distribution of final prices
% Comment this section out if doing timings
% subplot(1,2,1);hist(SimulPriceA(end,:),100);
% subplot(1,2,2);hist(SimulPriceB(end,:),100);
