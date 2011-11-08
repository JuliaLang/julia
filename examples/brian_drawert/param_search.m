function out=param_search()
close all;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Smax=10000;
V=1;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nfrogs_range = [5 10 15 20 25 30 40 50];
gamma_range = [ 0.0005 0.001 0.0025 0.005 0.01];
eta_range = [5 7.5 10 12.5 15 17.5];
nu_range = [0.15 0.3 0.5 0.7];
f_range = [ 0.05 0.1 0.25 0.5];
sigma_range = [0.1 0.2 0.275 0.4 0.5];
mu_range = [0.01 0.1 1 10 100];
%%%
nu1_range = [1.3863e-04 6.9315e-05 2.8768e-05];  % = -log( [ .25 .5 .75 ] )/Smax
sigma1_range = [5e-06 5e-05 5e-04];  % sigma0/(4*Smax) = 6.8750e-06, iff sigma0=0.275
phi_range = [ 0.1 0.5 1];
Ar_range = [ 1e-5 1e-4 1e-3 1e-2];
Ac_range = [ 0.001 0.01 0.1];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
num_runs = length(Nfrogs_range)*length(gamma_range)*length(eta_range)*length(nu_range)*length(f_range)*length(sigma_range)*length(mu_range);
num_runs2=length(Nfrogs_range)*length(gamma_range)*length(sigma_range)*length(nu1_range)*length(sigma1_range)*length(phi_range)*length(Ar_range)*length(Ac_range)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nfrogs = {};
gamma = {};
eta = {};
nu= {};
f = {};
sigma = {};
mu = {};
%%%
nu1=0;
sigma1=0;
phi=0;
Ar=0;
Ac=0;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for mu_ndx=1:length(mu_range)
    for sigma_ndx=1:length(sigma_range)
        for f_ndx=1:length(f_range)
            for nu_ndx=1:length(nu_range)
                for eta_ndx=1:length(eta_range)
                    for gamma_ndx=1:length(gamma_range)
                        for Nfrogs_ndx=1:length(Nfrogs_range)
                            run_param_set();
                            %if this parameter set works, check non-linear
                            if (eta_ndx==6 && nu_ndx==1 && f_ndx==2 && mu_ndx==3)
                            for nu1_ndx=0:length(nu1_range)
                                for sigma1_ndx=0:length(sigma1_range)
                                    for phi_ndx = 0:length(phi_range)
                                        if(phi_ndx>0)
                                        for Ar_ndx = 1:length(Ar_range)
                                            for Ac_ndx=1:length(Ac_range)
                            run_param_set_w_antibodies();
                                            end
                                        end
                                        end
                                    end
                                end
                            end
                            end
                        end
                    end
                end
            end
        end
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    function out = run_param_set_w_antibodies()
        nu1=0;
        if(nu1_ndx>0),nu1=nu1_range(nu1_ndx);end
        sigma1=0;
        if(sigma1_ndx>0),sigma1=sigma1_range(sigma1_ndx);end
        phi=0;
        Ar=0;
        Ac=0;
        Ar_ndx2=0;
        Ac_ndx2=0;
        if(phi_ndx>0)
            phi=phi_range(phi_ndx);
            Ar=Ar_range(Ar_ndx);
            Ac=Ac_range(Ac_ndx);
            Ar_ndx2=Ar_ndx;
            Ac_ndx2=Ac_ndx;
        end
        %%%
        filename=sprintf('data/param_search_data_%i_%i_%i_%i_%i_%i_%i--%i_%i_%i_%i_%i.mat',...
            mu_ndx,sigma_ndx,f_ndx,nu_ndx,eta_ndx,gamma_ndx,Nfrogs_ndx,...
            nu1_ndx,sigma1_ndx,phi_ndx,Ar_ndx2,Ac_ndx2);
        fid=fopen(filename);
        if(fid~=-1)
            fprintf('skipping: %s\n',filename);
            fclose(fid);
            return;
        end
        fprintf('%s\n',filename);
                            %out = run_param_set();
                            %return;
        %%%%%%
        Nfrogs = Nfrogs_range(Nfrogs_ndx);
        gamma = gamma_range(gamma_ndx);
        eta = eta_range(eta_ndx);
        nu=nu_range(nu_ndx);
        f = f_range(f_ndx);
        sigma = sigma_range(sigma_ndx);
        mu = mu_range(mu_ndx);
        %%%%%%
        out.param_set={Nfrogs,gamma,eta,nu,f,sigma,mu,nu1,sigma1,phi,Ar,Ac};
        %%%%%%
        end_time=1000;
        %%%%%%
        % Find: days to reach_Smax
        if(phi>0)
            y0 = zeros(2*Nfrogs+1,1);
        else
            y0 = zeros(Nfrogs+1,1);
        end
        y0(1)=100;  %first frog is infected
        options = odeset('Events',@events,'Refine',4);
        [t,y,te,ye,ie] = ode23(@dydt,[0 end_time],y0,options);
        out.days_to_reach_Smax = t(end);
        %%%%%%
        % Find: frac_surving_frogs
        [t2,y2] = ode23(@dydt,[0 end_time],y0);
        out.frac_surviving_frogs = sum( y2(end,1:Nfrogs) < Smax )/Nfrogs;
        out.y=y;
        out.t=t;
        out.y2=y2;
        out.t2=t2;
        %%%%%%
        save(filename,'-STRUCT','out')
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    function out = run_param_set()
        nu1=0;
        sigma1=0;
        phi=0;
        Ar=0;
        Ac=0;
        filename=sprintf('data/param_search_data_%i_%i_%i_%i_%i_%i_%i.mat',mu_ndx,sigma_ndx,f_ndx,nu_ndx,eta_ndx,gamma_ndx,Nfrogs_ndx);
        fid=fopen(filename);
        if(fid~=-1)
            fprintf('skipping: %s\n',filename);
            fclose(fid);
            return;
        end
        fprintf('%s\n',filename);
                            %out = run_param_set();
                            %return;
        %%%%%%
        Nfrogs = Nfrogs_range(Nfrogs_ndx);
        gamma = gamma_range(gamma_ndx);
        eta = eta_range(eta_ndx);
        nu=nu_range(nu_ndx);
        f = f_range(f_ndx);
        sigma = sigma_range(sigma_ndx);
        mu = mu_range(mu_ndx);
        %%%%%%
        out.param_set={Nfrogs,gamma,eta,nu,f,sigma,mu};
        %%%%%%
        end_time=1000;
        %%%%%%
        % Find: days to reach_Smax
        if(phi>0)
            y0 = zeros(2*Nfrogs+1,1);
        else
            y0 = zeros(Nfrogs+1,1);
        end
        y0(1)=100;  %first frog is infected
        options = odeset('Events',@events,'Refine',4);
        [t,y,te,ye,ie] = ode23(@dydt,[0 end_time],y0,options);
        out.days_to_reach_Smax = t(end);
        %%%%%%
        % Find: frac_surving_frogs
        [t2,y2] = ode23(@dydt,[0 end_time],y0);
        out.frac_surviving_frogs = sum( y2(end,1:Nfrogs) < Smax )/Nfrogs;
        out.y=y;
        out.t=t;
        out.y2=y2;
        out.t2=t2;
        %%%%%%
        save(filename,'-STRUCT','out')
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    function d = dydt(t,y)
        z = y;
        for i=1:Nfrogs
            if(y(i)>Smax)
                z(i)=0;
                d(i) = 0;
            else
                nu0 = nu*exp(-nu1*y(i));
                if(phi>0)
                    nu0=nu*exp(-phi*y(i+Nfrogs+1));
                end
                sigma0=sigma+sigma1*y(i);
                beta = gamma*nu0;
                d(i) = beta/V*y(Nfrogs+1) + (eta*nu0*f-sigma0)*y(i);
            end
        end
        d(Nfrogs+1) = sum(z(1:Nfrogs)*eta*(1-f)-(gamma/V)*y(Nfrogs+1)) - mu*y(Nfrogs+1);
        if(phi>0)
            for i=1:Nfrogs
                d(i+Nfrogs+1) = Ar*y(i) - Ac*y(i+Nfrogs+1);
            end
        end
        d=d';
     end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    function [value,isterminal,direction]= events(t,y)
        value(1:Nfrogs) = Smax-y(1:Nfrogs);
        value(Nfrogs+1:end)=1;
        isterminal=ones(size(value));
        direction=zeros(size(value));
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end%function param_search()