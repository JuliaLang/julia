function []=gk(n,myeps)
%clear
format longEng

%A = skewdec(n,n);
%r=max(A);
%s=max(r);
%A=A/s
A=myunifskew(n);
A;

[f,g]=size(myeps);
for KK=1:g
  eps = myeps(KK);
  %xlswrite('output.xls', A,'skew-symmetric-matrix A')
  e=ones(n,1);
  X=zeros(n,1);
  U=zeros(n,1);
  p=e/n;
  t=0;
  %tm=U/t
  stop=0;
  iter=0;
  tic
    while(stop~=1)
      t=t+1;
      iter=t;
      %iteration number
      
      if rem(iter, 100) == 0
        disp(iter)
      end
      
      iter ;
      %cumsum = zeros(n,1);
      for i=1:n
        cumsum(i)=sum(p(1:i));
      end
      %zi=rand(n,1);
      con=0;
      marker=rand;
      k=1;
      %while con~=1
      for i=2:n
        if cumsum(i-1)<=marker && marker<=cumsum(i)
          k=i;
          break;
          %con=1;
        end
      end
      %end
      
      X(k)=X(k)+1;
      for i=1:n
        U(i)=U(i)+A(i,k);
      end
      
      s= (sum(p(1:n).*exp((eps/2)*A(1:n,k))));
      for i=1:n
        p(i)=(p(i)*exp((eps/2)*A(i,k))) / s;
      end
      p;
      %FID=fopen('output.txt', 'a');
      %C=fwrite(FID,p);
      %fclose(FID);
      u=U/t;
      if u<=eps*e
        stop=1;
        x=X/t;
      end
      
      %disp('hello')
      
    end
    iter;
    x;
  toc
  time(KK)=toc;
  iteration(KK) = iter;
  etx=sum(x)
  AX=A*X;
  error=abs(AX)-abs(U);
  error;
  sum(error)
  
  if A*x <= eps*e
    disp(' Ax <= eps*e  ')
  end
  if error<10^-8
    
    disp('Assertion condition is satisfied i.e. AX-U<10^-8')
    
  else
    disp('Error:  AX-U<10^-8 not satisfied ')
  end
  
  disp('Time for');
  disp(eps);
  disp('is');
  disp(time(KK));
  disp('Number of iteration is ');
  disp(iteration(KK));
end

disp('Epsilon vector is ');
disp(myeps);
disp('time vector is');
disp(time);
disp('Iteration Vector is');
disp(iteration);

out = [myeps; time; iteration];
disp('Epsilon-Time-Iteration tradeoff');
disp(out);

%save outputallvar.mat
%save outputdat.dat A -ASCII 
%xlswrite('outputxls.xls', out,'eps-time-iterations GK')

%{
xlswrite('output.xls', p,'Probability p')
xlswrite('output.xls', b,'Stp crt Udivt')
xlswrite('output.xls', x,'Opt sol x')
xlswrite('output.xls', time,'Time Taken')
xlswrite('output.xls', iter,'no of iteration')
xlswrite('output.xls',error ,'Error')
%}

function [a]=myunifskew(n)
%a=rand(n)
%{
for i=1:n
   a(i,i)=0;
end
%}

a=zeros(n);

disp('a(i,j) initialized with zeros');

for i=1:n
    for j=1:i-1
        temp=rand;
        if (temp < 0.5 )
         a(i,j)= rand;
         a(j,i)= -a(i,j);
        else
         a(j,i)= rand;
         a(i,j)= -a(j,i);
        end
    
    end
    if rem(i,1000) == 0
        disp(i)
    end
end

%{
if a == -a'
    display('Skew symmetric MATRIX CREATED');
end

%}
