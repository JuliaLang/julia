function perf()
% simple performance tests

%% recursive fib %%

function f = fib(n)
  if n < 2 
    f = n;
    return
  else
    f = fib(n-1) + fib(n-2);
  end
end

fprintf('recursive fib(20): \n')
fib(5);  % warm up: make sure fib is compiled
tic(); f = fib(20); toc()

%% parse int %%

fprintf('parse_int: \n')
bin2dec('10');
tic()
for i=1:1000
    global n
    n=bin2dec('1111000011110000111100001111');
end
toc()

%% array constructors %%

fprintf('ones: \n')
small=ones(2,2);
tic(); o = ones(200,200); toc()

%% matmul and transpose %%

fprintf('A * A.T: \n');
small*small';
tic(); oo = o * o'; toc()

%% mandelbrot set: complex arithmetic and comprehensions %%

function n = mandel(z)
    n = 0;
    c = z;
    for n=0:79
        if abs(z)>2
            break
        end
        z = z^2+c;
    end
end

fprintf('mandelbrot: \n');
mandel(complex(-.53,.68));
tic()

r = -2:0:.1:0.5;
i = -1:.1:1;
res = zeros(length(r), length(i));
for x=1:length(r)
  for y=1:length(i)
    res(x,y) = mandel(complex(r,i));
  end
end
toc()

end
