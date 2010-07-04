function perf()
% simple performance tests

function assert(expr)
   if ~expr
     error('Assertion failed')
   end
end

%% recursive fib %%

function f = fib(n)
  if n < 2 
    f = n;
    return
  else
    f = fib(n-1) + fib(n-2);
  end
end

fprintf('recursive fib(20): ')
fib(5);  % warm up: make sure fib is compiled
tic(); f = fib(20); toc()
assert(f == 6765)

%% parse int %%

fprintf('parse_int: ')
bin2dec('10');
tic()
for i=1:1000
    global n
    n=bin2dec('1111000011110000111100001111');
end
toc()
assert(n == 252645135)

%% array constructors %%

fprintf('ones: ')
small=ones(2,2);
tic(); o = ones(200,200); toc()
assert(all(o) == 1)

%% matmul and transpose %%

fprintf('A * A.T: ');
small*small';
tic(); oo = o * o'; toc()
assert(all(oo == 200))

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

fprintf('mandelbrot: ');
mandel(complex(-.53,.68));

tic()
M = zeros(length(-2.0:.1:0.5), length(-1:.1:1));
count = 1;
for r = -2:0.1:0.5
  for i = -1:.1:1
    M(count) = mandel(complex(r,i));
    count = count + 1;
  end
end
toc()
assert(sum(sum(M)) == 14791)
end
