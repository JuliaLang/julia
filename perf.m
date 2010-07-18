function perf()
% simple performance tests

function nl()
   fprintf('\n')
end

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
nl()

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
nl()

%% array constructors %%

fprintf('ones: ')
small=ones(2,2);
tic(); o = ones(200,200); toc()
assert(all(o) == 1)
nl()

%% matmul and transpose %%

fprintf('A * transpose(A): ');
small*small';
tic(); oo = o * o'; toc()
assert(all(oo == 200))
nl()

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
assert(sum(sum(M)) == 14628)
nl()

%% numeric vector quicksort %%

function b = qsort(a) 
  b = qsort_kernel(a, 1, length(a));
end

function a = qsort_kernel(a, lo, hi)
    i = lo;
    j = hi;
    pivot = a((lo+hi)/2);
    % Partition
    while i <= j
        while a(i) < pivot, i = i + 1; end
        while a(j) > pivot, j = j - 1; end
        if i <= j
	    t = a(i);
	    a(i) = a(j);
	    a(j) = t;
            i = i + 1;
            j = j - 1;
        end
    end
    % Recursion for quicksort
    if lo < j; qsort_kernel(a, lo, j); end
    if i < hi; qsort_kernel(a, i, hi); end
end

fprintf('quicksort: ')
small=rand(3,1);
sort(small);
n = 5000;
v = rand(n,1);
tic(); v = sort(v); toc()
assert(issorted(v))
nl()

%% slow pi series %%

function sum = pisum()
    sum = 0.0;
    for j=1:500
        sum = 0.0;
        for k=1:10000
            sum = sum + 1.0/(k*k);
        end
    end
end

fprintf('pi sum: ')
tic(); s = pisum(); toc()
assert(abs(s-1.644834071848065) < 1e-12);

end
