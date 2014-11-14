%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Main function. All the tests are run here.           %%
%%  The functions declarations can be found at the end.  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function perf()

	warning off;
	if exist('OCTAVE_VERSION') == 0
    	maxNumCompThreads(1);
	end
	
	f = fib(20);
	assert(f == 6765)
	timeit('fib', @fib, 20)
	
	timeit('parse_int', @parseintperf, 1000)
	
	%% array constructors %%

	%o = ones(200,200);
	%assert(all(o) == 1)
	%timeit('ones', @ones, 200, 200)
	
	%assert(all(matmul(o) == 200))
	%timeit('AtA', @matmul, o)
	
	mandel(complex(-.53,.68));
	assert(sum(sum(mandelperf(true))) == 14791)
	timeit('mandel', @mandelperf, true)
	
	assert(issorted(sortperf(5000)))
	timeit('quicksort', @sortperf, 5000)
	
	s = pisum(true);
	assert(abs(s-1.644834071848065) < 1e-12);
	timeit('pi_sum',@pisum, true)
	
	%s = pisumvec(true);
	%assert(abs(s-1.644834071848065) < 1e-12);
	%timeit('pi_sum_vec',@pisumvec, true)
	
	[s1, s2] = randmatstat(1000);
	assert(round(10*s1) > 5 && round(10*s1) < 10);
	timeit('rand_mat_stat', @randmatstat, 1000)
	
	timeit('rand_mat_mul', @randmatmul, 1000);
	
	printfd(1)
	timeit('printfd', @printfd, 100000)

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Functions declarations  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function assert(bool)
   if ~bool
     error('Assertion failed')
   end
end

function timeit(name, func, varargin)
    lang = 'matlab';
    if exist('OCTAVE_VERSION') ~= 0
       lang = 'octave';
    end

    nexpt = 5;
    times = zeros(nexpt, 1);

    for i=1:nexpt
        tic(); func(varargin{:}); times(i) = toc();
    end

    times = sort(times);
    fprintf ('%s,%s,%.8f\n', lang, name, times(1)*1000);
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

%% parse int %%

function n = parseintperf(t)
    for i = 1:t
        n = randi([0,2^32-1],1,'uint32');
        s = dec2hex(n);
        m = hex2dec(s);
        assert(m == n);
    end
end

%% matmul and transpose %%

%function oo = matmul(o)
%    oo = o * o.';
%end

%% mandelbrot set: complex arithmetic and comprehensions %%

function n = mandel(z)
    n = 0;
    c = z;
    for n=0:79
        if abs(z)>2
            return
        end
        z = z^2+c;
    end
    n = 80;
end

function M = mandelperf(ignore)
  M = zeros(length(-2.0:.1:0.5), length(-1:.1:1));
  count = 1;
  for r = -2:0.1:0.5
    for i = -1:.1:1
      M(count) = mandel(complex(r,i));
      count = count + 1;
    end
  end
end

%% numeric vector quicksort %%

function b = qsort(a)
    b = qsort_kernel(a, 1, length(a));
end

function a = qsort_kernel(a, lo, hi)
    i = lo;
    j = hi;
    while i < hi
        pivot = a(floor((lo+hi)/2));
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
        if lo < j; a=qsort_kernel(a, lo, j); end
        lo = i;
	    j = hi;
    end
end

function v = sortperf(n)
    v = rand(n,1);
    v = qsort(v);
end

%% slow pi series %%

function sum = pisum(ignore)
    sum = 0.0;
    for j=1:500
        sum = 0.0;
        for k=1:10000
            sum = sum + 1.0/(k*k);
        end
    end
end

%% slow pi series, vectorized %%

function s = pisumvec(ignore)
    a = [1:10000]
    for j=1:500
        s = sum( 1./(a.^2));
    end
end

%% random matrix statistics %%

function [s1, s2] = randmatstat(t)
    n=5;
    v = zeros(t,1);
    w = zeros(t,1);
    for i=1:t
        a = randn(n, n);
        b = randn(n, n);
        c = randn(n, n);
        d = randn(n, n);
        P = [a b c d];
        Q = [a b;c d];
        v(i) = trace((P.'*P)^4);
        w(i) = trace((Q.'*Q)^4);
    end
    s1 = std(v)/mean(v);
    s2 = std(w)/mean(w);
end

function t = mytranspose(x)
    [m, n] = size(x);
    t = zeros(n, m);
    for i=1:n
        for j=1:m
            t(i,j) = x(j,i);
        end
    end
end

%% largish random number gen & matmul %%

function X = randmatmul(n)
    X = rand(n,n)*rand(n,n);
end

%% printf %%

function printfd(n)
    f = fopen('/dev/null','w');
    for i = 1:n
        fprintf(f, '%d %d\n', i, i);
    end
    fclose(f);
end
