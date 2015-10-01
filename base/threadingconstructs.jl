# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


export threadid, maxthreads, nthreads, @threads

threadid() = Int(ccall(:jl_threadid, Int16, ())+1)
maxthreads() = Int(unsafe_load(cglobal(:jl_max_threads, Cint)))
nthreads() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

function _threadsfor(forexpr)
    fun = gensym("_threadsfor")
    lidx = forexpr.args[1].args[1]			# index
    lf = forexpr.args[1].args[2].args[1]		# first
    ll = forexpr.args[1].args[2].args[2]		# last
    lbody = forexpr.args[2]				# body
    quote
	function $fun()
	    tid = threadid()
	    # divide loop iterations among threads
	    len, rem = divrem($(esc(ll))-$(esc(lf))+1, nthreads())
            # not enough iterations for all the threads?
            if len == 0
                if tid > rem
                    return
                end
                len, rem = 1, 0
            end
            # compute this thread's range
	    f = $(esc(lf)) + ((tid-1) * len)
	    l = f + len - 1
            # distribute remaining iterations evenly
	    if rem > 0
		if tid <= rem
		    f = f + (tid-1)
		    l = l + tid
		else
		    f = f + rem
		    l = l + rem
		end
	    end
            # run this thread's iterations
	    for $(esc(lidx)) = f:l
		$(esc(lbody))
	    end
	end
        ccall(:jl_threading_run, Void, (Any, Any), $fun, ())
    end
end

function _threadsblock(blk)
    fun = gensym("_threadsblock")
    esc(quote
        function $fun()
            $blk
        end
        ccall(:jl_threading_run, Void, (Any, Any), $fun, ())
    end)
end

function _threadscall(callexpr)
    fun = callexpr.args[1]
    esc(quote
        ccall(:jl_threading_run, Void, (Any, Any), $fun, $(Expr(:call, Core.svec, callexpr.args[2:end]...)))
    end)
end

macro threads(args...)
    na = length(args)
    if na != 2
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    tg = args[1]
    if !is(tg, :all)
        throw(ArgumentError("only 'all' supported as thread group for @threads"))
    end
    ex = args[2]
    if !isa(ex, Expr)
	throw(ArgumentError("need an expression argument to @threads"))
    end
    if is(ex.head, :for)
	return _threadsfor(ex)
    elseif is(ex.head, :block)
	return _threadsblock(ex)
    elseif is(ex.head, :call)
	return _threadscall(ex)
    else
        throw(ArgumentError("unrecognized argument to @threads"))
    end
end

