macro timeit(ex,name)
    @gensym t i
    quote
        $t = Inf
        for $i=1:5
            $t = min($t, @elapsed $ex)
        end
        @printf("julia, %s: best of 5 took %.1f ms\n", $name, $t*1000)
    end
end


