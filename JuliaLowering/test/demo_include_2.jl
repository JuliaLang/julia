
    module A
        another_global = "global in A"

        macro bar(ex)
            quote
                x = "`x` in @bar"
                (x, another_global, $ex)
            end
        end
    end

