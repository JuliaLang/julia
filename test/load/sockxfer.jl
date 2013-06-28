


function xfer(s, xfer_exp)
    xfer_size = 10^xfer_exp
    xfer_block = 10^(xfer_exp - 4)
    
    @sync begin
        @async begin
            # read in chunks of xfer_block
            bread = 0
            while bread < xfer_size
                data = read(s, Uint8, xfer_block)
                @assert length(data) == xfer_block
                bread = bread + xfer_block
            end
            
#            println("process $(myid()) received $bread bytes")
            
        end
        
        @async begin
            # write in chunks of xfer_block
            data = fill!(zeros(Uint8, xfer_block), int8(65))    
            bwritten = 0
            while bwritten < xfer_size
                write(s, data)
                bwritten = bwritten + xfer_block
            end
#            println("process $(myid()) sent $bwritten bytes")
        end
    end
end

