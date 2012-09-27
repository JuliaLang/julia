
module jtm

macro isdefined(this)
    quote
        try
            (($this) === ($this))
        catch e
            false
        end
    end
end

const  JULIA_JTM_DIR = "/home/jas/Desktop/julia/extras/tm4julia"
JTM_FILE(x::ASCIIString) = strcat("/home/jas/Desktop/julia/extras/tm4julia/",x,".jl")
JTM_FILE(x::ASCIIString,ext::ASCIIString) = strcat("/home/jas/Desktop/julia/extras/tm4julia/",x,".",ext)

if
include(JTM_FILE("bitwise/BitFields"))
import BitFields.set_bitfield
import BitFields.get_bitfield

include(JTM_FILE("bitstypes/TimezonedBitstypes"))

end # module

