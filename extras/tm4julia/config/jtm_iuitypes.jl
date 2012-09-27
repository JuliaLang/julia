
module jtm_iuitypes

export IUI, IUI8, IUI16, IUI32, IUI64, IUI128,
       IUI3264, IUI64128

import Base.*

  IUI8     = Union(Uint8  , Int8  )
  IUI16    = Union(Uint16 , Int16 )
  IUI32    = Union(Uint32 , Int32 )
  IUI64    = Union(Uint64 , Int64 )
  IUI128   = Union(Uint128, Int128)
  IUI3264  = Union(Uint64, Int64, Uint32 , Int32 )
  IUI64128 = Union(Uint64, Int64, Uint128, Int128)

  typealias IUI
      if (WORD_SIZE ==   8) IUI8
  elseif (WORD_SIZE ==  16) IUI16
  elseif (WORD_SIZE ==  32) IUI32
  elseif (WORD_SIZE ==  64) IUI64
  elseif (WORD_SIZE == 128) IUI128
  else   error("Unexpected WORD_SIZE ($WORD_SIZE).")
  end


end # module
