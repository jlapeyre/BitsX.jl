"""
    module BitsX

Tools for bits and masks. OUTOFDATEDOCS

* Building masks (efficiently):
  `mask`, `rightmask`, `leftmask`, and `rangemask`.

* Bit views and indexing into bits of `Real`s:
  `bits`, `bit`

* Diagnostic: `min_bits`, `min_dits`, `is_bitstring`, `bitsize`.

* Converting between representations of bit sequences:
  `bit_string`, `bool_tuple`, `bit_vector`, `bool_vector`
"""
module BitsX

import Reexport

"""
    const Word = UInt

The default data type used for bit vectors and views.
"""
const Word = UInt

export SBitArray

include("bstring.jl")
# import .BStrings: bstring
#export bstring

include("bitintegers_extra.jl")
include("bits.jl")
include("parse.jl")
#using .ParseBin: parse_bin
include("staticbitvectors.jl")
include("bitarrayviews.jl")
include("bstringview.jl")
include("sbitvectors.jl")
include("bitarraysx.jl")
import .BitArraysX

include("api.jl")
Reexport.@reexport using .API

end # module BitsX
