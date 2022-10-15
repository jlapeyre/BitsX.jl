"""
    module BitsX

Tools for bits and masks. This module is similar to and depends on Bits.jl.

* Building masks (efficiently):
  `mask`, `rightmask`, `leftmask`, and `rangemask`.

* Bit views and indexing into bits of `Real`s:
  `bits`, `bit`

* Diagnostic: `min_bits`, `min_dits`, `is_bitstring`, `bitsize`.

* Converting between representations of bit sequences:
  `bit_string`, `bool_tuple`, `bit_vector`, `bool_vector`
"""
module BitsX

import Bits as _Bits
import Bits: bits, bitsize, BitVector1Mask, BitVector1, AbstractBitVector1
export bits, bitsize, BitVector1Mask, BitVector1, AbstractBitVector1
export undigits, undigits2
export uint_type, min_uint_type

export mask, leftmask, rightmask, rangemask,
    asint, bit, min_bits, min_dits

export bit_string, is_bitstring, bit_vector, bit_vector2, bool_tuple, bool_vector

const Word = UInt

include("bitintegers.jl")
include("bits.jl")

end # module BitsX
