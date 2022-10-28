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

using Dictionaries: Dictionaries, Dictionary

export bits, StaticBitVector, StaticBitVector0, StaticBitVectorN
export StaticBitVector, AbstractStaticBitVector
export bitsizeof, bitlength
export undigits, undigits2
export uint_type, min_uint_type
export OneBased, ZeroBased

export mask, masked, leftmask, rightmask, rangemask,
    asint, bit, tstbit, unsafe_tstbit, min_bits, min_dits

export bit_string, bit_string!, is_bitstring, bit_vector, bit_vector2, bool_tuple, bool_vector

const Word = UInt

#include("dictstaticbits.jl")
include("bitintegers.jl")
include("bits.jl")
#include("static_bitvector.jl")

end # module BitsX
