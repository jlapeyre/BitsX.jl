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

export bits, StaticBitVector, StaticBitVector0, StaticBitVectorN
export StaticBitVector, AbstractStaticBitVector
export bitsizeof, bitlength
export undigits, undigits2
export uint_type, min_uint_type, min_uint_bit_width, min_uint_byte_width
export OneBased, ZeroBased

export mask, masked, leftmask, rightmask, rangemask,
    asint, bit, tstbit, unsafe_tstbit, min_bits, min_dits

export is_bitstring, bit_string, normalize_bitstring,
    bit_string!, bit_vector, bit_vector2, bool_tuple, bool_vector

export parse_bin

const Word = UInt

include("bitintegers.jl")
include("parse.jl")
include("bits.jl")

end # module BitsX
