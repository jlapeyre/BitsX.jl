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

export bits, StaticBitVector, StaticBitVector0, StaticBitVectorN, StaticBitVectorView
export AbstractStaticBitVector
export bitsizeof, bitlength
export undigits, undigits2
export uint_type, min_uint_type, min_uint_bit_width, min_uint_byte_width
export OneBased, ZeroBased
export datatype

export mask, masked, leftmask, rightmask, rangemask,
    asint, bit, bit0, tstbit, tstbit0, unsafe_tstbit, min_bits, min_dits

export is_bitstring, bit_string, normalize_bitstring, count_bits, bit_string!

# obsolete: bit_vector, bit_vector2, bool_tuple, bool_vector

export is_one_char, is_zero_char, is_binary_char, from_binary_char, to_binary_char_code

export parse_bin
export BitStringVector, bitvecview
export BitStringView, bitstringview

export selectbits

const Word = UInt

include("bitintegers.jl") # This is copied from BitIntegers.jl in order to include bug fixes
include("bitintegers_extra.jl")
include("parse.jl")
include("bits.jl")
include("bitvecview.jl")
include("bitstringview.jl")

end # module BitsX
