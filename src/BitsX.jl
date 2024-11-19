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

import Random

export bits, StaticBitVector, StaticBitVector0, StaticBitVectorN, StaticBitVectorView
export AbstractStaticBitVector
export bitsizeof, bitlength, bitsize, bitgetindex, biteachindex, bitlastindex
export undigits
# TODO: export these from submodule
# export min_uint_bit_width, min_uint_byte_width, min_uint_type, uint_type
export OneBased, ZeroBased
export datatype
export randbitstring, randbitstring!

export mask, masked, leftmask, rightmask, rangemask,
    asint, asuint, bit, bit0, tstbit, tstbit0, unsafe_tstbit, min_bits, min_dits

export is_bitstring, bit_string, normalize_bitstring, count_bits
export is_one_char, is_zero_char, is_binary_char, from_binary_char, to_binary_char_code, to_binary_char

export parse_bin, bitconvert
export BitArrayView, bitvecview, bitmatview, bitarrview, BitVectorView, BitMatrixView
export BitStringView, bitstringview
export fliporder

export binzero, binone, isbinzero, isbinone
export bit_count_ones, bit_count_zeros
export bitvector

export @bsv

"""
    const Word = UInt

The default data type used for bit vectors and views.
"""
const Word = UInt

export SBitArray

include("bitintegers_extra.jl")
include("bits.jl")

include("parse.jl")
using .ParseBin: parse_bin

include("staticbitvectors.jl")
include("bitvecview.jl")
include("bitstringview.jl")
include("bitconvert.jl")

using .BitConvert: bitconvert

include("sbitvectors.jl")

end # module BitsX
