module API

# utility-like functions
import ..BitsX._BitsX: is_one_char, is_zero_char, is_binary_char, is_bitstring,
    to_binary_char, to_binary_char_code, binzero, binone,
    isbinzero, isbinone, from_binary_char, ZeroBased, OneBased, IndexBase

export is_one_char, is_zero_char, is_binary_char, is_bitstring,
    to_binary_char, to_binary_char_code, binzero, binone,
    isbinzero, isbinone, from_binary_char, ZeroBased, OneBased, IndexBase

import ..BitsX._BitsX: randbitstring, randbitstring!, bitsizeof, bitsize,
    bitlength, rightmask, leftmask, rangemask, mask, masked, bit,
    bit0, tstbit, tstbit0, normalize_bitstring, undigits,
    bit_string, min_bits, min_dits, count_bits,
    bitaxes, bitaxes1, biteachindex, bitlastindex,
    bit_count_ones, bit_count_zeros, bitvector

export randbitstring, randbitstring!, bitsizeof, bitsize,
    bitlength, rightmask, leftmask, rangemask, mask, masked, bit,
    bit0, tstbit, tstbit0, normalize_bitstring, undigits,
    bit_string, min_bits, min_dits, count_bits,
    bitaxes, bitaxes1, biteachindex, bitlastindex,
    bit_count_ones, bit_count_zeros, bitvector

import ..BitsX.BitArrayViews: bitvecview, BitArrayView, BitVectorView, BitMatrixView, bitmatview
export  bitvecview, BitArrayView, BitVectorView, BitMatrixView, bitmatview

# I need bit_reverse, or breverse. I currently type-pirate bitreverse.
# These should be considered. But less likely to use in API
# import ..BitsX: asint, asuint, unsafe_tstbit, bitgetindex, fliporder,
# export asint, asuint

import ..BitsX.BStrings: bstring
export bstring

import ..BitsX.BStringViews: bstringview, BStringView
export bstringview, BStringView

import ..BitsX.ParseBin: parse_bin
export parse_bin

import ..BitsX.StaticBitVectors: bits, StaticBitVector
export bits, StaticBitVector

end # module API
