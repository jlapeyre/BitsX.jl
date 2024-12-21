module API

import ..BitsX.BitsBase
export BitsBase

import ..BitsX.BitsBase: is_one_char, is_zero_char, is_binary_char, is_bitstring, check_bitstring,
    to_binary_char, to_binary_char_code, binzero, binone,
    isbinzero, isbinone, from_binary_char, ZeroBased, OneBased, IndexBase, min_bits, min_dits,
    bitaxes, bitaxes1, biteachindex, bitlastindex, bitsizeof, bitsize, bitlength, count_bits

export is_one_char, is_zero_char, is_binary_char, is_bitstring, check_bitstring,
    to_binary_char, to_binary_char_code, binzero, binone,
    isbinzero, isbinone, from_binary_char, ZeroBased, OneBased, IndexBase, min_bits, min_dits,
    bitaxes, bitaxes1, biteachindex, bitlastindex, bitsizeof, bitsize, bitlength, count_bits

import ..BitsX.Bits: randbitstring, randbitstring!, BitStringSampler,
    rightmask, leftmask, rangemask, mask, masked, bit,
    bit0, tstbit, tstbit0, normalize_bitstring, undigits,
    bit_string,
    bit_count_ones, bit_count_zeros

export randbitstring, randbitstring!, BitStringSampler,
    rightmask, leftmask, rangemask, mask, masked, bit,
    bit0, tstbit, tstbit0, normalize_bitstring, undigits,
    bit_string,
    bit_count_ones, bit_count_zeros

import ..BitsX.BitArrays: bitvector, bitarray, bitarray!
export bitvector, bitarray, bitarray!

import ..BitsX.BitArrayViews: bitvecview, BitArrayView, BitVectorView, BitMatrixView, bitmatview
export  bitvecview, BitArrayView, BitVectorView, BitMatrixView, bitmatview

# I need bit_reverse, or breverse. I currently type-pirate bitreverse.

import ..BitsX.BStrings: bstring
export bstring

import ..BitsX.BStringViews: bstringview, BStringView
export bstringview, BStringView

import ..BitsX.ParseBin: parse_bin
export parse_bin

import ..BitsX.StaticBitVectors: bits, StaticBitVector, StaticBitVectorView
export bits, StaticBitVector, StaticBitVectorView

import ..BitsX.BitArraysX: AbstractBitArray, AbstractBitVector, BitArrayAlt, BitArrayX, Chunks
export AbstractBitArray, AbstractBitVector, BitArrayAlt, BitArrayX, Chunks

import ..BitsX.BitIntegersX
export BitIntegersX

end # module API
