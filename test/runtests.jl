# using BitsX: BitsX, min_bits, undigits, bits, bitsizeof, bitsize,
#     StaticBitVector, bit_string
# using BitsX: ZeroBased, OneBased
# using BitsX: parse_bin, bstring, bstringview, BStringView, bitvecview, BitVectorView,
#     bitmatview
# using BitsX: is_one_char, is_zero_char, is_binary_char, binzero, binone, isbinzero, isbinone,
#     to_binary_char, to_binary_char_code, from_binary_char
# using BitsX: bitlength, bitsize, bitsizeof, is_bitstring, bitvector, count_bits
# using BitsX: bit_count_ones, bit_count_zeros, bitreverse, bitreverse!
# using BitsX: rightmask, leftmask, mask, rangemask, asint, biteachindex, bit, normalize_bitstring
# using BitsX: randbitstring, randbitstring!

using BitsX.BitsBase: asuint, asint

using BitsX
import BitsX.BitIntegersX
import LinearAlgebra

using Test

import Documenter

include("test_aqua.jl")
include("test_bitsx.jl")

Documenter.DocMeta.setdocmeta!(BitsX, :DocTestSetup, :(using BitsX); recursive=true)
Documenter.doctest(BitsX)

# @static if Base.VERSION >= v"1.7"
#     include("test_jet.jl")
# end
