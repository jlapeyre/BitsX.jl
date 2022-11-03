# BitsX

[![Build Status](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/BitsX.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/BitsX.jl)

`BitsX` provides types and functions for manipulating bits in objects representing binary numbers: strings, integers,
arrays, tuples, etc.

It is similar to, and partly derived from, [`Bits.jl`](https://github.com/rfourquet/Bits.jl).
Some bugs are fixed and some design choices are different.
The scope of `BitsX` is a bit wider. In particular, it extends some functions in `Base`.

For example

* `bits` is like `Bits.bits` from [`Bits.jl`](https://github.com/rfourquet/Bits.jl), but also allows more input types
   and control conversion.
* `bit_string` extends `Base.bitstring` by allowing numbers other than unsigned integers.

### Some features

See the doc strings for these:

* `parse_bin`
* `bit`
* `bit0`
* `tstbit`
* `tstbit0`
* `bits`
* `count_bits`
* `is_bitstring`
* `bitsizeof`
* `bitlength`
* `rightmask`
* `leftmask`
* `rangemask`
* `mask`
* `masked`
* `normalize_bitstring`
* `undigits`
* `min_bits`
* `min_dits`

#### Views of bitstring as a vector and vice versa

These also enable efficient conversion.

* `bitvecview`
* `bitstringview`

#### Some types

* `StaticBitVectorView`
* `StaticBitVector`
* `StaticBitVector0`
* `StaticBitVectorN`
* `BitStringVector`
* `BitStringView`

#### Some utilites used in this package will be of wider use

* `is_one_char`
* `is_zero_char`
* `is_binary_char`
* `to_binary_char_code`
* `from_binary_char_code`
* `from_binary_char`
