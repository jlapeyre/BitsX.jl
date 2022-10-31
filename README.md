# BitsX

[![Build Status](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/BitsX.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/BitsX.jl)

`BitsX` provides types and functions for manipulating bits in bitstype types, such as `UInt` and `Float64`.
It is similar to, and partly derived from, [`Bits.jl`](https://github.com/rfourquet/Bits.jl).
Some bugs are fixed and some design choices are different.
In addition, the scope of `BitsX` is a bit wider. In particular, it extends some functions in `Base`.

For example

* `bits` is like `Bits.bits` from [`Bits.jl`](https://github.com/rfourquet/Bits.jl), but also allows more input types
   and control conversion.
* `bit_string` extends `Base.bitstring`
* `bit_vector` converts a binary string to a `BitVector`.

### Some features

See the doc strings for these:
* `parse_bin`
* `bit`
* `bits`
