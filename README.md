# BitsX

[![Build Status](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jlapeyre/BitsX.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/jlapeyre/BitsX.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/jlapeyre/BitsX.jl)

`BitsX` provides types and functions for manipulating bits in bitstype types, such as `UInt` and `Float64`.
It is similar to, and depends on, [`Bits.jl`](https://github.com/rfourquet/Bits.jl). The scope of `BitsX`
is also a bit wider. It has overlap with functions in `Base` and `Bits`, but modified (improved) for my needs.
`BitsX` can be seen as extending and complementing functions on `Base` and `Bits`.

For example

* `bits` extends `Bits.bits` from [`Bits.jl`](https://github.com/rfourquet/Bits.jl) to allow more input type
   and control conversion.
* `bit_string` extends `Base.bitstring`
* `bit_vector` converts a binary string to a `BitVector`.


`BitsX` may be used together with `Bits`, in particular for functionality that did not require modification.
For example `Bits.weight`.
