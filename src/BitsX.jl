"""
    module BitsX

`BitsX` is a collection of types and functions for working with representations bits.

A main idea is to have a convenient, performant, and uniform interface to working with representations
of bits in various data types. For example a vector of bits may be represented as a value of any
of `Vector{Bool}`, `UInt64`, `String`, etc. The abstractions supporting this idea are in [`BitsBase`](@ref) (for the most part).

# Vocabulary

- *bit string*:  an `AbstractString` whose characters are all `'1'` and `'0'`. In some contexts, formatting
   characters may be present as well.

!!! warn
    The API of `BitsX` is large, perhapse too large. Much of it is rather exploratory and incomplete.
    In particular, interfaces for types that require a lot of implemenation are incomplete.

    `BitsX.jl` has recently been reorganized and modularized. It might be a good idea to split it into
     smaller packages.

# Related packages

* [BitBasis.jl](https://github.com/QuantumBFS/BitBasis.jl)
* [BitFlags.jl](https://github.com/jmert/BitFlags.jl)
* [BitInformation.jl](https://github.com/milankl/BitInformation.jl)
* [BitIntegers.jl](https://github.com/rfourquet/BitIntegers.jl)
* [BitOperations.jl](https://github.com/oschulz/BitOperations.jl)
* [BitPermutations.jl](https://github.com/giacomogiudice/BitPermutations.jl)
* [Bits.jl](https://github.com/rfourquet/Bits.jl)
* [FieldFlags.jl](https://github.com/Seelengrab/FieldFlags.jl)
* [BitTwiddlingConvenienceFunctions](https://github.com/JuliaSIMD/BitTwiddlingConvenienceFunctions.jl)
* [BitConverter.jl](https://github.com/roshii/BitConverter.jl)
* [BitVectorExtensions.jl](https://github.com/andrewjradcliffe/BitVectorExtensions.jl)
* [BitMasks.jl](https://github.com/serenity4/BitMasks.jl)
* [FlagSets.jl](https://github.com/jessymilare/FlagSets.jl)
* [BitsFields.jl](https://github.com/JeffreySarnoff/BitsFields.jl)
* [BitSetTuples.jl](https://github.com/wouterwln/BitSetTuples.jl)
* [BitFloats.jl](https://github.com/rfourquet/BitFloats.jl)
"""
module BitsX

import Reexport

"""
    const Word = UInt

The default data type used in creating bit masks. This
is inherited by several funcions that use bit masks.

This is used inconsistently
"""
const Word = UInt

include("bitintegers_extra.jl")
include("bitsbase.jl")
include("bits.jl")
include("bitarrays.jl")
include("bstring.jl")
include("parse_bin.jl")
include("staticbitvectors.jl")
include("bitarrayviews.jl")
include("bstringview.jl")
include("sbitvectors.jl")
include("bitarraysx.jl")

include("api.jl")
Reexport.@reexport using .API

end # module BitsX

#  LocalWords:  BitsX
