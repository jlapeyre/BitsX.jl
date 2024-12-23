"""
    module BitsX

`BitsX` is a collection of types and functions for working with bits.

The API is large and much of it is rather exploratory. In particular interfaces for type that
require a lot of implemenation are incomplete. On the other hand, some pieces are solid and
seem useful.

A main interface idea in this repo is in [`BitsBase`](@ref).
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
