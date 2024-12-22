module ParseBin

using Base: CodeUnits
using ...BitsX.BitsBase: inttype

export parse_bin

###
### Faster parse of binary string than `Base.parse`
###

module _ParseBin

using Base: CodeUnits
using ...BitsX.BitsBase: is_one_char, is_zero_char
using ...BitIntegersX: min_uint_type, get_one_bit_masks

# Compute the narrowest unsigned integer type to represent binary number encoded in a string
_min_uint_type(s::AbstractString) = min_uint_type(ncodeunits(s))
_min_uint_type(v::AbstractVector{UInt8}) = min_uint_type(length(v))

_length_in_UInt8(s::AbstractString) = ncodeunits(s) # not all AbstractString
_length_in_UInt8(v::AbstractVector{UInt8}) = length(v)

_parse_bin(::Type{T}, s::AbstractString, filter) where T = _parse_bin(T, codeunits(s), filter)

# The permissive option differs in two ways:
# 1. it permits chars other than '1' and '0'
# 2. it computes the single bit masks on the fly, rather than using precomputed masks.
# There can be significant time-performance difference between using precomputed or on-the-fly masks,
# say 50% to 80%. Sometimes a dramatic difference 6x. When to choose one or the other
# is slightly complicated and counterintuitive.
# But it's probably worth doing, since the differences can be large.
# We would want to implement a routine that is both strict and computes masks on the fly.
# This is very easy. But there's no reason to do it until we can discriminate which to use.
function _parse_bin(::Type{T}, c::AbstractVector{UInt8}, filter::Bool) where T
    return filter ? __parse_bin_permissive(T, c)::T :
        __parse_bin(T, c, get_one_bit_masks(T))::T
end

_parse_via_BigInt(::Type{T}, c::CodeUnits) where T = _parse_via_BigInt(T, c.s)
_parse_via_BigInt(::Type{T}, s::AbstractString) where T = T(parse(BigInt, _ensure_string(s_or_c); base=2))

# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function __parse_bin(::Type{T}, c, one_bit_masks)::T where {T}
    x::T = zero(T)
length(one_bit_masks) >= length(c) || throw(BoundsError(one_bit_masks, length(c))) # This line increases perf.
    nmax = lastindex(c)
    @inbounds for i in eachindex(c)
        ch::UInt8 = c[nmax - i + 1]
        if is_one_char(ch)
            x::T += one_bit_masks[i]::T
        elseif ! is_zero_char(ch)
            throw(DomainError(ch, lazy"Invalid `Char` '$(Char(ch))' in binary string"))
        end
    end
    return x
end

# Routine for: 1) No precomputed masks available, 2) assume strict bitstring
__parse_bin(::Type{T}, c, ::Nothing) where T = __parse_bin(T, c) # get_masks returns nothing if masks not available
function __parse_bin(::Type{T}, c)::T where T
    x::T = zero(T)
    one_bit_mask::T = one(T)
    nmax = lastindex(c)
    @inbounds for i in eachindex(c)
        if is_one_char(c[nmax - i + 1]::UInt8)
            x += one_bit_mask
        end
        one_bit_mask *= 2
    end
    return x
end

# TODO:  prefer reverse(eachindex(c))
__parse_bin_permissive(::Type{T}, c, ::Nothing) where T = __parse_bin(T, c) # get_masks returns nothing if masks not available
function __parse_bin_permissive(::Type{T}, c)::T where T
    x::T = zero(T)
    one_bit_mask::T = one(T)
    nmax = lastindex(c)
    @inbounds for i in eachindex(c)
        ch = c[nmax - i + 1]::UInt8
        if is_one_char(ch)
            x += one_bit_mask
            i == nmax && break
            one_bit_mask *= 2
        elseif is_zero_char(ch)
            one_bit_mask *= 2
        end
    end
    return x
end

end  # module _ParseBin

import ._ParseBin: _parse_bin, _min_uint_type, _length_in_UInt8

"""
    parse_bin([::Type{T}], s::Union{AbstractString, AbstractVector{UInt8}}; filter=false)

Convert an integer coded as a binary string (characters `'1'` and `'0'`) to an unsigned integer.

The return type is `T`. If the first argument is omitted, then the return type is the narrowest suitable unsigned integer type.

The type `T` is typically a bits type `UInt8`, `UInt16`, `Float64`, etc., where the number of bits is a factor of `8`.
The largest unsigned integer type that is pre-generated from `BitIntegers.jl` is `UInt1024`. If `T` is omitted,
a type that has not been pre-generated will be created.

Leading zeros are included in the calculation of the width of the resulting integer.

If `filter` is `true`, then characters other than `'1'` and `'0'` are ignored rather than raising an error. In
this way, formatting, such as spaces, may be included in the input string.

!!! warning "Ascii only"
    Non-ascii characters in the bitstring, even with `filter=true` will probably result in an error or incorrect results.


# Examples
```jldoctest
julia> parse_bin(UInt8, "10001111") |> bitstring
"10001111"

julia> parse_bin(UInt8, "1000 1111")
ERROR: BoundsError: attempt to access 8-element Vector{UInt8} at index [9]

julia> parse_bin(UInt8, "10 00 11 11"; filter=true) |> bitstring
"10001111"

julia> parse_bin(Bool, "1")
true
```

Various input types are supported.
```jldoctest sstr
julia> s = "0001111100000100";

julia> parse_bin.(UInt16, (s, codeunits(s), collect(codeunits(s))))
(0x1f04, 0x1f04, 0x1f04)
```

The type parameter need not be a bitstype.
```jldoctest sstr
julia> x = parse_bin(Signed, s); (x, typeof(x))
(7940, Int16)

julia> x = parse_bin(Unsigned, s); (x, typeof(x))
(0x1f04, UInt16)

julia> x = parse_bin(Integer, s); (x, typeof(x))
(0x1f04, UInt16)

julia> x = parse_bin(BigInt, s); (x, typeof(x))
(7940, BigInt)
```

!!! warning
    If characters other than '0' and '1' are present and `filter=true`, then the minimum bit width may be computed incorrectly.
    However if `T` is a bitstype, then no minimum bit width need be computed.
```jldoctest
julia> parse_bin("1"^8; filter=true)
0xff

julia> parse_bin("1"^8 * " "^8; filter=true)
0x00ff
```

# Extended help

# Comparison with `Base.parse`

* `parse_bin` is roughly 5 to 10 faster.
* `parse_bin` can optionally ignore non-coding characters in the string
* `parse_bin` can accept `AbstractVector{UInt8}` as input.

# Performance

Usually `parse_bin` is faster than `Base.parse`. The factor varies from about 5 to 10. However, for bit widths
larger than about 650, `parse_bin` first parses the string as a `BigInt` using `Base.parse` and then converts
to the appropriate fixed-wdith unsigned integer type.

`parse_bin` is often much faster if `T` is supplied.
"""
@inline parse_bin(s; filter::Bool=false) = parse_bin(_min_uint_type(s), s; filter=filter)

# For concrete subtypes of T, bitwidth is known, and we first use bit-equiv unsigned
@inline function parse_bin(::Type{T},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false) where {T <: Union{AbstractFloat, Signed}}
    return reinterpret(T, parse_bin(Base.uinttype(T), s_or_c; filter=filter))
end

function parse_bin(::Type{T}, s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false) where T
    BigInt_better_thresh = 650 # approx.
    if _length_in_UInt8(s_or_c) > BigInt_better_thresh
        return T(parse(BigInt, s_or_c; base=2)) # TODO: do filtering if requested
    end
    _parse_bin(T, s_or_c, filter)::T
end

# Types Unsigned and Integer,  return the min-width unsigned int
# I tried signatues to make a single method, but was unable to get all cases correct.
function parse_bin(::Type{Unsigned} ,  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    parse_bin(s_or_c; filter=filter)
end
function parse_bin(::Type{Integer} ,  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    parse_bin(s_or_c; filter=filter)
end

@inline function parse_bin(::Type{Bool},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    # For very small types, filtering is actually faster.
    Bool(parse_bin(UInt8, s_or_c; filter=true))
end

# For Signed, compute min-width unsigned int, then reinterpret to Signed
@inline function parse_bin(::Type{Signed},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    x = parse_bin(s_or_c; filter=filter)
    return reinterpret(inttype(typeof(x)), x)
end

# For BigInt, we use Base.parse, if possible.
@inline function parse_bin(::Type{BigInt}, s_or_c::Union{AbstractString, CodeUnits};  filter::Bool=false)
    filter && throw(ArgumentError("filtering bit string not supported for type BigInt"))
    parse(BigInt, s_or_c; base=2)
end

# If c is not CodeUnits, we don't know how to recover string, so can't convert first to BigInt
@inline parse_bin(::Type{T}, c::AbstractVector{UInt8}; filter::Bool=false) where T = _parse_bin(T, c, filter)

end # module ParseBin
