using Base: CodeUnits

# Compute the narrowest unsigned integer type to represent binary number encoded in a string
_min_uint_type(s::AbstractString) = min_uint_type(ncodeunits(s))
_min_uint_type(v::AbstractVector{UInt8}) = min_uint_type(length(v))

_length_in_UInt8(s::AbstractString) = ncodeunits(s) # not all AbstractString
_length_in_UInt8(v::AbstractVector{UInt8}) = length(v)

"""
    parse_bin([::Type{UIntT}], s::Union{AbstractString, AbstractVector{UInt8}})

Convert an integer coded as a binary string (characters `'1'` and `'0'`) to an unsigned integer.
The return type is `UIntT`. If the first argument is omitted, then
the return type is the narrowest suitable unsigned integer type.

The type `UIntT` is named `UInt8`, `UInt16`, etc where the number of bits is a factor of `8`.
The largest type that is pre-generated from `BitIntegers.jl` is `UInt1024`. If `UIntT` is omitted,
a type that has not been pre-generated will be created.

Usually `parse_bin` is faster than `Base.parse`. The factor varies from about 5 to 9. However, for bit widths
larger than about 650, `parse_bin` first parses the string as a `BigInt` using `Base.parse` and then converts
to the appropriate fixed-wdith unsigned integer type.
"""
parse_bin(s; filter::Bool=false) = parse_bin(_min_uint_type(s), s)

function parse_bin(::Type{T}, s_or_c::Union{AbstractString, CodeUnits}) where T # ; filter::Bool=false) where T
    BigInt_better_thresh = 650 # approx.
    if _length_in_UInt8(s_or_c) > BigInt_better_thresh
        return T(parse(BigInt, s_or_c; base=2)) # TODO: do filtering if requested
    end
    __parse_bin(T, s_or_c)::T
end

# If c is not CodeUnits, we don't know how to recover string, so can't use BigInt
parse_bin(::Type{T}, c::AbstractVector{UInt8}) where T = __parse_bin(T, c)

__parse_bin(::Type{T}, s::AbstractString) where T = __parse_bin(T, codeunits(s))
__parse_bin(::Type{T}, c::AbstractVector{UInt8}) where T = _parse_bin(T, c, get_one_bit_masks(T))::T
#__parse_bin(::Type{T}, c::AbstractVector{UInt8}) where T = _parse_bin_permissive(T, c, get_one_bit_masks(T))::T

_parse_via_BigInt(::Type{T}, c::CodeUnits) where T = _parse_via_BigInt(T, c.s)
_parse_via_BigInt(::Type{T}, s::AbstractString) where T = T(parse(BigInt, _ensure_string(s_or_c); base=2))


# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function _parse_bin(::Type{T}, c, one_bit_masks)::T where {T}
    x::T = zero(T)
    length(one_bit_masks) >= length(c) || throw(BoundsError(one_bit_masks, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += one_bit_masks[i]::T)
    end
    return x
end

@inline function _parse_bin_permissive(::Type{T}, c, one_bit_masks)::T where {T}
    x::T = zero(T)
    #    length(one_bit_masks) >= length(c) || throw(BoundsError(one_bit_masks, length(c))) # This line increases perf.
    # Need some kind of check here
    j::Int = 1
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x::T += one_bit_masks[j]::T
            j += 1
        elseif c[i]::UInt8 == _ZERO_CHAR_CODE
            j += 1
        end
    end
    return x
end

_parse_bin(::Type{T}, c, ::Nothing) where T = _parse_bin(T, c) # get_masks returns nothing if masks not available
function _parse_bin(::Type{T}, c)::T where T
    x::T = zero(T)
    one_bit_mask::T = one(T)
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x += one_bit_mask
        end
        one_bit_mask *= 2
    end
    return x
end

_parse_bin_permissive(::Type{T}, c, ::Nothing) where T = _parse_bin(T, c) # get_masks returns nothing if masks not available
function _parse_bin_permissive(::Type{T}, c)::T where T
    x::T = zero(T)
    one_bit_mask::T = one(T)
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x += one_bit_mask
            one_bit_mask *= 2
        elseif c[i]::UInt8 == _ZERO_CHAR_CODE
            one_bit_mask *= 2
        end
    end
    return x
end

"""
    count_bits(s::AbstractString)
    count_bits(v::AbstractVector{UInt8})

Return the number of characters (or bytes) that are `'1'` or `'0'`.

It is assume that `s` is an ASCII string. `count_bits` may be useul if
the string includes formatting characters, for example spaces.
"""
count_bits(s::AbstractString) = count_bits(codeunits(s))
function count_bits(v::AbstractVector{UInt8})
    cnt = 0
    for x::UInt8 in v
        if x == _ZERO_CHAR_CODE || x == _ONE_CHAR_CODE
            cnt += 1
        end
    end
    return cnt
end
