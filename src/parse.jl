module ParseBin

using Base: CodeUnits
using BitsX: is_one_char, is_zero_char, inttype
using ..BitIntegersX: min_uint_type, get_one_bit_masks

export parse_bin

###
### Faster parse of binary string than `Base.parse`
###

# Compute the narrowest unsigned integer type to represent binary number encoded in a string
_min_uint_type(s::AbstractString) = min_uint_type(ncodeunits(s))
_min_uint_type(v::AbstractVector{UInt8}) = min_uint_type(length(v))

_length_in_UInt8(s::AbstractString) = ncodeunits(s) # not all AbstractString
_length_in_UInt8(v::AbstractVector{UInt8}) = length(v)

"""
    parse_bin([::Type{T}], s::Union{AbstractString, AbstractVector{UInt8}}; filter=false)

Convert an integer coded as a binary string (characters `'1'` and `'0'`) to an unsigned integer.
The return type is `T`. If the first argument is omitted, then
the return type is the narrowest suitable unsigned integer type.

The type `T` is typically a bits type `UInt8`, `UInt16`, `Float64`, etc., where the number of bits is a factor of `8`.
The largest unsigned integer type that is pre-generated from `BitIntegers.jl` is `UInt1024`. If `T` is omitted,
an type that has not been pre-generated will be created. `parse_bin` is often much faster if `T` is
supplied.

Leading zeros are included in the calculation of the width of the resulting integer.

Usually `parse_bin` is faster than `Base.parse`. The factor varies from about 5 to 9. However, for bit widths
larger than about 650, `parse_bin` first parses the string as a `BigInt` using `Base.parse` and then converts
to the appropriate fixed-wdith unsigned integer type.

If `filter` is `true`, then characters other than `'1'` and `'0'` are ignored rather than raising an error. In
this way, formatting, such as spaces, may be included in the input string.
"""
parse_bin(s; filter::Bool=false) = parse_bin(_min_uint_type(s), s; filter=filter)

function parse_bin(::Type{T},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false) where {T <: Union{AbstractFloat, Signed}}
    return reinterpret(T, parse_bin(Base.uinttype(T), s_or_c; filter=filter))
end

# TODO: Refactor these.
function parse_bin(::Type{Unsigned},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    parse_bin(s_or_c; filter=filter)
end

function parse_bin(::Type{Integer},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    parse_bin(s_or_c; filter=filter)
end

function parse_bin(::Type{Signed},  s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false)
    x = parse_bin(s_or_c; filter=filter)
    return reinterpret(inttype(typeof(x)), x)
end

function parse_bin(::Type{T}, s_or_c::Union{AbstractString, CodeUnits}; filter::Bool=false) where T # ; filter::Bool=false) where T
    BigInt_better_thresh = 650 # approx.
    if _length_in_UInt8(s_or_c) > BigInt_better_thresh
        return T(parse(BigInt, s_or_c; base=2)) # TODO: do filtering if requested
    end
    _parse_bin(T, s_or_c, filter)::T
end

# If c is not CodeUnits, we don't know how to recover string, so can't use BigInt
parse_bin(::Type{T}, c::AbstractVector{UInt8}; filter::Bool=false) where T = _parse_bin(T, c, filter)

_parse_bin(::Type{T}, s::AbstractString, filter) where T = _parse_bin(T, codeunits(s), filter)

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
            throw(DomainError(ch, "Invalid byte in binary string "))
        end
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
            one_bit_mask *= 2
        elseif is_zero_char(ch)
            one_bit_mask *= 2
        end
    end
    return x
end

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

end # module ParseBin
