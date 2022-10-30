# Compute the narrowest unsigned integer type to represent binary number encoded in a string
_min_uint_type(s::AbstractString) = min_uint_type(ncodeunits(s))
_min_uint_type(v::AbstractVector{UInt8}) = min_uint_type(length(v))

"""
    parse_bin([::Type{UIntT}], s::AbstractString)

Convert an integer coded as a binary string to an unsigned integer.
The return type is `UIntT`. If the first argument is omitted, then
the return type is the narrowest suitable unsigned integer type .
"""
parse_bin(s) = parse_bin(_min_uint_type(s), s)
parse_bin(::Type{T}, s::AbstractString) where T = parse_bin(T, codeunits(s))
parse_bin(::Type{T}, c::AbstractVector{UInt8}) where T = _parse_bin(T, c, get_masks(T))

_parse_bin(::Type{T}, s::AbstractString, facs) where T = _parse_bin(T, codeunits(s), facs)

# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function _parse_bin(::Type{T}, c, facs)::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += facs[i]::T)
    end
    return x
end

function _parse_bin(::Type{T}, c)::T where T
    x::T = zero(T)
    fac::T = one(T)
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x += fac
        end
        fac *= 2
    end
    return x
end
