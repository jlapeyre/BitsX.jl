"""
    parse_bin(s::AbstractString)

Convert an integer coded as a binary string to an unsigned integer.
The return type is the narrowest suitable unsigned integer type .
"""
parse_bin(s::AbstractString) = parse_bin(min_uint_type(ncodeunits(s)), s)
parse_bin(c::Base.CodeUnits) = parse_bin(min_uint_type(length(c)), c)
parse_bin(::Type{T}, s::AbstractString) where T = parse_bin(T, codeunits(s))
parse_bin(::Type{T}, c::Base.CodeUnits) where T = _parse_bin(c, get_masks(T), T)

_parse_bin(s::AbstractString, facs, ::Type{T}) where T = _parse_bin(codeunits(s), facs, T)

# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function _parse_bin(c, facs, ::Type{T})::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += facs[i]::T)
    end
    return x
end

function _parse_bin(c, ::Type{T})::T where T
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
