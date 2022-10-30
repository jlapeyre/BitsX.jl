parse_bin(s::AbstractString) = _parse_bin(s, min_uint_type(ncodeunits(s)))
parse_bin(c::Base.CodeUnits) = _parse_bin(c, min_uint_type(length(c)))
parse_bin(::Type{T}, s) where T = _parse_bin(s, T)::T

_parse_bin(s::AbstractString, ::Type{T}) where T = _parse_bin(codeunits(s), T)
_parse_bin(c::Base.CodeUnits, ::Type{T}) where T = __parse_bin(c, get_masks(T), T)

__parse_bin(s::AbstractString, facs, ::Type{T}) where T = __parse_bin(codeunits(s), facs, T)

# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function __parse_bin(c, facs, ::Type{T})::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += facs[i]::T)
    end
    return x
end

function __parse_bin(c, ::Type{T})::T where T
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
