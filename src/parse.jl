parse_bin(s::AbstractString) = _parse_bin(s, min_uint_type(ncodeunits(s)))
parse_bin(::Type{T}, s::AbstractString) where T = _parse_bin(s, T)::T

parse_bin(c::Base.CodeUnits) = _parse_bin(c, min_uint_type(length(c)))
# function parse_bin(c::Base.CodeUnits)
#     _byte_width = min_uint_byte_width(length(c))
#     uint_type = uint_type_bytes(_byte_width)
# #    return _parse_bin(c, _byte_width, uint_type)
#     return _parse_bin(c, uint_type)
# end

parse_bin(::Type{T}, c::Base.CodeUnits) where T = _parse_bin(c, T)::T
#parse_bin(::Type{T}, s::AbstractString, _byte_width::Integer) where T = _parse_bin(s, _byte_width, T)

_parse_bin(s::AbstractString, ::Type{T}) where T = _parse_bin(codeunits(s), T)
_parse_bin(c::Base.CodeUnits, _byte_width, ::Type{T}) where T = __parse_bin(c, get_masks(T), T)
_parse_bin(c::Base.CodeUnits, ::Type{T}) where T = __parse_bin(c, get_masks(T), T)
__parse_bin(s::AbstractString, facs, ::Type{T}) where T = __parse_bin(codeunits(s), facs, T)

# c -- code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function __parse_bin(c, facs, ::Type{T})::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += facs[i]::T)
    end
    return x
end

######

@inline function _parse_bin_general(::Type{T}, s::AbstractString)::T where T
    x::T = zero(T)
    fac::T = one(T)
    c = codeunits(s)
    @inbounds for i in eachindex(c)
        if c[i] == _ONE_CHAR_CODE
            x += fac
        end
        fac *= 2
    end
    return x
end
