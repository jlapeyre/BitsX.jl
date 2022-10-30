function parse_bin(s::AbstractString)
    _byte_width = min_uint_byte_width(ncodeunits(s))
    uint_type = uint_type_bytes(_byte_width)
    return _parse_bin(s, _byte_width, uint_type)
end

function parse_bin(::Type{T}, s::AbstractString)::T where T
    return _parse_bin(s, byte_width(T), T)
end

function parse_bin(c::Base.CodeUnits)
    _byte_width = min_uint_byte_width(length(c))
    uint_type = uint_type_bytes(_byte_width)
    return _parse_bin(c, _byte_width, uint_type)
end

parse_bin(::Type{T}, c::Base.CodeUnits) where T = _parse_bin(c, byte_width(T), T)::T

function parse_bin(::Type{T}, s::AbstractString, _byte_width::Integer)::T where T
    return _parse_bin(s, _byte_width, T)
end

_parse_bin(s::AbstractString, _byte_width::Integer, ::Type{T}) where T =
    _parse_bin(codeunits(s), _byte_width, T)

@inline function _parse_bin(c::Union{Base.CodeUnits, Vector{UInt8}}, byte_width, ::Type{T})::T where T
    facs = get_masks(T)
    return __parse_bin(c, facs, T)
end

# c -- code units, or Vector{UInt8}
#@inline function __parse_bin(c, facs::NTuple{NBits,T}, ::Type{T})::T where {T, NBits}
@inline function __parse_bin(c, facs, ::Type{T})::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c)))
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x::T += facs[i]::T
        end
    end
    return x
end

#@inline function __parse_bin(s::AbstractString, facs::NTuple{NBits,T}, ::Type{T})::T where {T, NBits}
@inline function __parse_bin(s::AbstractString, facs, ::Type{T})::T where {T}
    c::Base.CodeUnits{UInt8, String} = codeunits(s)
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c)))
    @inbounds for i in eachindex(c)
        if c[i]::UInt8 == _ONE_CHAR_CODE
            x::T += facs[i]::T
        end
    end
    return x
end

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
