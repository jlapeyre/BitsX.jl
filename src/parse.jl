# Compute the narrowest unsigned integer type to represent binary number encoded in a string
_min_uint_type(s::AbstractString) = min_uint_type(ncodeunits(s))
_min_uint_type(v::AbstractVector{UInt8}) = min_uint_type(length(v))

_length_in_UInt8(s::AbstractString) = ncodeunits(s) # not all AbstractString
_length_in_UInt8(v::AbstractVector{UInt8}) = length(v)

"""
    parse_bin([::Type{UIntT}], s::AbstractString)

Convert an integer coded as a binary string to an unsigned integer.
The return type is `UIntT`. If the first argument is omitted, then
the return type is the narrowest suitable unsigned integer type .
"""
parse_bin(s) = parse_bin(_min_uint_type(s), s)

_ensure_string(s) = s
_ensure_string(c::Base.CodeUnits) = c.s

function parse_bin(::Type{T}, s) where T
    BigInt_better_thresh = 650 # approx.
    if _length_in_UInt8(s) > BigInt_better_thresh
        return T(parse(BigInt, _ensure_string(s); base=2))
    end
    return parse_bin(T, codeunits(s))::T
end

parse_bin(::Type{T}, c::AbstractVector{UInt8}) where T = _parse_bin1(T, c, get_masks(T))::T
_parse_bin1(::Type{T}, s::AbstractString, facs) where T = _parse_bin1(T, codeunits(s), facs)::T

# c - code units, (or Vector{UInt8}, but unsafe_wrap allocates, codeunits does not)
@inline function _parse_bin1(::Type{T}, c, facs)::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    @inbounds for i in eachindex(c)
        c[i]::UInt8 == _ONE_CHAR_CODE && (x::T += facs[i]::T)
    end
    return x
end

# Rarely if ever called.
function _parse_bin1(::Type{T}, c::Base.CodeUnits, ::Nothing)::T where T
    return convert(T, parse(BigInt, c.s; base=2))
end

function _parse_bin_general(::Type{T}, c, ::Nothing)::T where T
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

# Ignore all (single code unit) characters except 0 and 1
@inline function _parse_bin_ignore(::Type{T}, c, facs)::T where {T}
    x::T = zero(T)
    length(facs) >= length(c) || throw(BoundsError(facs, length(c))) # This line increases perf.
    j::Int = 1
    @inbounds for i in eachindex(c)
        ci::UInt8 = c[i]::UInt8
        if ci == _ONE_CHAR_CODE
            x::T += facs[j]::T
        elseif ci !=  _ZERO_CHAR_CODE
            continue
        end
        j += 1
    end
    return x
end

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
