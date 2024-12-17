module BStrings

module _BStrings

@inline _bitsizeof(::Type{T}) where {T} = 8 * sizeof(T)

# Similar as bitstring, but string is constructed in reverse.
# Copied from Base.bitstring. This uses non-API functions
@inline function _bstring(x::T, sz::Int, rev::Bool) where {T}
    if sz > _bitsizeof(T)
        str = _inner_bstring!(sz, x, rev)
        @inbounds for i in sz:-1:(_bitsizeof(T) + 1)
            str[i] = UInt8('0')
        end
        return String(str)
    end

    _rem = sz % 4
    if iszero(_rem)
        str = _inner_bstring!(sz, x, rev)
        return String(str)
    end

    _pad  = 4 - _rem
    _sz = sz + _pad
    str = _inner_bstring!(_sz, x, rev)
    # Following might be dangerous.
    # It is efficient.
    rev && return _String(str, sz)
    # This is not so efficient
    return String(str[1 + _pad:_sz])
end

@inline function _bstring(x::T, rev::Bool) where {T}
    sz = 8 * sizeof(T)
    str = _inner_bstring!(sz, x, rev)
    return String(str)
end

# Need this barrier function
@inline function _inner_bstring!(sz, x, rev::Bool)
    rev ? __inner_bstring!(sz, x, i -> sz - i + 1) : __inner_bstring!(sz, x, identity)
end

@inline function __inner_bstring!(sz, x::T, indf) where {T}
    # Exactly the following three items are not exported by base.
    StringMemory = Base.StringMemory
    trunc_int = Base.trunc_int
    lshr_int = Base.lshr_int

    str = StringMemory(sz)
    i = sz
    @inbounds while i >= 4
        b = UInt32(sizeof(T) == 1 ? bitcast(UInt8, x) : trunc_int(UInt8, x))
        d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
        # Difference from Base: Indices are complemented wrt. sz
        str[indf(i-3)] = (d >> 0x00) % UInt8
        str[indf(i-2)] = (d >> 0x08) % UInt8
        str[indf(i-1)] = (d >> 0x10) % UInt8
        str[indf(i)]   = (d >> 0x18) % UInt8
        x = lshr_int(x, 4)
        i -= 4
    end
    return str
end

# Copied from Base "string.jl"
# I added the parameter `len`
function _String(v::Memory{UInt8}, len::Int)
    len == 0 && return ""
    len < 0 && error("String length must be non-negative")
    len <= length(v) || error("Memory not large enough for length")
    return ccall(:jl_genericmemory_to_string, Ref{String}, (Any, Int), v, len)
end

end # module _BStrings

import ._BStrings

"""
    bstring(x::T; [pad::Int], rev::Bool=true) where {T}

Return a string giving the literal bit representation of a primitive type.

If `rev` is `true`, then the string is reversed with respect to `Base.bitstring`.  If
`pad` is given, then the returned string will have length `pad`.  If `pad` is greater than
the width in bits of `x` then the string will be padded with `'0'`. If `pad` is smaller
than the width of `x`, then upper bits will be truncated.
"""
function bstring(x::T; pad::Int=_BStrings._bitsizeof(T), rev::Bool=true) where {T}
    isprimitivetype(T) || throw(ArgumentError(LazyString(T, " not a primitive type")))
    pad == _BStrings._bitsizeof(T) && return _BStrings._bstring(x, rev)
    return _BStrings._bstring(x, pad, rev)
end

end # module BStrings
