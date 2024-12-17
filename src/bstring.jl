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

# This is more than an order of magnitude slower than creating the input string `str`.
function _space_string(str, nsep, pad)
#    buf = IOBuffer(UInt8[];sizehint=pad, write=true)
    buf = IOBuffer()
    for (i, c) in enumerate(str)
        write(buf, c)
        i == pad && break
        if i % nsep == 0
            write(buf, ' ')
        end
    end
    String(take!(buf))
end

end # module _BStrings

import ._BStrings

"""
    bstring(x::T; [pad::Int], rev::Bool=true, [nsep::Int]) where {T}

Return a string giving the literal bit representation of a primitive type.

If `rev` is `true`, then the string is reversed with respect to `Base.bitstring`.

If `pad` is given, then the returned string will have length `pad`.  If `pad` is greater than
the width in bits of `x` then the string will be padded with `'0'`. If `pad` is smaller
than the width of `x`, then upper bits will be truncated.

If `nsep` is given then a space is inserted every `nsep` bits.

If `sep` is `true` and `nsep` is not given, then `nsep` is set to eight.
For example, `bstring(x; sep=true)`. This is for convenience in the most common case.
"""
function bstring(x::T; pad::Int=_BStrings._bitsizeof(T), rev::Bool=true,
                 nsep::Union{Int, Nothing}=nothing, sep::Bool=false) where {T}
    isprimitivetype(T) || throw(ArgumentError(LazyString(T, " not a primitive type")))
    str = (pad == _BStrings._bitsizeof(T) ? _BStrings._bstring(x, rev) :
        _BStrings._bstring(x, pad, rev))
    if isnothing(nsep)
        sep || return str
        nsep = 8
    end
    # Following is much less performant than creating `str`.
    return _BStrings._space_string(str, nsep, pad)
end

end # module BStrings
