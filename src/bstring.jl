module BStrings

using ..BitsX._BitsX: bitsizeof

# Need this barrier function, to make the anon func efficient. (probably is another way)
function _bin(x, len::Int, pad::Bool, sep::Int, rev::Bool)
    pad && (len = max(len, Base.top_set_bit(x)))
    __bin(x, len, sep, rev)
end

# Return length of string of `len` bits
# with spaces every `sep` bits.
function _with_spaces_length(len, sep)
    iszero(sep) && return len
    (nspc, r) = divrem(len, sep)
    # If r==0, the final separator would have no bits on one side.
    # So, reduce the number of separators by one.
    r == 0 && (nspc -= 1)
    return nspc + len
end

# Need this barrier function, too.
@inline  function __bin(x, len::Int, sep::Int, rev::Bool)
    # If sep != 0, then we need a longer string
    str_len = _with_spaces_length(len, sep)
    rev ? ___bin(x, len, str_len, sep, i -> str_len - i + 1) : ___bin(x, len, str_len, sep, identity)
end

## Following is modified from Base int_funcs.jl
# `len` is the unconditional number of bits in output string.
#    If sep==0, then `len` is also the unconditional length of the string
# `str_len` is the length of the string, which may include spaces if sep!=0
# `sep` is the number of bits between spaces. If sep==0, then no spaces are written.
# `indf` is either `identity` or a function reverses indices.
@inline  function ___bin(x, len::Int, str_len, sep::Int, indf)
    StringMemory = Base.StringMemory
    bitcast = Base.bitcast
    trunc_int = Base.trunc_int
    lshr_int = Base.lshr_int

    n = str_len
    char_ind = n # The index into the StringMemory
    a = StringMemory(char_ind)
    (nloops, _rem) = divrem(sep, 4)
    not_ragged = (_rem == 0)
    if iszero(sep) # Write no spaces
        @inbounds while char_ind >= 4
            b = UInt32(sizeof(typeof(x)) == 1 ? bitcast(UInt8, x) : trunc_int(UInt8, x))
            d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
            a[indf(char_ind-3)] = (d >> 0x00) % UInt8
            a[indf(char_ind-2)] = (d >> 0x08) % UInt8
            a[indf(char_ind-1)] = (d >> 0x10) % UInt8
            a[indf(char_ind)]   = (d >> 0x18) % UInt8
            x = lshr_int(x, 0x4)
            char_ind -= 4
        end
    # If sep is a multiple of four, the following is more efficient than the general case below.
    elseif not_ragged
        bit_ind = len
        loopcount = 0
        @inbounds while bit_ind >= 4
            b = UInt32(sizeof(typeof(x)) == 1 ? bitcast(UInt8, x) : trunc_int(UInt8, x))
            d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
            a[indf(char_ind-3)] = (d >> 0x00) % UInt8
            a[indf(char_ind-2)] = (d >> 0x08) % UInt8
            a[indf(char_ind-1)] = (d >> 0x10) % UInt8
            a[indf(char_ind)]   = (d >> 0x18) % UInt8
            x = lshr_int(x, 0x4)

            loopcount += 1
            if loopcount == nloops
                loopcount = 0
                ii = char_ind - 4
                if ii > 0
                    a[indf(char_ind - 4)] = ' '
                    char_ind -= 1
                end
            end
            char_ind -= 4
            bit_ind -= 4
        end
    else
        # `bit_ind` is the index of the bit to write. Can be different from `char_ind`.
        # We track `bit_ind` because we need to track how many characters (including spaces)
        # have been written, but also, how many bit characters.
        #
        # This could be done more efficiently.
        bit_ind = len
        @inbounds while bit_ind >= 4
            b = UInt32((x % UInt8)::UInt8)
            d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
            (char_ind, bit_ind) = _writechar(a, indf, sep, char_ind, bit_ind, (d >> 0x18) % UInt8)
            (char_ind, bit_ind) = _writechar(a, indf, sep, char_ind, bit_ind, (d >> 0x10) % UInt8)
            (char_ind, bit_ind) = _writechar(a, indf, sep, char_ind, bit_ind, (d >> 0x08) % UInt8)
            (char_ind, bit_ind) = _writechar(a, indf, sep, char_ind, bit_ind, (d >> 0x00) % UInt8)

            x >>= 0x4
        end
    end
    while char_ind > 0
        @inbounds a[indf(char_ind)] = 0x30 + ((x % UInt8)::UInt8 & 0x1)
        x >>= 0x1
        char_ind -= 1
    end
    String(a)
end

# Write a character '0' or '1' to the string.
# If we have written `sep` bits, then write a space.
# Return next values of `char_ind` and `bit_ind`
@inline function _writechar(a, indf, sep, char_ind, bit_ind, char)
    @inbounds a[indf(char_ind)] = char
    char_ind -= 1
    bit_ind -= 1
    if bit_ind % sep == 0
        char_ind > 0 || return (char_ind, bit_ind)
        @inbounds a[indf(char_ind)] = ' '
        char_ind -= 1
    end
    return (char_ind, bit_ind)
end

"""
    bstring(x::T; len::Union{Int, Nothing}, rev::Bool=true, sep::Int=0, pad::Bool=false)

Return a string giving the literal bit representation of `x` of a primitive type.

If `rev` is `true` (the default), then the string is reversed with respect to `Base.bitstring`.

If `len` is an `Int`, then the string will have length `len`.  If `len` is
greater than the bit-width of `x` then the string will be padded with `'0'`. If `len`
is smaller than the bit-width of `x`, then upper bits will be truncated.

If `pad` is `true`, then the behavior of `len` is modified: At least all bits other than
leading zeros are written.  By setting `len` large enough you can write leading zeros as
well.

If `sep` is greater than zero, a space is inserted every `sep` bits.

# Examples
All bits are written by default. Most significant bit is leftmost.
```jldoctest
julia> bstring(UInt16(11))
"1101000000000000"
```

If `rev` is `false`, the most significant bit is rightmost.
```jldoctest
julia> bstring(UInt16(11); rev=false)
"0000000000001011"
```

If `pad` is `true`, leading zeros are omitted.
```jldoctest
julia> bstring(UInt16(11); pad=true)
"1101"

julia> bstring(UInt16(11); pad=true, rev=false)
"1011"
```

`len` is the number of bits to write.
```jldoctest
julia> bstring(UInt16(11); len=8)
"11010000"

julia> bstring(UInt16(11); len=8, rev=false)
"00001011"

julia> bstring(UInt16(11); len=3)
"110"
```

If `len` is passed and `pad` is `true`, the most significant bit will always be written.
```jldoctest
julia> bstring(UInt16(11); len=3, pad=true)
"1101"

julia> bstring(UInt16(11); len=8, pad=true)
"11010000"
```

A space is inserted every `sep` bits.
```jldoctest
julia> x = 0xf0ff00aa00; bstring(x; sep=8)
"00000000 01010101 00000000 11111111 00001111 00000000 00000000 00000000"
```
"""
function bstring(x::T; len::Union{Int, Nothing}=nothing, pad::Bool=false, rev::Bool=true,
                 sep::Int=0) where {T}
    sep >= 0 || throw(ArgumentError("sep must be non-negative"))
    isnothing(len) || len >= 0 || throw(ArgumentError("len must be non-negative"))
    isprimitivetype(T) || throw(ArgumentError(LazyString(T, " not a primitive type")))
    if isnothing(len)
        len = pad ? 1 : bitsizeof(T)
    end
    _bin(x, len, pad, sep, rev)
end

end # module BStrings
