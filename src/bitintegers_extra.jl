###
### BitInteger support
###

using ..BitIntegers: UInt256, UInt512, UInt1024

const _UINT_TYPES = Dict{Int, DataType}()

for n in (8, 16, 32, 64, 128, 256, 512, 1024)
    _UINT_TYPES[n] = eval(Symbol(:UInt, n))
end

@inline get_uint_type_bits(n_bits::Integer) = get(_UINT_TYPES, n_bits, nothing)
@inline get_uint_type_bytes(n_bytes::Integer) = get(_UINT_TYPES, 8 * n_bytes, nothing)
@inline set_uint_type_bits!(::Type{UT}, n_bits::Integer) where UT = (_UINT_TYPES[n_bits] = UT)

"""
    min_uint_type(nbits::Integer)

Return the smallest unsigned integer type large enough to store `nbits` bits.
The number of bits in the type returned is a multiple of 8.
"""
@inline min_uint_type(n_bits::Integer) = uint_type(min_uint_bit_width(n_bits))

"""
    min_uint_byte_width(n_bits::Integer)

Return the minimum width in bytes of the unsigned integer needed to
represent `nbits` bits. The widths are restricted to multiples of eight.
"""
@inline function min_uint_byte_width(n_bits::Integer)
    n_bits >= 0 || throw(DomainError(n_bits, "Must be non-negative."))
    n_bits == 0 && return 1
    (q, r) = divrem(n_bits, 8)
    if iszero(r)
        return q
    else
        return (q + 1)
    end
end

"""
    min_uint_bit_width(nbits::Integer)

Return the minimum width in bits of the unsigned integer needed to
represent `nbits` bits. The widths are restricted to multiples of eight.
"""
@inline min_uint_bit_width(nbits::Integer) = 8 * min_uint_byte_width(nbits)

"""
    uint_type(n_bits::Integer)

Return an `n_bits`-bit unsigned integers type `UIntn_bits`.
`n_bits` must be a positive mulitple of `8`.

If `UIntn` does not exist, construct `UIntn` and `Intn`.
"""
@inline function uint_type(n_bits::Integer)
    _type = get_uint_type_bits(n_bits)
    if !isnothing(_type)
        return _type
    end
    n_bits >= 0 || throw(DomainError(n_bits, "Must be non-negative."))
    n_bits % 8 == 0 || throw(DomainError(n, "Must be a multiple of 8."))
    uint_sym = Symbol(:UInt, n_bits)
    eval(Meta.parse("BitIntegers.@define_integers $n_bits"))
    _uint_type::DataType = eval(uint_sym)
    set_uint_type_bits!(_uint_type, n_bits)
    return _uint_type
end

"""
    uint_type_bytes(n_bytes::Integer)

Return an `n_bytes`-byte unsigned integer type.

The returned type is named `UIntX` where `X = 8 * n_bytes`.
If `UIntX` does not exist construct `UIntX` and `IntX`.
"""
@inline uint_type_bytes(n_bytes::Integer) = uint_type(8 * n_bytes)

const _max_uint_in_bytes = 128
const _max_masks_in_bytes = 85
# const _UINT_TYPES_TUP = Tuple(uint_type.([8 * i for i in 1:_max_uint_in_bytes]))
const _UINT_TYPES_TUP = uint_type.([8 * i for i in 1:_max_uint_in_bytes])

const _UINT_ONE_BIT_MASKS =
    let
        arr = Any[]
        max_masks_in_bytes = 85
        for i in 1:_max_masks_in_bytes
            T = _UINT_TYPES_TUP[i]
#            tup = Tuple(T(2)^i for i in 0:(8*i - 1))
            tup = [T(2)^i for i in 0:(8*i - 1)]
            push!(arr, tup)
        end
#        [arr...]
        arr # probably best to leave this as Any
#        Tuple(arr)
    end

@inline function get_uint_one_bit_masks_bytes(n_bytes::Integer)
    @inbounds _UINT_ONE_BIT_MASKS[n_bytes]
end

@inline function get_uint_one_bit_masks_bits(n_bits::Integer)
    @inbounds _UINT_ONE_BIT_MASKS[div(n_bits, 8)]
end

for n_bytes in 1:_max_uint_in_bytes
    n_bits = 8 * n_bytes
    _type = Symbol(:UInt, n_bits)
    @eval byte_width(::Type{$_type}) = $n_bytes
    @eval bit_width(::Type{$_type}) = $n_bits
    if n_bytes <= _max_masks_in_bytes
        _masks = get_uint_one_bit_masks_bytes(n_bytes)
        @eval get_one_bit_masks(::Type{$_type}) = $_masks
    end
end

"""
    get_one_bit_masks(::Type{uint_type})

Return an array of all single bit (powers of two) masks for `uint_type`.

Masks are generated when `BitsX` is compiled. If masks are not available
for a type, then the sentinel `nothing` is returned. This signifies that
a routine that does not require masks should be used.
"""
get_one_bit_masks(_) = nothing

@inline function get_uint_one_bit_masks_type(::Type{UIntT}) where {UIntT}
    return get_uint_one_bit_masks_bytes(byte_width(UIntT))
end

###
### There is a performance bug in BitIntegers.jl I copied a bit of the code
### and added a fix.


# import BitIntegers: XBI, UBI, UBU, XBS, XBU, BBS, BBI, BBU, SHIFT_SPLIT_NBITS


# @inline @generated function BitIntegers.shift_call(sh_fun::Function, x::I, y::UBU) where {I<:XBI}
#     nbits = 8 * sizeof(I)
#     # performance issue does not affect integers with no more than 128 bits
#     # Looks like it *does* affect (GJL 2022)
# #    nbits <= 128 && return :($(:sh_fun)(x, y))
#     split = SHIFT_SPLIT_NBITS
#     mask = split - 1
#     quote
#         hi =  Base.:(>>)(y, $(trailing_zeros(split)))
#         $([:(hi == $val && return sh_fun(sh_fun(x, $(val * split)), y & $mask))
#             for val in 0:div(nbits, split)]...
#         )
#         (sh_fun == ashr_int && x < 0) ? -oneunit($I) : zero($I)
#     end
# end

# These functions are *not* imported from Base

# >>( x::XBS, y::UBU) = shift_call(ashr_int, x, y)
# >>( x::XBU, y::UBU) = shift_call(lshr_int, x, y)
# >>>(x::XBI, y::UBU) = shift_call(lshr_int, x, y)
# <<( x::XBI, y::UBU) = shift_call(shl_int, x, y)

# >>( x::BBS, y::XBU) = ashr_int(x, y)
# >>( x::BBU, y::XBU) = lshr_int(x, y)
# >>>(x::BBI, y::XBU) = lshr_int(x, y)
# <<( x::BBI, y::XBU) = shl_int(x, y)

# @inline >>( x::UBI, y::Int) = 0 <= y ? x >> unsigned(y) :  x << unsigned(-y)
# @inline <<( x::UBI, y::Int) = 0 <= y ? x << unsigned(y) :  x >> unsigned(-y)
# @inline >>>(x::UBI, y::Int) = 0 <= y ? x >>> unsigned(y) : x << unsigned(-y)