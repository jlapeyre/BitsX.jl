###
### bitsizeof
###

const _ZERO_CHAR_CODE = UInt8('0')
const _ONE_CHAR_CODE = UInt8('1')

# bitsizeof should give how many "addressable" bits are in the object
# This should be in runtest
"""
    bitsizeof(T::Type)

Return the number of indexable bits in an instance of type `T`.
Here "indexable" means via `bit(x::T, i)`.

If the number of indexable bits in an instance of type `T` cannot be computed
from the type alone, then an error is thrown.
"""
bitsizeof(T::Type) = _bitsizeof(Val(isbitstype(T)), T)
const _BITS_PER_BYTE = 8
_bitsizeof(isbits::Val{true}, T::Type) = sizeof(T) * _BITS_PER_BYTE
_bitsizeof(isbits::Val{false}, T::Type) = throw(MethodError(bitsizeof, (T,)))
bitsizeof(::Type{Bool}) = 1

const MPFR_EXP_BITSIZE = bitsizeof(Clong) # bytes_to_bits(sizeof(Clong))

"""
bitlength(x)

Return the number of bits in `x`. This is the number of
bits that can be indexed via `bits`. If `x` is an `isbitstype`
type, then this is the same as `bitsizeof(typeof(x))`.

In contrast, if `x` is of type `BigInt` or `BigFloat` the number
of bits is not encoded in the type, and in fact may vary from instance
to instance.
"""
bitlength(x::T) where T = bitsizeof(T)
# bitlength(x::T) where T = _bitlength(Val(isbitstype(T)), T)
# _bitlength(isbits::Val{true}, T::Type) = bitsizeof(T)
# _bitlength(isbits::Val{false}, T::Type) = throw(MethodError(bitlength, (T,)))
bitlength(x::BigFloat) =  1 + MPFR_EXP_BITSIZE + precision(x)
bitlength(x::BigInt) = abs(x.size) * Base.GMP.BITS_PER_LIMB # bitsizeof(Base.GMP.Limb)
bitlength(x::AbstractVector{Bool}) = length(x)

"""
    rightmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the right (lower)
are one, and all bits to the left of the `i`th bit (higher) are zero.

See `leftmask`, `rangemask`, `mask`.
# Examples
```julia-repl
julia> bit_string(rightmask(UInt8, 3))
"00000111"
```
"""
rightmask(i) = rightmask(Word, i)
rightmask(::Type{T}, i::Integer) where T = one(T) << i - one(T)

abstract type IndexBase end
struct OneBased <: IndexBase end
struct ZeroBased <: IndexBase  end

rightmask(::IndexBase, args...) = rightmask(args...)
rightmask(::ZeroBased, ::Type{T}, i::Integer) where T = rightmask(T, i + 1)

"""
    leftmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the left (higher)
are one, and all bits to the right of the `i`th bit (lower) are zero.

See `rightmask`, `rangemask`, `mask`.
# Examples
```julia-repl
julia> bit_string(leftmask(UInt8, 3))
"11111100"
```
"""
leftmask(i) = leftmask(Word, i)
leftmask(::Type{T}, i::Integer) where T = ~(one(T) << (i-1) - one(T))

leftmask(::IndexBase, args...) = leftmask(args...)
leftmask(::ZeroBased, ::Type{T}, i::Integer) where T = leftmask(T, i + 1)

"""
    rangemask([T=UInt], ilo, ihi)
    rangemask([T=UInt], (ilo, ihi)...)

Return `n::T` such that all bits in the range `ilo` to `ihi`, inclusive, are
one, and all other bits are zero. If `Tuples` `(ilo, ihi)...` are given,
then set bits in each range to one.

See `leftmask`, `rightmask`, `mask`.

# Examples
```julia-repl
julia> bit_string(rangemask(UInt8, 2, 7))
"01111110"

julia> bit_string(rangemask(UInt16, (1, 3), (5, 8), (14, 16)))
"1110000011110111"

julia> bit_string(rangemask(UInt8, (1, 5), (4, 8)))
"11111111"
```
"""
rangemask(args...) = rangemask(OneBased(), Word, args...)
rangemask(ib::IndexBase, ::Type{T}, ilo::Integer, ihi::Integer) where T = leftmask(ib, T, ilo) & rightmask(ib, T, ihi)
rangemask(ib::IndexBase, ::Type{T}, ranges::NTuple{2}...) where T = mapfoldl(x->rangemask(ib, T, x...), |, ranges)
rangemask(::Type{T}, args...) where T = rangemask(OneBased(), T, args...)

"""
    mask([T=UInt], i::Integer)
    mask([T=UInt], r::UnitRange)
    mask([T=UInt], itr)

Return `n::T` with the specified bits set to one and the rest zero.
The bits specified by each item in `itr` will be set to one.
Overlaps between ranges will have their bits set to one.

See `leftmask`, `rightmask`, `rangemask`.
# Examples
```julia-repl
julia> bit_string(mask(UInt8, 3))
"00000100"

julia> bit_string(mask(UInt8, (1, 5, 8)))
"10010001"

julia> bit_string(mask(UInt8, (2, 5, 8)))
"10010010"

julia> bit_string(mask(1:2:64))
"0101010101010101010101010101010101010101010101010101010101010101"

julia> bit_string(mask(UInt16, (1:3, 9, 14:16)))
"1110000100000111"
```
"""
mask(ib::IndexBase, ::Type{T}, ur::UnitRange) where T = rangemask(ib, T, ur.start, ur.stop)
#mask(::Type{T}, ur::UnitRange) where T = rangemask(T, ur.start, ur.stop)
# Following method has typical problem. For literal or::OR, it happens at compile time.
# For or::OR in variable it is 10x slower than the fallback below with `inds`.
# But, the fallback is slower for literal range, does not happen at compile time.
# mask(::Type{T}, or::OrdinalRange) where T = mask(T, collect(or))
mask(::Type{T}, i::Integer) where T = (one(T) << (i-one(T)))
mask(ib::IndexBase, ::Type{T}, inds) where T = mapfoldl(i->mask(ib, T, i), |, inds)
mask(::Type{T}, r::Base.OneTo) where T = rightmask(T, length(r))
mask(arg) = mask(OneBased(), Word, arg)
mask(::Type{T}, args...) where T = mask(OneBased(), T, args...)

# Modified from Bits.jl
asint(x::Integer) = x
asint(x::AbstractFloat) = reinterpret(Signed, x) # Signed gets all the types we need

# Taken from Bits.jl
"""
    masked(x, [j::Integer], i::Integer) -> typeof(x)

TODO: old docs
Return the result of applying the mask `mask(x, [j], i)` to `x`, i.e.
`x & mask(x, [j], i)`.
If `x` is a float, apply the mask to the underlying bits.

# Examples
```jldoctest
julia> masked(0b11110011, 1, 5) === 0b00010010
true

julia> x = rand(); masked(-x, 0, 63) === x
true
```
"""
masked(ib::IndexBase, x, args...) = x & mask(ib, typeof(x), args...)
masked(T::DataType, args...) = throw(MethodError(masked, (T, args...)))
masked(ib::IndexBase, x::AbstractFloat, args...) = reinterpret(typeof(x), masked(ib, asint(x), args...))
masked(args...) = masked(OneBased(), args...)

"""
    bit(x::Real, i::Integer)

Similar to `Bits.bit` from registered `Bits.jl` package. A difference is that
the return type here does not depend on the input type, but rather is always `Int`.
"""
bit(x::Integer, i::Integer) = (x >>> UInt(i-1)) & 1
bit(x::AbstractFloat, i::Integer) = bit(asint(x), i)
bit(x::Union{BigInt, BigFloat}, i::Integer) = Int(tstbit(x, i))
bit(x::AbstractVector{Bool}, i::Integer) = x[i] # % Int
bit(::ZeroBased, x, i::Integer) = bit(x, i + 1)
bit(::OneBased, args...) = bit(args...)

# TODO: allow elide bounds checking
# Assume the string is one code unit per code point: '1' or '0'
"""
    bit(str::AbstractString, i::Integer)::Int

Return `1` if the `i`th character of `str` is `'1'` and
`0` if it is `'0'`. Otherwise throw an `ArgumentError`.

It is assumed that `str` has one byte per character, or more precisely,
one `UInt8` code unit per code point. Thus, accessing the `i`th character
via `bit` may be more efficient than `str[i]`.
"""
function bit(str::AbstractString, i::Integer)
    byte = codeunit(str, i)
    byte == _ZERO_CHAR_CODE && return 0
    byte == _ONE_CHAR_CODE && return 1
    throw(ArgumentError("Invalid character code $byte for bit"))
end

function bit(v, i::Integer)
    b = v[i] # propagate inbounds ?
    b == one(b) && return 1
    b == zero(b) && return 0
    error("Unrecognized bit $b")
end

"""
    tstbit(x::Real, i::Integer) -> Bool

Similar to [`bit`](@ref) but returns the bit at position `i` as a `Bool`.

# Examples
```jldoctest
julia> tstbit(0b101, 3)
true
```
"""
tstbit(x, i::Integer) = bit(x, i) % Bool
tstbit(x::BigInt, i::Integer) = Base.GMP.MPZ.tstbit(x, i-1)

tstbit(::ZeroBased, x, i::Integer) = tstbit(x, i + 1)
tstbit(::OneBased, args...) = tstbit(args...)

# Maybe from Random module
function tstbit(x::BigFloat, i::Integer)
    prec = precision(x)
    if i > prec
        i -= prec
        if i > MPFR_EXP_BITSIZE
            (i == MPFR_EXP_BITSIZE + 1) ? (x.sign == -1) : false
        else
            tstbit(x.exp, i)
        end
    else
        nlimbs = (prec-1) ÷ Base.GMP.BITS_PER_LIMB + 1
        unsafe_tstbit(x.d, i + nlimbs * Base.GMP.BITS_PER_LIMB - prec)
    end
end

"""
    unsafe_tstbit(p::Ptr{T}, i::Integer)::Bool where {T}

Return the value of `i`th bit of `p`. This is dangerous,
there is no bounds check.
"""
unsafe_tstbit(p::Ptr{T}, i::Integer) where {T} =
    tstbit(unsafe_load(p, 1 + (i-1) ÷ bitsizeof(T)),
           mod1(i, bitsizeof(T)))

#_Bits.bits(x::T, n) where {T <: Real} = StaticBitVector(x & rightmask(T, n), n)
# # TODO: Why do I do the following method? Why not return x instead of x.x?
# _Bits.bits(x::StaticBitVector) = _Bits.bits(x.x)
# _Bits.bits(x::StaticBitVectorView) = x
# _Bits.bits(x::StaticBitVector, n) = _Bits.bits(x.x, n)
# _Bits.bits(x::StaticBitVectorView, n) = bits(x.x, n)
# min_bits(x::_Bits.AbstractBitVector1) =  min_bits(x.x)


"""
    normalize_bitstring(str::AbstractString)

Remove all characters (code points) from `str` that are not one
of `'0'` and `'1'`, if such characters exist. Otherwise, return `str`
unchanged.
"""
normalize_bitstring(str::AbstractString) = is_bitstring(str) ? str : replace(str, r"[^01]" => "")

# TODO: loosen this? Keep only '1' and '0' ?
# function _strip_bin_format(s::AbstractString)
#     ! any(x -> occursin(x, s), ('<', '>', ' ')) && return s
#     return replace(s, r"[<>\s]" => "")
# end

const _VEC_LIKE = Union{AbstractVector{<:Integer}, NTuple{<:Any, <:Integer}, Base.Generator{<:AbstractVector}}

# Not supplying `IntT` is much slower (+ 100ns) than it should be. It is much slower than the time
# required to lookup the correct `IntT`. Probably some kind of type instability
"""
    bits([IntT], dts::Union{AbstractVector{<:Integer},  NTuple{<:Any, <:Integer}}, n=length(dts))

Convert the container of binary digits `dts` to a `BitsVector1Mask`.

`IntT` is the storage type, i.e., the type that is wrapped. Input is not validated for
correctness, nor for having length greater than the number of bits in `IntT`. If
`IntT` is omitted, the smallest type capable of representing `dts` is used.
However, supplying `IntT` may result in faster conversion.

# Examples
```julia-repl
julia> bits((0,1,0,1))
<0101>
```
"""
bits(_digits::_VEC_LIKE, n=length(_digits)) = bits(min_uint_type(n)::DataType, _digits, n)
bits(::Type{IntT}, _digits::_VEC_LIKE, n=length(_digits)) where IntT =
    bits(undigits(IntT, _digits; base=2), n)

# This is too slow: min_uint_type(n)::DataType

"""
    undigits([IntT=Int], A; base=10)

The inverse of `digits`. That is `undigits(digits(n; base); base) == n`.
The number returned is of type `IntT` provided that the type is stable
under `+` and `*`, and that each element of `A` can be converted to `IntT`.

# Exceptions
* `OverflowError` if `A` represents a number larger than `typemax(IntT)`. For
   `Unsigned` types the check is more strict in that `length(A)` must not be
    greater than the number of bits in `IntT`, even if there are leading zeros.
"""
undigits(A; base=10) = undigits(Int, A, base=base)
function undigits(::Type{IntT}, A; base=10) where IntT
    base == 2 && IntT <: Unsigned && return _undigits_base_2(IntT, A)
    n = zero(IntT)
    @inbounds for i in reverse(eachindex(A))
        n = Base.checked_add(Base.checked_mul(base, n), IntT(A[i]))
    end
    return n
end

# This is only an optimization. Tests are highly variable for unknown reasons.
# So, I am not sure how much is saved here.
# This will fail for BigInt
function _undigits_base_2(::Type{IntT}, A) where IntT <: Unsigned
    bitsizeof(IntT) < length(A) && throw(OverflowError("Output data type is too small for input data"))
    n = zero(IntT)
    @inbounds for i = reverse(eachindex(A))
        n = IntT(2) * n + IntT(A[i])
    end
    return n
end

# This works, but is slower on some tests
#   bits(mapfoldl(i -> bit(b, i) << (n - i), |, 1:n), length(b))
# function Base.reverse(b::BitVector1Mask{T}) where T
#     n = length(b)
#     bits(mapfoldl(i -> bit(b, i) << (n - i), |, 1:n), n)
# end

# Return Tuple instead
# tupfindall(itr) = (n=count(itr); niter = (i for i in 1:n if itr[i]); NTuple{n, Int}((niter...,)))
# tupfindall(itr) = ((n, iter) = ntupfindall(itr); NTuple{n,Int}((iter...,)))
# ntupfindall(itr) = (n=count(itr); niter = (i for i in 1:n if itr[i]); (n, niter))
# tupfindall(f, itr) = ((i for (i, x) in enumerate(itr) if f(x))...,)
# findones(itr) = tupfindall(isone, itr)
# findzeros(itr) = tupfindall(iszero, itr)

###
### More bit functions
###

## Add more methods for Base.uinttype.
## Methods are defined in base for floating point types,
## and in the package BitIntegers.jl
let
    tups = [(Symbol(:Int, n), (Symbol(:UInt, n), Symbol(:Int, n), Symbol(:Float, n))) for n in (16, 32, 64)]
    for (t, ts) in ((:Int8, (:UInt8, :Int8, :Bool)), tups..., (:Int128, (:UInt128, :Int128)))
        for tp in ts
            @eval inttype(::Type{$tp}) = $t
            @eval uinttype(::Type{$tp}) = $(Symbol("U", t))
        end
    end
end

"""
    bit_string(n::Integer; pad=nothing)
    bit_string(v; pad=0)

Give the literal bitstring representation of the number `n`. If `n` is a primitive type,
`bit_string` is the same as `Base.bitstring`, except that the former allows specifying
left padding.

For `v` an iterable of known length, elements `x` in `v` are converted
characters `'0'` and `'1'` using `zero(x)` and `one(x)`.

`pad=0` ommits leading zeros in the output string

See also, `bit_vector`, `bool_vector`, and `bool_tuple`.

# Examples:

```julia-repl
julia> bit_string(128)
"0000000000000000000000000000000000000000000000000000000010000000"

julia> bit_string([1, 0, 1])
"101"

julia> bit_string(128; pad = 0)
"10000000"

julia> bit_string(128; pad = 9)
"010000000"

julia> bit_string(bits("1001"))
"1001"
```
"""
function bit_string(x::T; pad::Union{Nothing,Integer}=nothing) where T <: Integer
    isnothing(pad) && return bitstring(x)
    return string(convert(uinttype(T), x); pad=pad, base=2)
end

bit_string(x::AbstractFloat, args...) = bit_string(asint(x), args...)

# v -- an iterable of objects representing bit values as ones and zeros
function bit_string(v; pad::Union{Nothing,Integer}=0)
    n_buf = max(pad, length(v))
    return bit_string!(Vector{UInt8}(undef, n_buf), v)
end

"""
    bit_string!(buf::Vector{UInt8}, v)::String

Convert the iterable `v` of bit values into a bit string of length `length(buf)`
with characters `0` and `1`.
If `length(buf)` is greater than `length(v)`, then the excess characters are set
to `0`.

`v` must have a length in the sense that the call `length(v)` must succeed.
"""
function bit_string!(buf::Vector{UInt8}, v) #; pad::Union{Nothing,Integer}=0)
    n_pad = length(buf) - length(v)
    n_pad < 0 && throw(BoundsError(buf, length(v)))
    @inbounds for i in 1:n_pad
        buf[i] = _ZERO_CHAR_CODE
    end
    j::Int = n_pad + 1
    @inbounds for i in eachindex(v)
        x = v[i]
        buf[j] = iszero(x) ? _ZERO_CHAR_CODE : isone(x) ? _ONE_CHAR_CODE :
            throw(DomainError(x, "Must be 0 or 1."))
        j += 1
    end
    return String(take!(IOBuffer(buf)))
end


"""
    min_bits(n::Integer)
    min_bits(bit_str::AbstractString)
    min_bits(v)

Return the required number of bits in the binary representation
of `n` (or `bit_str`, or iterable `v`).

The returned value is the position of the leftmost bit equal to `1`, counting from the right.
Equivalently, the value is the length of the `bit_str` discounting leading zeros.

# Example
```julia-repl
julia> min_bits(2^10)
11

julia> min_bits("1"^10)
10

julia> min_bits([0,1,0])
2
```
"""
min_bits(n::T) where {T<:Integer} = 8 * sizeof(T) - Base.leading_zeros(n)
min_bits(v) = min_dits(v)

_zero(s::AbstractChar) = '0'
_zero(x) = zero(x)

"""
    min_dits(v)

Return the minimum number of "dits" needed to express `v`.

The first element not representing zero counting from the left determines the
return value. Input is not validated.

# Example
```julia-repl
julia> min_dits("03Q")
2

julia> min_dits([0, 3 , 17])
2
```
"""
function min_dits(v)
    n = Base.length(v)
    i = 0
    for c in v
        c !== _zero(c) && return (n - i)
        i += 1
    end
    return 0
end

min_dits(v::Integer) = throw(ErrorException("min_dits: use min_bits for $(typeof(v)) input $v"))

"""
    is_bitstring(bit_str::AbstractString; throw=false)

Return `true` if all characters in `bit_str` are either `'0'` or `'1'`,
otherwise `false`.
If `throw` is `true`, then throw an error rather than returning `false`.
"""
function is_bitstring(bit_str::AbstractString; throw=false)
    for c in codeunits(bit_str)
        if c != _ONE_CHAR_CODE && c != _ZERO_CHAR_CODE
            throw && Base.throw(
                DomainError(c, "'$c' is not a bit. Characters must be one of ('0', '1')."))
            return false
        end
    end
    return true
end

"""
    bool_tuple([IntT=Bool], bit_str::AbstractString; check=true)

Parse `bit_str` to a `Tuple` of `Bool`s.

If `IntT` is supplied then return a `Tuple` of `one(IntT)` and `zero(IntT)`.
`bit_str` is first validated if `check` is `true`.

# Examples

```julia-repl
julia> bool_tuple("10010")
(true, false, false, true, false)

julia> bool_tuple(Int, "10010")
(1, 0, 0, 1, 0)
```
"""
bool_tuple(bit_str::AbstractString; check=true) =
    bool_tuple(Bool, bit_str::AbstractString; check=check)

@inline function bool_tuple(::Type{IntT}, bit_str::AbstractString; check=true) where IntT
    check && is_bitstring(bit_str; throw=true)
    return Tuple(c == _ONE_CHAR_CODE ? one(IntT) : zero(IntT) for c in codeunits(bit_str))
end

"""
    bit_vector(bit_str::AbstractString; check=true)

Parse `bit_str` to a `BitVector`. If `check` is `true` then `bit_str` is first validated.
"""
bit_vector(bit_str::AbstractString; check=true) = BitVector(bool_vector(bit_str; check=check))

# much slower to use generator, although memory usage may be smaller
function _bit_vector_old(bit_str::AbstractString; check=true)
    check && is_bitstring(bit_str; throw=true)
    return BitVector(c == _ONE_CHAR_CODE ? true : false for c in codeunits(bit_str))
end

"""
    bool_vector([IntT=Bool], bit_str::AbstractString; check=true)

Parse `bit_str` to a `Vector{Bool}`.

Return instead a `Vector{IntT}` if `IntT` is passed.
If `check` is `true` then `bit_str` is first validated.
"""
bool_vector(bit_str::AbstractString; check=true) =
    bool_vector(Bool, bit_str::AbstractString; check=check)

@inline function bool_vector(::Type{IntT}, bit_str::AbstractString; check=true) where IntT
    check && is_bitstring(bit_str; throw=true)
    return [c == _ONE_CHAR_CODE ? one(IntT) : zero(IntT) for c in codeunits(bit_str)]
end

# Copied doc from Bits.jl
"""
    bits(x::Real)

Create an immutable view on the bits of `x` as a vector of `Bool`, similar to a `BitVector`.
If `x` is a `BigInt`, the vector has length [`Bits.INF`](@ref).
Currently, no bounds check is performed when indexing into the vector.

# Examples
```jldoctest
julia> v = bits(Int16(2^8+2^4+2+1))
<00000001 00010011>

julia> permutedims([v[i] for i in 8:-1:1])
1×8 Array{Bool,2}:
 false  false  false  true  false  false  true  true

julia> bits(true)
<1>

julia> bits(big(2)^63)
<...0 10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000>

julia> bits(Float32(-7))
<1|10000001|1100000 00000000 00000000>

julia> ans[1:23] # creates a vector of bits with a specific length
<1100000 00000000 00000000>
```
"""
bits(x::Real) = StaticBitVectorView(x)

"""
    bits(x::T, n) where {T<:Real}

Create a view of the bits of `x` that is truncated to the first `n` bits.

The underlying data is of type `T`. The truncated bits are zeroed.
Indexing at positions larger than `n` will not error, but will return zero.

# Examples
```julia-repl
julia> bits(0x5555)
<01010101 01010101>

julia> bits(0x5555, 10)
<01 01010101>

julia> bits(bits(0x5555, 10))
<00000001 01010101>
```
"""
bits(x::Real, n) = StaticBitVector(x, n)

bits(::IndexBase, x::Real, n) = bits(x, n)
bits(::ZeroBased, x::Real, n) = StaticBitVector0(x, n)

# ** StaticBitVector

# similar to a BitVector, but with only 1 word to store bits (instead of 1 array thereof)
abstract type AbstractStaticBitVector{T<:Real} <: AbstractVector{Bool} end

struct StaticBitVectorView{T} <: AbstractStaticBitVector{T}
    x::T
end

Base.convert(::Type{StaticBitVectorView{T}}, x) where T = StaticBitVectorView(T(x))

Base.zero(::Type{V}) where V <: AbstractStaticBitVector = convert(V, 0)
Base.zero(::V) where V <: AbstractStaticBitVector = convert(V, 0)
# what is `one` worth here?
Base.one(::Type{V}) where V <: AbstractStaticBitVector = convert(V, 1)
Base.one(::V) where V <: AbstractStaticBitVector = convert(V, 1)

Base.count_ones(x::AbstractStaticBitVector) = Base.count_ones(x.x)

abstract type AbstractStaticBitVectorLen{T} <: AbstractStaticBitVector{T} end

struct StaticBitVector{T<:Real} <: AbstractStaticBitVectorLen{T}
    x::T
    len::Int
    function StaticBitVector{T}(x::T, n::Integer) where {T<:Real}
        return new(x & rightmask(T, n), n)
    end
end

StaticBitVector{T}(x::Real, n::Integer) where {T<:Real} = StaticBitVector{T}(T(x), n)


struct StaticBitVector0{T<:Real} <: AbstractStaticBitVectorLen{T}
    x::T
    len::Int
    function StaticBitVector0{T}(x::T, n::Integer) where {T<:Real}
        return new(x & rightmask(T, n), n)
    end
end


StaticBitVector(x::T, n::Integer) where T = StaticBitVector{typeof(x)}(x, n)
StaticBitVector0(x::T, n::Integer) where T = StaticBitVector0{typeof(x)}(x, n)

index_base(::Any) = OneBased()
index_base(::Type{<:StaticBitVector0}) = ZeroBased()

bitlength(x::AbstractStaticBitVector{T}) where T = bitsizeof(T)
bitlength(x::AbstractStaticBitVectorLen) = x.len
bitsizeof(::Type{<:AbstractStaticBitVector{T}}) where T  = bitsizeof(T)

# TODO: could define ZeroTo. This is done ad hoc around Julia ecosystem
Base.axes1(v::StaticBitVector0) = 0:(bitlength(v) - 1)

Base.size(v::AbstractStaticBitVector) = (bitlength(v),)

# Assume 1 "based"
# TODO!: do bounds checking
function Base.getindex(ib::IndexBase, v::AbstractStaticBitVector, i::Integer)
    return tstbit(ib, v.x, i)
end

Base.getindex(v::AbstractStaticBitVector, i::Integer) = getindex(index_base(typeof(v)), v, i)

function Base.getindex(v::AbstractStaticBitVector, a::AbstractVector{<:Integer})
    xx, _ = foldl(a, init=(zero(asint(v.x)), 0)) do xs, i
        x, s = xs
        (x | getindex(v, i) << s, s+1) # better
    end
    v isa AbstractStaticBitVectorLen && return typeof(v)(xx, length(a))
    return StaticBitVector(xx, length(a))
end

function Base.getindex(v::AbstractStaticBitVector, a::AbstractUnitRange{<:Integer})
    i0, i1 = extrema(a)
    x = masked(asint(v.x), i0:i1) >> (i0-1)
    v isa AbstractStaticBitVectorLen && return typeof(v)(x, length(a))
    return StaticBitVector(x, length(a))
end

Base.getindex(::ZeroBased, v::AbstractStaticBitVector, a::AbstractUnitRange{<:Integer}) =
    getindex(v, a .+ 1)

### StaticBitVectorN

struct StaticBitVectorN{T<:Real, N} <: AbstractStaticBitVectorLen{T}
    x::T
    function StaticBitVectorN{T, N}(x::T) where {T<:Real, N}
        return new{T, N}(x & rightmask(T, N))
    end
    function StaticBitVectorN{T}(x::T, n::Integer) where {T<:Real}
        return new{T, N}(x & rightmask(T, n))
    end
end

StaticBitVectorN(x::T, ::Val{N}) where {T, N} = StaticBitVectorN{T, N}(x)
bitlength(::StaticBitVectorN{<:Any, N}) where N = N

# WARNING: These definitions allow, for example efficient inserting an Int into a Dictionary
# with StaticBitVectorN keys. But, it may have unintended consequences for hashing
# Advantage is this is more general than writing methods for Dicitonary and we
# don't have to depend on Dictionaries
Base.isequal(v::StaticBitVectorN, x) = isequal(x, v.x)
Base.isequal(v1::StaticBitVectorN, v2::StaticBitVectorN) = isequal(v1.x, v2.x)

Base.convert(::Type{StaticBitVectorN{T,N}}, x) where {T, N} = StaticBitVectorN{T,N}(T(x))
Base.convert(::Type{StaticBitVectorN{T,N}}, x::StaticBitVectorN{T,N}) where {T, N} = x

# Try to improve perf by making N inferrable
Base.setindex!(v::Array{<:StaticBitVectorN}, val::Union{Number,AbstractString}, inds::Int...) =
     _setindex!(v, val, inds...)

function _setindex!(
    v::Array{StaticBitVectorN{T,N}}, val::Union{Number,AbstractString}, inds...) where {T, N}
    return setindex!(v, StaticBitVectorN{T,N}(val), inds...)
end

const _int_types = ((Symbol(pref, :Int, n) for n in (8, 16, 32, 64, 128) for pref in ("", "U"))...,)
for T in (_int_types..., :BigInt)
    @eval (Base.$T)(x::AbstractStaticBitVector) = ($T)(x.x)
end

Base.Integer(x::AbstractStaticBitVector) = x.x

for op in (:xor, :(&), :(|), :(+), :(-), :(*)) # it is actually useful sometimes to do +,-
    @eval (Base.$op)(x::AbstractStaticBitVectorLen{T}, y::Real) where T = bits(($op)(x.x, T(y)), bitlength(x))
    @eval (Base.$op)(x::StaticBitVectorView{T}, y::Real) where T = bits(($op)(x.x, T(y)))
    @eval (Base.$op)(y::Real, x::StaticBitVectorView{T}) where T = bits(($op)(x, y))
    @eval (Base.$op)(y::Real, x::AbstractStaticBitVectorLen{T}) where T = bits(($op)(x, y))
    @eval (Base.$op)(y::AbstractStaticBitVectorLen, x::AbstractStaticBitVectorLen) =
        (length(x) == length(y) || throw(DimensionMismatch()); bits(($op)(x.x, y.x), length(x)))
    # @eval (Base.$op)(y::AbstractStaticBitVectorLen, x::AbstractStaticBitVectorLen) =
    #     ((length(x) == length(y) || throw(DimensionMismatch(string($op) * ": $(x.len) != $(y.len)"))) ; bits(($op)(x.x, y.x), length(x)))
end

# TODO: Is there an accessor method for .x ?
# We don't want lexicographic sorting. Also it uses slow fallback methods.
Base.:(<)(v1::AbstractStaticBitVector, v2::AbstractStaticBitVector) = v1.x < v2.x
Base.isless(v1::AbstractStaticBitVector, v2::AbstractStaticBitVector) = v1 < v2
# Base.:(<)(v1::_Bits.AbstractBitVector1, v2::_Bits.AbstractBitVector1) = v1.x < v2.x
# Base.isless(v1::_Bits.AbstractBitVector1, v2::_Bits.AbstractBitVector1) = v1 < v2

# Could maybe use args... above for these
for op in (:(~),)
    @eval (Base.$op)(x::StaticBitVector{T}) where T = bits(($op)(x.x), bitlength(x))
    @eval (Base.$op)(x::StaticBitVectorView{T}) where T = bits(($op)(x.x))
end

Base.zero(b::AbstractStaticBitVectorLen) = bits(zero(b.x), bitlength(b))
Base.one(b::AbstractStaticBitVectorLen) = bits(one(b.x), bitlength(b))

"""
    reverse(b::AbstractStaticBitVectorLen)

Return `b` with the bit order reversed.
"""
function Base.reverse(b::AbstractStaticBitVectorLen{T}) where T
    c = zero(T)
    for i in eachindex(b)
        c |= b[i] << (lastindex(b) - i)
    end
    return bits(c, length(b))
end

# ** show

sig_exp_bits(x) = Base.Math.significand_bits(typeof(x)), Base.Math.exponent_bits(typeof(x))
sig_exp_bits(x::BigFloat) = precision(x), MPFR_EXP_BITSIZE

showsep(io, v::AbstractStaticBitVector, i) = _showsep(io, v.x, i)
showsep(io, v::StaticBitVector0, i) = _showsep(io, v.x, i + 1)

_showsep(io, _, i) = (i % 8 == 0) && print(io, ' ')

function _showsep(io, x::AbstractFloat, i)
    sigbits, expbits = sig_exp_bits(x)
    if i == sigbits || i == sigbits + expbits
        print(io, '|')
    elseif i < sigbits && i % 8 == 0 || i > sigbits && (i-sigbits) % 8 == 0
        print(io, ' ')
    end
end

function Base.show(io::IO, v::AbstractStaticBitVector)
    if v.x isa BigInt && v isa StaticBitVector # TODO: not convinced about "infinite" num digits
        print(io, "<...", v.x < 0 ? "1 " : "0 ")
    else
        print(io, "<")
    end
    for i = reverse(eachindex(v))
        i != lastindex(v) && showsep(io, v, i)
        show(io, v[i] % Int)
    end
    print(io, ">")
end

Base.show(io::IO, ::MIME"text/plain", v::AbstractStaticBitVector) = show(io, v)

const BoolOrVal = Union{Bool, Val{true}, Val{false}}

"""
    bits([T], s::AbstractString)

Convert the string `s` representing a binary number to a `StaticBitVector`. This
method can be used to convert the string representation of
an `b::StaticBitVector` back to `b`. So, it behaves like `Base.parse(::Int, x)`
in that the bits in the string are in the same order as the bits in `b`.

If `T` is not specified, the smallest unsigned integer type capable of representing `s`
will be used.

Spaces, and the characters '>', '<' are stripped from `s` if `strip` is `true`.

# Examples
```julia-repl
julia> bits("00101")
<00101>

julia> bits("<01000000 11111111>")
<01000000 11111111>
```
"""
function bits(::Type{T}, ::Type{ST}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T <: Real, ST <: AbstractStaticBitVector}
    return _bits(s, _toVal(strip), T, ST)
end

# Not sure why we have to do this. Forces specialization
_toVal(x::Bool) = x ? Val(true) : Val(false)
_toVal(x::Val) = x

function bits(::Type{T}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T <: Real}
    return _bits(s, _toVal(strip), T, StaticBitVector)
end

# The method is never dispatched to. The one below is preferred. So we have to work around
# function bits(::Type{ST}, s::AbstractString; strip::Bool=false) where {ST <: AbstractStaticBitVector{T}} where T

# There must be a better solution!
_get_param(::Type{V}) where {V <: AbstractStaticBitVector} = isconcretetype(V) ? __get_param(V) : Nothing
__get_param(::Type{V}) where {V <: AbstractStaticBitVector{T}} where T = T

function bits(::Type{ST}, s::AbstractString; strip::BoolOrVal=Val(false)) where {ST <: AbstractStaticBitVector}
    return _bits(s, _toVal(strip), _get_param(ST), ST)
end

# ensure that T is not computed later
function bits(Ty::Type{StaticBitVectorN{T, N}}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T, N}
    return _bits(s, _toVal(strip), T, Ty)
end

bits(s::AbstractString; strip::BoolOrVal=Val(false)) =
    _bits(s, _toVal(strip), Nothing, StaticBitVector)

_bits(s::AbstractString, ::Val{true}, ::Type{T}, ::Type{ST}) where {T, ST} =
    _bits(normalize_bitstring(s), Val(false), T, ST)

_bits(s::AbstractString, ::Val{false}, ::Type{Nothing}, ::Type{ST}) where {ST} =
    _bits(s, Val(false), min_uint_type(ncodeunits(s)), ST)

function _bits(s::AbstractString, ::Val{false}, ::Type{T}, ::Type{ST}) where {T, ST}
    x = parse(T, s, base=2)
    return __bits(x, ncodeunits(s), T, ST)
end

__bits(x::Real, Ndum, Tdum, Ty::Type{StaticBitVectorN{T, N}}) where {T, N} = Ty(x)
__bits(x::Real, N, T, Ty::Type{StaticBitVectorN}) = StaticBitVectorN{T, N}(x)
__bits(x::Real, N, T, ::Type{ST}) where {ST <: AbstractStaticBitVectorLen} = ST(x, N)

function myparse(s::String)
    T = min_uint_type(ncodeunits(s))
    return myparse(s, T)
end

# Hmm this can be faster than Base.parse. Or much slower
function myparse(s::String, ::Type{T}) where T
    x = zero(T)
    c = codeunits(s)
    _one = one(T)
    @inbounds for i in eachindex(c)
        if c[i] == _ONE_CHAR_CODE
            x = x | _one << (i - 1)
        end
    end
    return x
end

# bit shifting and oring may be slow, depending on T
# Multiplying by 2 and adding seems uniformly faster


#myparse2(s::AbstractString) = _myparse2_general(min_uint_type(ncodeunits(s)), s)
myparse2(s::AbstractString) = myparse2(min_uint_type(ncodeunits(s)), s)

@inline function myparse2(::Type{T}, s::AbstractString)::T where T
    x::T = zero(T)
    c = codeunits(s)
    @inbounds for i in eachindex(c)
        if c[i] == _ONE_CHAR_CODE
            x += facs104[i]
        end
    end
    return x
end

@inline function _myparse2_general(::Type{T}, s::AbstractString)::T where T
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

