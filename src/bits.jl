const BoolOrVal = Union{Bool, Val{true}, Val{false}}

# Not sure why we have to do this. Forces specialization
_toVal(x::Bool) = x ? Val(true) : Val(false)
_toVal(x::Val) = x

# TODO: what do we expect that this supports? Can you index into Generator ? No.
const _VEC_LIKE = Union{AbstractVector{<:Integer}, NTuple{<:Any, <:Integer}, Base.Generator{<:AbstractVector}}

const _ZERO_CHAR_CODE = UInt8('0')
const _ONE_CHAR_CODE = UInt8('1')

"""
    is_one_char(x)

Return `true` if `x` is equal to `'1'` or its ASCII code.
"""
is_one_char(x) = x == _ONE_CHAR_CODE

"""
    is_zero_char(x)

Return `true` if `x` is equal to `'0'` or its ASCII code.
"""
is_zero_char(x) = x == _ZERO_CHAR_CODE
is_one_char(x::Char) = is_one_char(UInt8(x))
is_zero_char(x::Char) = is_zero_char(UInt8(x))

"""
    is_binary_char(x)

Return `true` if `x` is equal to either `'0'` or `'1'` or their ASCII codes.
"""
is_binary_char(x) = is_one_char(x) || is_zero_char(x)

# TODO: This is probably 10x slower because of the possibility
# of throwing an error. But, safer to start.
# Convert `x` to the character `'0'` or `'1'`.
"""
    to_binary_char_code(x::T)::UInt8

Convert `x` to the ASCII code for character `'0'` or `'1'`.  One of `isbinzero(x)` or
`isbinone(x)` must return `true`.

`isbinzero` and `isbinone` fall back to `iszero` and `isone` where this makes sense.

If `zero(::T)` and `one(::T)` do not give the a useful result for `T` you can define


`T` must implement `iszero` (or `zero`) and `isone` (or `one`). Alternatively, you can
implement `isbinzero` (or `binzero`) and `isbinone` (or `binone`) 

See also [`binzero`](@ref), [`binone`](@ref).
"""
function to_binary_char_code(x::T) where T
    isbinzero(x) && return _ZERO_CHAR_CODE
    isbinone(x) && return _ONE_CHAR_CODE
    throw(DomainError(x, "Must be zero or one value of type $T."))
end

"""
    binzero(x::T)

The value of type `T` representing bit value zero.

If `T <: Integer`, or a matrix thereof, return `zero(::T)`.  If `T` is other
`AbstractArray`, `AbstractString`, `AbstractFloat`, `Complex`, throw a `MethodError`.

The fallback method returns `zero(x)`.
"""
binzero(x) = zero(x)

"""
    binone(x::T)

The value of type `T` representing bit value one.

This sometimes coincides with `one`, which represents the multiplicative identity in
Julia.
"""
binone(x) = one(x)
binone(::Type{Char}) = '1'

"""
    binzero(::Type{Char})
    binzero(::{Char})

Return `'0'`.
"""
binzero(::Type{Char}) = '0'
binzero(::Char) = '0'
binone(::Char) = '1'
binone(x::T) where {T <: AbstractMatrix{<:Integer}} = one(x)
binzero(x::T) where {T <: AbstractMatrix{<:Integer}} = zero(x)

let nb = Union{AbstractArray, AbstractString, AbstractFloat, Complex}
    global binone(x::nb) = throw(MethodError(binone, (x,)))
    global binzero(x::nb) = throw(MethodError(binzero, (x,)))
end

"""
    isbinzero(x)

Return `true` if `x` represents bit value zero.

See also [`binzero`](@ref).
"""
isbinzero(x) = x == binzero(x)

"""
    isbinone(x)

Return `true` if `x` represents bit value one.

See also [`binone`](@ref).
"""
isbinone(x) = x == binone(x)

# TODO: what is the best way to do this ? For example, instead
# isbinone(x) = isone(x)
# and then special cases to throw on String's etc.
# or, as we have now...
isbinone(x::T) where {T <: AbstractMatrix{<:Integer}} = isone(x)
isbinzero(x::T) where {T <: AbstractMatrix{<:Integer}} = iszero(x)

"""
    to_binary_char(x::T)::Char

Convert `x` to the `Char` `'0'` or `'1'`.
`x` must be equal to either `binzero(T)` or `binone(T)`.
"""
to_binary_char(x) = Char(to_binary_char_code(x))

from_binary_char(::Type{T}, x::Char) where T = from_binary_char(T, UInt8(x))

"""
    from_binary_char([::Type{T} = Bool], x)

Convert the characters `'0'` and `'1'` (or `UInt8('0')` and `UInt8('1')`) to `binzero(T)`
and `binone(T)`.
"""
from_binary_char(::Type{T}, x::UInt8) where T = from_binary_char(T, x, Val(true))

function from_binary_char(::Type{T}, x::UInt8, check::Val{true}) where T
    is_one_char(x) && return binone(T)
    is_zero_char(x) && return binzero(T)
    throw(DomainError(x, "Must be '0' or '1'."))
end

function from_binary_char(::Type{T}, x::UInt8, check::Val{false}) where T
    is_one_char(x) && return one(T)
    return zero(T)
end

from_binary_char(x) = from_binary_char(Bool, x)

###
### randbitstring
###

"""
    randbitstring([rng = default_rng()], n::Integer, [dims])

Return a random string of `'1'`s and `'0'`s of length `n`.

The distribution is uniform over all such strings. If `dims` is given
return an `Array` of random bitstrings with dimensions `dims`.
"""
@inline randbitstring(rng::Random.AbstractRNG, n::Integer) = Random.randstring(rng, (_ZERO_CHAR_CODE, _ONE_CHAR_CODE), n)
@inline randbitstring(n::Integer, args...) = randbitstring(Random.default_rng(), n, args...)

function randbitstring(rng::Random.AbstractRNG, n::Integer, dims::NTuple{N,Int}) where N
    return randbitstring!(rng, Array{String, N}(undef, dims), n)
end

"""
    randbitstring!([rng = default_rng()], a::AbstractArray, n::Integer)

Fill array `a` with random bitstrings.
"""
randbitstring!(a::AbstractArray, n::Integer) =
    randbitstring!(Random.default_rng(), a, n)

function randbitstring!(rng::Random.AbstractRNG, a::Array, n::Integer)
    for i in eachindex(a)
        @inbounds a[i] = randbitstring(rng, n)
    end
    return a
end

###
### bitsizeof
###

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
bitsizeof(::Type{<:NTuple{N, <:Integer}}) where {N} = N
const _BITS_PER_BYTE = 8
_bitsizeof(isbits::Val{true}, T::Type) = sizeof(T) * _BITS_PER_BYTE
_bitsizeof(isbits::Val{false}, T::Type) = throw(MethodError(bitsizeof, (T,)))
bitsizeof(::Type{Bool}) = 1

const MPFR_EXP_BITSIZE = bitsizeof(Clong) # bytes_to_bits(sizeof(Clong))

"""
    bitlength(x::T)

Return the number of bits in the instance `x` of type `T`.
This is the number of bits that can be indexed via `bit`. If `x` is an `isbitstype`
type, then this is the same as `bitsizeof(T)`.

In contrast, if `x` is of type `BigInt` or `BigFloat` the number
of bits is not encoded in the type, and in fact may vary from instance
to instance.
"""
bitlength(x::T) where T = bitsizeof(T)
bitlength(x::NTuple{N, <:Integer}) where {N} = N
bitlength(x::BigFloat) =  1 + MPFR_EXP_BITSIZE + precision(x)
bitlength(x::BigInt) = abs(x.size) * Base.GMP.BITS_PER_LIMB # bitsizeof(Base.GMP.Limb)
bitlength(v::AbstractArray) = length(v)

"""
    bitlength(str::AbstractString)

The number of characters in `str`, which is assumed to contain
only `'0'` and `'1'`. If `str` contains other characters, the returned
value will be incorrect.
"""
bitlength(s::AbstractString) = ncodeunits(s)

"""
    rightmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the right (lower)
are one, and all bits to the left of the `i`th bit (higher) are zero.

See also [`leftmask`](@ref), `rangemask`, `mask`.
# Examples
```julia-repl
julia> bitstring(rightmask(UInt8, 3))
"00000111"
```
"""
@inline rightmask(i) = rightmask(Word, i)
@inline rightmask(::Type{T}, i::Integer) where T = one(T) << i - one(T)

abstract type IndexBase end
struct OneBased <: IndexBase end
struct ZeroBased <: IndexBase  end

@inline rightmask(::IndexBase, args...) = rightmask(args...)
@inline rightmask(::ZeroBased, ::Type{T}, i::Integer) where T = rightmask(T, i + 1)
@inline rightmask(::ZeroBased, i::Integer) = rightmask(i + 1)

"""
    leftmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the left (higher)
are one, and all bits to the right of the `i`th bit (lower) are zero.

See `rightmask`, `rangemask`, `mask`.
# Examples
```julia-repl
julia> bitstring(leftmask(UInt8, 3))
"11111100"
```
"""
leftmask(i) = leftmask(Word, i)
# TODO: tests show efficiency of defining as ~rightmask(i-1), is this ok?
leftmask(::Type{T}, i::Integer) where T = ~rightmask(T, i - 1)
#leftmask(::Type{T}, i::Integer) where T = ~(one(T) << (i-1) - one(T))

leftmask(::IndexBase, args...) = leftmask(args...)
leftmask(::ZeroBased, ::Type{T}, i::Integer) where T = leftmask(T, i + 1)
leftmask(::ZeroBased, i::Integer) = leftmask(i + 1)

"""
    rangemask([T=UInt], ilo, ihi)
    rangemask([T=UInt], (ilo, ihi)...)

Return `n::T` such that all bits in the range `ilo` to `ihi`, inclusive, are
one, and all other bits are zero. If `Tuples` `(ilo, ihi)...` are given,
then set bits in each range to one.

See `leftmask`, `rightmask`, `mask`.

# Examples
```julia-repl
julia> bitstring(rangemask(UInt8, 2, 7))
"01111110"

julia> bitstring(rangemask(UInt16, (1, 3), (5, 8), (14, 16)))
"1110000011110111"

julia> bitstring(rangemask(UInt8, (1, 5), (4, 8)))
"11111111"
```
"""
rangemask(args...) = rangemask(OneBased(), Word, args...)
rangemask(ib::IndexBase, ::Type{T}, ilo::Integer, ihi::Integer) where T = leftmask(ib, T, ilo) & rightmask(ib, T, ihi)
rangemask(ib::IndexBase, ::Type{T}, ranges::NTuple{2}...) where T = mapfoldl(x->rangemask(ib, T, x...), |, ranges)
rangemask(ib::IndexBase, ::Type{T}, ur::UnitRange) where T = rangemask(ib, T, ur.start, ur.stop)
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
julia> bitstring(mask(UInt8, 3))
"00000100"

julia> bitstring(mask(UInt8, (1, 5, 8)))
"10010001"

julia> bitstring(mask(UInt8, (2, 5, 8)))
"10010010"

julia> bitstring(mask(1:2:64))
"0101010101010101010101010101010101010101010101010101010101010101"

julia> bitstring(mask(UInt16, (1:3, 9, 14:16)))
"1110000100000111"
```
"""
mask(ib::IndexBase, ::Type{T}, ur::UnitRange) where T = rangemask(ib, T, ur)
# Following method has typical problem. For literal or::OR, it happens at compile time.
# For or::OR in variable it is 10x slower than the fallback below with `inds`.
# But, the fallback is slower for literal range, does not happen at compile time.
# mask(::Type{T}, or::OrdinalRange) where T = mask(T, collect(or))
mask(ib::OneBased, ::Type{T}, i::Integer) where T = (one(T) << (i-one(T)))
mask(ib::IndexBase, ::Type{T}, inds) where T = mapfoldl(i->mask(ib, T, i), |, inds)
mask(::OneBased, ::Type{T}, r::Base.OneTo) where T = rightmask(T, length(r))
mask(arg) = mask(OneBased(), Word, arg)
mask(::Type{T}, args...) where T = mask(OneBased(), T, args...)

# Modified from Bits.jl
asint(x::Integer) = x
asint(x::AbstractFloat) = reinterpret(Signed, x) # Signed gets all the types we need

asuint(x::Integer) = unsigned(x)
asuint(x::AbstractFloat) = reinterpret(Unsigned, x) # Signed gets all the types we need

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
# TODO: support other types with masked, such as bitstring, BitVector, ...

"""
    bit(x::Real, i::Integer)

Similar to `Bits.bit` from registered `Bits.jl` package. A difference is that
the return type here does not depend on the input type, but rather is always `Int`.
(Check a. is this true and b. what do we prefer?)
"""
@inline bit(x::Integer, i::Integer) = ((Base.:(>>>)(x, UInt(i-1))) & 1) % Int
bit(x::AbstractFloat, i::Integer) = bit(asint(x), i)
bit(x::Union{BigInt, BigFloat}, i::Integer) = Int(tstbit(x, i))

function bit(x::AbstractVector{Bool}, i::Integer)
    @boundscheck checkbounds(x, i)
    return @inbounds x[i] # % Int
end
@inline bit(::ZeroBased, x, i::Integer) = bit(x, i + 1)
@inline bit(::OneBased, args...) = bit(args...)

"""
    bit0(x, i)

Like `bit(x, i)` except the first bit has index `0` rather than `1`.
"""
@inline bit0(x, i) = bit(ZeroBased(), x, i)

"""
    tstbit0(x, i)

Like `tstbit(x, i)` except the first bit has index `0` rather than `1`.
"""
@inline tstbit0(x, i) = tstbit(ZeroBased(), x, i)


## NB: In the following few functions there are two kinds of checking.
## 1) is the index in bounds
## 2) is the value at that index valid.

# TODO: Using Val here may offer no advantage
# Assume the string is one code unit per code point: '1' or '0'
"""
    bit(str::AbstractString, i::Integer)::Int

Return `1` if the `i`th character of `str` is `'1'` and
`0` if it is `'0'`. Otherwise throw an `ArgumentError`.

It is assumed that `str` has one byte per character, or more precisely,
one `UInt8` code unit per code point. Thus, accessing the `i`th character
via `bit` may be more efficient than `str[i]`.
"""
function bit(str::AbstractString, i::Integer; check::BoolOrVal=Val(true))
    @boundscheck checkbounds(str, i)
    byte = @inbounds codeunit(str, i)
    return from_binary_char(Int, byte, _toVal(check))
end

# Not using Val here seems not to affect performance
function bit(v, i::Integer; check=true)
    @boundscheck checkbounds(v, i)
    b = @inbounds v[i]
    isone(b) && return 1
    check && !iszero(b) && throw(DomainError(b, "Unrecognized bit $b"))
    return 0
end

# TODO: Do bounds checking somehow for Tuple. No method for checkbounds.
function bit(@nospecialize(v::Tuple), i::Integer; check=true)
    b = v[i]
    isone(b) && return 1
    check && !iszero(b) && throw(DomainError(b, "Unrecognized bit $b"))
    return 0
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
#@inline tstbit(x, i::Integer) = bit(x, i) % Bool
#@inline tstbit(x::Integer, i::Integer) = ((Base.:(>>>)(x, UInt(i-1))) & 1) % Bool
#@inline tstbit(x::Integer, i::Integer) = ((Base.:(>>>)(x, UInt(i-1))) & 1) === one(typeof(x)) ? true : false
@inline tstbit(x::Integer, i::Integer) = ((>>>(x, UInt(i-1))) & 1) != 0
@inline tstbit(x, i::Integer) = bit(x, i) % Bool
tstbit(x::BigInt, i::Integer) = Base.GMP.MPZ.tstbit(x, i-1)

@inline tstbit(::ZeroBased, x, i::Integer) = tstbit(x, i + 1)
@inline tstbit(::OneBased, args...) = tstbit(args...)

# Maybe from Random module via Bits.jl
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


# This is fast
"""
    normalize_bitstring(str::AbstractString)

Remove all characters (more precisely, code points) from `str` that are not one
of `'0'` and `'1'`, if such characters exist. Otherwise, return `str`
unchanged.
"""
function normalize_bitstring(str::AbstractString)
    nbits = count_bits(str)
    nbits == ncodeunits(str) && return str
    v = Base.StringVector(nbits) # Not exported, but fast
    j = 1
    for i in eachindex(codeunits(str))
        c = @inbounds codeunit(str, i)
        if is_binary_char(c)
            @inbounds v[j] = c
            j += 1
        end
    end
    return String(v)
end

###
### More bit functions
###

## Add more methods for Base.uinttype.
## Methods are defined in base for floating point types,
## and in the package BitIntegers.jl
let
#   Why did I have Float here? It is already defined. Did we not want to extend the Base functions?
#    tups = [(Symbol(:Int, n), (Symbol(:UInt, n), Symbol(:Int, n), Symbol(:Float, n))) for n in (16, 32, 64)]
    tups = [(Symbol(:Int, n), (Symbol(:UInt, n), Symbol(:Int, n))) for n in (16, 32, 64)]
    for (t, ts) in ((:Int8, (:UInt8, :Int8, :Bool)), tups..., (:Int128, (:UInt128, :Int128)))
        for tp in ts
            @eval inttype(::Type{$tp}) = $t
            @eval Base.uinttype(::Type{$tp}) = $(Symbol("U", t))
        end
    end
end

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


"""
    bit_string(n::Integer; pad=nothing)

Give the literal bitstring representation of the number `n`. If `n` is a primitive type,
`bit_string` is the same as `Base.bitstring`, except that the former allows specifying
left padding.

`pad=0` omits leading zeros in the output string

# Examples:

```julia-repl
julia> bit_string(128)
"0000000000000000000000000000000000000000000000000000000010000000"

julia> bit_string(128; pad = 0)
"10000000"

julia> bit_string(128; pad = 9)
"010000000"

julia> bit_string(1.0)
"0011111111110000000000000000000000000000000000000000000000000000"
```
"""
function bit_string(x::T; pad::Union{Nothing,Integer}=nothing) where T <: Integer
    isnothing(pad) && return bitstring(x)
    return string(reinterpret(Base.uinttype(T), x); pad=pad, base=2)
end

bit_string(x::AbstractFloat, args...;pad=nothing) = bit_string(asint(x), args...;pad=pad)

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
    is_bitstring(bit_str::Union{AbstractString, AbstractVector{UInt8}}; throw=false)

Return `true` if all characters in `bit_str` are either `'0'` or `'1'`,
otherwise `false`.
If `throw` is `true`, then throw an `ArgumentError` rather than returning `false`.
"""
function is_bitstring(bit_str::Union{AbstractString, AbstractVector{UInt8}}; throw=false)
    result = _is_bitstring(bit_str)
    (throw && !result) && Base.throw(
        ArgumentError("Argument is not a bit string"))
    return result
end

_is_bitstring(v::AbstractVector{UInt8}) = all(is_binary_char, v)
_is_bitstring(s::AbstractString) = _is_bitstring(codeunits(s))

"""
    count_bits(s::AbstractString)
    count_bits(v::AbstractVector{UInt8})

Return the number of characters (or bytes) that are `'1'` or `'0'`,
(`0x30` or `0x31`).

It is assumed that `s` is an ASCII string. `count_bits` may be useul if
the string includes formatting characters, for example spaces.
"""
count_bits(s::AbstractString) = count_bits(codeunits(s))
count_bits(v::AbstractVector{UInt8}) = count(is_binary_char, v)

###
### bitgetindex
###

"""
    bitgetindex(::Type{OT}=T, x::T, bitinds)::Bool

Return the sub-collection of the bits in `x` specified by `bitinds`.
The return type is `OT` which defaults to the input type.

The leftmost bit has index `1`. Compare to `bits(x, bitinds)` for which
the rightmost bit has index `1`.

For example, for bitstring `x::String` this is equivalent to `x[bitinds]`, but is
more efficient.
"""
function bitgetindex end

@inline bitgetindex(x::T, bitind::Integer) where T = bitgetindex(T, x, bitind)
# Disabled this one

# TODO: Which behavior do we want here. Pick only one
#@inline bitgetindex(x::T, bitinds) where T = bitgetindex(T, x, bitinds)
@inline bitgetindex(x, bitinds) = bitgetindex(Bool, x, bitinds)

# In analogy to indexing into an Array, a single element is returned as an eltype
#@inline bitgetindex(x, bitind::Integer) = bitgetindex(Bool, x, bitind)

#bitgetindex(::Type{T}, v, i::Integer) where T = bitgetindex(T, v, Int(i))
bitgetindex(::Type{T}, v::AbstractArray, i::Int) where T = v[i] % T
bitgetindex(::Type{<:Any}, v::Tuple, i::Int) = v[i]
# Hmm. Yes, I do want this. But it's not tested :(
# bitgetindex(bv::AbstractArray{Bool}, inds...) = getindex(bv, inds...)

"""
    bitgetindex(bv::AbstractArray{Bool}, ind::Integer)

Return `bv[ind]`.
"""
bitgetindex(bv::AbstractArray{Bool}, ind::Integer) = getindex(bv, ind)

# Method required for dispatch disambiguation
bitgetindex(bv::AbstractArray{Bool}, ind::Int) = getindex(bv, ind)


bitgetindex(A::AbstractArray, inds::AbstractArray{<:Int}) = getindex(A, inds)
bitgetindex(A::AbstractArray, ind::Int, inds::Int...) = getindex(A, ind, inds...)
# This is really slow. Ruins some kind of inference
bitgetindex(A::AbstractArray, inds::NTuple{N, <:Integer}) where N = getindex(A, collect(inds))

# NB. StringVector is not exported. I am supposed to use IOBuffer instead.
# This may break.
# @inline function bitgetindex(::Type{Vector{UInt8}}, s::String, bitinds)
#     sv = Base.StringVector(length(bitinds)) # StringVector always seems faster
#     return _bitgetindex!(sv, s, bitinds)
# end

# This is better than above
@inline function bitgetindex(::Type{Vector{UInt8}}, s::String, bitinds)
    s1 = s[bitinds]
    return unsafe_wrap(Vector{UInt8}, s1)
end


@inline function _bitgetindex!(dest, s::String, bitinds)
    for i in eachindex(bitinds)
        bi = @inbounds bitinds[i]
        c = codeunits(s)[bi]
        @inbounds dest[i] = c
    end
    return dest
end

# This will return a single Char for bitinds::Int even though we specified Type{String}
# We call getindex, because this only deals with codeunits, and Base has optimized this.
@inline bitgetindex(::Type{String}, s::String, bitinds) = getindex(s, bitinds)
# No this is slow
# bitgetindex(::Type{String}, s::String, bitinds) = String(bitgetindex(Vector{UInt8}, s, bitinds))

"""
    fliporder(::Type{T}, i::Int)
    fliporder(x::T, i::Int)

If `i` is an index into bits of `x` (or type `T`) from the left,
return the index from the right. Also flips an index from the right to
one from the left.
"""
fliporder(::Type{T}, i::Int) where T = bitsizeof(T) - i + 1
fliporder(x, i::Int) = bitlength(x) - i + 1

# Leftmost bit is index 1
# bit(x, i) considers rightmost bit 1
@inline function bitgetindex(::Type{T2}, x::Real, bitinds) where {T2}
    xin = asint(x)
    xout = zero(xin)
    for i in eachindex(bitinds)
        @inbounds bi = fliporder(xout, bitinds[i])
        xout += (xin >>> (bi - i)) & (1 << (i - 1))
    end
    return xout % T2
end

function bitgetindex(::Type{T2}, x::T, bitinds::AbstractUnitRange{<:Integer}) where {T<:Real, T2}
    xout = asint(x)
    _i0, _i1 = extrema(bitinds)
    i1 = fliporder(xout, _i0)
    i0 = fliporder(xout, _i1)
    return reinterpret(T, masked(xout, i0:i1) >>> (i0-1)) % T2
end

###
### bitreverse, bitrotate
###

## These are exported by Base.

# Use duck typing. This covers AbstractString, AbstractArray
Base.bitreverse(s) = reverse(s)
Base.bitreverse(n::Real) = bitreverse(asint(n))

#@inline bitreverse(x::T) where T = bitreverse(T, x)
# We could restrict the type, but we don't want to verify that x::Vector{Int} only has 0 and 1
# The downside, is you can get erroneous wrong output for wrong input
#bitreverse(v) = reverse(v)
bitreverse!(v) = reverse!(v)

###
### bitsize
###
## Don't confuse with bitsizeof

bitsize(a) = size(a)
bitsize(x::Real) = (bitlength(x),)
bitsize(s::AbstractString) = (ncodeunits(s),)
bitsize(t::Tuple) = (length(t),)

# FIXME: @inline at call sites is a v1.8 feature
# So I disabled it in three places below.
# Find out how important it is.
function bitaxes(A)
#    @inline
    map(Base.oneto, bitsize(A))
end

# performance optimization, as in Base.
#bitaxes1(A::AbstractArray{<:Any,0}) = OneTo(1)
bitaxes1(A) = bitaxes(A)[1]
biteachindex(A) = bitaxes1(A)
# bitaxes1(A) = (@inline; bitaxes(A)[1])
# biteachindex(A) = (@inline(); bitaxes1(A))
bitlastindex(A) = last(biteachindex(A))

"""
    bit_count_ones(v)

Count the number of bit values in `v` equal to one.
"""
bit_count_ones(x) = count_ones(x)
bit_count_ones(v::AbstractArray) = count(isone, v)

"""
    bit_count_ones(s::AbstractString)

Count the number of characters in `s` equal to `'1'`.
"""
bit_count_ones(s::AbstractString) = sum(is_one_char, codeunits(s))

"""
    bit_count_zeros(v)

Count the number of bit values in `v` equal to one.
"""
bit_count_zeros(x) = count_zeros(x)
bit_count_zeros(s::AbstractString) = sum(is_zero_char, codeunits(s))

"""
    bit_count_zeros(s::AbstractString)

Count the number of characters in `s` equal to `'0'`.
"""
bit_count_zeros(v::AbstractArray) = count(iszero, v)

###
### bitvector
###

# Does this interface make sense? Should the user just do this: BitVector(bitvecview(s))
# """
#     bitvector(args...)
# Create a `BitVector`. This allows initializing with a bit string.
# Otherwise `args` are passed to `BitVector`.
# """
# bitvector(args...) = BitVector(args...)

# Does this interface make sense? Should the user just do this: BitVector(bitvecview(s))
"""
    bitvector(s::AbstractString)

Return a `BitVector` with the bits in bitstring `s`.
"""
bitvector(s::AbstractString) = BitVector(bitvecview(s))

"""
    bitvector(x::Union{Integer, Base.IEEEFloat})

Return a `BitVector` with the bits in `x`.
"""
function bitvector(x::Union{Integer, Base.IEEEFloat})
    _bits = BitArray(undef, 8 * sizeof(x))
    xi = asint(x)
    if sizeof(x) <= 8
        @inbounds _bits.chunks[1] = xi
    else
        for i in eachindex(_bits.chunks)
            @inbounds _bits.chunks[i] = (xi >> (64 * (i - 1)) % UInt64)
        end
    end
    return _bits
end
