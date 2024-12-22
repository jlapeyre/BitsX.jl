module Bits

import ..BitsX: Word

import ..BitsX.BitsBase: BitsBase, bitsizeof, bitlength, ZeroBased, OneBased, IndexBase, asint,
    from_binary_char, is_binary_char, is_zero_char, is_one_char, count_bits, bitsize,
    biteachindex

import ..BitsX.BitsBase._BitsBase: _BitsBase, BoolOrVal, _toVal, _zero, _ZERO_CHAR_CODE, _ONE_CHAR_CODE

import Random

export randbitstring, randbitstring!, BitStringSampler,
    rightmask, leftmask, rangemask, mask, masked, bit,
    bit0, tstbit, tstbit0, normalize_bitstring, undigits,
    bit_string,
    bit_count_ones, bit_count_zeros,
    bitcollect, BitStringArray

module _Bits

import ...BitsX.BitsBase: bitsizeof
import ...BitsX.BitsBase._BitsBase: _ZERO_CHAR_CODE, _ONE_CHAR_CODE

import Random
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

# TODO: A much faster way to generate random bits strings than the current implementation
# is to generate random bits: for example `bstring(rand(UInt))`
@inline _randbitstring(rng::Random.AbstractRNG, n::Integer) = Random.randstring(rng, (_ZERO_CHAR_CODE, _ONE_CHAR_CODE), n)::String

struct BitStringSampler
    nbits::Int
end

end # module _Bits

import ._Bits: _Bits, BitStringSampler

###
### randbitstring
###


# Following causes return eltype to be `String` when returning an array.
# Otherwise it is `Any`. I think this is not in the API, so this is a bit fragile.
Base.eltype(::Type{BitStringSampler}) = String

Random.rand(rng::Random.AbstractRNG, d::Random.SamplerTrivial{BitStringSampler})::String = _Bits._randbitstring(rng, d[].nbits)::String

"""
    randbitstring([rng = default_rng()], nbits::Integer, [dims])

Return a random string of `'1'`s and `'0'`s of length `nbits`.

The distribution is uniform over all such strings. If `dims` is given
return an `Array` of random bitstrings with dimensions `dims`.

# Examples
```jldoctest
julia> import Random; rng = Random.seed!(10);

julia> randbitstring(rng, 10)
"1011110111"
```

See also [`randbitstring!`](@ref).
"""
@inline randbitstring(rng::Random.AbstractRNG, nbits::Integer, args...) = rand(rng, BitStringSampler(nbits), args...)
@inline randbitstring(nbits::Integer, args...) = randbitstring(Random.default_rng(), nbits, args...)

"""
    randbitstring!([rng = default_rng()], a::AbstractArray, nbits::Integer)

Fill array `a` with random bitstrings.
"""
@inline randbitstring!(rng::Random.AbstractRNG, a::AbstractArray, nbits::Integer) = Random.rand!(rng, a, BitStringSampler(nbits))
@inline randbitstring!(a::AbstractArray, nbits::Integer) = randbitstring!(Random.default_rng(), a, nbits)

# TODO: rightmask and leftmask might be better called lomask and himask
"""
    rightmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the right (lower)
are one, and all bits to the left of the `i`th bit (higher) are zero.

See also [`leftmask`](@ref), `rangemask`, `mask`.
# Examples
```jldoctest
julia> bitstring(rightmask(UInt8, 3))
"00000111"
```
"""
@inline rightmask(i) = rightmask(Word, i)
@inline rightmask(::Type{T}, i::Integer) where T = one(T) << i - one(T)

@inline rightmask(::IndexBase, args...) = rightmask(args...)
@inline rightmask(::ZeroBased, ::Type{T}, i::Integer) where T = rightmask(T, i + 1)
@inline rightmask(::ZeroBased, i::Integer) = rightmask(i + 1)

"""
    leftmask([T=UInt], i)

Return `n::T` such that the `i`th bit and all bits to the left (higher)
are one, and all bits to the right of the `i`th bit (lower) are zero.

See `rightmask`, `rangemask`, `mask`.
# Examples
```jldoctest
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
```jldoctest
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
```jldoctest
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

# Taken from Bits.jl
"""
    masked([ib::IndexBase], x::T, inds...) where {T}

Return the result of applying the mask determined by `inds` to `x`.

That is, return `x & mask(T, inds...)`

# Examples
```jldoctest
julia> masked(0b11110011, 1:5) === 0b00010011
true

julia> x = rand(); masked(-x, 1:63) === x
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

function bit(x::AbstractArray{Bool}, i::Integer)
    @boundscheck checkbounds(x, i)
    return @inbounds x[i] # % Int
end
@inline bit(::ZeroBased, x, i::Integer) = bit(x, i + 1)
@inline bit(::OneBased, args...) = bit(args...)

function bit(x, inds::AbstractVector)
    [bit(x, i) for i in inds]
end

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
    check && !iszero(b) && throw(DomainError(b, lazy"Unrecognized bit $b"))
    return 0
end

# TODO: Do bounds checking somehow for Tuple. No method for checkbounds.
function bit(@nospecialize(v::Tuple), i::Integer; check=true)
    b = v[i]
    isone(b) && return 1
    check && !iszero(b) && throw(DomainError(b, lazy"Unrecognized bit $b"))
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
    base == 2 && IntT <: Unsigned && return _Bits._undigits_base_2(IntT, A)
    n = zero(IntT)
    @inbounds for i in reverse(eachindex(A))
        n = Base.checked_add(Base.checked_mul(base, n), IntT(A[i]))
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
```jldoctest
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
# Not yet implemented
# function bit_string(x::T; pad::Union{Nothing,Integer}=nothing, nsep::Union{Nothing,Integer}=nothing) where T <: Integer
#     !isnothing(nsep) && return _bit_string(x, pad, nsep)
    isnothing(pad) && return bitstring(x)
    return string(reinterpret(Base.uinttype(T), x); pad=pad, base=2)
end

bit_string(x::AbstractFloat, args...;pad=nothing) = bit_string(asint(x), args...;pad=pad)

# function _bit_string(x, pad, nsep)
# end

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

!!! warning "Unstable interface"

`bitgetindex` will likely be removed.
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

# @inline function _bitgetindex!(dest, s::String, bitinds)
#     for i in eachindex(bitinds)
#         bi = @inbounds bitinds[i]
#         c = codeunits(s)[bi]
#         @inbounds dest[i] = c
#     end
#     return dest
# end

# This is better than above
@inline function bitgetindex(::Type{Vector{UInt8}}, s::String, bitinds)
    s1 = s[bitinds]
    return unsafe_wrap(Vector{UInt8}, s1)
end


# This will return a single Char for bitinds::Int even though we specified Type{String}
# We call getindex, because this only deals with codeunits, and Base has optimized this.
@inline bitgetindex(::Type{String}, s::String, bitinds) = getindex(s, bitinds)
# No this is slow
# bitgetindex(::Type{String}, s::String, bitinds) = String(bitgetindex(Vector{UInt8}, s, bitinds))

# This description might mention endianness.
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

# Return same type T, same as input x::T
function bitgetindex(::Type{T2}, x::T, bitinds::AbstractUnitRange{<:Integer}) where {T<:Real, T2}
    xout = asint(x)
    _i0, _i1 = extrema(bitinds)
    i1 = fliporder(xout, _i0)
    i0 = fliporder(xout, _i1)
    return reinterpret(T, masked(xout, i0:i1) >>> (i0-1))
end

###
### bitreverse, bitrotate
###

## These are exported by Base.

# PIRACY !
# Use duck typing. This covers AbstractString, AbstractArray
Base.bitreverse(s) = reverse(s)
Base.bitreverse(n::Real) = bitreverse(asint(n))

#@inline bitreverse(x::T) where T = bitreverse(T, x)
# We could restrict the type, but we don't want to verify that x::Vector{Int} only has 0 and 1
# The downside, is you can get erroneous wrong output for wrong input
#bitreverse(v) = reverse(v)
bitreverse!(v) = reverse!(v)

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

"""
    bitcollect(obj)

Convert the representation of a bit array `obj` to an `Array{Bool}`.
"""
function bitcollect(obj)
    array = Array{Bool}(undef, bitsize(obj))
    # Assume LinearIndices. Probably not always correct
    for (i, ind) in enumerate(biteachindex(obj))
        array[i] = bit(obj, ind)
    end
    array
end

# TODO: Check row vs column major
"""
   BitStringArray{T <: AbstractVector{<:AbstractString}, N} <: AbstractArray{Bool, N}

An array of `Bool`s represented by a vector of bitstrings.
"""
struct BitStringArray{T <: AbstractVector{<:AbstractString}, N} <: AbstractArray{Bool, N}
    data::T
    dims::NTuple{N, Int}
    len::Int

    function BitStringArray(strs::ST, dims::NTuple{N, Int}) where {ST <: AbstractVector{<: AbstractString}, N}
        if !isempty(strs)
            n = bitlength(first(strs))
            all(x -> bitlength(x) == n, strs) || throw(DimensionMismatch("Strings in BitStringArray must have the same length"))
        end
        len = prod(dims)
        return new{ST, length(dims)}(strs, dims, len)
    end
end

function BitStringArray(strs::T) where {T <: AbstractVector{<:AbstractString}}
    if isempty(strs)
        return BitStringArray(strs, (0,))
    end
    BitStringArray(strs, (bitlength(first(strs)), size(strs)...,))
end

Base.size(B::BitStringArray) = B.dims
Base.size(B::BitStringArray, n::Integer) = size(B, n)

# We may not want to store this. rather, compute it every time.
Base.length(B::BitStringArray) = B.len

function Base.getindex(B::BitStringArray, i::Integer)
    bit(first(B.data), i)
end

function Base.getindex(B::BitStringArray, inds::Integer...)
    i1 = first(inds)
    inds = inds[2:end]
    bit(B.data[inds...], i1)
end


end # module Bits
