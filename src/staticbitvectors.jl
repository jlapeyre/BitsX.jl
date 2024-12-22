#  This name is misleading
module StaticBitVectors

module _StaticBitVectors

import ...BitsX.Bits: normalize_bitstring, undigits, rightmask

import ...BitsX.BitsBase._BitsBase: _BitsBase, _toVal, BoolOrVal

import ...BitsX.BitsBase: ZeroBased, OneBased, IndexBase,
    ZeroBased, OneBased, bitsizeof, bitsize, tstbit, bit

import ...BitsX.BitIntegersX
import ...BitsX.ParseBin: parse_bin

# TODO: what do we expect that this supports? Can you index into Generator ? No.
const _VEC_LIKE = Union{AbstractVector{<:Integer}, NTuple{<:Any, <:Integer}, Base.Generator{<:AbstractVector}}

# TODO: We could also store the bits in a Tuple of values

# TODO:
# Not supplying `IntT` is much slower (+ 100ns, eg 10 x) than it should be. It is much slower than the time
# required to lookup the correct `IntT`.
"""
    bits([IntT], dts::Union{AbstractVector{<:Integer},  NTuple{<:Any, <:Integer}}, n=length(dts))

Convert the container of binary digits `dts` to a `StaticBitVector`.

`IntT` is the storage type, i.e., the type that is wrapped. Input is not validated for
correctness, nor for having length greater than the number of bits in `IntT`. If `IntT` is
omitted, the smallest type capable of representing `dts` is used.  However, supplying
`IntT` results in faster conversion because the output type can be inferred by the
compiler.

# Examples
```jldoctest
julia> bits((0,1,0,1))
<1010>
```
"""
function bits(_digits::_VEC_LIKE, n::Int=length(_digits))
    #    bits(undigits(BitIntegerX.min_uint_type(n), _digits; base=2), n)
    T = BitIntegersX.min_uint_type(n)
    return bits(T, _digits, n)
end

function bits(::ZeroBased, _digits::_VEC_LIKE, n::Int=length(_digits))
    T = BitIntegersX.min_uint_type(n)
    return bits(ZeroBased(), T, _digits, n)
end

function bits(::Type{IntT}, _digits::_VEC_LIKE, n=length(_digits)) where IntT
#    return bits(undigits(IntT, _digits; base=2)::IntT, n)
    return StaticBitVector{IntT}(undigits(IntT, _digits; base=2)::IntT, n)
end

function bits(::ZeroBased, ::Type{IntT}, _digits::_VEC_LIKE, n=length(_digits)) where IntT
    return StaticBitVector0{IntT}(undigits(IntT, _digits; base=2)::IntT, n)
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

julia> permutedims([v[i] for i in 1:8]) # Use permutedims to show on one line.
1×8 Matrix{Bool}:
 1  1  0  0  1  0  0  0

julia> bits(true)
<1>

julia> bits(big(2)^63)
<...0 10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000>

julia> bits(Float32(-7))
<1|10000001|1100000 00000000 00000000>
```
"""
bits(x::Real) = StaticBitVectorView(x)

# FIXME from above
# ```jldoctest
# julia> ans[1:23] # creates a vector of bits with a specific length
# <1100000 00000000 00000000>
# ```


"""
    bits(x::T, n) where {T<:Real}

Create a view of the bits of `x` that is truncated to the first `n` bits.

The underlying data is of type `T`. The truncated bits are zeroed.
Indexing at positions larger than `n` will not error, but will return zero.

# Examples
```jldoctest
julia> bits(0x5555)
<01010101 01010101>

julia> bits(0x5555, 10)
<01 01010101>
```
"""
bits(x::Real, n::Integer) = StaticBitVector(x, n)

bits(::IndexBase, x::Real, n::Integer) = bits(x, n)
bits(::ZeroBased, x::Real, n::Integer) = StaticBitVector0(x, n)

# ** StaticBitVector


# similar to a BitVector, but with only 1 word to store bits (instead of 1 array thereof)
abstract type AbstractStaticBitVector{T<:Real} <: AbstractVector{Bool} end
#abstract type AbstractStaticBitVector{BitIntegers.UBI} <: AbstractVector{Bool} end

@inline datatype(::Type{<:AbstractStaticBitVector{T}}) where T = T
@inline Base.parent(bv::AbstractStaticBitVector) = getfield(bv, :x)

Base.bitstring(bv::AbstractStaticBitVector) = bitstring(parent(bv))

Base.:(>>)(x::T, i) where T<:AbstractStaticBitVector = T(x.x >> i)

struct StaticBitVectorView{T} <: AbstractStaticBitVector{T}
    x::T
end

# Otherwise fallback is used. This is often only twice as fast as fallback
function Base.isequal(a::AbstractStaticBitVector, b::AbstractStaticBitVector)
    return isequal(a.x, b.x) && isequal(length(a), length(b))
end

function Base.:(==)(a::AbstractStaticBitVector, b::AbstractStaticBitVector)
    return isequal(a.x, b.x) && isequal(length(a), length(b))
end

# This hash is not equal to AbstractArray. They won't be considered equal in a Dict
function Base.hash(a::AbstractStaticBitVector)
    return hash(a.x, hash(length(a)))
end

# _count is called by count, sum, etc.
# TODO: Do similar counting for StaticBitVectorLen
Base._count(::typeof(identity), B::StaticBitVectorView, ::Colon, init) = init + count_ones(B.x)

Base.convert(::Type{StaticBitVectorView{T}}, x) where T = StaticBitVectorView(T(x))

Base.zero(::Type{V}) where V <: AbstractStaticBitVector = convert(V, 0)
Base.zero(::V) where V <: AbstractStaticBitVector = convert(V, 0)
# what is `one` worth here?
Base.one(::Type{V}) where V <: AbstractStaticBitVector = convert(V, 1)
Base.one(::V) where V <: AbstractStaticBitVector = convert(V, 1)

abstract type AbstractStaticBitVectorLen{T} <: AbstractStaticBitVector{T} end

"""
    bitstring(bv::AbstractStaticBitVector)

Return `bv` as a `String` of only  `'0'` and `'1'`.
"""
Base.bitstring(bv::AbstractStaticBitVectorLen) = string(parent(bv); base=2)

Base.:(>>)(x::T, i) where T<:AbstractStaticBitVectorLen = T(x.x >> i, length(x))
Base.:(<<)(x::T, i) where T<:AbstractStaticBitVectorLen = T(x.x << i, length(x))

# This is no faster than the more general method below.
# Why is this one restricted to AbstractStaticBitVectorLen ?
# Whey not use it for all AbstractStaticBitVector ?
# Lack of tests.
# This is slower in fact than more generic method below
# @inline function Base.iterate(bv::AbstractStaticBitVectorLen, i::Int=1)
#     if (i % UInt) - 1 < length(bv)
#         (@inbounds bv[i], i + 1)
#     else
#         nothing
#     end
# end

function Base.iterate(bv::AbstractStaticBitVector, i::Int=0)
    i >= length(bv) && return nothing
    (((>>>(bv.x, UInt(i))) & UInt64(1)) != 0, i + 1)
end

struct StaticBitVector{T<:Real} <: AbstractStaticBitVectorLen{T}
    x::T
    len::Int
    function StaticBitVector{T}(x::T, n::Integer) where {T<:Real}
        return new{T}(x & rightmask(T, n), n)
    end
end

StaticBitVector{T}(x::Real, n::Integer) where {T<:Real} = StaticBitVector{T}(T(x), n)

"""
    StaticBitVector0{T<:Real}

A zero-based indexed bit vector backed by an unsigned integer type.
"""
struct StaticBitVector0{T<:Real} <: AbstractStaticBitVectorLen{T}
    x::T
    len::Int
    function StaticBitVector0{T}(x::T, n::Integer) where {T<:Real}
        return new(x & rightmask(T, n), n)
    end
end

StaticBitVector(x::T, n::Integer) where T = StaticBitVector{typeof(x)}(x, n)
StaticBitVector0(x::T, n::Integer) where T = StaticBitVector0{typeof(x)}(x, n)

@inline index_base(::Any) = OneBased()
@inline index_base(::Type{<:StaticBitVector0}) = ZeroBased()

bitsize(x::AbstractStaticBitVector{<:Real}) = bitsize(parent(x))
bitlength(x::AbstractStaticBitVector) = prod(bitsize(x))
bitlength(x::AbstractStaticBitVectorLen) = x.len
# TODO use data_type above ?
bitsizeof(::Type{<:AbstractStaticBitVector{T}}) where T  = bitsizeof(T)

# TODO: could define ZeroTo. This is done ad hoc around Julia ecosystem
Base.axes1(v::StaticBitVector0) = 0:(bitlength(v) - 1)
Base.axes(v::StaticBitVector0) = (Base.axes1(v),)

Base.size(v::AbstractStaticBitVector) = (bitlength(v),)

# Assume 1 "based"
# TODO!: do bounds checking
@inline function Base.getindex(ib::IndexBase, v::AbstractStaticBitVector, i::Integer)
    return tstbit(ib, v.x, i)
end

@inline Base.getindex(v::AbstractStaticBitVector, i::Integer) = getindex(index_base(typeof(v)), v, i)

function Base.getindex(v::AbstractStaticBitVector, a)
    return _getindex(Base.IteratorSize(a), v, a)
end

@inline function _getindex(::Union{Base.HasLength, Base.HasShape{1}}, v::AbstractStaticBitVector, a)
    return StaticBitVector(bit(v.x, a), length(a))
end

function _getindex(::Union{Base.HasLength, Base.HasShape{1}}, v::AbstractStaticBitVectorLen, a::AbstractVector{<:Integer})
    return typeof(v)(bit(v.x, a), length(a))
end

# TODO: This is incorrect sometimes
# Eg. bits(5)[1:2] fails
function Base.getindex(v::AbstractStaticBitVector, a::AbstractUnitRange{<:Integer})
    x = bit(v.x, a)
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

# Disable this. Causes ambiguities and is not tested.
# Base.convert(::Type{StaticBitVectorN{T,N}}, x) where {T, N} = StaticBitVectorN{T,N}(T(x))
Base.convert(::Type{StaticBitVectorN{T,N}}, x::StaticBitVectorN{T,N}) where {T, N} = x

# Try to improve perf by making N inferrable
Base.setindex!(v::Array{<:StaticBitVectorN}, val::Union{Number, AbstractString}, inds::Int...) =
     _setindex!(v, val, inds...)

function _setindex!(
    v::Array{StaticBitVectorN{T,N}}, val::Union{Number, AbstractString}, inds...) where {T, N}
    return setindex!(v, StaticBitVectorN{T,N}(val), inds...)
end

const _int_types = ((Symbol(pref, :Int, n) for n in (8, 16, 32, 64, 128) for pref in ("", "U"))...,)
for T in (_int_types..., :BigInt, :count_ones)
    @eval (Base.$T)(x::AbstractStaticBitVector) = ($T)(x.x)
end

function Base.count_zeros(x::AbstractStaticBitVector)
    num_unused_bits = (bitlength(x.x) - bitlength(x)) # unused are zeroed
    count_zeros(x.x) - num_unused_bits
end

# These are much faster than fallback methods
bit_count_zeros(x::AbstractStaticBitVector) = count_zeros(x)
bit_count_ones(x::AbstractStaticBitVector) = count_ones(x)

Base.Integer(x::AbstractStaticBitVector) = parent(x)

for op in (:xor, :(&), :(|), :(+), :(-), :(*)) # it is actually useful sometimes to do +,-
    @eval (Base.$op)(x::AbstractStaticBitVectorLen{T}, y::Real) where T = bits(($op)(x.x, T(y)), bitlength(x))
    @eval (Base.$op)(x::StaticBitVectorView{T}, y::Real) where T = bits(($op)(x.x, T(y)))
    @eval (Base.$op)(y::Real, x::StaticBitVectorView{T}) where T = bits(($op)(x, y))
    @eval (Base.$op)(y::StaticBitVectorView{T}, x::StaticBitVectorView{T}) where T = bits(($op)(x.x, y.x))
    @eval (Base.$op)(y::Real, x::AbstractStaticBitVectorLen{T}) where T = bits(($op)(x, y))
    @eval (Base.$op)(y::AbstractStaticBitVectorLen, x::AbstractStaticBitVectorLen) =
        (length(x) == length(y) || throw(DimensionMismatch()); bits(($op)(x.x, y.x), length(x)))
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

# TODO: reverse method for type T should be fundamental. For our type, built on top.
"""
    reverse(b::AbstractStaticBitVectorLen)

Return `b` with the bit order reversed.
"""
function Base.reverse(b::AbstractStaticBitVectorLen{T}) where T
    c = zero(T)
    for i in eachindex(b) # @inbounds means nothing here (yet)
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
    if v.x isa BigInt &&
        # Hmm. Not sure here about printing dots. Sometimes a `BigInt` has
        # just 64 bits
        (isa(v, StaticBitVector) || isa(v, StaticBitVectorView)) # TODO: not convinced about "infinite" num digits
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

"""
    bits([T], s::AbstractString; strip::Bool=false)

Convert the string `s` representing a binary number to a `StaticBitVector`. This method
can be used to convert the string representation of an `b::StaticBitVector` back to
`b`. So, it behaves like `Base.parse(::Int, x)` in that the bits in the string are in the
same order as the bits in `b`.

`T` should be an unsigned integer type: `UInt8`, `UInt256`, etc.  If `T` is not specified,
the smallest unsigned integer type capable of representing `s` will be used. If `T` is not
specified, the compiler cannot infer the output type which degrades performance.

Spaces, and the characters '>', '<' are stripped from `s` if `strip` is `true`.

# Examples
```jldoctest
julia> bits("00101")
<00101>

julia> bits("<01000000 11111111>"; strip=true)
<01000000 11111111>
```
"""
function bits(::Type{T}, ::Type{ST}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T <: Real, ST <: AbstractStaticBitVector}
    return _bits(s, _toVal(strip), T, ST)
end

"""
    bits(::Type{BitVector}, s::AbstractString; strip::BoolOrVal=Val(false))

Convert bitstring `s` to `BitVector`. Optionally strip characters other than `'0'` and `'1'` from `s`.
"""
bits(::Type{BitVector}, s::AbstractString; strip::BoolOrVal=Val(false)) = _bits(s, _toVal(strip), BitVector)
_bits(s::AbstractString, ::Val{false}, ::Type{BitVector}) = BitVector(bitvecview(s))
# TODO: could save allocation by iterating over good Char's rather than allocating with normalize_bitstring
# There are a few other uses of this. A function name might be normalize_bitstring_iter.
_bits(s::AbstractString, ::Val{true}, ::Type{BitVector}) = BitVector(bitvecview(normalize_bitstring(s)))

function bits(::Type{T}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T <: Real}
    return _bits(s, _toVal(strip), T, StaticBitVector)
end

# Don't convert string to bitstype, return a string, possibly stripped
bits(::Type{T}, s::AbstractString; strip::BoolOrVal=Val(false)) where {T<:AbstractString} =
    _bits(s, _toVal(strip), T)

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
    _bits(s, Val(false), BitIntegersX.min_uint_type(ncodeunits(s)), ST)

# Don't convert to bits type. If T==String, return string, unchanged or normalized.
_bits(s::AbstractString, ::Val{false}, ::Type{T}) where {T<:AbstractString} = T(s)
_bits(s::AbstractString, ::Val{true}, ::Type{T}) where {T<:AbstractString} = T(normalize_bitstring(s))

_bits(s::AbstractString, ::Val{false}, ::Type{T}, ::Type{ST}) where {T, ST} =
    __bits(parse_bin(T, s), ncodeunits(s), T, ST)

__bits(x::Real, Ndum, Tdum, Ty::Type{StaticBitVectorN{T, N}}) where {T, N} = Ty(x)
__bits(x::Real, N, T, Ty::Type{StaticBitVectorN}) = StaticBitVectorN{T, N}(x)
__bits(x::Real, N, T, ::Type{ST}) where {ST <: AbstractStaticBitVectorLen} = ST(x, N)

# wtf? needed to resolve ambiguous convert
Base.convert(::Type{StaticBitVectorView{T}}, x::StaticBitVectorView{T}) where T = x

# TODO: could use @checkbounds, etc. to allow @inbounds

# We use more general methods now
# bitgetindex(::Type{T}, x::T, bitinds) where {T<:AbstractStaticBitVector} = x[bitinds]
# bitgetindex(x::AbstractVector{Bool}, bitinds) = x[bitinds]

end  # module _StaticBitVectors

import ._StaticBitVectors: bits, StaticBitVector, StaticBitVectorView
export bits, StaticBitVector, StaticBitVectorView

end # module StaticBitVectors
