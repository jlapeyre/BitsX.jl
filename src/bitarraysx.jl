module BitArraysX

export AbstractBitArray, AbstractBitMatrix, AbstractBitVector,
    BitArrayX, BitVectorX, BitMatrixX,
    BitArrayAlt, BitVectorAlt, BitMatrixAlt,
    Chunks

module _BitArraysX

bitsizeof(::Type{T}) where T = sizeof(T) * 8

###
### AbstractBitArray
###

# The type parameters are a bit unusual, `T` refers to the backing storage
# type. The eltype is always `Bool`.
abstract type AbstractBitArray{T, N} <: AbstractArray{Bool, N} end

# """
#     AbstractBitVector{T}

# Supertype for one-dimensional arrays (or array-like types) with
# elements of type `T`. Alias for [`AbstractBitArray{T,1}`](@ref).
# """
const AbstractBitVector{T} = AbstractBitArray{T,1}

# """
#     AbstractBitMatrix{T}

# Not a good doc string.
# Supertype for two-dimensional arrays (or array-like types) with
# elements of type `T`. Alias for [`AbstractBitArray{T,2}`](@ref).
# """
const AbstractBitMatrix{T} = AbstractBitArray{T,2}

@inline function unsafe_bitgetindex(B::AbstractBitArray{T}, i::Int) where T
    chunk_ind, bit_in_chunk_ind = get_chunks_id(B, i)
    u = T(1) << bit_in_chunk_ind
    Bc = B.chunks
    @inbounds r = (Bc[chunk_ind] & u) != 0
    return r
end

@inline function Base.getindex(B::AbstractBitArray, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitgetindex(B, i)
end

@inline function unsafe_bitsetindex!(B::AbstractBitArray, x::Bool, i::Int)
    i1, i2 = get_chunks_id(B, i)
    _unsafe_bitsetindex!(B, x, i1, i2)
end

@inline function Base.setindex!(B::AbstractBitArray, x, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitsetindex!(B, convert(Bool, x), i)
    return B
end

@inline function _unsafe_bitsetindex!(B::AbstractBitArray{T}, x::Bool, i1::Int, i2::Int) where T
    u = T(1) << i2
    Bc = B.chunks
    @inbounds begin
        c = Bc[i1]
        Bc[i1] = ifelse(x, c | u, c & ~u)
    end
end

# Test whether the given array has a value associated with index i.
# Return false if the index is out of bounds, or has an undefined reference.
Base.isassigned(B::AbstractBitArray, i::Int) = 1 <= i <= length(B)

###
### Chunks
###

# """
#     Chunks{T<:Unsigned}

# Chunks is a wrapper used to pass data to constructors of `AbstractBitArray`
# that is intended to be used directly as "chunks". If an un-wrapped array is
# passed it is assumed to be interpreted as an array of `Bool`s.

# People have discussed constructing `Base.BitArray` by passing chunks. Something
# like `Chunk` would be useful there, to distinguish clearly from existing methods.
# """
struct Chunks{T<:Unsigned}
    chunks::Vector{T}
end
Base.length(chunks::Chunks) = length(chunks.chunks)

# Compute length from dimensions, checking that they are non-negative.
function _length_from_dims(dims)
    len = 1
    i = 1
    for d in dims
        d >= 0 || throw(ArgumentError(lazy"dimension size must be ≥ 0, got $d for dimension $i "))
        len *= d
        i += 1
    end
    len
end

"""
   BitArrayX{T<:Unsigned, N} <: AbstractArray{Bool, N}

A partial reimplementation of `BitArray` for chunks of type `T` other
than `UInt64`.

`BitArraysX` is meant to get some of the basic functionality of `BitArray`.
The motivating use case is wrapping existing chunk data.
"""
struct BitArrayX{T<:Unsigned, N} <: AbstractBitArray{T, N}
    chunks::Vector{T}
    len::Int
    dims::NTuple{N, Int}

    function BitArrayX{T, N}(::UndefInitializer, dims::Vararg{Int,N}) where {T, N}
        len = _length_from_dims(dims)
        nc = num_bit_chunks(T, len)
        chunks = Vector{T}(undef, nc)
        nc > 0 && (chunks[end] = T(0))
        b = new(chunks, len, dims)
#        N != 1 && (b.dims = dims)
        return b
    end

    function BitArrayX{T, N}(_chunks::Chunks{T}, dims::Vararg{Int,N}) where {T, N}
        chunks = _chunks.chunks
        len = _length_from_dims(dims)
        nc = num_bit_chunks(T, len)
        nc > length(chunks) && throw(DimensionMismatch(lazy"Length of chunks $(length(chunks)) is smaller than required $nc"))
        nc > 0 && (chunks[end] = T(0))
        b = new(chunks, len, dims)
        # If we need to make this immutable, then do the following.
        # Or maybe do Union{NTuple, Nothing}
#        N != 1 && (b.dims = dims)
        return b
    end
end

const BitVectorX{T} = BitArrayX{T, 1} where T
const BitMatrixX{T} = BitArrayX{T, 2} where T

Base.length(b::AbstractBitArray) = b.len
Base.size(b::AbstractBitArray) = b.dims

# This will be useful if BitArrayX becomes mutable
Base.size(b::AbstractBitVector) = (b.len,)

@inline function Base.size(B::AbstractBitVector, d::Integer)
    d < 1 && throw_boundserror(size(B), d)
    ifelse(d == 1, B.len, 1)
end

BitArrayX{T}(::UndefInitializer, dims...) where {T<:Unsigned} = BitArrayX(T, undef, dims...)
BitArrayX(::Type{T}, ::UndefInitializer, dims::NTuple) where T = BitArrayX(T, undef, dims...)
BitArrayX(::Type{T}, ::UndefInitializer, dims::Integer...) where T = BitArrayX(T, undef, map(Int,dims))
BitArrayX(::Type{T}, ::UndefInitializer, dims::Int...) where T = BitArrayX{T, length(dims)}(undef, dims...)

# TODO: these involve both T and N. Easy to fix. Add T
# BitArrayX{N}(::UndefInitializer, dims::Integer...) where {N} = BitArrayX{N}(undef, map(Int,dims))::BitArrayX{N}
# BitArrayX{N}(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArrayX{N}(undef, map(Int, dims)...)
# BitArrayX{N}(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArrayX{N}(undef, map(Int, dims)...)

function BitArrayX(chunks::Chunks{T}) where T
    len = length(chunks) * bitsizeof(T)
    BitArrayX{T, 1}(chunks, len)
end

Base.IndexStyle(::Type{<:BitArrayX}) = Base.IndexLinear()

@inline function get_chunks_id(::BitArrayX{T}, i::Int) where T
    (_divX(T, i-1) + 1, _modX(T, i - 1))
end

"""
    log2nbits(::Type{T}) where T

Return base-2 log of the number of bits in the representation of the type `T`.

The value is likely only meaningful for primitive types `T`.
The returned value is compiled constant for each `T`.
"""
@inline @generated function log2nbits(::Type{T}) where T
    :(Int(log2(bitsizeof($T))))
end

# Integer-divide l by bitsize of `T`
@inline _divX(::Type{T}, l) where {T<:Unsigned} =  l >> log2nbits(T)

# Return the value of type `T` with all bits set
@inline get_msk(::Type{T}) where {T<:Unsigned} = ~T(0)

# Return l mod bitsizeof(T)
@inline _modX(::Type{T}, l) where {T<:Unsigned} = l & (bitsizeof(T) - 1)

# Return a mask with the lower `l` bits set
@inline _msk_end(::Type{T}, l::Int) where {T<:Unsigned} = get_msk(T) >>> _modX(T, -l)

# Return a mask for the coding bits in the final chunk in `B`.
# For example for UInt64 and `length(B)` equal 67, only the three lowest
# bits in the second (and final) chunk encode data.
@inline _msk_end(B::BitArrayX{T}) where T = _msk_end(T, length(B))

"""
    num_bit_chunks(::Type{T}, n::Int) where T

Return the number of elements of type `T` are needed to represent `n` bits.
"""
function num_bit_chunks(::Type{T}, n::Int) where T
    _divX(T, n + bitsizeof(T) - 1)
end

## TODO: If this is needed, it could be made generic
## Perf tests show that this iterator is not needed. But I copied the form from Base.BitArray
## custom iterator ##
function Base.iterate(B::BitArrayX{T}, i::Int=0) where {T}
    i >= length(B) && return nothing
    (B.chunks[_divX(T, i)+1] & (T(1)<<_modX(T, i)) != 0, i+1)
end

## Indexing: setindex! ##

Base.@propagate_inbounds function setindex!(B::BitArrayX, X::AbstractArray, J0::Union{Colon,AbstractUnitRange{Int}})
    _setindex!(IndexStyle(B), B, X, to_indices(B, (J0,))[1])
end

@inline function Base.setindex!(B::BitArrayX{T}, X::AbstractArray, I::BitArrayX{T}) where {T}
    @boundscheck checkbounds(B, I)
    _unsafe_setindex!(B, X, I)
end

# The following does not work for BitArrayAlt without some modification.
# Quick perf test shows that this is also faster for, say UInt8 and UInt32
# than using fallback in abstractarrays.jl.
# Modfied from Base
# # Assigning an array of bools is more complicated, but we can still do some
# # work on chunks by combining X and I 64 bits at a time to improve perf by ~40%
function _unsafe_setindex!(B::BitArrayX{T}, X::AbstractArray, I::BitArrayX{T}) where T
    Bc = B.chunks
    Ic = I.chunks
    length(Bc) == length(Ic) || throw_boundserror(B, I)
    lc = length(Bc)
    lx = length(X)
    last_chunk_len = _modX(T, length(B)-1)+1

    Xi = first(eachindex(X))
    lastXi = last(eachindex(X))
    for i = 1:lc
        @inbounds Imsk = Ic[i]
        @inbounds C = Bc[i]
        u = T(1)
        for j = 1:(i < lc ? bitsizeof(T) : last_chunk_len)
            if Imsk & u != 0
                Xi > lastXi && Base.throw_setindex_mismatch(X, count(I))
                @inbounds x = convert(Bool, X[Xi])
                C = ifelse(x, C | u, C & ~u)
                Xi = nextind(X, Xi)
            end
            u <<= 1
        end
        @inbounds Bc[i] = C
    end
    if Xi != nextind(X, lastXi)
        Base.throw_setindex_mismatch(X, count(I))
    end
    return B
end

## Unless the following are implemented correctly, to some level of completeness
## the the interface will be broken.
##
# ## similar, fill!, copy! etc ##

# Base.similar(B::BitArrayX{T}) where {T} = BitArrayX{T}(undef, size(B))
# Base.similar(B::BitArrayX{T}, dims::Int...) where {T} = BitArrayX{T}(undef, dims)
# Base.similar(B::BitArrayX{T}, dims::Dims) where {T} = BitArrayX{T}(undef, dims...)

# Base.similar(B::BitArrayX{V}, T::Type{Bool}, dims::Dims) where {V} = BitArrayX{V}(undef, dims)
# # changing type to a non-Bool returns an Array
# # (this triggers conversions like float(bitvector) etc.)
# Base.similar(B::BitArrayX{V}, T::Type, dims::Dims) where V = Array{T}(undef, dims)

# function Base.fill!(B::BitArrayX{T}, x) where {T}
#     y = convert(Bool, x)
#     isempty(B) && return B
#     Bc = B.chunks
#     if !y
#         fill!(Bc, 0)
#     else
#         fill!(Bc, get_msk(T))
#         Bc[end] &= _msk_end(B)
#     end
#     return B
# end

###
### BitArrayAlt
###

struct BitArrayAlt{T<:Unsigned, N} <: AbstractBitArray{T, N}
    chunks::Vector{T}
    len::Int
    dims::NTuple{N, Int}

    function BitArrayAlt{T, N}(_chunks::Chunks{T}, dims::Vararg{Int,N}) where {T, N}
        num_bits = first(dims)
        chunks = _chunks.chunks
        len = _length_from_dims(dims)
        nc = num_bit_chunks(T, len)
        if nc > length(chunks)
            throw(DimensionMismatch(lazy"Length of chunks $(length(chunks)) is smaller than required $nc"))
        end
        # We don't want to zero the last chunk. Because this clobbers data.
        return new(chunks, len, dims)
        # This struct is currently immutable
        # So we can't make the following optimization
        # N != 1 && (b.dims = dims)
    end
end

const BitVectorAlt{T} = BitArrayAlt{T, 1} where T
const BitMatrixAlt{T} = BitArrayAlt{T, 2} where T

Base.IndexStyle(::Type{<:BitArrayAlt}) = Base.IndexLinear()

function BitArrayAlt(chunks::Chunks{T}) where T
    len = length(chunks) * bitsizeof(T)
    BitArrayAlt{T, 1}(chunks, len)
end

BitArrayAlt(chunks::Chunks, dims::NTuple) = BitArrayAlt(chunks, dims...)
BitArrayAlt(chunks::Chunks{T}, dims::Integer...) where T = BitArrayAlt(chunks, map(Int, dims)...)

function BitArrayAlt(chunks::Chunks{T}, dims::Int...) where T
    BitArrayAlt{T, length(dims)}(chunks, dims...)
end

# Return the number of elements of type T needed to encode n_coding_bits.
# For example if T=UInt8, and n_coding_bits = 21, then the results is
# three. That is three UInt8's per block
function _get_block_size(::Type{T}, n_coding_bits) where {T<:Unsigned}
    a, b = divrem(n_coding_bits, bitsizeof(T))
    block_size = iszero(b) ? a : a + 1
    block_size
end

# `blk_ind` is the block number starting with zero
# `blk_bit` is the bit position within the block
# `n_coding_bits` is the number of encoding bits (used bits) per block.
#   For example: If `n_coding_bits = 21`, and `T=UInt8`, then a block
#   is three UInt8s, or 24 bits. The first three bits per block are unused.
# Data is a `Vector{T}`.
# Data is conceptually paritioned into "blocks" of `block_size` elements each.
# The length in bits of a block is `block_size * bitsizeof(T)`
# Logically, we want blocks of size `n_coding_bits` bits. But we have to pad this
# to `block_size * bitsizeof(T)` in general.
function get_chunks_id(B::BitArrayAlt{T}, i) where T

    n_coding_bits = first(size(B))
    block_size = _get_block_size(T, n_coding_bits)
    bitsz = bitsizeof(T)

    # Blocks are sequences of elements of type T, block_size elements in length.
    # blk_ind is block number. It's 0 for first block
    #         (ie it is an offset)
    # blk_bit is the bit pos within the block given by blk_ind
    (blk_ind, blk_bit) = divrem(i - 1, n_coding_bits)

    # Unused bits are at the beginning of the block. If they were at the end, we'd omit this.
    blk_bit += (block_size * bitsz - n_coding_bits)

    # elem_ind is the index of the element within the block
    # elem_bit is the index of the bit within element given by elem_ind.
    # These both also start at zero.
    (elem_ind, elem_bit) = divrem(blk_bit, bitsz)

    blk = block_size * blk_ind
    elem_ind_in_chunks = blk + elem_ind + 1
    (elem_ind_in_chunks, elem_bit)
end

end #module _BitArraysX

import ._BitArraysX: AbstractBitArray, AbstractBitMatrix, AbstractBitVector,
    BitArrayX, BitVectorX, Chunks, BitArrayAlt, BitVectorX, BitVectorAlt,
    BitMatrixAlt, BitMatrixX

"""
    Chunks{T<:Unsigned}

Chunks is a wrapper used to pass data to constructors of `AbstractBitArray`
that is intended to be used directly as "chunks". If an un-wrapped array is
passed it is assumed to be interpreted as an array of `Bool`s.

People have discussed constructing `Base.BitArray` by passing chunks. Something
like `Chunk` would be useful there, to distinguish clearly from existing methods.
"""
Chunks


end # module BitArraysX
