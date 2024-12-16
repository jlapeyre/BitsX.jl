module BitArraysX

export BitArrayX, BitVectorX, Chunks

struct Chunks{T<:Unsigned}
    chunks::Vector{T}
end
Base.length(chunks::Chunks) = length(chunks.chunks)

function _length_from_dims(dims)
    len = 1
    i = 1
    for d in dims
        d >= 0 || throw(ArgumentError("dimension size must be ≥ 0, got $d for dimension $i"))
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
struct BitArrayX{T<:Unsigned, N} <: AbstractArray{Bool, N}
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
        nc > length(chunks) && throw(DimensionMismatch("Length of chunks is too small"))
        nc > 0 && (chunks[end] = T(0))
        b = new(chunks, len, dims)
        # If we need to make this immutable, then do the following
        # Or maybe do Union{NTuple, Nothing}
#        N != 1 && (b.dims = dims)
        return b
    end
end

Base.length(b::BitArrayX) = b.len
Base.size(b::BitArrayX) = b.dims

const BitVectorX{T} = BitArrayX{T, 1} where T

BitArrayX{T}(::UndefInitializer, dims...) where {T<:Unsigned} = BitArrayX(T, undef, dims...)
BitArrayX(::Type{T}, ::UndefInitializer, dims::NTuple) where T = BitArrayX(T, undef, dims...)
BitArrayX(::Type{T}, ::UndefInitializer, dims::Integer...) where T = BitArrayX(T, undef, map(Int,dims))
BitArrayX(::Type{T}, ::UndefInitializer, dims::Int...) where T = BitArrayX{T, length(dims)}(undef, dims...)

# BitArrayX{N}(::UndefInitializer, dims::Integer...) where {N} = BitArrayX{N}(undef, map(Int,dims))::BitArrayX{N}
# BitArrayX(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArrayX{N}(undef, map(Int, dims)...)
# BitArrayX{N}(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArrayX{N}(undef, map(Int, dims)...)

function BitArrayX(chunks::Chunks{T}) where T
    len = length(chunks) * sizeof(T) * 8
    BitArrayX{T, 1}(chunks, len)
end

Base.IndexStyle(::Type{<:BitArrayX}) = Base.IndexLinear()

# TODO: I think with generated functions, these could be generated
# for any Unsigned, eg. types in BitIntegers
# These are defined for BitArray
for nbits in (8, 16, 32, 64, 128)
    divname = Symbol(:_div, nbits)
    modname = Symbol(:_mod, nbits)
    shiftn = Int(log2(nbits))
    T = Symbol(:UInt, nbits)
    @eval const $(Symbol(:_msk, nbits)) = ~$T(0)
    @eval @inline $divname(l) = l >> $shiftn
    @eval @inline _divX(::Type{$T}, l) = $divname(l)
    @eval @inline $modname(l) = l & $(nbits - 1)
    @eval @inline _modX(::Type{$T}, l) = $modname(l)
    # @inline _blsr(x)= x & (x-1) #zeros the last set bit. Has native instruction on many archs. needed in multidimensional.jl
    # @inline _msk_end(l::Int) = _msk64 >>> _mod64(-l)
    # @inline _msk_end(B::BitArray) = _msk_end(length(B))
    @eval @inline $(Symbol(:get_chunks_id_, nbits))(i::Int) = $divname(i-1)+1, $modname(i-1)
    #    @eval Base.getindex(b::BitArrayX{$T}, i::Integer) =
    # TODO: This can be moved out of eval, with bit of rewriting.
    # Types will be inferred and computation done at compiletime.
    @eval @inline function unsafe_bitgetindex(Bc::Vector{$T}, i::Int)
        i1, i2 = $(Symbol(:get_chunks_id_, nbits))(i)
        u = $T(1) << i2
        @inbounds r = (Bc[i1] & u) != 0
        return r
    end
end

function num_bit_chunks(::Type{T}, n::Int) where T
    _divX(T, n + (8 * sizeof(T) - 1))
end

Base.isassigned(B::BitArrayX, i::Int) = 1 <= i <= length(B)

@inline function Base.getindex(B::BitArrayX, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitgetindex(B.chunks, i)
end

###
### BitArrayAlt
###

struct BitArrayAlt{T<:Unsigned, N} <: AbstractArray{Bool, N}
    chunks::Vector{T}
    len::Int
    dims::NTuple{N, Int}

#     function BitArrayAlt{T, N}(::UndefInitializer, dims::Vararg{Int,N}) where {T, N}
#         len = _len_from_dims(dims)
#         nc = num_bit_chunks(T, len)
#         chunks = Vector{T}(undef, nc)
#         nc > 0 && (chunks[end] = T(0))
#         b = new(chunks, len, dims)
# #        N != 1 && (b.dims = dims)
#         return b
#     end

    function BitArrayAlt{T, N}(_chunks::Chunks{T}, dims::Vararg{Int,N}) where {T, N}
        num_bits = first(dims)
        chunks = _chunks.chunks
        len = _len_from_dims(dims)
        nc = num_bit_chunks(T, len)
        if nc > length(chunks)
            throw(DimensionMismatch("Length of chunks is too small"))
        end
        nc > 0 && (chunks[end] = T(0))
        b = new(chunks, len, dims)
#        N != 1 && (b.dims = dims)
        return b
    end
end


end # module BitArraysX
