"""
    BitArrays

This module implements a few functions for constructing `Base.BitArray` objects.
"""
module BitArrays

using ..BitsX.BitsBase: asint, bitlength, bitsize, bitsizeof

export bitvector, bitarray, bitarray!

"""
    bitvector(x::Union{Integer, Base.IEEEFloat})

Return a `BitVector` with the bits in `x`.

# Examples
```jldoctest
julia> (bitvector(UInt8(11)), )
(Bool[1, 1, 0, 1, 0, 0, 0, 0],)

julia> bstring(bitvector(UInt64(1<<32 -1)))
"1111111111111111111111111111111100000000000000000000000000000000"

julia> typeof(bitvector(UInt64(1<<32 -1)))
BitVector (alias for BitArray{1})
```
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

bitarray(x::Union{Integer, Base.IEEEFloat}, dims::Tuple) = bitarray(x, dims...)

"""
    bitarray(x::Union{Integer, Base.IEEEFloat}, [dims])

Return a `BitArray` from the bits in `x`.

# Examples
```jldoctest
julia> (bitarray(typemax(UInt16)),)
(Bool[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],)

julia> bitarray(typemax(UInt16), (2, 8))
2×8 BitMatrix:
 1  1  1  1  1  1  1  1
 1  1  1  1  1  1  1  1
```
"""
function bitarray(x::Union{Integer, Base.IEEEFloat}, dims::Int...)
    if isempty(dims)
        dims = (bitsize(x),)
    else
        prod(dims) <= bitlength(x) ||
            throw(DimensionMismatch(lazy"Storage bits $(bitlength(x)) too small for dims $dims"))
    end
    _bits = BitArray(undef, dims...)
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

"""
    bitarray!(vector::AbstractVector{<:Unsigned}, [dims]) where {T}

Return a `BitArray` with dimensions `dims` using `vector` as `chunks`.

If `vector` is not a `Vector`, it will be copied. If `vector` is a `Vector{T}`
then it will be reinterpreted to `T  == UInt64` after possible padding.

# Examples
```jldoctest
julia> (bitarray!([UInt8(1), UInt8(2)]),)
(Bool[1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],)
```
"""
bitarray!(vector::AbstractVector{UInt64}, dims::Tuple) = bitarray!(vector, dims...)
function bitarray!(vector::AbstractVector{UInt64}, dims::Int...)
    # The type of Tuple `dims` may not be changed. So we have to start with the desired
    # shape. However, the constructor also allocates chunks to accommodate the dims.  We
    # choose dims of the correct shape but as small as possible because we will throw
    # these chunks away and replaces them with `vector`.
    nbits = length(vector) * 64
    if isempty(dims)
        dims = (nbits,)
    else
        prod(dims) <= nbits ||
            throw(DimensionMismatch(lazy"Storage bits $nbits too small for dims $dims"))
    end
    bit_array = BitArray(undef, ones(Int, length(dims))...)
    bit_array.chunks = vector
    # The internal constructor of BitArray does not set dims if one dimensional.
    # We do the same.
    length(dims) > 1 && (bit_array.dims = dims)
    bit_array.len = prod(dims)

    # This masking is probably necessary for length of `bit_array` less than the number of
    # bits in `chunks`.
    bit_array.chunks[end] &= Base._msk_end(bit_array)
    bit_array
end

bitarray!(vector::AbstractVector{T}, dims::Integer...) where {T <: Unsigned} =
    bitarray!(vector, dims)

function bitarray!(vector::AbstractVector{T}, dims=(length(vector) * bitsizeof(T),)) where {T <: Unsigned}
    nbits = length(vector) * bitsizeof(T)
    prod(dims) <= nbits ||
        throw(DimensionMismatch(lazy"Storage bits $nbits too small for dims $dims"))
    width = sizeof(T)
    # Pad the Vector with up to `npad` elements so that it can be reinterpreted as `Vector{UInt64}`
    if width < 8
        npad = div(8, width)
        excess = rem(length(vector), npad)
        if !iszero(excess)
            elements_needed = npad - excess
            for _ in 1:elements_needed
                push!(vector, zero(T))
            end
        end
    end

    vector64 = reinterpret(UInt64, vector)
    # `vector64` is a wrapper type with supertype `AbstractVector{UInt64}`.
    # It will be copied implicitly in the following call.
    bitarray!(vector64, dims)
end

# From Scott Jones on https://discourse.julialang.org/t/i-have-vector-uint8-i-need-bitvector/2286/5
# function make_bitvector(v::Vector{UInt8})
#     siz = sizeof(v)
#     bv = falses(siz<<3)
#     unsafe_copy!(reinterpret(Ptr{UInt8}, pointer(bv.chunks)), pointer(v), siz)
#     bv
# end

# function make_bitvector(v::Vector{UInt8}, dim::Integer)
#     siz = sizeof(v)
#     (((dim + 63) >>> 6) << 3) < siz && error("$dim too small for size $siz vector")
#     bv = falses(dim)
#     unsafe_copy!(reinterpret(Ptr{UInt8}, pointer(bv.chunks)), pointer(v), siz)
#     bv
# end

end # module BitArrays
