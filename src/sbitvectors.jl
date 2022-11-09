###
### SBitArray{N, M}
###

## Unfinished. In fact, barely started
##
## This is largely a copy of BitArray, but with Tuple's for storage. There is already at least one other package trying to do this. It should probably also
## store the dimensions as type information as StaticArrays.jl does.
##
## It's not clear what kind of performance gains we can get. I see substantial improvement
## in iterating over bits. But not enough that I want to pursue finishing this, as that would
## be a big job. However, the benchmarks I tried so far did not include storing dimensions in
## type parameters.
##
## Compare this type to the types in staticbitvectors.jl, which stores the data in wide unsigned
## integer types supported by LLVM that are exposed in Julia via BitIntegers.jl.
## I was not able to get the performance from these types that I would expect from using Tuples
## of UInt64 as we do here.

# We actually want to encode the dimensions as parameters
struct SBitArray{N, M} <: AbstractArray{Bool, N}
    chunks::NTuple{M, UInt64}
    dims::NTuple{N, Int}
end

const SBitVector = SBitArray{1}
const SBitMatrix = SBitArray{2}

@inline function unsafe_bitgetindex(Bc::NTuple{M, UInt64}, i::Int) where {M}
    i1, i2 = Base.get_chunks_id(i)
    u = UInt64(1) << i2
    @inbounds r = (Bc[i1] & u) != 0
    return r
end

@inline function Base.getindex(B::SBitArray, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitgetindex(B.chunks, i)
end

@inline Base.length(B::SBitArray{<:Any,M}) where M = M
@inline Base.size(B::SBitVector) = (length(B),)
@inline Base.size(B::SBitArray) = B.dims

## custom iterator ##
@inline function Base.iterate(B::SBitArray, i::Int=0)
    i >= length(B) && return nothing
    (@inbounds(B.chunks[Base._div64(i)+1]) & (UInt64(1) << Base._mod64(i)) != 0, i+1)
end
