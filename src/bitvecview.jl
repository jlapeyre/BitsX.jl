###
### BitArrayView
###

const _DEFAULT_CHECK = true

"""
    BitArrayView{V, N, T} <: AbstractArray{V, N}

View an object of type `T` as an `AbstractArray{V, N}`
where `V` is the eltype and `N` the number of dimensions.
"""
struct BitArrayView{V, N, T, ET} <: AbstractArray{V, N}
    s::T
    dims::NTuple{N, Int}

    # TODO: could relax restriction on s, and rely on duck typing
    function BitArrayView{V, ET}(s::Union{AbstractString, Real}, len=bitlength(s); check::Bool=_DEFAULT_CHECK) where {V, ET}
        check && isa(s, AbstractString) && is_bitstring(s; throw=true)
        dims = (len,)
        return new{V, length(dims), typeof(s), ET}(s, dims)
    end

    function BitArrayView{V, N, ET}(s::Union{AbstractString, Real}, dims; check::Bool=_DEFAULT_CHECK) where {V, N, ET}
        check && isa(s, AbstractString) && is_bitstring(s; throw=true)
        bitlength(s) >= prod(dims) || throw(DimensionMismatch("Input string length to small for array view"))
        N == length(dims) || throw(DimensionMismatch("dims don't match dimension"))
        return new{V, N, typeof(s), ET}(s, dims)
    end
end

const BitVectorView{V, T} = BitArrayView{V, 1, T}
const BitMatrixView{V, T} = BitArrayView{V, 2, T}

#BitArrayView(s::AbstractString); check=_DEFAULT_CHECK) = BitArrayView{Bool}(s, Val(ET); check=check)
BitArrayView(s::AbstractString, ::Val{ET}=Val(ET); check=_DEFAULT_CHECK) where {ET} = BitArrayView{Bool, ET}(s; check=check)
BitArrayView(s::AbstractString, ET::Bool; check=_DEFAULT_CHECK) = BitArrayView{Bool, ET}(s; check=check)

@inline Base.parent(bv::BitArrayView) = bv.s

# This allows fast collect, copy, etc.
# Calls: collect -> copyto! -> unalias -> mightalias -> dataids
Base.dataids(bv::BitArrayView) = Base.dataids(parent(bv))


"""
    is_little_endian(bv::BitArrayView)

Return `true` if the bits in the representation `bv` of `parent(bv)`, where the latter is
a bitsttype, have the least significant bit at index `1`.

For other wrapped types, such as `String`, return `true` if the indexing is not reversed.
"""
is_little_endian(::BitArrayView{<:Any, <:Any, <:Any, ET}) where ET = ET

"""
    bitvecview([::Type{T} = Bool], str::AbstractString; check=true)

Return a view of the bitstring `str` as an `AbstractVector{T}`.

No data is copied. If `check` is `true`, then `str` is validated
upon construction. A valid `str` must consist of only `'0'` and `'1'`.
Passing `false` for `check` with invalid `str` will likely give
incorrect results, silently.

If you instead convert `str` to `Vector{Bool}`, construction may take longer,
but accessing will be faster. So `bitvecview` might be more useful if
you want to do only a few operations.

To convert `str` to a `Vector{T}` call `collect` or `copy` on the returned view of `str`.
Likewise `Tuple` and `BitVector` can be used to convert to the corresponding types.
"""
bitvecview(str::AbstractString, ::Val{ET}=Val(false); check=_DEFAULT_CHECK) where ET = BitArrayView{Bool, 1, ET}(str, (bitlength(str),); check=check)
bitvecview(str::AbstractString, ET::Bool; check=_DEFAULT_CHECK) = BitArrayView{Bool, 1, ET}(str, (bitlength(str),); check=check)

#bitvecview(str::AbstractString; check=_DEFAULT_CHECK) = BitArrayView{Bool, 1, false}(str, (bitlength(str),); check=check)
# No difference in efficiency
#bitvecview(str::AbstractString; check=_DEFAULT_CHECK) = BitArrayView(str; check=check)

bitvecview(::Type{V}, str::AbstractString, ::Val{ET}=Val(false); check=_DEFAULT_CHECK) where {V, ET} =
    BitArrayView{V, 1, ET}(str, (bitlength(str),); check=check)

#bitvecview(x::Real, et::Val{ET}=Val(false)) where ET = bitvecview(Bool, x, bitlength(x), et)
bitvecview(x::Real, len::Integer=bitlength(x), et::Val{ET}=Val(false)) where ET = bitvecview(Bool, x, len, et)
bitvecview(::Type{V}, x::Real, len::Integer=bitlength(x), ::Val{ET}=Val(false)) where {V, ET} = BitArrayView{V, 1, ET}(x, (len,))

"""
    bitmatview([::Type{V} = Bool], str, [ncols::Integer = isqrt(bitlength(str))]; kwargs...)

Return a view of the bitstring `str` as a matrix, a `BitMatrixView{V, T} <: AbstractMatrix{T}`.

If `ncols` is supplied, the number of rows will be the largest compatible with the length of `str`.
On construction `str` will be validated as a bitstring. If the keyword arg `check=false` is passed,
then no such check is made.
"""
bitmatview(args...; kwargs...) = bitmatview(Bool, args...; kwargs...)

function bitmatview(::Type{V}, data, ::Val{ET}=Val(false); kwargs...) where {V, ET}
    n = isqrt(bitlength(data))
    return BitArrayView{V, 2, ET}(data, (n, n); kwargs...)
end

function bitmatview(::Type{V}, data, ncols::Integer, ::Val{ET}=Val(false); kwargs...) where {V, ET}
    nrows = div(bitlength(data), ncols)
    return BitArrayView{V, 2, ET}(data, (ncols, nrows); kwargs...)
end

function bitmatview(::Type{V}, data, dims, ::Val{ET}=Val(false); kwargs...) where {V, ET}
    return BitArrayView{V, 2, ET}(data, dims; kwargs...)
end

bitarrview(z, dims; kwargs...) = bitarrview(Bool, z, dims; kwargs...)
bitarrview(::Type{V}, z, dims, ::Val{ET}=Val(ET); kwargs...) where {V, ET} = BitArrayView{V, length(dims), ET}(z, dims; kwargs...)

@inline Base.size(bs::BitArrayView{<:Any, N, V}) where {N, V<:Real} = bs.dims
@inline Base.size(bs::BitArrayView) = bs.dims # (ncodeunits(parent(bs)),)
@inline Base.IndexStyle(::BitArrayView) = Base.IndexLinear()

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V, false}, i::Integer) where {T, V<:AbstractString}
    @boundscheck checkbounds(bs, i)
    @inbounds is_one_char(codeunit(parent(bs), i)) % T
end

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V, true}, i::Integer) where {T, V<:AbstractString}
    ir = bitlength(parent(bs)) - i + 1
    @boundscheck checkbounds(bs, ir)
    @inbounds is_one_char(codeunit(parent(bs), ir)) % T
end

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V, false}, i::Integer) where {T, V<:Real}
    @boundscheck checkbounds(bs, i)
    @inbounds bitgetindex(parent(bs), i) % T
end

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V, true}, i::Integer) where {T, V<:Real}
    ir = bitlength(parent(bs)) - i + 1
    @boundscheck checkbounds(bs, ir)
    @inbounds bitgetindex(parent(bs), ir) % T
end

@inline Base.sizeof(s::BitArrayView{V}) where V = length(s) * sizeof(V) # should this always be UInt8 ?
@inline Base.elsize(::Type{<:BitArrayView{V}}) where V = sizeof(V)

@inline function Base.iterate(bs::BitArrayView, i=1)
    if (i % UInt) - 1 < length(bs)
        (@inbounds bs[i], i + 1)
    else
        nothing
    end
end

# Avoids a slow fallback with `sum`
Base._count(::typeof(identity), B::BitArrayView, ::Colon, init) = init + sum(isone, B)

# This returns an AbstractString not neccessarily a String. Probably want a String
Base.String(bs::BitArrayView{<:Any, <:Any, <:AbstractString}) = parent(bs)
Base.String(bs::BitArrayView{<:Any, <:Any, <:Real}) = String(bitstringview(parent(bs)))

# Disable these. Not the correct thing and not tested.
# TODO: do reverse in the wrapper, not the parent object
# for func in (:reverse, :bitreverse)
#     @eval Base.$(func)(bs::BitArrayView, args...) = BitArrayView(Base.$(func)(parent(bs)), args...)
# end

# for func in (:reverse, :bitreverse)
#     @eval Base.$(func)(bs::BitVectorView, args...) = BitArrayView(Base.$(func)(parent(bs)), args...)
# end
