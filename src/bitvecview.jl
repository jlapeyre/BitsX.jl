###
### BitArrayView
###

const _DEFAULT_CHECK = true

"""
    BitArrayView{V, N, T} <: AbstractArray{V, N}

View an object of type `T` as an `AbstractArray{V, N}`
where `V` is the eltype and `N` the number of dimensions.
"""
struct BitArrayView{V, N, T} <: AbstractArray{V, N}
    s::T
    dims::NTuple{N, Int}

    # TODO: could relax restriction on s, and rely on duck typing
    function BitArrayView{V}(s::Union{AbstractString, Real}, len=bitlength(s); check::Bool=_DEFAULT_CHECK) where V
        check && isa(s, AbstractString) && is_bitstring(s; throw=true)
        dims = (len,)
        return new{V, length(dims), typeof(s)}(s, dims)
    end

    function BitArrayView{V, N}(s::Union{AbstractString, Real}, dims; check::Bool=_DEFAULT_CHECK) where {V, N}
        check && isa(s, AbstractString) && is_bitstring(s; throw=true)
        bitlength(s) >= prod(dims) || throw(DimensionMismatch("Input string length to small for array view"))
        N == length(dims) || throw(DimensionMismatch("dims don't match dimension"))
        return new{V, N, typeof(s)}(s, dims)
    end
end

const BitVectorView{V, T} = BitArrayView{V, 1, T}
const BitMatrixView{V, T} = BitArrayView{V, 2, T}

BitArrayView(s::AbstractString; check=_DEFAULT_CHECK) = BitArrayView{Bool}(s; check=check)

@inline Base.parent(bv::BitArrayView) = bv.s

# This allows fast collect, copy, etc.
# Calls: collect -> copyto! -> unalias -> mightalias -> dataids
Base.dataids(bv::BitArrayView) = Base.dataids(parent(bv))

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
bitvecview(str::AbstractString; check=_DEFAULT_CHECK) = BitArrayView(str; check=check)
bitvecview(::Type{V}, str::AbstractString; check=_DEFAULT_CHECK) where V =
    BitArrayView{V, 1}(str; check=check)

bitvecview(x::Real, args...) = bitvecview(Bool, x, args...)
bitvecview(::Type{V}, x::Real, args...) where V = BitArrayView{V}(x, args...)

"""
    bitmatview([::Type{V} = Bool], str, [ncols::Integer = isqrt(bitlength(str))]; kwargs...)

Return a view of the bitstring `str` as a matrix, a `BitMatrixView{V, T} <: AbstractMatrix{T}`.

If `ncols` is supplied, the number of rows will be the largest compatible with the length of `str`.
On construction `str` will be validated as a bitstring. If the keyword arg `check=false` is passed,
then no such check is made.
"""
bitmatview(args...; kwargs...) = bitmatview(Bool, args...; kwargs...)

function bitmatview(::Type{V}, data; kwargs...) where V
    n = isqrt(bitlength(data))
    return BitArrayView{V, 2}(data, (n, n); kwargs...)
end

function bitmatview(::Type{V}, data, ncols::Integer; kwargs...) where V
    nrows = div(bitlength(data), ncols)
    return BitArrayView{V, 2}(data, (ncols, nrows); kwargs...)
end

function bitmatview(::Type{V}, data, dims; kwargs...) where V
    return BitArrayView{V, 2}(data, dims; kwargs...)
end

bitarrview(z, dims; kwargs...) = bitarrview(Bool, z, dims; kwargs...)
bitarrview(::Type{V}, z, dims; kwargs...) where V = BitArrayView{V, length(dims)}(z, dims; kwargs...)

@inline Base.size(bs::BitArrayView{<:Any, N, V}) where {N, V<:Real} = bs.dims
@inline Base.size(bs::BitArrayView) = bs.dims # (ncodeunits(parent(bs)),)
@inline Base.IndexStyle(::BitArrayView) = Base.IndexLinear()

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V}, i::Integer) where {T, V<:AbstractString}
    @boundscheck checkbounds(bs, i)
    @inbounds is_one_char(codeunit(parent(bs), i)) % T
end

@inline function Base.getindex(bs::BitArrayView{T, <:Any, V}, i::Integer) where {T, V<:Real}
    @boundscheck checkbounds(bs, i)
    @inbounds bitgetindex(parent(bs), i) % T
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

# TODO: do reverse in the wrapper, not the parent object
for func in (:reverse, :bitreverse)
    @eval Base.$(func)(bs::BitArrayView, args...) = BitArrayView(Base.$(func)(parent(bs)), args...)
end
