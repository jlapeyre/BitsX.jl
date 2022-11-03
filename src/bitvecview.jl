###
### BitStringVector{T<:AbstractString}
###

const _DEFAULT_CHECK = true

struct BitStringVector{V, T<:AbstractString} <: AbstractVector{V}
    s::T
    function BitStringVector{V}(s::AbstractString; check::Bool=_DEFAULT_CHECK) where V
        check && is_bitstring(s; throw=true)
        return new{V, typeof(s)}(s)
    end
end

BitStringVector(s::AbstractString; check=_DEFAULT_CHECK) = BitStringVector{Bool}(s; check=check)

@inline Base.parent(bv::BitStringVector) = bv.s

# This allows fast collect, copy, etc.
# Calls: collect -> copyto! -> unalias -> mightalias -> dataids
Base.dataids(bv::BitStringVector) = Base.dataids(parent(bv))

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
bitvecview(str::AbstractString; check=_DEFAULT_CHECK) = BitStringVector(str; check=check)
bitvecview(::Type{V}, str::AbstractString; check=_DEFAULT_CHECK) where V =
    BitStringVector{V}(str; check=check)

@inline Base.size(bs::BitStringVector) = (ncodeunits(parent(bs)),)
@inline Base.IndexStyle(::BitStringVector) = Base.IndexLinear()

@inline function Base.getindex(bs::BitStringVector{V}, i::Integer) where V
    @boundscheck checkbounds(bs, i)
    @inbounds is_one_char(codeunit(parent(bs), i)) % V
end

@inline function Base.getindex(bs::BitStringVector, v::AbstractVector)
    @boundscheck checkbounds(bs, v)
    return @inbounds bitvecview(parent(bs)[v])
end

@inline Base.sizeof(s::BitStringVector{V}) where V = length(s) * sizeof(V) # should this always be UInt8 ?
@inline Base.elsize(::Type{<:BitStringVector{V}}) where V = sizeof(V)

@inline function Base.iterate(bs::BitStringVector, i=1)
    if (i % UInt) - 1 < length(bs)
        (@inbounds bs[i], i + 1)
    else
        nothing
    end
end

Base.String(bs::BitStringVector) = parent(bs)

for func in (:reverse,)
    @eval Base.$(func)(bs::BitStringVector, args...) = BitStringVector(Base.$(func)(parent(bs)), args...)
end
