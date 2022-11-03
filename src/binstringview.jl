struct BinStringView{DT, AT} <: AbstractString
    data::AT
end

"""
   binstringview(v)

Return an `AbstractString` view of the collection of numbers `v`.
Each element of `v` must satifiy either `isone` or `iszero`.

`String(binstringview(v))` converts `v` to a `String.
"""
binstringview(v) = BinStringView{eltype(v), typeof(v)}(v)
Base.parent(sv::BinStringView) = sv.data
Base.ncodeunits(sv::BinStringView) = length(parent(sv))

# Called by sizeof, for example
Base.codeunit(sv::BinStringView) = UInt8

Base.isvalid(sv::BinStringView, i::Integer) = in(i, Base.axes1(parent(sv)))
Base.length(sv::BinStringView) = length(parent(sv))

function Base.getindex(sv::BinStringView, i::Integer)
    @boundscheck checkbounds(sv, i)
    @inbounds to_binary_char(parent(sv)[i])
end

@inline function Base.getindex(sv::BinStringView, v::AbstractVector{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds binstringview(parent(sv)[v])
end

@inline function Base.getindex(sv::BinStringView, v::AbstractUnitRange{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds binstringview(parent(sv)[v])
end

# Warning! non-"1 based vectors" might fail here
@inline function Base.iterate(bv::BinStringView, i::Int=firstindex(parent(bv)))
    if (i % UInt) - 1 < lastindex(parent(bv))
        (@inbounds bv[i], i + 1)
    else
        nothing
    end
end

for func in (:reverse, :reverse!)
    @eval Base.$(func)(bv::BinStringView, args...) = binstringview(Base.$(func)(parent(bv), args...))
end

# wtf? needed to resolve ambiguous convert
Base.convert(::Type{BitsX.StaticBitVectorView{T}}, x::BitsX.StaticBitVectorView{T}) where T = x

# This allows fewer allocations and higher perf in print_to_string
# Makes String(x) twice as fast. But still not as fast as using collect as below.
Base._str_sizehint(b::BinStringView) = sizeof(b)

# Note that calling String(x) on this type
# x::BinStringView{Bool, BitsX.StaticBitVectorView{Int64}}
# is very slow. Better to call String(collect(x))
# But, in general collecting is not better.
# This could be fixed.

# So we define this as a workaround
Base.String(x::BinStringView{T, StaticBitVectorView{V}}) where {T,V} = String(collect(x))
# And this. Maybe use a supertype instead. Other AbstractStaticBitVector probably need this.
Base.String(x::BinStringView{T, StaticBitVector{V}}) where {T,V} = String(collect(x))
