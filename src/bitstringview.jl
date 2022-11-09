struct BitStringView{DT, AT} <: AbstractString
    data::AT
end

"""
   bitstringview(v)

Return an `AbstractString` view of the collection of numbers `v`.
Each element of `v` must satifiy either `isone` or `iszero`.

`String(bitstringview(v))` converts `v` to a `String.
"""
bitstringview(v) = BitStringView{eltype(v), typeof(v)}(v)
Base.parent(sv::BitStringView) = sv.data
Base.ncodeunits(sv::BitStringView) = bitlength(parent(sv))

_getindex(sv, i) = bitgetindex(parent(sv), i)

# Called by sizeof, for example
Base.codeunit(sv::BitStringView) = UInt8
Base.codeunit(sv::BitStringView, i::Integer) = to_binary_char_code(_getindex(sv, i)) #  % codeunit(sv)

Base.isvalid(sv::BitStringView, i::Integer) = in(i, bitaxes1(parent(sv)))
Base.length(sv::BitStringView) = bitlength(parent(sv))

function Base.getindex(sv::BitStringView, i::Integer)
    @boundscheck checkbounds(sv, i)
    @inbounds to_binary_char(_getindex(sv, i))
end

@inline function Base.getindex(sv::BitStringView, v::AbstractVector{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds bitstringview(_getindex(sv, v))
end

@inline function Base.getindex(sv::BitStringView, v::AbstractUnitRange{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds bitstringview(_getindex(sv, v))
end

# Warning! non-"1 based vectors" might fail here
@inline function Base.iterate(bv::BitStringView, i::Int=firstindex(parent(bv)))
    if (i % UInt) - 1 < bitlastindex(parent(bv))
        (@inbounds bv[i], i + 1)
    else
        nothing
    end
end

for func in (:reverse, :reverse!)
    @eval Base.$(func)(bv::BitStringView, args...) = bitstringview(Base.$(func)(parent(bv), args...))
end

# wtf? needed to resolve ambiguous convert
Base.convert(::Type{BitsX.StaticBitVectorView{T}}, x::BitsX.StaticBitVectorView{T}) where T = x

# This allows fewer allocations and higher perf in print_to_string
# Makes String(x) twice as fast. But still not as fast as using collect as below.
Base._str_sizehint(b::BitStringView) = sizeof(b)

Base.String(bs::BitStringView) = String(copyto!(Base.StringVector(length(bs)), codeunits(bs)))
