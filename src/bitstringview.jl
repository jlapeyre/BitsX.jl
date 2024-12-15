module BitStringViews

import ..BitsX

# Maybe @bsv should be removed, or have a different name
export bitstringview, BitStringView, @bsv

# TODO: length and field `len` is working correctly everywhere.

struct BitStringView{AT} <: AbstractString
    data::AT
    len::Int
end

function Base.show(bs::BitStringView)
    Base.show(stdout, bs)
end
function Base.show(io::IO, bs::BitStringView)
    print(io, '"')
    # `for c in bs` iterates bitlength of bs.data times
    # Ignores `bs.len`. I don't know why. So there is
    # still a bug to find. But this eliminates it in printing here.
    for i in eachindex(bs)
        print(bs[i])
    end
    print(io, '"')
end

"""
   bitstringview(v)

Return an `AbstractString` view of the collection of numbers `v` that represent a sequence of bits.
Each element of `v` must satifiy either `isone` or `iszero`.

`String(bitstringview(v))` converts `v` to a `String.
"""
function bitstringview(v, pad::Integer=BitsX.bitlength(v))
    if iszero(pad)
        pad1 = BitsX.min_bits(v)
    else
        pad1 = Int(pad)
    end
    BitStringView{typeof(v)}(v, pad1)
end

#bitstringview(v) = BitStringView{eltype(v), typeof(v)}(v)

"""
    @bsv ex

Return `bitstringview(ex)`.
"""
macro bsv(expr)
    :(bitstringview($expr))
end

Base.parent(sv::BitStringView) = sv.data
Base.ncodeunits(sv::BitStringView) = sv.len # BitsX.bitlength(parent(sv))

function _getindex(sv, i::Integer)
    bdiff = BitsX.bitlength(sv.data) - length(sv)
    BitsX.bitgetindex(parent(sv), i + bdiff)
end

function _getindex(sv, inds)
    BitsX.bitgetindex(parent(sv), inds)
end

# Called by sizeof, for example
Base.codeunit(sv::BitStringView) = UInt8
Base.codeunit(sv::BitStringView, i::Integer) = BitsX.to_binary_char_code(_getindex(sv, i)) #  % codeunit(sv)

Base.isvalid(sv::BitStringView, i::Integer) = in(i, BitsX.bitaxes1(parent(sv)))
Base.length(sv::BitStringView) = sv.len # bitlength(parent(sv))

function Base.getindex(sv::BitStringView, i::Integer)
    @boundscheck checkbounds(sv, i)
    @inbounds BitsX.to_binary_char(_getindex(sv, i))
end

@inline function Base.getindex(sv::BitStringView, v::AbstractVector{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds BitStringView(_getindex(sv, v), length(v))
end

@inline function Base.getindex(sv::BitStringView, v::AbstractVector{Bool})
    @boundscheck checkbounds(sv, v)
    return @inbounds bitstringview(_getindex(sv, v), length(v))
end

@inline function Base.getindex(sv::BitStringView, v::UnitRange{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds bitstringview(_getindex(sv, v), length(v))
end

# Warning! non-"1 based vectors" might fail here
@inline function Base.iterate(bv::BitStringView, i::Int=firstindex(parent(bv)))
    if (i % UInt) - 1 < BitsX.bitlastindex(parent(bv))
        (@inbounds bv[i], i + 1)
    else
        nothing
    end
end

function Base.reverse(bv::BitStringView{T}) where {T <: Integer}
    rev = Base.bitreverse(parent(bv)) >> (8 * sizeof(T) - length(bv))
    bitstringview(rev, length(bv))
end


Base.bitreverse(bv::BitStringView{T}) where {T <: Integer} = bitstringview(Base.bitreverse(parent(bv)), length(bv))

for func in (:reverse, :reverse!)
    @eval Base.$(func)(bv::BitStringView, args...) = bitstringview(Base.$(func)(parent(bv), args...), length(bv))
end

# wtf? needed to resolve ambiguous convert
Base.convert(::Type{BitsX.StaticBitVectorView{T}}, x::BitsX.StaticBitVectorView{T}) where T = x

# Removed following line because it causes a several second hang in the REPL after loading this package
# and pressing any key.
#
# This allows fewer allocations and higher perf in print_to_string
# Makes String(x) twice as fast. But still not as fast as using collect, or the
# even faster method below.
# Base._str_sizehint(b::BitStringView) = sizeof(b)

Base.String(bs::BitStringView) = String(copyto!(Base.StringVector(length(bs)), codeunits(bs)))

end # module BitStringViews
