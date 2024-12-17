module BitStringViews

import ..BitsX

# Maybe @bsv should be removed, or have a different name
export bitstringview, BitStringView, @bsv

# TODO: length and field `len` is working correctly everywhere.

struct BitStringView{AT} <: AbstractString
    data::AT
    len::Int

    function BitStringView{AT}(data::AT, len::Integer) where AT
        (len > BitsX.bitlength(data) || len < 0) &&
            throw(DomainError(len, "Length of BitStringView out of range for data type."))
        new{AT}(data, len)
    end
end

BitStringView(data, len) = BitStringView{typeof(data)}(data, len)

function Base.show(bs::BitStringView)
    Base.show(stdout, bs)
end
function Base.show(io::IO, bs::BitStringView)
    print(io, '"')
    for c in bs
        print(io, c)
    end
    print(io, '"')
end

function BitsX.bit(bs::BitStringView, i::Integer)
    BitsX.bit(parent(bs), i)
end

"""
   bitstringview(v, [n])

Return an `AbstractString` view of `v` that represent a sequence of bits.

`v` may be of any type such that `BitsX.to_binary_char` returns a character
(i.e. does not throw an error) for each element of `v` as determined by
`BitsX.bitgetindex`.

# Examples
```julia-repl
julia> bitstringview([1,0,1,1])
"1011"

julia> bitstringview([true, false, true, false])
"1010"

julia> bitstringview(UInt8(1 << 4 - 1))
"00001111"

julia> bitstringview("101") # `bitgetindex` is implemented for binary strings.
"101"
```

If `n` is omitted, then the length of the string is the number of bits in `v`. For
example, for `v = UInt64(1)`, the length of the string is `64`. For `v::AbstractArray`,
the number of bits is `length(v)`.

If `n` is zero, then the length of the string is the minimum number of bits needed to
represent `v`. For example

# Examples
```julia-repl
julia> length(bitstringview(UInt64(7)))
64

julia> length(bitstringview(UInt64(7), 5))
5

julia> length(bitstringview(UInt64(7), 0)
3

julia> bitstringview(UInt64(7), 0)
"111"

julia> bitstringview(UInt16(7))
"0000000000000111"
```

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
#    if (i % UInt) - 1 < BitsX.bitlastindex(parent(bv))
    if (i % UInt) - 1 < bv.len
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
