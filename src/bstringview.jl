module BStringViews

import ..BitsX
import ..BitsX.BStrings

# Maybe @bsv should be removed, or have a different name
export BStringView, @bsv, bstringview

# TODO: length and field `len` is working correctly everywhere.

struct BStringView{AT} <: AbstractString
    data::AT
    len::Int

    function BStringView{AT}(data::AT, len::Integer) where AT
        (len > BitsX.bitlength(data) || len < 0) &&
            throw(DomainError(len, "Length of BStringView out of range for data type."))
        new{AT}(data, len)
    end
end

BStringView(data, len) = BStringView{typeof(data)}(data, len)

function Base.show(bs::BStringView)
    Base.show(stdout, bs)
end
function Base.show(io::IO, bs::BStringView)
    print(io, '"')
    for c in bs
        print(io, c)
    end
    print(io, '"')
end

function BitsX.bit(bs::BStringView, i::Integer)
    bs[i]
#    BitsX.bit(parent(bs), i)
end

"""
   bstringview(v, [n])

Return an `AbstractString` view of `v` that represent a sequence of bits.

`v` may be of any type such that `BitsX.to_binary_char` returns a character
(i.e. does not throw an error) for each element of `v` as determined by
`BitsX.bit`.

# Examples
```julia-repl
julia> bstringview([1,0,1,1])
"1011"

julia> bstringview([true, false, true, false])
"1010"

julia> bstringview(UInt8(1 << 4 - 1))
"11110000"

julia> bstringview("110") # `bit` is implemented for binary strings.
"110"
```

If `n` is omitted, then the length of the string is the number of bits in `v`. For
example, for `v = UInt64(1)`, the length of the string is `64`. For `v::AbstractArray`,
the number of bits is `length(v)`.

If `n` is zero, then the length of the string is the minimum number of bits needed to
represent `v`. For example

# Examples
```julia-repl
julia> length(bstringview(UInt64(7)))
64

julia> length(bstringview(UInt64(7), 5))
5

julia> length(bstringview(UInt64(7), 0)
3

julia> bstringview(UInt64(7), 0)
"111"

julia> bstringview(UInt16(7))
"1110000000000000"
```

`String(bstringview(v))` converts `v` to a `String.
"""
function bstringview(v, pad::Integer=BitsX.bitlength(v))
    if iszero(pad)
        pad1 = BitsX.min_bits(v)
    else
        pad1 = Int(pad)
    end
    BStringView{typeof(v)}(v, pad1)
end

# bstringview(v, args...) = bstringview(v, args...)

# A fallback method to convert an array of bools to a
# binary string.
BStrings.bstring(v::AbstractArray) = String(bstringview(v))

"""
    @bsv ex

Return `bstringview(ex)`.
"""
macro bsv(expr)
    :(bstringview($expr))
end

Base.parent(sv::BStringView) = sv.data
Base.ncodeunits(sv::BStringView) = sv.len # BitsX.bitlength(parent(sv))

function _getindex(sv, i::Integer)
    # bdiff = BitsX.bitlength(sv.data) - length(sv)
    # BitsX.bit(parent(sv), i + bdiff)
    BitsX.bit(parent(sv), i)
end

function _getindex(sv, inds)
    BitsX.bit(parent(sv), inds)
end

# Called by sizeof, for example
Base.codeunit(sv::BStringView) = UInt8
Base.codeunit(sv::BStringView, i::Integer) = BitsX.to_binary_char_code(_getindex(sv, i)) #  % codeunit(sv)

Base.isvalid(sv::BStringView, i::Integer) = in(i, BitsX.bitaxes1(parent(sv)))
Base.length(sv::BStringView) = sv.len # bitlength(parent(sv))

function Base.getindex(sv::BStringView, i::Integer)
    @boundscheck checkbounds(sv, i)
    @inbounds BitsX.to_binary_char(_getindex(sv, i))
end

@inline function Base.getindex(sv::BStringView, v::AbstractVector{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds BStringView(_getindex(sv, v), length(v))
end

@inline function Base.getindex(sv::BStringView, v::AbstractVector{Bool})
    @boundscheck checkbounds(sv, v)
    return @inbounds bstringview(_getindex(sv, v), length(v))
end

@inline function Base.getindex(sv::BStringView, v::UnitRange{<:Integer})
    @boundscheck checkbounds(sv, v)
    return @inbounds bstringview(_getindex(sv, v), length(v))
end

# Warning! non-"1 based vectors" might fail here
@inline function Base.iterate(bv::BStringView, i::Int=firstindex(parent(bv)))
#    if (i % UInt) - 1 < BitsX.bitlastindex(parent(bv))
    if (i % UInt) - 1 < bv.len
        (@inbounds bv[i], i + 1)
    else
        nothing
    end
end

function Base.reverse(bv::BStringView{T}) where {T <: Integer}
    rev = Base.bitreverse(parent(bv)) >> (8 * sizeof(T) - length(bv))
    bstringview(rev, length(bv))
end

Base.bitreverse(bv::BStringView{T}) where {T <: Integer} = bstringview(Base.bitreverse(parent(bv)), length(bv))

for func in (:reverse, :reverse!)
    @eval Base.$(func)(bv::BStringView, args...) = bstringview(Base.$(func)(parent(bv), args...), length(bv))
end

# wtf? needed to resolve ambiguous convert
Base.convert(::Type{BitsX.StaticBitVectorView{T}}, x::BitsX.StaticBitVectorView{T}) where T = x

# Removed following line because it causes a several second hang in the REPL after loading this package
# and pressing any key.
#
# This allows fewer allocations and higher perf in print_to_string
# Makes String(x) twice as fast. But still not as fast as using collect, or the
# even faster method below.
# Base._str_sizehint(b::BStringView) = sizeof(b)

Base.String(bs::BStringView) = String(copyto!(Base.StringVector(length(bs)), codeunits(bs)))

end # module BStringViews
