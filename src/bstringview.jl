module BStringViews

# Maybe @bsv should be removed, or have a different name
export BStringView, @bsv, bstringview

import ...BitsX.BitsBase

struct BStringView{AT} <: AbstractString
    data::AT
    len::Int

    function BStringView{AT}(data::AT, len::Integer) where AT
        (len > BitsBase.bitlength(data) || len < 0) &&
            throw(DomainError(len, "Length of BStringView out of range for data type."))
        new{AT}(data, len)
    end
end

BStringView(data, len) = BStringView{typeof(data)}(data, len)

"""
    @bsv ex

Return `bstringview(ex)`.
"""
macro bsv(expr)
    :(bstringview($expr))
end


"""
   bstringview(v, [n])

Return an `AbstractString` view of `v` that represent a sequence of bits.

`v` may be of any type such that `BitsBase.to_binary_char` returns a character
(i.e. does not throw an error) for each element of `v` as determined by
`Bits.bit`.

# Examples
```jldoctest
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
```jldoctest
julia> length(bstringview(UInt64(7)))
64

julia> length(bstringview(UInt64(7), 5))
5

julia> length(bstringview(UInt64(7), 0))
3

julia> bstringview(UInt64(7), 0)
"111"

julia> bstringview(UInt16(7))
"1110000000000000"
```

`String(bstringview(v))` converts `v` to a `String.
"""
function bstringview(v, pad::Integer=BitsBase.bitlength(v))
    if iszero(pad)
        pad1 = BitsBase.min_bits(v)
    else
        pad1 = Int(pad)
    end
    BStringView{typeof(v)}(v, pad1)
end

module _BStringViews

import ...BitsX.BStrings
import ...BitsX.BitsBase
import ...BitsX.Bits

import ..BStringViews: BStringView, bstringview

# TODO: length and field `len` is working correctly everywhere.

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

function BitsBase.bit(bs::BStringView, i::Integer)
    bs[i]
#    BitsBase.bit(parent(bs), i)
end

# A fallback method to convert an array of bools to a
# binary string.
BStrings.bstring(v::AbstractArray) = String(bstringview(v))

Base.parent(sv::BStringView) = sv.data
Base.ncodeunits(sv::BStringView) = sv.len # BitsBase.bitlength(parent(sv))

function _getindex(sv, i::Integer)
    # bdiff = BitsBase.bitlength(sv.data) - length(sv)
    # BitsBase.bit(parent(sv), i + bdiff)
    BitsBase.bit(parent(sv), i)
end

function _getindex(sv, inds)
    BitsBase.bit(parent(sv), inds)
end

# Called by sizeof, for example
Base.codeunit(sv::BStringView) = UInt8
Base.codeunit(sv::BStringView, i::Integer) = BitsBase.to_binary_char_code(_getindex(sv, i)) #  % codeunit(sv)

Base.isvalid(sv::BStringView, i::Integer) = in(i, BitsBase.bitaxes1(parent(sv)))
Base.length(sv::BStringView) = sv.len # bitlength(parent(sv))

function Base.getindex(sv::BStringView, i::Integer)
    @boundscheck checkbounds(sv, i)
    @inbounds BitsBase.to_binary_char(_getindex(sv, i))
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
#    if (i % UInt) - 1 < BitsBase.bitlastindex(parent(bv))
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

# Removed following line because it causes a several second hang in the REPL after loading this package
# and pressing any key.
#
# This allows fewer allocations and higher perf in print_to_string
# Makes String(x) twice as fast. But still not as fast as using collect, or the
# even faster method below.
# Base._str_sizehint(b::BStringView) = sizeof(b)

Base.String(bs::BStringView) = String(copyto!(Base.StringVector(length(bs)), codeunits(bs)))

end # module _BStringViews


end # module BStringViews
