"""
    BitsBase

This submodule implements some abstractions for working with bits that are used by other submodules.

The conceit is that it should be possible to write (performant) function methods for working with bits
that have minimal dependence on the representation of the bits. Bits can be represented by unsigned
integers, arrays of unsigned integers, `String`s, and lazy views of each of these as a different
bit representation. We want to write methods that don't depend on which representation we use.

Analogs to [`collect`](@ref), `size`, `getindex`, `eachindex`, `axes` are
 `bitcollect`, [`bitsize`](@ref), `bit`, `biteachindex`, and [`bitaxes`](@ref).

An example of a function built on these abstractions is [`bitcollect`](@ref), which
has this definition
```julia
function bitcollect(obj)
    array = Array{Bool}(undef, bitsize(obj))
    # Assume LinearIndices. Probably not always correct
    for (i, ind) in enumerate(biteachindex(obj))
        array[i] = bit(obj, ind)
    end
    array
end
```
Note that `bitcollect` depends on `bitsize`, `bit`, and `biteachindex`. The latter
depends on `bitaxes1`, which depends on `bitaxes`.

# Examples

Here are some concrete examples.
```jldoctest
julia> s = "10101010"; x = parse(UInt8, s; base=2); v = Bool[1,0,1,0,1,0,1,0];

julia> bitlength.((x, v, s, codeunits(s)))
(8, 8, 8, 8)
```

# Performance

A goal is that there be little or no performance penalty for using this interface.

For example, because the bitstrings contain only ASCII characters, `bitlength(::String)` does not
traverse the entire string
```julia
julia> s = "1"^1000;

julia> @btime length(\$s)
  401.550 ns (0 allocations: 0 bytes)
1000

julia> @btime bitlength(\$s)
  1.763 ns (0 allocations: 0 bytes)
1000
```
"""
module BitsBase

export is_one_char, is_zero_char, is_binary_char, is_bitstring, check_bitstring,
    to_binary_char, to_binary_char_code, binzero, binone,
    isbinzero, isbinone,
    from_binary_char, ZeroBased, OneBased, IndexBase,
    asint, asuint, inttype

function bitsizeof end

module _BitsBase

import ..bitsizeof

const BoolOrVal = Union{Bool, Val{true}, Val{false}}

# Not sure why we have to do this. Forces specialization
_toVal(x::Bool) = x ? Val(true) : Val(false)
_toVal(x::Val) = x

const _ZERO_CHAR_CODE = UInt8('0')
const _ONE_CHAR_CODE = UInt8('1')

_zero(s::AbstractChar) = '0'
_zero(x) = zero(x)

_bitsizeof(isbits::Val{true}, T::Type) = sizeof(T) * 8
_bitsizeof(isbits::Val{false}, T::Type) = throw(MethodError(bitsizeof, (T,)))

end # module _BitsBase

import ._BitsBase

abstract type IndexBase end
struct OneBased <: IndexBase end
struct ZeroBased <: IndexBase  end

# Modified from Bits.jl
asint(x::Integer) = x
asint(x::AbstractFloat) = reinterpret(Signed, x) # Signed gets all the types we need

asuint(x::Integer) = unsigned(x)
asuint(x::AbstractFloat) = reinterpret(Unsigned, x) # Signed gets all the types we need

"""
    is_one_char(x)

Return `true` if `x` is equal to `'1'` or its ASCII code.

# Examples
```jldoctest
julia> is_one_char.(('0', '1', 'c', UInt8('0'), UInt8('1'), UInt8('c')))
(false, true, false, false, true, false)

julia> is_one_char.((Int('1'), 42))
(true, false)
```
"""
is_one_char(x::Integer) = x == _BitsBase._ONE_CHAR_CODE

"""
    is_zero_char(x)

Return `true` if `x` is equal to `'0'` or its ASCII code.

# Examples
```jldoctest
julia> is_zero_char.(('0', '1', 'c', UInt8('0'), UInt8('1'), UInt8('c')))
(true, false, false, true, false, false)
```
"""
is_zero_char(x::Integer) = x == _BitsBase._ZERO_CHAR_CODE
is_one_char(x::Char) = is_one_char(UInt8(x))
is_zero_char(x::Char) = is_zero_char(UInt8(x))

"""
    is_binary_char(x)

Return `true` if `x` is equal to either `'0'` or `'1'` or their ASCII codes.

# Examples
```jldoctest
julia> is_binary_char.(('1', Int('1'), '0', 'a', 100))
(true, true, true, false, false)
```
"""
is_binary_char(x) = is_one_char(x) || is_zero_char(x)

# TODO: This is probably 10x slower because of the possibility
# of throwing an error. But, safer to start.
# Convert `x` to the character `'0'` or `'1'`.
"""
    to_binary_char_code(x::T)::UInt8

Convert `x` to the ASCII code for character `'0'` or `'1'`.  One of `isbinzero(x)` or
`isbinone(x)` must return `true`.

`isbinzero` and `isbinone` fall back to `iszero` and `isone` where this makes sense.

`T` must implement `iszero` (or `zero`) and `isone` (or `one`). Alternatively, you can
implement `isbinzero` (or `binzero`) and `isbinone` (or `binone`)

# Examples
```jldoctest
julia> to_binary_char_code.((1, '1', true, 0x01, 0, '0', false, 0x00))
(0x31, 0x31, 0x31, 0x31, 0x30, 0x30, 0x30, 0x30)

julia> to_binary_char_code(42)
ERROR: DomainError with 42:
Must be zero or one value of type Int64.
```
See also [`binzero`](@ref), [`binone`](@ref).
"""
function to_binary_char_code(x::T) where T
    isbinzero(x) && return _BitsBase._ZERO_CHAR_CODE
    isbinone(x) && return _BitsBase._ONE_CHAR_CODE
    throw(DomainError(x, LazyString("Must be zero or one value of type ", T, ".")))
end

"""
    binzero(x::T)

The value of type `T` representing bit value zero.

If `T <: Integer`, or a matrix thereof, return `zero(::T)`.  If `T` is other
`AbstractArray`, `AbstractString`, `AbstractFloat`, `Complex`, throw a `MethodError`.

The fallback method returns `zero(x)`.
"""
binzero(x) = zero(x)

"""
    binone(x::T)

The value of type `T` representing bit value one.

# Examples
```jldoctest
julia> binone.((Char, '-', Int, UInt8, 42))
('1', '1', 1, 0x01, 1)
```
"""
binone(x) = one(x)
binone(::Type{Char}) = '1'
binone(::Type{AbstractString}) = "1"

"""
    binzero(x)

The value of type `T` representing bit value zero.

# Examples
```jldoctest
julia> binzero.((Char, '-', Int, UInt8, 42))
('0', '0', 0, 0x00, 0)
```
"""
binzero(::Type{Char}) = '0'
binzero(::Char) = '0'
binone(::Char) = '1'
binone(x::T) where {T <: AbstractMatrix{<:Integer}} = one(x)
binzero(x::T) where {T <: AbstractMatrix{<:Integer}} = zero(x)

let nb = Union{AbstractArray, AbstractString, AbstractFloat, Complex}
    global binone(x::nb) = throw(MethodError(binone, (x,)))
    global binzero(x::nb) = throw(MethodError(binzero, (x,)))
end

"""
    isbinzero(x)

Return `true` if `x` represents bit value zero.

See also [`binzero`](@ref).
"""
isbinzero(x) = x == binzero(x)

"""
    isbinone(x)

Return `true` if `x` represents bit value one.

See also [`binone`](@ref).
"""
isbinone(x) = x == binone(x)

# TODO: what is the best way to do this ? For example, instead
# isbinone(x) = isone(x)
# and then special cases to throw on String's etc.
# or, as we have now...
isbinone(x::T) where {T <: AbstractMatrix{<:Integer}} = isone(x)
isbinzero(x::T) where {T <: AbstractMatrix{<:Integer}} = iszero(x)

"""
    to_binary_char(x::T)::Char

Convert `x` to the `Char` `'0'` or `'1'`.
`x` must be equal to either `binzero(T)` or `binone(T)`.

# Examples
```jldoctest
julia> to_binary_char.((1, '1', true, 0x01, 0, '0', false, 0x00))
('1', '1', '1', '1', '0', '0', '0', '0')
```
"""
to_binary_char(x) = Char(to_binary_char_code(x))

from_binary_char(::Type{T}, x::Char) where T = from_binary_char(T, UInt8(x))

"""
    from_binary_char([::Type{T} = Bool], x)

Convert the characters `'0'` and `'1'` (or `UInt8('0')` and `UInt8('1')`) to `binzero(T)`
and `binone(T)`.

# Examples
```jldoctest
julia> from_binary_char.(Bool, ('1', UInt8('1'), '0', UInt8('0')))
(true, true, false, false)

julia> from_binary_char('c')
ERROR: DomainError with 99:
```
"""
from_binary_char(::Type{T}, x::UInt8) where T = from_binary_char(T, x, Val(true))

function from_binary_char(::Type{T}, x::UInt8, check::Val{true}) where T
    is_one_char(x) && return binone(T)
    is_zero_char(x) && return binzero(T)
    throw(DomainError(x, "Must be '0' or '1'."))
end

function from_binary_char(::Type{T}, x::UInt8, check::Val{false}) where T
    is_one_char(x) && return one(T)
    return zero(T)
end

from_binary_char(x) = from_binary_char(Bool, x)


"""
    is_bitstring(bit_str::Union{AbstractString, AbstractVector{UInt8}})

Return `true` if all characters in `bit_str` are either `'0'` or `'1'`, otherwise `false`.

# Examples
```jldocstring
julia> is_bitstring("11001100")
true

julia> is_bitstring("1100 1100")
false
```
"""
is_bitstring(s) = count_bits(s) == ncodeunits(s)
is_bitstring(s::AbstractVector{UInt8}) = count_bits(s) == length(s)
# Following are usually slower. But there are some mysterious dependencies on string length
# is_bitstring(v::AbstractVector{UInt8}) = all(is_binary_char, v)
# is_bitstring(s::AbstractString) = is_bitstring(codeunits(s))

"""
    count_bits(s::AbstractString)
    count_bits(v::AbstractVector{UInt8})

Return the number of characters (or bytes) that are `'1'` or `'0'`,
(`0x30` or `0x31`).

It is assumed that `s` is an ASCII string. `count_bits` may be useul if
the string includes formatting characters, for example spaces.
"""
count_bits(s::AbstractString) = count_bits(codeunits(s))
count_bits(v::AbstractVector{UInt8}) = count(is_binary_char, v)

"""
    check_bitstring(bit_str::Union{AbstractString, AbstractVector{UInt8}})

Throw an `ArgumentError` unless  all characters in `bit_str` are either `'0'` or `'1'`.
Otherwise return nothing.

# Examples
```jldocstring
julia> check_bitstring("11001100")
true

julia> check_bitstring("1010010b")
ERROR: ArgumentError: Argument is not a bit string
```
"""
function check_bitstring(bit_str::Union{AbstractString, AbstractVector{UInt8}})
    is_bitstring(bit_str) && return nothing
    throw(ArgumentError("Argument is not a bit string"))
end

###
### bitsizeof
###

# bitsizeof should give how many "addressable" bits are in the object
# This should be in runtest
"""
    bitsizeof(::Type{T})

Return the number of (usefully) indexable bits in an instance of type `T`.
Here "indexable" means via `bit(x::T, i)`.

For some types, such as `Integer`s, `bit(x::T, i)` returns `0` for `i` greater than
`bitsizeof(T)`.

If the number of indexable bits in an instance of type `T` cannot be computed
from the type alone, then an error is thrown.

# Examples
```jldoctest
julia> bitsizeof.((UInt8, UInt64, NTuple{5, Int}, StaticBitVectorView{Int64}))
(8, 64, 5, 64)

julia> bitsizeof(String)
ERROR: MethodError: no method matching bitsizeof(::Type{String})

julia> bitsizeof(BigInt)
ERROR: MethodError: no method matching bitsizeof(::Type{BigInt})
```
"""
bitsizeof(::Type{T}) where T = _BitsBase._bitsizeof(Val(isbitstype(T)), T)
#bitsizeof(T::Type) = _BitsBase._bitsizeof(Val(isbitstype(T)), T)
bitsizeof(::Type{<:NTuple{N, <:Integer}}) where {N} = N
bitsizeof(::Type{Bool}) = 1

"""
    bitlength(x::T)

Return the number of bits in the instance `x` of type `T`.
This is the number of bits that can be indexed via `bit`. If `x` is an `isbitstype`
type, then this is the same as `bitsizeof(T)`.

In contrast, if `x` is of type `BigInt` or `BigFloat` the number
of bits is not encoded in the type, and in fact may vary from instance
to instance.
"""
bitlength(x::T) where T = bitsizeof(T)
bitlength(x::NTuple{N, <:Integer}) where {N} = N

function bitlength(x::BigFloat)
    MPFR_EXP_BITSIZE = bitsizeof(Clong)
    1 + MPFR_EXP_BITSIZE + precision(x)
end

bitlength(x::BigInt) = abs(x.size) * Base.GMP.BITS_PER_LIMB # bitsizeof(Base.GMP.Limb)
bitlength(v::AbstractArray) = length(v)

"""
    bitlength(str::AbstractString)

The number of characters in `str`, which is assumed to contain
only `'0'` and `'1'`. If `str` contains other characters, the returned
value will be incorrect.
"""
bitlength(s::AbstractString) = ncodeunits(s)


"""
    min_bits(n::Integer)
    min_bits(bit_str::AbstractString)
    min_bits(v)

Return the required number of bits in the binary representation
of `n` (or `bit_str`, or iterable `v`).

The returned value is the position of the leftmost bit equal to `1`, counting from the right.
Equivalently, the value is the length of the `bit_str` discounting leading zeros.

# Examples
```jldoctest
julia> min_bits(2^10)
11

julia> min_bits("1"^10)
10

julia> min_bits([0,1,0])
2
```
"""
min_bits(n::T) where {T<:Integer} = 8 * sizeof(T) - Base.leading_zeros(n)
min_bits(v) = min_dits(v)

"""
    min_dits(v)

Return the minimum number of "dits" needed to express `v`.

The first element not representing zero counting from the left determines the
return value. Input is not validated.

# Examples
```jldoctest
julia> min_dits("03Q")
2

julia> min_dits([0, 3, 17])
2
```
"""
function min_dits(v)
    n = Base.length(v)
    i = 0
    for c in v
        c !== _BitsBase._zero(c) && return (n - i)
        i += 1
    end
    return 0
end

min_dits(v::Integer) = throw(ErrorException(lazy"min_dits: use min_bits for $(typeof(v)) input $v"))

## Add more methods for Base.uinttype.
## Methods are defined in base for floating point types,
## and in the package BitIntegers.jl
let
#   Why did I have Float here? It is already defined. Did we not want to extend the Base functions?
#    tups = [(Symbol(:Int, n), (Symbol(:UInt, n), Symbol(:Int, n), Symbol(:Float, n))) for n in (16, 32, 64)]
    tups = [(Symbol(:Int, n), (Symbol(:UInt, n), Symbol(:Int, n))) for n in (16, 32, 64)]
    for (t, ts) in ((:Int8, (:UInt8, :Int8, :Bool)), tups..., (:Int128, (:UInt128, :Int128)))
        for tp in ts
            @eval inttype(::Type{$tp}) = $t
            @eval Base.uinttype(::Type{$tp}) = $(Symbol("U", t))
        end
    end
end

###
### bitsize
###
## Don't confuse with bitsizeof

bitsize(a) = size(a)
bitsize(x::Real) = (bitlength(x),)
bitsize(s::AbstractString) = (ncodeunits(s),)
bitsize(t::Tuple) = (length(t),)

# FIXME: @inline at call sites is a v1.8 feature
# So I disabled it in three places below.
# Find out how important it is.
function bitaxes(A)
#    @inline
    map(Base.oneto, bitsize(A))
end

# performance optimization, as in Base.
#bitaxes1(A::AbstractArray{<:Any,0}) = OneTo(1)
bitaxes1(A) = bitaxes(A)[1]
biteachindex(A) = bitaxes1(A)
# bitaxes1(A) = (@inline; bitaxes(A)[1])
# biteachindex(A) = (@inline(); bitaxes1(A))
bitlastindex(A) = last(biteachindex(A))

# using BitsX.BitsBase._BitsBase
# import ._BitsBase: is_one_char, is_zero_char, is_binary_char, is_bitstring, check_bitstring,
#     to_binary_char, to_binary_char_code, binzero, binone,
#     isbinzero, isbinone, from_binary_char, ZeroBased, OneBased, IndexBase

end # module BitsBase
