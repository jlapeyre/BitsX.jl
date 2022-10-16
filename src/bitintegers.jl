using BitIntegers: BitIntegers, UInt512, UInt1024

const _UINT_TYPES = Dict{Int, DataType}()

for n in (8, 16, 32, 64, 128, 512, 1024)
    _UINT_TYPES[n] = eval(Symbol(:UInt, n))
end

"""
    min_uint_type(nbits::Integer)

Return the smallest unsigned integer type large enough to store `nbits` bits.
The number of bits in the type returned is a multiple of 8.
"""
function min_uint_type(nbits::Integer)::DataType
    nbits >= 0 || throw(DomainError(nbits, "Must be non-negative."))
    nbits == 0 && return UInt8
    (q, r) = divrem(nbits, 8)
    if iszero(r)
        return uint_type(nbits)
    else
        return uint_type((q + 1) * 8)
    end
end

"""
    uint_type(n::Int)

Return an `n`-bit unsigned integers type `UIntn`.
`n` must be a positive mulitple of `8`.

If `UIntn` does not exist, construct `UIntn` and `Intn`.
"""
@inline function uint_type(n::Integer)
    _type = get(_UINT_TYPES, n, nothing)
    if !isnothing(_type)
        return _type
    end
    n >= 0 || throw(DomainError(n, "Must be non-negative."))
    n % 8 == 0 || throw(DomainError(n, "Must be a multiple of 8."))
    uint_sym = Symbol(:UInt, n)
    eval(Meta.parse("BitIntegers.@define_integers $n"))
    _uint_type::DataType = eval(uint_sym)
    _UINT_TYPES[n] = _uint_type
    return _uint_type
end
