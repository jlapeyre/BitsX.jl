###
### BitInteger support
###

using BitIntegers: BitIntegers, UInt256, UInt512, UInt1024

const _UINT_TYPES = Dict{Int, DataType}()

for n in (8, 16, 32, 64, 128, 256, 512, 1024)
    _UINT_TYPES[n] = eval(Symbol(:UInt, n))
end

"""
    min_uint_type(nbits::Integer)

Return the smallest unsigned integer type large enough to store `nbits` bits.
The number of bits in the type returned is a multiple of 8.
"""
function min_uint_type(nbits::Integer)
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
    uint_type(n::Integer)

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

const _max_uint_fac = 128
const _UINT_TYPES_TUP = Tuple(uint_type.([8 * i for i in 1:128]))

function make_masks()
    arr = Any[]
    for i in 1:128
        T = _UINT_TYPES_TUP[i]
        tup = Tuple(T(2)^i for i in 0:(8*i - 1))
        push!(arr, tup)
    end
    return Tuple(arr)
end

const _UINT_ONE_BIT_MASKS = make_masks()

# const facs104 = Tuple(UInt104(2)^i for i in 0:103)
# const facs128 = Tuple(UInt128(2)^i for i in 0:127)
# const facs256 = Tuple(UInt256(2)^i for i in 0:255)
