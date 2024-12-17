## Endianess and bit order is potentially confusing

# Return x::T with lower half of bits set, and upper half unset
function lowhalfones(::Type{T}) where {T<:Unsigned}
    T(1) << (4 * sizeof(T)) - T(1)
end

h8 = lowhalfones(UInt8)

# This method is defined in Base.
@assert bitstring(h8) == "00001111"

# This method defined in BitsX
@assert bitstringview(h8) == "00001111"

# This method defiend in BitsX
@assert string(bits(h8)) == "<00001111>"

# We want to use these representations generically to represent collections of bits.
# But the first character of a bitstring is leftmost in its representation.
# This represents the msb.
@assert bitstring(h8)[1] == '0'

# In the view given by `bits`, the first bit is the lsb, and rightmost in the representation.
@assert bits(h8)[1] == true

# Looks like `bit` is like `getindex` with lsg as index 1.
# And `bitgetindex` is as well, but takes the msb as index 1.
#
# This is the same as string indexing
@assert bitgetindex(bitstring(h8), 1) == '0'

# This is not consistent with bits
@assert bitgetindex(h8, 1) === 0x00

# This looks correct
@assert bitgetindex(bits(h8), 1) == true

# Now we look at BitArray.

# 64-bit unsigned integer with lower half of bits set
h64 = lowhalfones(UInt64)

# most sig bits are all zero
@assert all(==('0'), bitstring(h64)[1:32])

# least sig bits are all one
@assert all(==('1'), bitstring(h64)[33:64])

# And bits(.) of course behaves the same as it did for UInt8
@assert bits(h64)[1] == true

bv = BitArray(undef, 64)
# This has a one-element chunk
@assert length(bv.chunks) == 1

# Set the single element to UInt64 with lower half of bits set
bv.chunks[1] = h64

# `bitstring` method in `Base`
@assert bitstring(bv.chunks[1]) == "0000000000000000000000000000000011111111111111111111111111111111"

# Another `bitstring` method in `Base`
# The lsb is printed first, and the msb is printed last!
@assert bitstring(bv) == "11111111 11111111 11111111 11111111 00000000 00000000 00000000 00000000 "

# Now collect two representations into a Vector{Bool}
# The fist elements of the `Vector`s, which are of course, printed first, are the least sig
bool_arr_1::Vector{Bool} = collect(bv)
bool_arr_2::Vector{Bool} = collect(bits(bv.chunks[1]))
@assert bool_arr_1 == bool_arr_2

# There are two issues
# In what order are the bits printed for various representations?
# Does indexing with `getindex` or `bitgetindex` return the same value on different representations?
# Following convention and implementation in Base Julia, `getindex` cannot be consistent.

# Let's test the `bit` interface. Recall that h64 has only the lower half of bits set.
# So in a correct implementation, the first element should be 1 (true) and the last (64th)
# element should be 0 (false).
#
# These are correct
@assert bit(h64, 1) === 1
@assert bit(h64, 64) === 0
@assert bitlength(h64) == 64

# These are correct
# These all dispatch to the method for AbstractArray{Bool}
@assert bit(bits(h64), 1) === true
@assert bit(bits(h64), 64) === false
@assert bit(bv, 1) === true
@assert bit(bv, 64) === false

# These are not consistent with the examples above.
# If we wanted to keep the representation of bitstrings that Julia uses,
# then we should invert the index here.
@assert bit(bitstring(h64), 1) === 0
@assert bit(bitstring(h64), 64) === 1

nothing;
