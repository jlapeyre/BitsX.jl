using BitsX: BitsX, min_bits, undigits, bits, min_uint_type, uint_type, bitsizeof,
    StaticBitVector
using Test

@testset "bitsizeof" begin
    @test bitsizeof(Bool) == 1
    @test bitsizeof(Int64) == 64
    @test bitsizeof(UInt64) == 64
    @test bitsizeof(UInt8) == 8
    @test_throws MethodError bitsizeof(BigInt)
    @test_throws MethodError bitsizeof(BigFloat)
end

@testset "Bits.Xjl" begin
    n = 12345
    @test undigits(digits(n)) == n
    @test undigits(digits(n, base=2), base=2) == n
    @test undigits(digits(n, base=2), base=10) != n
    tup = (1,1,1,0,0,0)
    @test Tuple(bits(tup)) == tup
    @test Tuple(bits("000111")) == tup
    @test Tuple(bits("<000111>"; strip=true)) == tup
    @test_throws ArgumentError Tuple(bits("<000111>"; strip=false))
    @test string(bits(Tuple(bits("111000")))) == "<111000>"
    @test reverse(bits("111000")) == bits("000111")

    @test bits("0101") isa StaticBitVector{UInt8}
    @test bits(Int, "0101") isa StaticBitVector{Int}

    @test length(bits("1"^100)) == 100
    @test_throws OverflowError undigits(UInt, fill(1, 1000))

    @test min_bits((0,1,0,1)) == 3
    @test min_bits([0,1,0,1]) == 3
    @test min_bits((1,1,0,1)) == 4
    @test min_bits(()) == 0
    @test min_bits((0,)) == 0
    @test min_bits("0101") == 3
    @test min_bits("1101") == 4
    @test min_bits("0") == 0
    @test min_bits("") == 0
end


@testset "bitintegers" begin
    @test min_uint_type(0) == UInt8
    @test min_uint_type(1) == UInt8
    @test min_uint_type(7) == UInt8
    @test min_uint_type(8) == UInt8
    @test min_uint_type(9) == UInt16
    @test_throws DomainError min_uint_type(-1)
    @test min_uint_type(1000) == BitsX.UInt1000
    @test uint_type(8) == UInt8
    @test uint_type(80) == BitsX.UInt80
end
