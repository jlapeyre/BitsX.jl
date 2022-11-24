using BitsX: BitsX, min_bits, undigits, bits, bitsizeof, bitsize,
    StaticBitVector

using BitsX: parse_bin, bitstringview, BitStringView

import BitsX.BitIntegersX

using Test

@testset "bitstringview" begin
    v = [1, 0, 1, 0, 1]
    bs = bitstringview(v)
    @test bs isa BitStringView
    @test bs == "10101"
    @test parent(bs) == v
    @test ncodeunits(bs) == length(v)
    @test codeunit(bs) == UInt8
    @test codeunit(bs, 1) == 0x31
    @test isvalid(bs, 1)
    @test ! isvalid(bs, 10)
    @test length(bs) == length(v)
    @test bs[end] == '1'
    @test bs[[1,3,5]] == "111"
    sbs = String(bs)
    @test sbs isa String
    @test [x for x in bs] == [x for x in sbs]
    @test reverse(bitstringview([1,1,0,0])) == "0011"

    n = UInt64(1000)
    bs1 = bitstringview(n)
    sbs1 = "0000000000000000000000000000000000000000000000000000001111101000"
    @test bs1 == sbs1
    @test bs1[1] == '0'
    @test bs1[end-3] == '1'
    @test parent(bs1) === n
    @test ncodeunits(bs1) == 64
    @test codeunit(bs1) == UInt8
    @test codeunit(bs1, 64 - 3) == 0x31
    @test isvalid(bs1, 1)
    @test isvalid(bs1, 64)
    @test !isvalid(bs1, 65)
    @test_broken bs1[55:60] == "11111"
    @test String([x for x in bs1]) == bs1

    @test bitstringview(sbs1) == sbs1
    @test bitstringview(sbs1) !== sbs1

    t = (1, 1, 0, 0)
    tv = bitstringview(t)
    @test tv == "1100"
    @test bitsize(t) == (4, )

    tv1 = bitstringview(BitVector(t))
    @test tv1 == "1100"
end



@testset "parse_bin" begin
    @test parse_bin("") === 0x00
    @test parse_bin(UInt, "") === UInt(0)
    @test parse_bin("1") === 0x01
    @test parse_bin(UInt, "1") === UInt(1)
    @test parse_bin("0") === 0x00
    @test_throws DomainError parse_bin("0_")
    @test parse_bin("0_"; filter=true) === 0x00
    @test parse_bin("1001 0110 1111"; filter=true) == parse_bin("100101101111")
    @test parse_bin("00000001") === 0x01
    @test parse_bin("000000001") === 0x0001
    # Should succeed
    @test_throws BoundsError parse_bin(UInt8, "0000000001")
    @test parse_bin("10000001") == 129
end

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
    @test_throws DomainError Tuple(bits("<000111>"; strip=false))
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
    BX = BitIntegersX
    @test BX.min_uint_type(0) == UInt8
    @test BX.min_uint_type(1) == UInt8
    @test BX.min_uint_type(7) == UInt8
    @test BX.min_uint_type(8) == UInt8
    @test BX.min_uint_type(9) == UInt16
    @test_throws DomainError BX.min_uint_type(-1)
    @test BX.min_uint_type(1000) == BX.UInt1000
    @test BX.uint_type(8) == UInt8
    @test BX.uint_type(80) == BX.UInt80
end
