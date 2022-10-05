using BitsX: min_bits, undigits, bits, BitVector1Mask
using Test

@testset "Bits.Xjl" begin
    n = 12345
    @test undigits(digits(n)) == n
    @test undigits(digits(n, base=2), base=2) == n
    @test undigits(digits(n, base=2), base=10) != n
    tup = (1,1,1,0,0,0)
    @test Tuple(bits(tup)) == tup
    @test Tuple(bits("000111")) == tup
    @test Tuple(bits("<000111>")) == tup
    @test string(bits(Tuple(bits("111000")))) == "<111000>"
    @test reverse(bits("111000")) == bits("000111")

    @test bits("0101") isa BitVector1Mask{UInt}
    @test bits(Int, "0101") isa BitVector1Mask{Int}

    @test_throws OverflowError bits("1"^100)
    @test_throws OverflowError undigits(UInt, fill(1, 1000))
end
