@testset "bitarrayalt" begin
    balt = BitArrayAlt(Chunks(rand(UInt8, 10)));
    @test length(balt.chunks) == 10
    @test eltype(balt) == Bool
    @test length(balt) == 80
    @test balt[1] isa Bool
    @test balt[end] isa Bool
    @test length([x for x in balt]) == length(balt)

    # We don't want this!
    @test balt[:] isa Vector{Bool}
    # We do want this.
    @test_broken balt[:] isa BitArrayAlt{UInt8}

    balt16 = BitArrayAlt(Chunks(rand(UInt16, 10)))
    @test length(balt16.chunks) == 10
    @test length(balt16) == 160

    chunks = Chunks(rand(UInt8, 3 * 10));
    # Use 21 of the 24 bits in the first dimension
    balt = BitArrayAlt(chunks, (21, 10))

    # Check that discarding the first 3 bits gives the expeceted result
    _test_one = (balt, i) -> begin
        ind = (i-1)*3
        bstring(balt.chunks[ind+1])[4:end] * bstring(balt.chunks[ind+2])  * bstring(balt.chunks[ind+3]) == bstring(balt[:, i])
    end
    @test all(_test_one(balt, i) for i in 1:10)

    balt[1] = true;
    @test balt[1]
    balt[1] = false;
    @test ! balt[1]
end

@testset "bitarrayx" begin
    ba = BitArrayX{UInt8}(undef, 128)
    @test length(ba.chunks) == 16
    @test eltype(ba) == Bool
    @test length(ba) == 128
    @test ba[1] isa Bool
    @test ba[end] isa Bool
    @test length([x for x in ba]) == 128

    # We don't want this!
    @test ba[:] isa Vector{Bool}
    # We do want this.
    @test_broken ba[:] isa BitArrayX{UInt8}

    ba[1] = true;
    @test ba[1]
    ba[1] = false;
    @test ! ba[1]
end

@testset "more bits.jl" begin
    @test asint(3) === 3
    @test reinterpret(Float64, asint(3.0)) === 3.0
    @test asint(reinterpret(Float64, -1)) === -1

    @test bit(0, 1) == 0
    @test bit(1, 1) == 1
    @test bit(5, 1) == 1
    @test bit(4, 1) == 0
    s = "11001100"
    @test [bit(s, i) for i in biteachindex(s)] == [1,1,0,0,1,1,0,0]
    # TODO: masked

    @test normalize_bitstring("") === ""
    @test normalize_bitstring("1") === "1"
    @test normalize_bitstring("x") === ""
    @test normalize_bitstring("0") === "0"
    @test normalize_bitstring("110011") === "110011"
    @test normalize_bitstring("11 00 11") === "110011"

    @test is_bitstring("110011")
    @test ! is_bitstring("zebra")
    @test ! is_bitstring("110 011")
    @test is_bitstring(codeunits("110011"))
    @test isnothing(check_bitstring(codeunits("110011")))
    @test_throws ArgumentError check_bitstring("110 011")
    @test count_bits("110011") == 6
    @test count_bits("1z1e0 b 01r1a") == 6

    for x in (0, 1, 5, 1234)
        n0 = bit_count_zeros(x)
        n1 = bit_count_ones(x)
        br = bitreverse(x)
        @test n0 + n1 == bitlength(x)
        s = String(bstringview(x))
        n0s = bit_count_zeros(s)
        n1s = bit_count_ones(s)
        brs = bitreverse(s)
        @test n0s == n0
        @test n1s == n1
        @test bstringview(br) == brs
        v = collect(bitvecview(x))
        n0v = bit_count_zeros(v)
        n1v = bit_count_ones(v)
        @test n0v == n0
        @test n1v == n1
    end
end

@testset "bitvector" begin
    b1 = BitVector([1,0,1,0,0,0,0,0])
    b2 = bitvector(UInt8(5))
    @test b1 == b2
end

@testset "rightmask, leftmask, rangemask" begin
    for i in 0:63
        @test rightmask(i) === UInt(2)^i - 1
        @test rightmask(OneBased(), i) === UInt(2)^i - 1
        @test rightmask(ZeroBased(), i - 1) === UInt(2)^i - 1
        @test leftmask(i+1) === ~rightmask(i)
        @test leftmask(ZeroBased(), i) === ~rightmask(i)
    end
    @test leftmask(UInt16, 1) isa UInt16
    @test rangemask(UInt16, 1, 7) isa UInt16
    for i in 0:7
        @test rightmask(UInt8, i) === UInt8(2^i - 1)
    end
    # TODO: desired?
    for i in (64, 100, 10^3)
        m = rightmask(i)
        @test m === typemax(typeof(m))
    end

    for i in 1:63
        @test mask(i) == 2^(i-1)
    end

    @test mask((10, 20, 30)) === mask(10) | mask(20) | mask(30)
    @test mask((10, 11, 12, 13)) === mask(10:13)
    @test mask((10, 11, 12, 13, 60, 61, 62)) === mask((10:13, 60:62))
    @test mask((10, 11, 12, 13, 60, 61, 62)) === mask((10:13, 60, 61, 62))
    @test mask(Base.oneto(20)) === mask(1:20)
end

@testset "char, code, zero, one" begin
    @test !is_one_char(1)
    @test !is_one_char(0)
    @test !is_one_char(2)
    @test is_one_char('1')
    @test !is_one_char('0')
    @test is_one_char(UInt8('1'))
    @test !is_one_char(UInt8('0'))
    @test is_one_char(UInt('1'))
    @test !is_one_char(UInt('0'))

    @test !is_zero_char(1)
    @test !is_zero_char(0)
    @test !is_zero_char(2)
    @test is_zero_char('0')
    @test !is_zero_char('1')
    @test is_zero_char(UInt8('0'))
    @test !is_zero_char(UInt8('1'))
    @test is_zero_char(UInt('0'))
    @test !is_zero_char(UInt('1'))

    @test_throws MethodError is_zero_char("zebra")

    for c in ('0', '1', UInt8('0'), UInt8('1'))
        @test is_binary_char(c)
    end
    for c in ('a', UInt8('a'), 0, 1)
        @test !is_binary_char(c)
    end
    @test_throws MethodError is_binary_char("zebra")

    for c in ("0", "1", cos, complex(1), 1.0, [0, 1])
        @test_throws MethodError binzero(c)
        @test_throws MethodError binone(c)
    end

    for T in (Int, UInt, UInt8, Int8, BigInt)
        @test binzero(T(47)) == 0
        @test binone(T(47)) == 1
        @test isa(binzero(T(47)), T)
        @test isa(binone(T(47)), T)
    end

    dims = (2, 2)
    @test binzero(rand(Int, dims)) == zeros(Int, (2,2))
    @test binone(rand(Int, dims)) == LinearAlgebra.diagm(ones(Int, dims[1]))

    for (zero_c, one_c, neither_c) in ((0, 1, 2), ('0', '1', 'a'))
        @test isbinzero(zero_c)
        @test !isbinzero(one_c)
        @test !isbinzero(neither_c)
        @test isbinone(one_c)
        @test !isbinone(zero_c)
        @test !isbinone(neither_c)
    end

    @test_throws DomainError to_binary_char(3)
    @test_throws DomainError to_binary_char_code(3)

    for (c0, c1) in ((0, 1), ('0', '1'), (false, true))
        @test to_binary_char(c0) === '0'
        @test to_binary_char(c1) === '1'
        @test to_binary_char_code(c0) === UInt8('0')
        @test to_binary_char_code(c1) === UInt8('1')
    end
    @test_throws DomainError to_binary_char_code(0x30)
    @test_throws DomainError to_binary_char_code(0x31)

    @test from_binary_char('0') === false
    @test from_binary_char('1') === true
    @test_throws DomainError from_binary_char('a')
    @test from_binary_char(Int, '0') === 0
    @test from_binary_char(Int, '1') === 1
    @test from_binary_char(Char, '1') === '1'
    @test from_binary_char(Char, '0') === '0'
end

@testset "randbitstring" begin
    n = 10
    s = randbitstring(n)
    @test isa(s, String)
    @test is_bitstring(s)
    @test length(s) == n
    @test bitlength(s) == n
    @test isa(randbitstring(n, (2,)), Vector{String})
    @test isa(randbitstring(n, (2, 3)), Matrix{String})

    sa = randbitstring(n, (2, 3))
    @test size(randbitstring!(sa, n)) == (2, 3)
    @test size(randbitstring!(sa, n + 1)) == (2, 3)
end

@testset "bitsizeof bitlength bitsize" begin
    @test bitsizeof(Int) == sizeof(Int) * 8
    @test bitsizeof(Int8) == 8
    @test bitsizeof(Int16) == 16
    @test bitsizeof(UInt16) == 16
    # TODO: Is this what we want?
    @test bitsizeof(Char) == 32
    @test_throws MethodError bitsizeof(1)
    for T in (String, Vector{Bool}, BigInt, BigFloat)
        @test_throws MethodError bitsizeof(T)
    end
    @test bitsizeof(NTuple{3, Int}) == 3

    for T in (Int, UInt8, BigInt, String, Array)
        @test_throws MethodError bitlength(T)
        @test_throws MethodError bitsize(T)
    end
    # TODO: test types defined in BitsX
    ba = [0,1,0,0,1]
    for bs in ("01001", ba, Bool[ba...], BitVector(ba), bstringview(ba), Tuple(ba))
        @test bitlength(bs) == 5
        @test bitsize(bs) == (5,)
    end
    @test bitlength(1) == sizeof(Int) * 8
    @test bitlength(UInt8(1)) == 8
    @test bitlength(Int16(1)) == 16
    @test bitlength(big(1)) == 64
    @test bitlength(true) == 1

    @test bitlength("zebra") == 5
end


@testset "bstringview" begin
    v = [1, 0, 1, 0, 0]
    bs = bstringview(v)
    @test bs isa BStringView
    @test bs == "10100"
    @test parent(bs) == v
    @test ncodeunits(bs) == length(v)
    @test codeunit(bs) == UInt8
    @test codeunit(bs, 1) == 0x31
    @test isvalid(bs, 1)
    @test !isvalid(bs, 10)
    @test length(bs) == length(v)
    @test bs[end] == '0'
    @test bs[end-1] == '0'
    @test bs[[1,3,5]] == "110"
    sbs = String(bs)
    @test sbs isa String
    @test [x for x in bs] == [x for x in sbs]
    @test reverse(bstringview([1,1,0,0])) == "0011"
    rev1 = reverse(bstringview(UInt(1) << 4 - 1, 8))
    @test String(rev1) == "00001111"
    @test rev1 == "00001111"

    bs0 = bstringview(UInt8(1) << 8 - UInt8(1))
    @test bs0 == "11111111"
    @test cmp(bs0, "11111111") == 0

    bs1 = bstringview(UInt64(1) << 8 - UInt64(1), 8)
    @test bs1 == "11111111"
    @test cmp(bs1, "11111111") == 0

    n = UInt64(1000)
    bs1 = bstringview(n)
    sbs1 = "0001011111000000000000000000000000000000000000000000000000000000"
    @test bs1 == sbs1
    @test bs1[1] == '0'
    @test bs1[4] == '1'
    @test parent(bs1) === n
    @test ncodeunits(bs1) == 64
    @test codeunit(bs1) == UInt8
    @test codeunit(bs1, 4) == 0x31
    @test isvalid(bs1, 1)
    @test isvalid(bs1, 64)
    @test !isvalid(bs1, 65)
    @test bs1[6:11] == "111110"
    @test String([x for x in bs1]) == bs1

    @test bstringview(sbs1) == sbs1
    @test bstringview(sbs1) !== sbs1

    t = (1, 1, 0, 0)
    tv = bstringview(t)
    @test tv == "1100"
    @test bitsize(t) == (4, )

    tv1 = bstringview(BitVector(t))
    @test tv1 == "1100"

    bv = bstringview(5, 4)
    @test length(bv) == 4
    @test String(bv) == "1010"
    # Get "0000" below
    @test String(bv[1:4]) == "1010"

    bss = bstringview.([1 2; 3 4], 3)
    @test string(bss) == "BStringView{Int64}[\"100\" \"010\"; \"110\" \"001\"]"

    let str = bstring(BitVector(undef, 3))
        @test isa(str, String)
        @test length(str) == 3
    end
end

@testset "bstring" begin
    @test bstring(5; sep=100) == bstring(5)
    @test bstring(5; sep=0) == bstring(5)
    @test bstring(5; len=0) == ""
    @test length(bstring(5; sep=1)) == 127
    @test_throws ArgumentError bstring(5; len=-1)
    @test_throws ArgumentError bstring(5; sep=-42)
end

@testset "bitvecview" begin
    s = "1100"
    v = Bool[1, 1, 0, 0]
    bv = bitvecview(s)
    @test bv == v
    @test bv isa BitVectorView{Bool, String}

    s1 = "111010011"
    @test bitmatview(s1) == Bool[1 0 0; 1 1 1; 1 0 1]

    s2 = "111000"
    @test bitmatview(s2, 2) == Bool[1 1 0; 1 0 0]
    @test bitmatview(s2, (2, 2)) == Bool[1 1 ; 1 0]
    @test_throws DimensionMismatch bitmatview(s2, (2, 4))

    n = 5
    @test length(bitvecview(n)) == 64
    @test length(bitvecview(Int8(n))) == 8
    @test length(bitvecview(n, 10)) == 10
    @test size(bitmatview(UInt16(n))) == (4, 4)

    @test bitvecview(s2, Val(true)) == bitvecview(bitreverse(s2), Val(false))
    @test bitvecview(123, 64, Val(true)) == bitvecview(bitreverse(123), 64, Val(false))
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

    @test isa(parse_bin(Integer, "1100"), Unsigned)
    @test parse_bin(Unsigned, "1100") == parse_bin("1100")
    @test isa(parse_bin(Signed, "1100"), Int8)

    _identity(x) = parse_bin(typeof(x), bitstring(x))
    for T in (Int, UInt, Int16, Int16, UInt16, Float16, Float32, Float64)
        x = T(11)
        @test _identity(x) === x
    end
    @test parse_bin(Bool, "1")
    @test !parse_bin(Bool, "0")
end

@testset "BitsX.jl" begin
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
