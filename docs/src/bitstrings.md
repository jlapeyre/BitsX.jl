## Contents
```@contents
Pages = ["bitstrings.md"]
Depth = 5
```

```@meta
CurrentModule = BitsX
```

## Creating and parsing bit strings

[`parse_bin`](@ref) converts a bit string to a bitstype value, such as `UInt64`.
[`parse_bin`](@ref) is similar to `Base.parse`, but is faster, and more versatile.

[`bstring`](@ref) creates a bit string from the bits in a bitstype value, or another object representing an array of bits. [`bstring`](@ref) is similar to `Base.bitstring` in function and performance, but is more versatile.

### Functions

#### `parse_bin`
```@docs
BitsX.ParseBin.parse_bin
```

#### `bstring`
```@docs
BitsX.BStrings.bstring
```

#### `bstringview`
```@docs
BitsX.BStringViews.bstringview
```

#### `normalize_bitstring`
```@docs
BitsX.Bits.normalize_bitstring
```

#### `randbitstring`
```@docs
BitsX.Bits.randbitstring
```

#### `randbitstring!`
```@docs
BitsX.Bits.randbitstring!
```

#### `is_bitstring`
```@docs
BitsX.BitsBase.is_bitstring
```

#### `check_bitstring`
```@docs
BitsX.BitsBase.check_bitstring
```
