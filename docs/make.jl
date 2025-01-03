using Documenter

#push!(LOAD_PATH,"../src/")
push!(LOAD_PATH,"..")

using BitsX

# You can do this:
Documenter.DocMeta.setdocmeta!(BitsX, :DocTestSetup, :(using BitsX); recursive=true)

# The following in index.md
# ```@meta
# DocTestSetup = quote
#     using BitsX
# end
# ```

makedocs(
    sitename = "BitsX",
    format = Documenter.HTML(),
    doctest = true,
    modules = Module[BitsX],
    warnonly = true,

    pages = [
        "Introduction" => "index.md",
        "BitsBase" => "bitsbase.md",
        "Working with bit strings" => "bitstrings.md",
        "Masks and miscellany" => "bits.md",
        "BitArrays" => "bitarrays.md",
        "Everything else" => "misc.md",
    ]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    repo = "https://github.com/jlapeyre/BitsX.jl.git"
)
