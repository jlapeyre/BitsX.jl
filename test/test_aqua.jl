using BitsX
using Aqua: Aqua

@testset "aqua unbound_args" begin
    Aqua.test_unbound_args(BitsX)
end

@testset "aqua undefined exports" begin
    Aqua.test_undefined_exports(BitsX)
end

# TODO: Much to fix here
# @testset "aqua test ambiguities" begin
#     Aqua.test_ambiguities([BitsX, Core, Base])
# end

# There is a lot of piracy. All in bitintegers.jl
# @testset "aqua piracy" begin
#     Aqua.test_piracy(BitsX)
# end

@testset "aqua project extras" begin
    Aqua.test_project_extras(BitsX)
end

@testset "aqua state deps" begin
    Aqua.test_stale_deps(BitsX)
end

@testset "aqua deps compat" begin
    Aqua.test_deps_compat(BitsX)
end

@testset "aqua project toml formatting" begin
    Aqua.test_project_toml_formatting(BitsX)
end
