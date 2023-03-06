module BitConvert

using Base: CodeUnits
import ..ParseBin

bitconvert(::Type{T}, data::Union{AbstractString, CodeUnits}; kwargs...) where {T <: Real} =
    ParseBin.parse_bin(T, data; kwargs...)

end # module BitConvert
