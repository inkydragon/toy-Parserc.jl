#=
Parsec:
- Julia version: 1.1.0
=#

module Parsec

using MLStyle

Maybe{T} = Union{Some{T}, Nothing}

include("Token.jl")
include("TypedFunc.jl")
include("Combinator.jl")

end # module Parsec
