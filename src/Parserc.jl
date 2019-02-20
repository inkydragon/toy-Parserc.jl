#=
Parsec:
- Julia version: 1.1.0
=#

module Parserc

using MLStyle

export lex, literal, literal_by_type, literal_by_val,
    and, or, not, trans, Fn, parse, Token, Parser,
    (⇒), (&), (|), (!), (<<), (>>), (∞),
    LexerTable, Reserved

Maybe{T} = Union{Some{T}, Nothing}

include("Token.jl")
include("TypedFunc.jl")
include("Combinator.jl")
include("Lexer.jl")

end # module Parserc
