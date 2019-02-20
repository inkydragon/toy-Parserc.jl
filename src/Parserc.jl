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

lexer_table = [
    :parens => function (str, offset)
        let c = str[offset]
            c in ('(', ')') ? String([c]) : nothing
        end
    end,

    :identifier => let id_regex = r"\G[a-zA-Z_]{1}[a-zA-Z_0-9]*"
        function (str, offset)
            res = match(id_regex, str, offset)
            res === nothing ? nothing : res.match
        end
    end,

    :space => let space_regex = r"\G\s"
        function (str, offset)
            res = match(space_regex, str, offset)
            res === nothing ? nothing : res.match
        end
    end,

    :number => let num_regex = r"0[Xx][\da-fA-F]+|\d+(?:\.\d+|)(?:E\-{0,1}\d+|)"
        function (str, offset)
            res = match(num_regex, str, offset)
            res === nothing ? nothing : res.match
        end
    end,

    :string => let str_regex = r"\G[A-Z]\'([^\\\']+|\\.)*?\'|\'([^\\\']+|\\.)*?\'"
        function (str, offset)
            res = match(str_regex, str, offset)
            res === nothing ? nothing : res.match
        end
    end

]

# Test text
text = "(a 1 2) (d e (e f g)) ('abc') #"
lex(
    text,
    convert(LexerTable, lexer_table),
    Reserved(),
    "a.txt"
) |> ( x -> foreach(println, x) )

end # module Parserc
