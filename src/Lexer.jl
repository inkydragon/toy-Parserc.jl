#=
Lexer:
- Julia version: 1.1.0
=#

LexerTable = Vector{Pair{Symbol, Function}}
Reserved = Dict{String, Symbol}

const NEWLINE = '\n'

function lex(
        c :: Channel,
        text :: String,
        lexer_table :: LexerTable,
        reserved :: Reserved,
        fname :: String
    )
    text_length = length(text)
    colno   :: Int = 1
    lineno  :: Int = 1
    pos     :: Int = 1

    while text_length >= pos
        is_unknown = true
        for (typ, case) in lexer_table
            pat :: Union{String, Nothing} = case(text, pos)
            if pat === nothing
                continue
            end

            let typ = get(reserved, pat) do
                    typ
                end
                put!(c, Token(typ, pat, pos, lineno, colno, fname))
            end

            pat_chars = collect(pat)
            n = length(pat_chars)
            line_inc = count(pat_chars .== NEWLINE)

            if line_inc !== 0
                latest_newline_idx = findlast(==(NEWLINE), pat_chars)
                colno = n - latest_newline_idx + 1
                lineno += line_inc
            else
                colno += n
            end
            pos += n
            is_unknown = false
            break
        end

        if !is_unknown
            continue
        end

        println("No handler for character `$(repr(text[pos]))`.")
        char = text[pos]
        put!(c, Token(:unknown, String([char]), pos, lineno, colno, fname))
        if char == '\n'
            lineno += 1
            colno += 1
        end
        pos += 1

    end
end
lex(text :: String, lexer_table :: LexerTable, reserved :: Reserved, fname :: String) =
    Channel(c -> lex(c, text, lexer_table, reserved, fname))


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
# lex(
#     text,
#     convert(LexerTable, lexer_table),
#     Reserved(),
#     "a.txt"
# ) |> ( x -> foreach(println, x) ) ∘ collect
