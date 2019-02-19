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
