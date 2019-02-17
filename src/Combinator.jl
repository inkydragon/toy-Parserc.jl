#=
Combinator:
- Julia version: 1.1.0
=#

import Base: (&), (|), (<<), (>>), getindex
using Test

Stream{T} = AbstractArray{T, 1}
Parser{T} = Fn{Stream{Token}, Tuple{Maybe{T}, Stream{Token}}}

function parse(p :: Parser{T}, s :: Stream{Token}) :: Tuple{Maybe{T}, Stream{Token}} where T
    p(s)
end

# Test Data
mock_stream = [
    Token(:keyword, "let", 1, 1, 1, "..."),
    Token(:identifier, "a", 1, 2, 1, "..."),
]
mock_stream2 = [
    Token(:identifier, "a", 1, 2, 1, "..."),
]
mock_stream3 = [
    Token(:keyword, "let", 1, 1, 1, "..."),
]

#=
    Parse functions
=#
literal_by_type(typ :: Symbol) :: Parser{Token} =
    Parser{Token}(
        (stream :: Stream{Token}) ->
        @match stream begin
            [hd, tl...] && if hd.typ === typ end => (Some(hd), tl)
            a => (nothing, a)
        end
    )
# # Test:
parse_id = literal_by_type(:identifier)
# @info parse(parse_id, mock_stream) # => nothing

mock_stream0 = [
    Token(:identifier, "a", 1, 2, 1, "..."),
    Token(:keyword, "let", 1, 1, 1, "..."),
]
# @info parse(parse_id, mock_stream0) # => Some(xxx)

literal_by_val(val :: String) :: Parser{Token} =
    Parser{Token}(
        (stream :: Stream{Token}) ->
        @match stream begin
            [hd, tl...] && if hd.val === val end => (Some(hd), tl)
            a => (nothing, a)
        end
    )
# # Test:
parse_val = literal_by_val("let")
# @info parse(parse_val, mock_stream) # => Some(xxx)

literal(predicate) :: Parser{Token} =
    Parser{Token}(
        @λ begin
            ([hd, tl...] && if predicate(hd) end) -> (Some(hd), tl)
            a -> (nothing, a)
        end
    )

and(p1 :: Parser{A}, p2 :: Parser{B}) where {A, B} =
    Parser{Tuple{A, B}}(
        (stream :: Stream{Token}) ->
        @match p1(stream) begin
            (nothing, _) && b => b
            (some1, tl) =>
            @match p2(tl) begin
                (nothing, _) => (nothing, stream)
                (some2, tl)  => (Some((some1.value, some2.value)), tl)
            end
        end
    )
@inline (&)(p1 :: Parser{A}, p2 :: Parser{B}) where {A, B} = and(p1, p2)
# # Test:
# @info parse(parse_val & parse_id, mock_stream) # => Some(Token, Token)

function or(p1 :: Parser{A}, p2 :: Parser{A}) where A
    Parser{A}(
        (stream :: Stream{Token}) ->
        @match p1(stream) begin
            (nothing, _) => p2(stream)
            a => a
        end
    )
end
@inline (|)(p1 :: Parser{A}, p2 :: Parser{A}) where A = or(p1, p2)
# # Test:
# @info parse(parse_id | parse_val, mock_stream2)
# @info parse(parse_id | parse_val, mock_stream3)

struct Inf <: Number end
∞ = Inf()

function seq(p :: Parser{A}, atleast :: Number, atmost :: Number) :: Parser{Vector{A}} where A
    Parser{Vector{A}}(
    atmost === ∞ ?
        function (stream :: Stream{Token})
            res :: Vector{A} = []
            remained = stream
            while true
                (elt, remained) = p(remained)
                if elt === nothing
                    break
                end
                push!(res, elt.value)
            end
            length(res) <= atleast ?
                (nothing, stream) :
                (Some(res), remained)
        end :
        function (stream :: Stream{Token})
            res :: Vector{A} = []
            remained = stream
            while true
                if length(res) >= atmost
                    break
                end
                (elt, remained) = p(remained)
                if elt === nothing
                    break
                end
                push!(res, elt.value)
            end
            length(res) <= atleast ?
                (nothing, stream) :
                (Some(res), remained)
        end
    )
end
# 重载 []
getindex(p :: Parser{A}, atleast :: Int) where A =
    seq(p, atleast, ∞)
getindex(p :: Parser{A}, atleast :: Int, atmost :: T) where {A, T <: Number} =
    seq(p, atleast, atmost)
# # Test:
# @info parse((parse_id | parse_val)[0], mock_stream)

mock_stream4 = [
    Token(:keyword, "let", 1, 1, 1, "..."),
    Token(:identifier, "a", 1, 2, 1, "..."),
    Token(:keyword, "let", 1, 3, 1, "..."),
    Token(:identifier, "a", 1, 4, 1, "..."),
]
mock_stream5 = [
    Token(:a, "a", 1, 1, 1, "..."),
    Token(:b, "b", 1, 2, 1, "..."),
]
mock_stream6 = [
    Token(:identifier, "a", 1, 2, 1, "..."),
    Token(:identifier, "a", 1, 2, 1, "..."),
    Token(:identifier, "a", 1, 3, 1, "..."),
    Token(:identifier, "a", 1, 4, 1, "..."),
]
# @info parse((parse_id | parse_val)[0], mock_stream4) # remained = []

# @info parse((parse_id | parse_val)[1], mock_stream4) # remained = []
# @info parse((parse_id | parse_val)[1], mock_stream5) # result = nothing

# @info parse((parse_id | parse_val)[1,2], mock_stream4) # len(result) = len(remain) = 2
# @info parse(parse_id[1,2], mock_stream6) # len(result) = len(remain) = 2

#=
ref: 一个parserc的教程 - 综合讨论区 / Julia入门 - Julia中文社区
+ [添加等号](https://discourse.juliacn.com/t/topic/1431/2)
+ [修改判断顺序](https://discourse.juliacn.com/t/topic/1431/4)
=#

opt(p :: Parser{A}) where A =
    Parser{Maybe{A}}(
        (stream :: Stream{Token}) ->
        @match p(stream) begin
            (nothing, _) => (Some(nothing), stream)
            (some, tl)   => (Some(some), tl)
        end
    )
# # Test:
# @info parse(opt(parse_id), mock_stream3) # Some(nothing)
# @info parse(opt(parse_id), mock_stream2) # Some(Some(xxx))

# trans :: Parser a -> (a -> b) -> Parser b
trans(p :: Parser{A}, fn :: Fn{A, B}) where {A, B} =
    Parser{B}(
        (stream :: Stream{Token}) ->
        @match p(stream) begin
            (nothing, _) && a => a
            (some, tl) => (Some(fn(some.value)), tl)
        end
    )
(⇒)(p :: Parser{A}, fn :: Fn{A, B}) where {A, B} = trans(p, fn)
# # Test:
parse_sym = parse_id ⇒ Fn{Token, Symbol}(x -> Symbol(x.val))
# @info parse(parse_sym, mock_stream2)

function not(p :: Parser{A}) :: Parser{Token} where A
    Parser{Token}(
        stream ->
        @match (stream, p(stream)) begin
            ([hd, tl...], (nothing, _)) => (Some(hd), tl)
            _ => (nothing, stream)
        end
    )
end
@inline (!)(p :: Parser{A}) where A = not(p)
# # Test:
# @info parse(!parse_id, mock_stream3) # works fine for wo
# println(parse(!parse_id, mock_stream3)) # Backup

#= av43674707 END =#
