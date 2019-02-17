#=
Token: 定义 Token 结构体
- Julia version: 1.1.0
=#

struct Token
    typ :: Symbol   # type
    val :: String   # value
    offeset :: Int  # string offset
    lineno  :: Int  # line number
    colno   :: Int  # column number
    filename :: Union{Nothing, String}  # file name
end
