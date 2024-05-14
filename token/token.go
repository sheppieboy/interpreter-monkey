package token

type TokenType string

type Token struct{
	Type TokenType
	Literal string
}

const (
	ILLEGAL = "ILLEGAL"
	EOF = "EOF"

	//identifiers + literals

	IDENT = "IDENT"
	INT = "INT"
	STRING = "STRING"

	//Operators
	ASSIGN = "="
	PLUS = "+"
	MINUS = "-"
	BANG = "!"
	ASTERISK = "*"
	SLASH = "/"

	LT = "<"
	GT = ">"
	EQ = "=="
	NOT_EQ = "!="

	//Delimiters
	COMMA = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"
	LBRACKET = "["
	RBRACKET = "]"

	//keywords
	FUNCTION = "FUNCTION"
	LET = "LET"
	IF = "IF"
	ELSE = "ELSE"
	TRUE = "TRUE"
	FALSE = "FALSE"
	RETURN = "RETURN"
)

var keywords = map[string]TokenType{
	"fn": FUNCTION,
	"let": LET,
	"if": IF,
	"else": ELSE,
	"true": TRUE,
	"false": FALSE,
	"return": RETURN,
}

func LookupIdentifier(ident string) TokenType{
	if tok, ok := keywords[ident]; ok{
		return tok
	}
	return IDENT
}