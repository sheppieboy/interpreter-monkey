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

	//Operators
	ASSIGN = "="
	PLUS = "+"

	//Delimiters
	COMMA = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	//keywords
	FUNCTION = "FUNCTION"
	LET = "LET"
)

var keywords = map[string]TokenType{
	"fn": FUNCTION,
	"let": LET,
}

func LookupIdentifier(ident string) TokenType{
	if tok, ok := keywords[ident]; ok{
		return tok
	}
	return IDENT
}