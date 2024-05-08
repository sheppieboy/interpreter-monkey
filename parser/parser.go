package parser

import (
	"interpreter-monkey/lexer"
	"interpreter-monkey/token"
)

type Parser struct{
	l *lexer.Lexer

	currToken token.Token
	peekToken token.Token
}

func New(l *lexer.Lexer) *Parser{
	p := &Parser{l:l}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken(){
	p.currToken = p.peekToken
	p.peekToken = p.l.NextToken()
}