package parser

import (
	"fmt"
	"interpreter-monkey/ast"
	"interpreter-monkey/lexer"
	"interpreter-monkey/token"
)

type Parser struct{
	l *lexer.Lexer
	currToken token.Token
	peekToken token.Token
	errors []string
}

func New(l *lexer.Lexer) *Parser{
	p := &Parser{l:l, errors: []string{},}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken(){
	p.currToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}
	for p.currToken.Type != token.EOF{
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.currToken.Type{
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return nil
	}
}


func (p *Parser) parseLetStatement()*ast.LetStatement{
	stmt := &ast.LetStatement{Token: p.currToken}

	if !p.expectPeek(token.IDENT){
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.currToken, Value: p.currToken.Literal}

	if !p.expectPeek(token.ASSIGN){
		return nil
	}

	//skip over expressiosn for now
	if !p.currTokenIs(token.SEMICOLON){
		p.nextToken()
	}


	fmt.Println(stmt)
	return stmt
}

func (p *Parser) currTokenIs(t token.TokenType) bool{
	return p.currToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool{
	if p.peekTokenIs(t){
		p.nextToken()
		return true
	}else{
		p.peekError(t)
		return false
	}
}

func (p *Parser) peekTokenIs(t token.TokenType) bool{
	return p.peekToken.Type == t
}

func (p *Parser) Errors() []string{
	return p.errors
}

func (p *Parser) peekError(t token.TokenType){
	msg := fmt.Sprintf("expected next token to be %s, go %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement{
	stmt := &ast.ReturnStatement{Token: p.currToken}
	p.nextToken()

	//skip over expressiosn for now
	if !p.currTokenIs(token.SEMICOLON){
		p.nextToken()
	}
	return stmt
}