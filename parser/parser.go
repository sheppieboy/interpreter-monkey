package parser

import (
	"fmt"
	"interpreter-monkey/ast"
	"interpreter-monkey/lexer"
	"interpreter-monkey/token"
)

//precedences
const (
	_ int = iota
	LOWEST
	EQUALS // ==
	LESSGREATER // > or <
	SUM //+
	PRODUCT //*
	PREFIX // -X or !X
	CALL // fn(x)
)

type Parser struct{
	l *lexer.Lexer
	currToken token.Token
	peekToken token.Token
	errors []string
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser{
	p := &Parser{l:l, errors: []string{},}
	p.nextToken()
	p.nextToken()
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
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
		program.Statements = append(program.Statements, stmt)
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
		return p.parseExpressStatement()
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
	for !p.currTokenIs(token.SEMICOLON){
		p.nextToken()
	}

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
	for !p.currTokenIs(token.SEMICOLON){
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseExpressStatement() *ast.ExpressionStatement{
	stmt := &ast.ExpressionStatement{Token: p.currToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if stmt.Expression == nil{
		fmt.Println("NIL")
		return nil
	}

	if p.peekTokenIs(token.SEMICOLON){
		p.nextToken()
	}
	fmt.Println("called")
	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression{
	prefix := p.prefixParseFns[p.currToken.Type]
	if prefix == nil{
		return nil
	}
	leftExp := prefix()
	return leftExp
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn func(ast.Expression) ast.Expression
)

func (p* Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn){
	p.prefixParseFns[tokenType] = fn
}

func (p* Parser) registerInfix(tokenType token.TokenType, fn infixParseFn){
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) parseIdentifier() ast.Expression{
	return &ast.Identifier{Token: p.currToken, Value: p.currToken.Literal}
}