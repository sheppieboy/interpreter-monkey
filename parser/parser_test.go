package parser

import (
	"fmt"
	"interpreter-monkey/ast"
	"interpreter-monkey/lexer"
	"testing"
)
func TestLetStatements(t *testing.T){
	input := `
	let x = 5;
	let y = 10;
	let foobar = 838383;
	`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)
	if program == nil {
		t.Fatalf("ParsePogram() returned nil")
	}
	if len(program.Statements) != 3{
		t.Fatalf("program.Statements does not contain 3 statements. go=%d", len(program.Statements))
	}

	tests := []struct{
		expectedIdentifier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, tt := range tests{
		stmt := program.Statements[i]
		if !testLetStatement(t, stmt, tt.expectedIdentifier){
			return
		}
	}
}

func testLetStatement( t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let'. got=%q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)
	if !ok{
		t.Errorf("s not *ast.LetStatement. got=%T", s)
		return false
	}

	if letStmt.Name.Value != name{
		t.Errorf("letStmt.Name.Value not '%s'. got=%s", name, letStmt.Name.Value)
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("s.Name not '%s'. got=%s", name, letStmt.Name)
	}

	return true
}	

func checkParserErrors(t *testing.T, p *Parser){
	errors := p.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has '%d' error(s)", len(errors))

	for _, msg := range errors{
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

func TestReturnStatements(t *testing.T){
	input := `
	return 5;
	return 10;
	return 993322;
	`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 3{
		t.Fatalf("program.Statements does not contain the 3 statements. got=%d", len(program.Statements))
	}

	for _, stmt := range program.Statements{
		returnStmt, ok := stmt.(*ast.ReturnStatement)
		if !ok {
			t.Errorf("stmt not *ast.returnStatement. got=%T", stmt)
			continue
		}

		if returnStmt.TokenLiteral() != "return"{
			t.Errorf("returnStmt.TokenLiteral not 'return, got=%q", returnStmt.TokenLiteral())
		}
	}
}

func TestIdentifierExpression(t *testing.T){
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	//check if added to statements
	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. go=%d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

	//type check to see if foobar has the type of Expression Statement
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}


	ident, ok := stmt.Expression.(*ast.Identifier) 

	if !ok {
		t.Fatalf("exp not *ast.Identifier. got=%T", stmt.Expression)
	}
	if ident.Value != "foobar" {
		t.Errorf("ident.Value not %s. got=%s", "foobar", ident.Value)
	}
	if ident.TokenLiteral() != "foobar" {
		t.Errorf("ident.TokenLiteral not %s. got=%s", "foobar", ident.TokenLiteral())
	}
}

func TestIntegerLiteralExpression(t *testing.T){
	input := "5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	//statement length
	if len(program.Statements) != 1{
		t.Fatalf("program does not have enough statements. got=%d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

	//type check
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	literal, ok := stmt.Expression.(*ast.IntegerLiteral)

	// expression typecheck
	if !ok {
		t.Fatalf("expression not *ast.IntegerLiteral. got=%T", stmt.Expression)
	}

	//value check
	if literal.Value != 5 {
		t.Errorf("literal value not %d. got %d", 5, literal.Value)
	}

	//token literal
	if literal.TokenLiteral() != "5" {
		t.Errorf("literal.TokenLiteral not %s. got %s", "5", literal.TokenLiteral())
	}
}

func TestParsingPrefixExpressions(t *testing.T){
	prefixTests := []struct{
		input string
		operator string
		integerValue int64
	}{
		{"!5;", "!", 5},
		{"-15", "-", 15},
	}

	for _, tt := range prefixTests{

		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain %d statements. got=%d", 1, len(program.Statements))
		}

		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

		if !ok{
			t.Fatalf("program.Statements[0] is not an ast.ExpressionStatement. got=%T", program.Statements[0])
		}

		exp, ok := stmt.Expression.(*ast.PrefixExpression)

		if !ok {
			t.Fatalf("stmt is not ast.PrefixExpression. got=%T", stmt.Expression)
		}

		if exp.Operator != tt.operator{
			t.Fatalf("exp.Operator is not '%s. got=%s", tt.operator, exp.Operator)
		}

		if !testIntegerLiteral(t, exp.Right, tt.integerValue){
			return 
		}
	}
}

func testIntegerLiteral(t *testing.T, il ast.Expression, value int64) bool{
	integ, ok := il.(*ast.IntegerLiteral)

	if !ok {
		t.Errorf("il not *ast.IntegerLiteral. got=%T", il)
		return false
	}

	if integ.Value != value {
		t.Errorf("integ.Value not %d. got=%d", value, integ.Value)
		return false
	}
	if integ.TokenLiteral() != fmt.Sprintf("%d", value){
		t.Errorf("integ.TokenLiteral not %d. got=%s", value, integ.TokenLiteral())
		return false
	}
	return true
}

