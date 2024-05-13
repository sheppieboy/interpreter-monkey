package object

import "fmt"
type Integer struct{
	Value int64
}

func (i *Integer)Inspect() string{
	return fmt.Sprintf("%d", i.Value)
}

type ObjectType string

const (
	INTEGER_OBJ = "INTEGER"
	BOOOLEAN_OBJ = "BOOLEAN"
	NULL_OBJ = "NULL"
)

func (i *Integer) Type() ObjectType {
	return INTEGER_OBJ
}

type Boolean struct {
	Value bool
}
func (b *Boolean) Type() ObjectType {
	return BOOOLEAN_OBJ
}

func (b *Boolean) Inspect() string {
	return fmt.Sprintf("%t", b.Value)
}

type Null struct{}

func (n *Null) Type() ObjectType{
	return NULL_OBJ
}
func(n *Null) Inspect() string {
	return "null"
}

