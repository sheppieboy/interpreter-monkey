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
)

func (i *Integer) Type() ObjectType {
	return INTEGER_OBJ
}

