
Decls 
* ImportDecl* NonImportDecl+

ImportDecl 
* ImportFuncDecl
* ImportTypeDecl


ImportFuncDecl
* import func Ident ( FuncArg* ) Type? ;
* import type Ident ;
 
NonImportDecl
* TypeDecl
* FuncDecl

TypeDecl 
* type Ident = Type

FuncDecl
* public? func Ident ( FuncArg* ) Type? CodeBlock 

FuncArg 
* Ident : Type ,

CodeBlock
* { Statement* }

Statement 
* loop CodeBlock
* while ( Expr ) CodeBlock
* if ( Expr ) CodeBlock 
* if ( Expr ) CodeBlock else CodeBlock
* let Ident = Expr ;
* Ident = Expr ; 
* return Expr? ;
* panic ; 
* ;

Type
* uint
* int
* bool
* bytes32
* struct { StructField+ }
* ( )
* ( CommaedTypes )
* \[ \] Type
* [ UnsignedInteger ] Type
* anytype
* Ident

StructField
* Ident : Type ,

CommaedTypes
* Type
* CommaedTypes , Type

Expr
* \- Expr
* ! Expr
* ~ Expr

Expr1
* Expr1 + Factor 
* Expr1 - Factor
* Factor

Factor
* Factor * Term1
* Factor / Term1
* Factor % Term1
* Term1

Term1
* Term1 < Term2
* Term1 > Term2
* Term1 <= Term2
* Term1 >= Term2
* Term2

Term2 
* Term2 == Term3
* Term2 != Term3
* Term3

Term3 
* Term3 & Term4 
* Term4

Term4 
* Term4 ^ Term5
* Term5

Term5 
* Term5 | Term6 
* Term6

Term6 
* Term6 && Term7 
* Term7

Term7
* Term7 || Term8 
* Term8

Term8
* ( Expr ) 
* Ident
* null
* UnsignedInteger
* SignedInteger 
* unsafecast ( Expr , Type )
* ( CommaedExprs , ) 
* struct { FieldInitializer+ } 
* newarray ( Expr , Type ) 
* newfixedarray ( UnsignedInteger , Expr ) 
* newfixedarray ( UnsignedInteger ) 
* uint ( Expr ) 
* int ( Expr ) 
* bytes32 ( Expr ) 
* hash ( Expr ) 
* hash ( Expr , Expr ) 
* len ( Expr ) 
* Term8 [ Expr ] 
* Term8 . Ident 
* Term8 . UnsignedInteger
* Ident ( CommaedExprs? ) 
* Term8 with { [ Expr ] = Expr } 
* Term8 with { Ident : Expr } 

FieldInitializer
* Ident : Expr , 

CommaedExprs
* Expr 
* CommaedExprs , Expr

Ident
* regex:"[a-zA-Z_][a-zA-Z_01-9]*"

UnsignedInteger 
* "0" 
* regex:"[1-9][0-9]*"

SignedInteger
* "0s" 
* regex:"[1-9][0-9]*s" 
