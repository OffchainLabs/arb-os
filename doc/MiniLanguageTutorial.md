# Mini language tutorial

Mini is a programming language and compiler designed for writing code for the Arbitrum Virtual Machine (AVM) platform.  The language is simple but has some features customized for AVM.  This tutorial will tell you what you need to know to write Mini programs.

[This document lists potential changes or improvements to Mini in brackets like this.  Your feedback on these ideas is welcome.]

## Structure of a program

A Mini program is written as a set of source code files.  You can write separate source code files, and compile and link them together to make an executable AVM program.

## Top level declarations

A Mini source code file consists of a series of top-level declarations.  Import declarations declare types and functions that live in other files and are being imported so that the code in the current file can use them. Non-import declarations are everything else. 

The import declarations must come first. It is an error to put an import declaration after a non-import declaration.

Within each group (import or non-import) the order of declarations does not matter.

### Import declarations

`import` type *name*;

> This declares an imported type, which is assumed to be defined in another source code file.  The code in the local file can refer to the type as *name*, but it cannot know anything about the internals of the type. 

`import func` *name* ( *argname1: type1, argname2: type2, ...* ) *returntype*;

> This declares an imported function, which is assumed to be defined in another source code file. The code in the local file can call this function as if it were in the local file.  The syntax follows functional declaration syntax (as defined below), except that an import ends with a semicolon where a local function declaration would instead have the function's code.
>
> The compiler does not check whether the type signature declared here matches the type signature of the actual implementation of the function elsewhere.  If the type signatures are different, this may lead to a runtime error.
>
> [Potential change: we could add the word unsafe to this somehow. Then we could say that every construction that can lead to an undetected type error has the word `unsafe` in its name.]
>
> [Potential improvement: allow interfaces to be specified in separate files, and allow a code file to say that it implements or uses a particular interface. This would make it easier to maintain consistency. Alternatively, the linker could verify consistency.]

### Non-import declarations

`type` *name* = *type*

> This declares a type alias, allowing name to be used as a synonym for the specified type.  (Note that in the current syntax there is not a semicolon at the end.)

var *name* : *type* ;

> This declares a global variable. If type is an atomic type, the variable will be initialized to the zero value for that type. Otherwise the variable will be uninitialized. Reading a global variable of non-atomic type before initializing it will cause undefined behavior.

`[public]  func`` *name* ( *argname1: type1, argname2: type2, ...* ) *returntype codeblock*

> This declares a function and provides its code.
>
> The public modifier is optional.  It indicates that the function can be called by code outside this source code file. Non-public functions cannot be called directly by outside code.  (However, pointers to non-public functions can be passed to outside code, and this would allow the pointed-to function to be called externally.
>
> If the function does not return a value, *returntype* should be `void`. Otherwise the function will return a single value of the specified type. (We'll see below that the type can be a tuple, allowing multiple values to be packaged together into a single return value.)
>
> If *returntype* is not `void`, then the compiler must be able to infer that execution cannot reach the end of *codeblock* (so that the function terminates via a `return` statement, or the function runs forever).

## Types

Mini is a type-checked language.  The compiler should catch any inconsistent use of types. We believe there are only two ways that type errors can go undetected by the compiler: (1) `import func` statements that use a different type signature from the actual implementation of the function, and (2) incorrect uses of the `unsafecast` operator.

Mini has the following types:

`bool`

> true or false (an atomic type with the zero value `false`)

`uint`

> a 256-bit unsigned integer (an atomic type with the zero value `0`, and a numeric type)

`int`

> a 256-bit signed (twos complement) integer (an atomic type with the zero value `0`, and a numeric type)

`bytes32`

> a string of 32 bytes (an atomic type with the zero value of 32 zero bytes)

( *type1*, *type2*, ... )

> a tuple, consisting of zero or more ordered, typed fields (a compound type)

[ *size* ] *type*

> a fixed-size array of values, all of the same type; *size* must be a constant nonzero unsigned integer (a compound type)

[ ] *type*

> an array of values, all of the same type (a compound type)

`struct` { *name1: type1, name2:type2, ... ,* }

> a struct with one or more named, typed fields (a compound type)

`func` ( *type1, type2, ...*) *returntype*

> a reference to a function

`anytype`

> a value of unknown type

void

> strictly speaking, this is not a type but is used as the "return type" for a function that does not return a value

## Equality and assignability for types

Two types are equal if they have the same structure. Type aliases, as defined by non-import declarations, do not create a new type but simply define a shorthand method for referring to the underlying type.  

Every atomic type is equal to itself.

Two tuple types are equal if they have the same number of fields and their field types are equal, field-by-field.

Two fixed-size array types are equal if they have the same size and same field type.

Two array types are equal if they have the same field type.

Two struct types are equal if have the same number of fields, and each field has the same name and equal type, field-by-field.

Two func types are equal if they have the same number of argument types, each argument type is equal, argument-by-argument, and the return types are equal (or both return types are `void`).

`anytype` equals itself.

Each imported type equals itself.

Unless specified as equal by the rules above, a pair of types is unequal.

A variable, field or function parameter of type *T1* can receive a value of type *T2* if either (a) *T1* and *T2* are equal, or (b) *T1* is `anytype`.

## Values

All values in Mini are immutable. There is no way to modify a value. You can only create new values that are equal to existing ones with modifications.  (That's what the `with` operator does.)

Because values are immutable, there is no notion of a reference to a value.  As far as the semantics of Mini are concerned, there are only values, and any assignment or passing of values is done by copying (although the compiler can optimize by creating a new pointer to the underlying immutable object). 

[Implementation note: Because of immutability, the compiler can choose whether to implement "copying" of an object by creating a fresh copy of its contents or by just creating a new pointer reference to the object. The difference only affects the efficiency of the generated code. It is impossible to create a cyclic data structure. This means that the underlying implementation doesn't need to use garbage collection but can always use reference-counting to achieve optimal cleanup of unused copy-by-reference objects.]

### Comparing values for equality

Two values are equal if they have the same type and the same contents.  Equality checking for compound types works as expected, with a "deep comparison" of the fields.  Two function references are equal if they refer to the same function.  

Values of type anytype are not understood by the compiler; they will be equal if they have the same representation in the underlying architecture.  So it could be the case that if values of two different types are constructed, and both are assigned to variables of type anytype, the resulting values could test as equal.  (Details of data representations are not described here.) So caution is advised before comparing `anytype` values.

[Potential improvement: prohibit equality comparison of anytypes.]

## Codeblocks

`{` *statement* * `}`

> A codeblock is a sequence of zero or more statements, enclosed in curly braces.  Codeblocks for the bodies of functions, `if` statements, and loops.  Local variables may be declared and used within a codeblock. A local variable that is declared within a codeblock can be used only within that same codeblock (or other codeblocks nested inside of it).

## Statements

`loop` *codeblock*

> An "infinite loop" which executes codeblock repeatedly. The only way to exit the loop is via a `return` statement (or a panic).

`while` ( *condition* ) *codeblock*

> Like a loop, except *condition*  (which must be an expression of type `bool`) is evaluated before each iteration, and the loop terminates if *condition* is found to be false.

`if` ( *condition* ) *codeblock*

`if` ( *condition* ) *codeblock* `else` *codeblock*

`if` ( *condition* ) *codeblock* `elseif` ( *condition* ) *codeblock*

`if` ( *condition* ) *codeblock* `elseif` ( *condition* ) *codeblock* `else` *codeblock*

> If statements, with the expected behavior.  You can use as many elseifs as you want.

`let` *name* = *expression* ;

> Create a new local variable and initialize it with the value of *expression*.  The compiler infers that the new variable has the same type as *expression* .  The variable goes out of scope and stops existing when execution leaves the current codeblock.

`let` ( *name1* , *name2*, ... ) = *expression* ;

> Create multiple new variables based on unpacking a tuple. *expression* must be a tuple type, with the number of fields in the tuple equal to the number of names on the left-hand side.  The compiler creates a new local variable for each name on the left-hand side, and infers the type of each new variable based on the type of the corresponding field of the right-hand side tuple.
>
> [Potential improvement: Allow left-hand side names to be replaced by `_`, allowing unneeded components to be discarded without needing to create a variable.]
>
> [Potential improvement: Allow assignment directly into existing variables, rather than requiring creation of new variables.  Would have done this already but couldn't figure out a clean syntax for it.]
>
> [Potential improvement: This could become a more general pattern-matching assignment mechanism.  Currently it pattern-matches only for a one-level tuple.]

*name* = *expression* ;

> Assign a new value to a variable, which might be a global variable, a local variable, or a function argument. The type of *expression* must be assignable to the variable's type.

`return` ;

> Return from the current function. This is an error unless the function's *returntype* is `void`.

`return` *expression* ;

> Return a value from the current function. The value of *expression* must be assignable to the function's *returntype*.

`panic` ;

> Generate an error condition.  The precise effect of this depends on the underlying AVM architecture.

;

> A null statement which does nothing.

## Expressions

There are many types of expressions, which we'll catalog here.   Operator precedence works roughly as expected.  Generally the operators listed earlier here have higher precedence.

Mini never automatically converts types to make an operation succeed.  Programmers will need to do explicit conversions. The compiler will report an error if a conversion would be necessary.

\- *expression*

> Unary minus. Defined only for type `int`, and produces an `int`. This will panic if the result is not expressible in the `int` datatype (that is, if the value of expression is `MaxNegInt`).

! *expression*

> Logical negation, defined only for type `bool`, produces a `bool`.

~ *expression*

> Bitwise negation.  Defined for numeric types and `bytes32`. Produces a result of the same type as the operand.

*expression* + *expression*

*expression* - *expression*

> Addition and subtraction.  Both operands must have the same numeric type, and the result is of that same type. This does 256-bit arithmetic and does not check for overflow or underflow.

*expression* * *expression*

*expression* / *expression*

*expression* % *expression*

> Multiplication, integer division, and modulo. Both operands must have the same numeric type, and the result is of that same type. Multiplication does 256-bit arithmetic and does not check for overflow or underflow.  Division and modulo panic if the second operand is zero.

*expression* < *expression*

*expression* > *expression*

*expression* <= *expression*

*expression* >= *expression*

> Numeric comparisons. Both operands must have the same numeric type. The result has type `bool`.

*expression* == *expression*

*expression* != *expression*

> Equality comparison, under the rules described above. The result has type `bool`.

*expression* & *expression*

*expression* ^ *expression*

*expression* | *expression*

> Bitwise and, xor, and or.  Both operands must have the same atomic type, and the result will have that same type.

*expression* && *expression*

*expression* || *expression*

> Logical and / or.  Both operands must have type `bool`, and the result has type `bool`.  Execution shortcuts, so that the second operand is evaluated only if the outcome is still in doubt after evaluating the first operand.

`uint`( *expression* )

`int`( *expression* )

`bytes32`( *expression* )

> Type conversions. The operand must be an atomic type. The result type is per the operator name.

`len` ( *expression* ) 

> Get the length of *expression*, whose value must be a non-fixed size array. (Panic if *expression* has any other type.) Result is a `uint`.

`hash` ( *expression* )

`hash` ( *expression* , *expression* )

> Compute the hash of the value(s).  The single-argument version can take a value of any type.  The two-argument version requires both arguments to be `bytes32`. Both produce a `bytes32`.

`struct` { *name1* : *expression1* , *name2* : *expression2* , ... }

> Create a new struct object. The types of the struct fields are inferred from the types of the expressions. Returns a struct value whose type is determined by the sequence of names and expression types given.

( *expression1* , *expression2*, ... )

> Create a new tuple value, whose type will be inferred from the number and types of the expressions.

`newarray` ( *expression* , *type* )

> Create a new array object. *expression*, which must have type `uint`, gives the size of the array, and *type* is the type of its elements. The contents of the array are initialized to the zero value if *type* is an atomic type, or uninitialized otherwise.

`newfixedarray` ( *size* )

> Create a new fixed-array of `anytype` elements. *size*, which must be a `uint` constant, is the size of the new array.

`newfixedarray` ( *size* , *expression* )

> Create a new fixed-size array of elements, with every slot initialized to the value of *expression*. *size*, which must be a `uint` constant, is the size of the new array. The element type is inferred from the type of *expression*.

`unsafecast` ( *expression* , *type* )

> Evaluate *expression*, and then treat the in-memory representation of the result as an object having type *type*. This is an unsafe operation.  It is most often used to convert a value of type `anytype` into a more specific type, when the programmer knows the real type of the value.  

*arrExpression* [ *indexExpression* ]

> Get an element of an array.  *arrExpression* must have type [ ]T or [N]T for some type T.  *indexExpression* must have type `uint`.  The access is bounds-checked, and this will panic at runtime if the index is outside the bounds of the array. The result has type T.

*expression* . *name*

> Access a field of a struct.  The type of *expression* must be a struct that has a field called *name*. The result has the type of that field.

*expression* . *number*

> Access a field of a tuple.  *number*, which must be a `uint`, specifies which field number to access.  *expression* must be a tuple type with more than *number* fields. The result has the type of that field.

*funcExpression* ( *argExpression1* , *argExpression2* , ... )

> Function call.  The value of *funcExpression* must be a function reference. (Typically *funcExpression* will just be the name of a function.) The number of *argExpressions* must be consistent with the number of arguments in *funcExpression*'s type, and each *argExpression* must be assignable to the type of the corresponding argument of *funcExpression*.  The result has the type of *funcExpression's* return value. (If the return type is void, then the result is the empty tuple ( ).)

arrayExpression with { [ indexExpression ] = valExpression }

> Create a new array by copying an existing array with one element modified.  *arrayExpression*, which must be an array type, specifies the array to start with. *indexExpression*, which must have type `uint`, specifies which slot in the array should be modified.  *valExpression*, whose type must be assignable to the element type of the array, is the new value to put into the slot.  The result has the same type as *arrayExpression*. If the index is out of bounds, this will cause either a compile-time error or a runtime panic.  

structExpression with { name : valExpression }

> Create a new struct by copying an existing struct with one field modified.  *structExpression*, which must be a struct type with a field called *name*, specifies the struct to start with.  *valExpression*, which must have a type assignable to the named field, is the value that will be assigned to the named field in the newly created struct. 

`false`

`true`

`null`

> Literal values. `false` and `true` are of type bool, and `null` is the empty tuple ( ).

*number*

> An integer constant, in decimal form. This will be interpreted as a `uint`, and it must be representable as a `uint`. If a literal number is followed by the single character 's', it is interpreted as a signed integer `int`; in this case it must be representable as an `int`.

*name*

> A reference to a local variable, a global variable, or a function. It will have the type of the referenced variable or function.

`asm` ( ) *type* { *instructions* }

`asm` ( *expression1* , *expression2* , ... ) *type* { *instructions* }

> Escape to assembly code.  The arguments (*expression1*, *expression2*, etc.), if any, are pushed onto the AVM stack (with *expression1* at the top of stack). Then the *instructions*, which are a sequence of AVM assembly instructions, are executed.  If *type* is `void`, the assembly instructions are assumed to consume the arguments and leave nothing on the stack. If *type* is non-`void`, the assembly instructions are assumed to consume the arguments and leave on the stack a single value of type *type*, which becomes the result of this expression.



